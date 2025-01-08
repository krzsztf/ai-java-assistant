(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [org.httpkit.client :as http])
  (:import [java.io File]))

(def ^:private java-std-packages
  #{"java." "javax." "sun." "com.sun." "lombok." "com.fasterxml.jackson."})

(defn- external-class?
  "Check if a class is external to project"
  [class-name project-package]
  (or (some #(str/starts-with? class-name %) java-std-packages)
      (and (not (str/blank? project-package))
           (not (str/starts-with? class-name project-package)))))

(defn- get-package-name
  "Extract package name from fully qualified class name"
  [class-name]
  (str/join "." (butlast (str/split class-name #"\."))))

(defn- group-external-deps
  "Group external dependencies by package"
  [deps]
  (->> deps
       (remove std-lib-class?)
       (group-by get-package-name)
       (map first)
       sort))

(def ^:private llm-config
  {:anthropic {:url "https://api.anthropic.com/v1/messages"
               :model "claude-3-opus-20240229"
               :cost {:input 0.015, :output 0.075}
               :api-key (System/getenv "ANTHROPIC_API_KEY")}
   :ollama {:url (str (or (System/getenv "OLLAMA_HOST") "http://localhost:11434") "/api/generate")
            :model (or (System/getenv "OLLAMA_MODEL") "qwen2.5-coder:7b")}})

(defn- estimate-tokens [text]
  (int (/ (count text) 4)))

(defn- call-llm
  "Call LLM API for analysis"
  [type prompt]
  (let [config (type llm-config)]
    (try 
      (case type
        :anthropic @(http/post (:url config)
                              {:headers {"x-api-key" (:api-key config)
                                       "anthropic-version" "2023-06-01"
                                       "content-type" "application/json"}
                               :body (json/write-str
                                     {:model (:model config)
                                      :max_tokens 4096
                                      :messages [{:role "user" :content prompt}]})})
        :ollama @(http/post (:url config)
                           {:headers {"Content-Type" "application/json"}
                            :body (json/write-str
                                   {:model (:model config)
                                    :prompt prompt})}))
      (catch Exception e
        (println "Failed to connect to" (name type) ":" (.getMessage e))
        nil))))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan" :validate
    [#(try (let [f (io/file %)] (and (.exists f) (.isDirectory f)))
           (catch Exception _ false)) "Must be a valid, accessible directory"]]
   ["-p" "--package PKG" "Project package prefix (e.g. com.example)" :default ""]]
   ["-a" "--analyze" "Submit dependency graph for AI analysis"]
   ["-l" "--llm MODEL" "LLM to use for analysis (anthropic or ollama)" :default
    "anthropic" :validate
    [#(contains? #{"anthropic" "ollama"} %)
     "Must be either 'anthropic' or 'ollama'"]]
   ["-r" "--reverse-deps" "Include reverse dependencies in analysis"]])

(defn find-java-files
  "Recursively find all .java files in the given directory"
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".java"))))

(defn parse-source
  "Extract package, class name and imports from Java source code"
  [content file]
  (let [package (when-let [m (re-find #"package\s+([^;]+);" content)]
                  (let [pkg (str/trim (second m))]
                    (println "Found package:" pkg)
                    pkg))
        class-name
          (if-let
            [m (re-find
                 #"(?:@\w+\s*)*(?:\w+\s+)*(?:class|interface|enum)\s+(\w+)"
                 content)]
            (second m)
            (str/replace (.getName file) #"\.java$" ""))
        imports (->> (re-seq #"import\s+(?:static\s+)?([^;]+);" content)
                     (map second)
                     (remove #(str/includes? % "*"))
                     (map str/trim)
                     ((fn [xs]
                        (when (seq xs)
                          (println "Found imports in" (.getName file) ":")
                          (doseq [x xs] (println "  -" x)))
                        xs))
                     set)]
    {:package package, :class-name class-name, :imports imports}))

(defn parse-java-file
  "Parse a Java file and extract its dependencies"
  [^File file]
  (try
    (let [{:keys [package class-name imports]} (parse-source (slurp file) file)]
      {:class (if package (str package "." class-name) class-name),
       :imports imports})
    (catch Exception e
      (println "Warning: Failed to parse" (.getPath file) "-" (.getMessage e))
      nil)))

(defn build-dependency-graph
  "Build dependency graph from parsed Java files"
  [parsed-files]
  (println "\nBuilding dependency map...")
  (let [classes (set (map :class parsed-files))
        _ (println "Found" (count classes) "unique classes")
        project-pkg (:package options)
        deps-map (reduce (fn [acc {:keys [class imports]}]
                           (let [filtered-imports (set (remove #(external-class? % project-pkg)
                                                         imports))]
                             (println "Class"
                                      class
                                      "depends on"
                                      (count filtered-imports)
                                      "project classes")
                             (assoc acc class filtered-imports)))
                   {}
                   parsed-files)
        reverse-deps (reduce (fn [acc [class deps]]
                               (reduce (fn [m dep]
                                         (update m dep (fnil conj #{}) class))
                                 acc
                                 deps))
                       {}
                       deps-map)]
    {:dependencies deps-map, :reverse-dependencies reverse-deps}))

(defn format-dependency-data
  "Format dependency data for API request"
  [{:keys [dependencies reverse-dependencies]}]
  (str/join "\n"
            (for [class (sort (keys dependencies))]
              (str "Class: "
                   class
                   "\n"
                   "  Dependencies: "
                   (str/join ", " (sort (get dependencies class)))
                   (when reverse-dependencies
                     (str "\n"
                          "  Used by: "
                          (str/join ", " (sort (get reverse-dependencies class)))))))))

(def ^:private refactoring-prompt
  "Analyze this Java project dependency graph and identify the top 3 most important, concrete refactoring improvements. For each:
1. Identify specific classes and their problematic dependency patterns
2. Explain why it's a problem (e.g., tight coupling, circular dependencies)
3. Suggest a specific, actionable solution

Focus only on the most critical issues that would give the biggest improvement in maintainability.

Dependency graph:

%s")

(defn get-refactoring-advice
  "Get refactoring advice using specified LLM"
  [dep-data llm]
  (let [prompt (format refactoring-prompt (format-dependency-data dep-data))
        llm-type (keyword llm)]
    (when-let [response (call-llm llm-type prompt)]
      (when (= 200 (:status response))
        (let [advice (case llm-type
                      :anthropic (get-in (json/read-str (:body response)) ["content" 0 "text"])
                      :ollama (get (json/read-str (:body response)) "response"))]
          (cond-> {:advice advice}
            (= :anthropic llm-type)
            (assoc :cost
                  (let [input-tokens (estimate-tokens prompt)
                        output-tokens (estimate-tokens advice)
                        costs (get-in llm-config [:anthropic :cost])]
                    {:input-tokens input-tokens
                     :output-tokens output-tokens
                     :total-cost (+ (* (:input costs) (/ input-tokens 1000))
                                   (* (:output costs) (/ output-tokens 1000)))}))))))))

(defn print-dependencies
  "Print dependency information for all classes"
  [{:keys [dependencies reverse-dependencies]}]
  (println "\nDependency Analysis Results:")
  (println "============================")
  (let [all-classes (sort (keys dependencies))]
    (doseq [class all-classes]
      (println "\nClass:" class)
      (let [all-deps (get dependencies class)
            project-deps (filter #(some #{(get-package-name %)} 
                                      (map get-package-name (keys dependencies))) 
                               all-deps)
            external-deps (group-external-deps all-deps)]
        (if (seq project-deps)
          (do 
            (println "  Project Dependencies:")
            (doseq [dep (sort project-deps)]
              (println "    →" dep)))
          (println "  Project Dependencies: none"))
        (if (seq external-deps)
          (do
            (println "  External Packages:")
            (doseq [pkg (sort external-deps)]
              (println "    →" pkg)))
          (println "  External Packages: none")))
      (when reverse-dependencies
        (if-let [rev-deps (seq (get reverse-dependencies class))]
          (do (println "  Used by:")
              (doseq [dep (sort rev-deps)] (println "    ←" dep)))
          (println "  Used by: none"))))))

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond errors (do (println "Errors:" errors) (System/exit 1))
          (nil? (:dir options))
            (do (println "Please specify directory with -d option\n" summary)
                (System/exit 1))
          :else
            (let [java-files (find-java-files (:dir options))
                  total-files (count java-files)
                  _ (println "Found" total-files "Java files to process...")
                  parsed-files
                    (->> java-files
                         (map-indexed
                           (fn [idx file]
                             (println "Processing file:" (.getPath file))
                             (when (zero? (mod (inc idx) 10))
                               (println "Progress:" (inc idx) "/" total-files))
                             (parse-java-file file)))
                         (keep identity))
                  _ (println "Successfully parsed"
                             (count parsed-files)
                             "files. Building graph...")
                  graph-data (build-dependency-graph parsed-files)
                  dep-graph (if (:reverse-deps options)
                             graph-data
                             (dissoc graph-data :reverse-dependencies))
                  _ (println "\nDependency Analysis Results:")]
              (print-dependencies dep-graph)
              (when (:analyze options)
                (if-let [{:keys [advice cost]}
                           (get-refactoring-advice dep-graph (:llm options))]
                  (do (println "\nRefactoring Suggestions from Claude:")
                      (println "================================")
                      (println advice)
                      (println "\nAPI Cost Information:")
                      (println "==================")
                      (printf "Input tokens: %d ($.%03d)\n"
                              (:input-tokens cost)
                              (int (* 1000
                                      (* (get-in llm-config
                                                 [:anthropic
                                                  :cost-per-1k-input-tokens])
                                         (/ (:input-tokens cost) 1000)))))
                      (printf "Output tokens: %d ($.%03d)\n"
                              (:output-tokens cost)
                              (int (* 1000
                                      (* (get-in llm-config
                                                 [:anthropic
                                                  :cost-per-1k-output-tokens])
                                         (/ (:output-tokens cost) 1000)))))
                      (when cost
                        (printf "Total cost: $.%03d\n"
                                (int (* 1000 (:total-cost cost))))))
                  (println "\nFailed to get analysis from" (:llm options))))))))
