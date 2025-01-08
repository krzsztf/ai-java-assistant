(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [org.httpkit.client :as http])
  (:import [java.io File]))

(def ^:private java-std-packages
  #{"java." "javax." "sun." "com.sun." "lombok." "com.fasterxml.jackson."})

(defn- std-lib-class?
  "Check if a class is from Java standard library"
  [class-name]
  (some #(str/starts-with? class-name %) java-std-packages))

(def ^:private llm-configs
  {:anthropic {:cost-per-1k-input-tokens 0.015
               :cost-per-1k-output-tokens 0.075
               :token-estimator (fn [text] (int (/ (count text) 4)))
               :api-key (System/getenv "ANTHROPIC_API_KEY")}
   :ollama {:url (or (System/getenv "OLLAMA_HOST") "http://localhost:11434")
            :model (or (System/getenv "OLLAMA_MODEL") "codellama")}})

(defn- call-ollama
  "Call Ollama API for analysis"
  [prompt]
  (try
    @(http/post (str (:url (:ollama llm-configs)) "/api/generate")
                {:headers {"Content-Type" "application/json"}
                 :body (json/write-str
                        {:model (:model (:ollama llm-configs))
                         :prompt prompt})})
    (catch Exception e
      (println "Failed to connect to Ollama:" (.getMessage e))
      nil)))

(defn- call-anthropic
  "Call Anthropic API for analysis"
  [prompt]
  @(http/post "https://api.anthropic.com/v1/messages"
              {:headers {"x-api-key" (:api-key (:anthropic llm-configs))
                        "anthropic-version" "2023-06-01"
                        "content-type" "application/json"}
               :body (json/write-str
                      {:model "claude-3-opus-20240229"
                       :max_tokens 4096
                       :messages [{:role "user"
                                 :content prompt}]})}))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan"
    :validate [#(try 
                  (let [f (io/file %)]
                    (and (.exists f) (.isDirectory f)))
                  (catch Exception _ false))
              "Must be a valid, accessible directory"]]
   ["-a" "--analyze" "Submit dependency graph for AI analysis"]
   ["-l" "--llm MODEL" "LLM to use for analysis (anthropic or ollama)"
    :default "anthropic"
    :validate [#(contains? #{"anthropic" "ollama"} %) "Must be either 'anthropic' or 'ollama'"]]]) 

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
        class-name (if-let [m (re-find #"(?:@\w+\s*)*(?:\w+\s+)*(?:class|interface|enum)\s+(\w+)" content)]
                    (second m)
                    (str/replace (.getName file) #"\.java$" ""))
        imports (->> (re-seq #"import\s+(?:static\s+)?([^;]+);" content)
                    (map second)
                    (remove #(str/includes? % "*"))
                    (map str/trim)
                    (do 
                      (fn [xs]
                        (when (seq xs)
                          (println "Found imports in" (.getName file) ":")
                          (doseq [x xs]
                            (println "  -" x)))
                        xs))
                    ((fn [xs]
                       (when (seq xs)
                         (println "Found imports in" (.getName file) ":")
                         (doseq [x xs]
                           (println "  -" x)))
                       xs))
                    set)]
    {:package package
     :class-name class-name
     :imports imports}))

(defn parse-java-file
  "Parse a Java file and extract its dependencies"
  [^File file]
  (try
    (let [{:keys [package class-name imports]} (parse-source (slurp file) file)]
      {:class (if package (str package "." class-name) class-name)
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
        deps-map (reduce (fn [acc {:keys [class imports]}]
                          (let [filtered-imports (set (remove std-lib-class? imports))]
                            (println "Class" class "depends on" (count filtered-imports) "project classes")
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
    {:dependencies deps-map
     :reverse-dependencies reverse-deps}))

(defn format-dependency-data
  "Format dependency data for API request"
  [{:keys [dependencies reverse-dependencies]}]
  (str/join "\n" 
    (for [class (sort (keys dependencies))]
      (str "Class: " class "\n"
           "  Dependencies: " (str/join ", " (sort (get dependencies class))) "\n"
           "  Used by: " (str/join ", " (sort (get reverse-dependencies class)))))))

(defn get-refactoring-advice
  "Get refactoring advice using specified LLM"
  [dep-data llm]
  (let [prompt (str "Analyze this Java project dependency graph and identify the top 3 most important, concrete refactoring improvements. For each:\n"
                    "1. Identify specific classes and their problematic dependency patterns\n"
                    "2. Explain why it's a problem (e.g., tight coupling, circular dependencies)\n"
                    "3. Suggest a specific, actionable solution\n\n"
                    "Focus only on the most critical issues that would give the biggest improvement in maintainability.\n\n"
                    "Dependency graph:\n\n"
                    (format-dependency-data dep-data))]
    (case llm
      "anthropic"
      (when-let [api-key (:api-key (:anthropic llm-configs))]
        (let [response (call-anthropic prompt)]
          (if (= 200 (:status response))
            (let [response-body (json/read-str (:body response))
                  advice (get-in response-body ["content" 0 "text"])
                  config (:anthropic llm-configs)
                  input-tokens ((:token-estimator config) (format-dependency-data dep-data))
                  output-tokens ((:token-estimator config) advice)
                  input-cost (* (:cost-per-1k-input-tokens config) (/ input-tokens 1000))
                  output-cost (* (:cost-per-1k-output-tokens config) (/ output-tokens 1000))
                  total-cost (+ input-cost output-cost)]
              {:advice advice
               :cost {:input-tokens input-tokens
                     :output-tokens output-tokens
                     :total-cost total-cost}})
            (println "Error getting refactoring advice:" (:status response) (:body response)))))
      
      "ollama"
      (when-let [response (call-ollama prompt)]
        (if (= 200 (:status response))
          {:advice (-> response :body json/read-str (get "response"))}
          (println "Error getting refactoring advice:" (:status response) (:body response)))))))

(defn print-dependencies
  "Print dependency information for all classes"
  [{:keys [dependencies reverse-dependencies]}]
  (println "\nDependency Analysis Results:")
  (println "============================")
  (let [all-classes (sort (keys dependencies))]
    (doseq [class all-classes]
      (println "\nClass:" class)
      (if-let [deps (seq (remove std-lib-class? (get dependencies class)))]
        (do
          (println "  Dependencies:")
          (doseq [dep (sort deps)]
            (println "    →" dep)))
        (println "  Dependencies: none"))
      (if-let [rev-deps (seq (get reverse-dependencies class))]
        (do
          (println "  Used by:")
          (doseq [dep (sort rev-deps)]
            (println "    ←" dep)))
        (println "  Used by: none")))))

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors (do (println "Errors:" errors)
                (System/exit 1))
      (nil? (:dir options)) (do (println "Please specify directory with -d option\n" summary)
                               (System/exit 1))
      :else (let [java-files (find-java-files (:dir options))
                  total-files (count java-files)
                  _ (println "Found" total-files "Java files to process...")
                  parsed-files (->> java-files
                                  (map-indexed 
                                    (fn [idx file]
                                      (println "Processing file:" (.getPath file))
                                      (when (zero? (mod (inc idx) 10))
                                        (println "Progress:" (inc idx) "/" total-files))
                                      (parse-java-file file)))
                                  (keep identity))
                  _ (println "Successfully parsed" (count parsed-files) "files. Building graph...")
                  dep-graph (build-dependency-graph parsed-files)
                  _ (println "\nDependency Analysis Results:")]
              (print-dependencies dep-graph)
              (when (:analyze options)
                (if-let [{:keys [advice cost]} (get-refactoring-advice dep-graph (:llm options))]
                    (do
                      (println "\nRefactoring Suggestions from Claude:")
                      (println "================================")
                      (println advice)
                      (println "\nAPI Cost Information:")
                      (println "==================")
                      (printf "Input tokens: %d ($.%03d)\n" 
                             (:input-tokens cost)
                             (int (* 1000 (* claude-cost-per-1k-input-tokens (/ (:input-tokens cost) 1000)))))
                      (printf "Output tokens: %d ($.%03d)\n"
                             (:output-tokens cost)
                             (int (* 1000 (* claude-cost-per-1k-output-tokens (/ (:output-tokens cost) 1000)))))
                      (when cost
                        (printf "Total cost: $.%03d\n"
                               (int (* 1000 (:total-cost cost))))))
                    (println "\nFailed to get analysis from" (:llm options)))))))
