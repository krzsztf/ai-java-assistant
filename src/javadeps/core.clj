(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [org.httpkit.client :as http])
  (:import [java.io File]))

(def ^:private anthropic-api-key (System/getenv "ANTHROPIC_API_KEY"))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan"
    :validate [#(try 
                  (let [f (io/file %)]
                    (and (.exists f) (.isDirectory f)))
                  (catch Exception _ false))
              "Must be a valid, accessible directory"]]
   ["-a" "--analyze" "Submit dependency graph to Anthropic API for analysis"]])

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
                          (let [filtered-imports (set (filter #(str/starts-with? % (first (str/split class #"\."))) imports))]
                            (println "Class" class "depends on" (count imports) "classes")
                            (assoc acc class imports)))
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
  "Send dependency data to Anthropic API and get refactoring advice"
  [dep-data]
  (when anthropic-api-key
    (let [response @(http/post "https://api.anthropic.com/v1/messages"
                              {:headers {"x-api-key" anthropic-api-key
                                       "anthropic-version" "2023-06-01"
                                       "content-type" "application/json"}
                               :body (json/write-str
                                     {:model "claude-3-opus-20240229"
                                      :max_tokens 4096
                                      :messages [{:role "user"
                                                :content (str "Analyze this Java project dependency graph and suggest potential refactoring improvements:\n\n"
                                                            (format-dependency-data dep-data))}]})})]
      (if (= 200 (:status response))
        (-> response
            :body
            json/read-str
            (get-in ["content" 0 "text"]))
        (println "Error getting refactoring advice:" (:status response) (:body response))))))

(defn print-dependencies
  "Print dependency information for all classes"
  [{:keys [dependencies reverse-dependencies]}]
  (println "\nDependency Analysis Results:")
  (println "============================")
  (let [all-classes (sort (keys dependencies))]
    (doseq [class all-classes]
      (println "\nClass:" class)
      (if-let [deps (seq (get dependencies class))]
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
                (if anthropic-api-key
                  (if-let [advice (get-refactoring-advice dep-graph)]
                    (do
                      (println "\nRefactoring Suggestions from Claude:")
                      (println "================================")
                      (println advice))
                    (println "\nFailed to get analysis from Anthropic API"))
                  (println "\nError: ANTHROPIC_API_KEY environment variable not set")))))))
