(ns javadeps.core
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [javadeps.analyze :as analyze]
   [javadeps.llm :as llm])
  (:import
   (java.io File)))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan" :validate
    [#(try (let [f (io/file %)] (and (.exists f) (.isDirectory f)))
           (catch Exception _ false)) "Must be a valid, accessible directory"]]
   ["-p" "--package PKG" "Project package prefix (e.g. com.example)"]
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

(defn print-dependencies
  "Print dependency information for all classes"
  [dep-data]
  (println "\nDependency Analysis Results:")
  (println "============================")
  (println (analyze/format-dependency-data dep-data)))

(defn find-java-files
  "Recursively find all .java files in the given directory"
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".java"))))

(defn parse-java-file
  "Parse a Java file and extract its dependencies"
  [^File file]
  (try
    (let [{:keys [package class-name imports] :as analysis} (analyze/parse-source (slurp file) (.getName file))]
      (assoc analysis :class (if package (str package "." class-name) class-name)))
    (catch Exception e
      (println "Warning: Failed to parse" (.getPath file) "-" (.getMessage e))
      nil)))

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      (do (println "Errors:" errors)
          (System/exit 1))
      (nil? (:dir options))
      (do (println "Please specify directory with -d option\n" summary)
          (System/exit 1))
      (nil? (:package options))
      (do (println "Please specify package with -p option\n" summary)
          (System/exit 1))
      :else
      (let [java-files (find-java-files (:dir options))
            _ (println "Found" (count java-files) "Java files to process...")
            parsed-files (->> java-files
                           (map parse-java-file)
                           (keep identity))
            _ (println "Successfully parsed" (count parsed-files) "files")
            graph-data (analyze/build-dependency-graph parsed-files (:package options))
            dep-graph (if (:reverse-deps options)
                        graph-data
                        (dissoc graph-data :reverse-dependencies))
            formatted-data (analyze/format-dependency-data dep-graph)]
        (println "\nDependency Analysis Results:")
        (println "============================")
        (println formatted-data)
        (when (:analyze options)
          (if-let [{:keys [advice cost]} (llm/get-refactoring-advice
                                           formatted-data
                                           (:llm options))]
            (do
              (println "\nRefactoring Suggestions:")
              (println "=======================")
              (println advice)
              (when cost
                (println "\nAPI Cost Information:")
                (println "====================")
                (printf "Input tokens: %d ($.%03d)\n"
                        (:input-tokens cost)
                        (int (* 1000 (/ (:input-tokens cost) 1000))))
                (printf "Output tokens: %d ($.%03d)\n"
                        (:output-tokens cost)
                        (int (* 1000 (/ (:output-tokens cost) 1000))))
                (printf "Total cost: $.%03d\n"
                        (int (* 1000 (:total-cost cost))))))
            (println "\nFailed to get analysis from" (:llm options))))))))
