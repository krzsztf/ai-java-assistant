(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan"
    :validate [#(.isDirectory (io/file %)) "Must be a valid directory"]]])

(defn find-java-files
  "Recursively find all .java files in the given directory"
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".java"))))

(defn extract-package-name
  "Extract package name from Java source code"
  [content]
  (when-let [package-match (re-find #"package\s+([^;]+);" content)]
    (str/trim (second package-match))))

(defn extract-class-name
  "Extract class name from Java source code or fallback to file name"
  [content file]
  (if-let [class-match (re-find #"(?:public\s+)?(?:class|interface|enum)\s+(\w+)" content)]
    (second class-match)
    (str/replace (.getName file) #"\.java$" "")))

(defn extract-imports
  "Extract all imports from Java source code"
  [content]
  (->> (re-seq #"import\s+([^;]+);" content)
       (map (comp str/trim second))
       (remove #(str/includes? % "*"))
       set))

(defn parse-java-file
  "Parse a Java file and extract its dependencies"
  [^File file]
  (try
    (let [content (slurp file)
          package-name (extract-package-name content)
          class-name (extract-class-name content file)
          full-class-name (if package-name
                           (str package-name "." class-name)
                           class-name)]
      {:file file
       :class full-class-name
       :imports (extract-imports content)})
    (catch Exception e
      (println "Warning: Failed to parse" (.getPath file) "-" (.getMessage e))
      nil)))

(defn build-dependency-graph
  "Build dependency graph from parsed Java files"
  [parsed-files]
  (let [class-map (into {} (map (juxt :class identity) parsed-files))
        deps-map (reduce (fn [acc {:keys [class imports]}]
                          (assoc acc class
                                 (set (filter #(get class-map %) imports))))
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

(defn print-dependencies
  "Print dependency information for all classes"
  [{:keys [dependencies reverse-dependencies]}]
  (doseq [class (sort (keys dependencies))]
    (println "\nClass:" class)
    (when-let [deps (seq (get dependencies class))]
      (println "  Depends on:")
      (doseq [dep (sort deps)]
        (println "    -" dep)))
    (when-let [rev-deps (seq (get reverse-dependencies class))]
      (println "  Is depended on by:")
      (doseq [dep (sort rev-deps)]
        (println "    -" dep)))))

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors (do (println "Errors:" errors)
                (System/exit 1))
      (nil? (:dir options)) (do (println "Please specify directory with -d option\n" summary)
                               (System/exit 1))
      :else (let [java-files (find-java-files (:dir options))
                  parsed-files (keep parse-java-file java-files)
                  dep-graph (build-dependency-graph parsed-files)]
              (print-dependencies dep-graph)))))