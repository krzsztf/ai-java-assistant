(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(def cli-options
  [["-d" "--dir DIR" "Directory to scan"
    :validate [#(try 
                  (let [f (io/file %)]
                    (and (.exists f) (.isDirectory f)))
                  (catch Exception _ false))
              "Must be a valid, accessible directory"]]])

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
  (let [class-map (into {} (map (juxt :class identity) parsed-files))
        _ (println "Found" (count class-map) "unique classes")
        deps-map (reduce (fn [acc {:keys [class imports]}]
                          (let [filtered-imports (set (filter #(get class-map %) imports))]
                            (println "Class" class "depends on" (count filtered-imports) "classes")
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

(defn print-dependencies
  "Print dependency information for all classes"
  [{:keys [dependencies reverse-dependencies]}]
  (println "\nDependency Analysis Results:")
  (println "============================")
  (let [all-classes (sort (distinct (concat (keys dependencies)
                                          (keys reverse-dependencies))))]
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
              (print-dependencies dep-graph)))))
