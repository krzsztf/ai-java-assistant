(ns javadeps.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.github.javaparser StaticJavaParser]
           [com.github.javaparser.ast CompilationUnit]
           [java.io File]))

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
  "Extract package name from CompilationUnit"
  [^CompilationUnit cu]
  (when-let [package (.getPackageDeclaration cu)]
    (-> package
        (.get)
        (.getNameAsString))))

(defn extract-class-name
  "Extract primary class name from CompilationUnit"
  [^CompilationUnit cu file]
  (if-let [type-name (-> cu
                         (.getPrimaryType)
                         (.map #(.getNameAsString %))
                         (.orElse nil))]
    type-name
    ;; Fallback to file name without .java
    (str/replace (.getName file) #"\.java$" "")))

(defn extract-imports
  "Extract all imports from CompilationUnit"
  [^CompilationUnit cu]
  (->> (.getImports cu)
       (map #(.getNameAsString %))
       (set)))

(defn parse-java-file
  "Parse a Java file and extract its dependencies"
  [^File file]
  (try
    (let [cu (StaticJavaParser/parse file)
          package-name (extract-package-name cu)
          class-name (extract-class-name cu file)
          full-class-name (if package-name
                           (str package-name "." class-name)
                           class-name)]
      {:file file
       :class full-class-name
       :imports (extract-imports cu)})
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
