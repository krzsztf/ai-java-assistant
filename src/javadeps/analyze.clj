(ns javadeps.analyze
  (:require [clojure.string :as str]))

(def ^:private java-std-packages
  #{"java." "javax." "sun." "com.sun." "lombok." "com.fasterxml.jackson."})

(defn external-class?
  "Check if a class is external to project"
  [class-name project-package]
  (or (some #(str/starts-with? class-name %) java-std-packages)
      (and (not (str/blank? project-package))
           (not (str/starts-with? class-name project-package)))))

(defn get-package-name
  "Extract package name from fully qualified class name"
  [class-name]
  (str/join "." (butlast (str/split class-name #"\."))))

(defn parse-source
  "Extract package, class name and imports from Java source code"
  [content filename]
  (let [package (when-let [m (re-find #"package\s+([^;]+);" content)]
                  (second m))
        class-name (if-let [m (re-find #"(?:@\w+\s*)*(?:\w+\s+)*(?:class|interface|enum)\s+(\w+)" content)]
                    (second m)
                    (str/replace filename #"\.java$" ""))
        imports (->> (re-seq #"import\s+(?:static\s+)?([^;]+);" content)
                    (map second)
                    (remove #(str/includes? % "*"))
                    (map str/trim)
                    set)]
    {:package package
     :class-name class-name 
     :imports imports}))

(defn build-dependency-graph
  "Build dependency graph from parsed Java files"
  [parsed-files project-pkg]
  (let [deps-map (reduce (fn [acc {:keys [class imports]}]
                          (let [filtered-imports (set (remove #(external-class? % project-pkg) imports))]
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
  "Format dependency data for output"
  [{:keys [dependencies reverse-dependencies]}]
  (str/join "\n"
            (for [class (sort (keys dependencies))]
              (str "Class: " class "\n"
                   "  Dependencies: " (str/join ", " (sort (get dependencies class)))
                   (when reverse-dependencies
                     (str "\n  Used by: " (str/join ", " (sort (get reverse-dependencies class)))))))))
