(ns javadeps.analyze
  "Core analysis functions for Java dependency scanning.
   This namespace contains the pure functional core of the application,
   handling parsing and analysis of Java source files."
  (:require
   [clojure.string :as string]))

;; Set of package prefixes that identify standard Java libraries and common external dependencies
;; The #{} syntax creates a set literal - sets are collections of unique values
(def ^:private java-util-packages
  #{"java." "javax." "sun." "com.sun." "lombok."})

(defn util-class?
  "Determines if a class is external to the project by checking:
   1. If it belongs to standard Java packages (like java.*, javax.*)
   2. If it doesn't match the project's package prefix
   
   Parameters:
   - class-name: Fully qualified Java class name (e.g., 'java.util.List')
   - project-package: The base package of the project (e.g., 'com.example')
   
   Returns true if the class is external, false if it's part of the project."
  [class-name project-package]
  (or
    ;; Check if class belongs to standard Java packages
    ;; some returns true if any item in java-ext-packages matches the predicate
    (some #(string/starts-with? class-name %) java-util-packages)))

(defn get-package-name
  "Extract package name from fully qualified class name"
  [class-name]
  (string/join "." (butlast (string/split class-name #"\."))))

(defn find-class-references
  "Find references to other classes in the source code.
   Looks for words that start with uppercase letters (likely class names).
   
   Parameters:
   - content: Source code content
   - class-name: Name of current class (to exclude self-references)
   
   Returns a set of class names found in the code."
  [content class-name]
  (->> (re-seq #"(?<=[(\s])[A-Z]\w*(?=[(\s])" content)  ; Find words starting with uppercase
       (remove #{"String" "Integer" "Boolean" "Double" "Float" "Object" "Class"}) ; Remove common Java types
       (remove #{class-name})  ; Remove self-references
       set))

(defn parse-source
  "Parses Java source code to extract key elements using regular expressions.
   
   Parameters:
   - content: String containing Java source code
   - filename: Name of the source file (used as fallback for class name)
   
   Returns a map containing:
   - :package - The Java package declaration
   - :class-name - Name of the primary class/interface/enum
   - :imports - Set of fully qualified import statements
   - :class-refs - Set of potential class references in the code
   
   Example:
   (parse-source \"package com.example;\nimport java.util.List;\" \"MyClass.java\")
   => {:package \"com.example\"
       :class-name \"MyClass\"
       :imports #{\"java.util.List\"}
       :class-refs #{\"OtherClass\"}}"
  [content filename]
  (let [;; Extract package declaration using regex
        ;; re-find returns first match, second gets the capture group
        package (when-let [m (re-find #"package\s+([^;]+);" content)]
                  (second m))
        ;; Extract class/interface/enum name, falling back to filename if not found
        ;; Complex regex handles annotations and modifiers before the type
        class-name (if-let [m (re-find #"(?:@\w+\s*)*(?:\w+\s+)*(?:class|interface|enum)\s+(\w+)" content)]
                     (second m)
                     (string/replace filename #"\.java$" ""))
        ;; Extract and process import statements
        imports (->> (re-seq #"import\s+(?:static\s+)?([^;]+);" content) ; Find all imports
                     (map (fn [[full imp]]         ; Handle static imports
                            (if (string/includes? full "static")
                              (string/join "." (butlast (string/split imp #"\.")))
                              imp)))
                     (remove #(string/includes? % "*")) ; Remove wildcard imports
                     (map string/trim)      ; Clean up whitespace
                     set)
        ;; Find potential class references in the code
        class-refs (find-class-references content class-name)]
    ;; Return map of parsed elements
    {:package package
     :class-name class-name
     :imports imports
     :class-refs class-refs}))

(defn build-dependency-graph
  "Constructs a bidirectional dependency graph from parsed Java files.
   
   Parameters:
   - parsed-files: Sequence of maps, each containing :class, :imports, and :class-refs
   - project-pkg: Base package name to filter external dependencies
   
   Returns a map with two keys:
   - :dependencies - Map of {class -> set of classes it depends on}
   - :reverse-dependencies - Map of {class -> set of classes that depend on it}
   
   Example:
   (build-dependency-graph 
     [{:class \"com.example.A\" 
       :imports #{\"com.example.B\"}
       :class-refs #{\"C\"}}]
     \"com.example\")
   => {:dependencies {\"com.example.A\" #{\"com.example.B\" \"com.example.C\"}}
       :reverse-dependencies {\"com.example.B\" #{\"com.example.A\"}}}"
  [parsed-files project-pkg]
  (let [;; Create map of simple class names to fully qualified names
        class-map (reduce (fn [acc {:keys [class]}]
                            (assoc acc
                                   (last (string/split class #"\."))
                                   class))
                    {}
                    parsed-files)
        ;; Build forward dependency map
        deps-map (reduce (fn [acc {:keys [class imports class-refs package]}]
                           ;; Filter out external dependencies and add to map
                           (let [;; Process explicit imports
                                 filtered-imports (->> imports
                                                       (remove #(util-class? % project-pkg))
                                                       (map #(if (string/starts-with? % project-pkg)
                                                               %
                                                               (get-package-name %)))
                                                       set)
                                 ;; Process class references, qualifying them with package if found in class-map
                                 package-deps (->> class-refs
                                                   (keep #(get class-map %))
                                                   set)]
                             (assoc acc class (into filtered-imports package-deps))))
                   {}  ; Start with empty map
                   parsed-files)
        ;; Build reverse dependency map
        reverse-deps (reduce (fn [acc [class deps]]
                             ;; For each dependency, add the current class as a reverse dep
                               (reduce (fn [m dep]
                                       ;; fnil provides empty set if key doesn't exist
                                         (update m dep (fnil conj #{}) class))
                                 acc
                                 deps))
                       {}  ; Start with empty map
                       deps-map)]
    ;; Return both dependency maps
    {:dependencies deps-map
     :reverse-dependencies reverse-deps}))

(defn format-dependency-data
  "Format dependency data for output"
  [{:keys [dependencies reverse-dependencies]}]
  (string/join "\n"
    (for [class (sort (keys dependencies))]
      (str "Class: " class "\n"
           "  Dependencies: " (string/join ", " (sort (get dependencies class)))
           (when reverse-dependencies
             (str "\n  Used by: " (string/join ", " (sort (get reverse-dependencies class)))))))))
