(ns javadeps.analyze
  "Core analysis functions for Java dependency scanning.
   This namespace contains the pure functional core of the application,
   handling parsing and analysis of Java source files."
  (:require [clojure.string :as str])) ; str provides string manipulation functions

;; Set of package prefixes that identify standard Java libraries and common external dependencies
;; The #{} syntax creates a set literal - sets are collections of unique values
(def ^:private java-std-packages
  #{"java." "javax." "sun." "com.sun." "lombok." "com.fasterxml.jackson."})

(defn external-class?
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
   ;; some returns true if any item in java-std-packages matches the predicate
   (some #(str/starts-with? class-name %) java-std-packages)
   
   ;; Check if class is outside project package (when project package is specified)
   (and (not (str/blank? project-package))
        (not (str/starts-with? class-name project-package)))))

(defn get-package-name
  "Extract package name from fully qualified class name"
  [class-name]
  (str/join "." (butlast (str/split class-name #"\."))))

(defn parse-source
  "Parses Java source code to extract key elements using regular expressions.
   
   Parameters:
   - content: String containing Java source code
   - filename: Name of the source file (used as fallback for class name)
   
   Returns a map containing:
   - :package - The Java package declaration
   - :class-name - Name of the primary class/interface/enum
   - :imports - Set of fully qualified import statements
   
   Example:
   (parse-source \"package com.example;\nimport java.util.List;\" \"MyClass.java\")
   => {:package \"com.example\"
       :class-name \"MyClass\"
       :imports #{\"java.util.List\"}}"
  [content filename]
  (let [;; Extract package declaration using regex
        ;; re-find returns first match, second gets the capture group
        package (when-let [m (re-find #"package\s+([^;]+);" content)]
                  (second m))
        
        ;; Extract class/interface/enum name, falling back to filename if not found
        ;; Complex regex handles annotations and modifiers before the type
        class-name (if-let [m (re-find #"(?:@\w+\s*)*(?:\w+\s+)*(?:class|interface|enum)\s+(\w+)" content)]
                    (second m)
                    (str/replace filename #"\.java$" ""))
        
        ;; Extract and process import statements
        imports (->> (re-seq #"import\s+(?:static\s+)?([^;]+);" content) ; Find all imports
                    (map second)           ; Get capture group from each match
                    (remove #(str/includes? % "*")) ; Remove wildcard imports
                    (map str/trim)         ; Clean up whitespace
                    set)]                  ; Convert to set for uniqueness
    
    ;; Return map of parsed elements
    {:package package
     :class-name class-name 
     :imports imports}))

(defn build-dependency-graph
  "Constructs a bidirectional dependency graph from parsed Java files.
   
   Parameters:
   - parsed-files: Sequence of maps, each containing :class and :imports
   - project-pkg: Base package name to filter external dependencies
   
   Returns a map with two keys:
   - :dependencies - Map of {class -> set of classes it depends on}
   - :reverse-dependencies - Map of {class -> set of classes that depend on it}
   
   Example:
   (build-dependency-graph 
     [{:class \"com.example.A\" :imports #{\"com.example.B\"}}]
     \"com.example\")
   => {:dependencies {\"com.example.A\" #{\"com.example.B\"}}
       :reverse-dependencies {\"com.example.B\" #{\"com.example.A\"}}}"
  [parsed-files project-pkg]
  (let [;; Build forward dependency map
        deps-map (reduce (fn [acc {:keys [class imports]}]
                          ;; Filter out external dependencies and add to map
                          (let [filtered-imports (set (remove #(external-class? % project-pkg) imports))]
                            (assoc acc class filtered-imports)))
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
  (str/join "\n"
            (for [class (sort (keys dependencies))]
              (str "Class: " class "\n"
                   "  Dependencies: " (str/join ", " (sort (get dependencies class)))
                   (when reverse-dependencies
                     (str "\n  Used by: " (str/join ", " (sort (get reverse-dependencies class)))))))))
