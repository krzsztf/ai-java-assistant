(ns javadeps.analyze-test
  (:require
   [clojure.test :refer :all]
   [javadeps.analyze :as analyze]))

(deftest test-external-class?
  (testing "Standard Java packages are external"
    (is (analyze/external-class? "java.util.List" "com.example"))
    (is (analyze/external-class? "javax.swing.JFrame" "com.example"))
    (is (analyze/external-class? "sun.misc.Signal" "com.example")))
  (testing "Project packages are not external"
    (is (not (analyze/external-class? "com.example.MyClass" "com.example")))
    (is (not (analyze/external-class? "com.example.sub.MyClass" "com.example"))))
  (testing "Other packages are external when project package is specified"
    (is (analyze/external-class? "org.apache.commons.Lang" "com.example")))
  (testing "No packages are external when project package is blank"
    (is (not (analyze/external-class? "org.apache.commons.Lang" "")))
    (is (not (analyze/external-class? "com.example.MyClass" "")))))

(deftest test-get-package-name
  (testing "Extracts package name from fully qualified class name"
    (is (= "java.util" (analyze/get-package-name "java.util.List")))
    (is (= "com.example.sub" (analyze/get-package-name "com.example.sub.MyClass")))
    (is (= "" (analyze/get-package-name "MyClass")))))

(deftest test-find-class-references
  (testing "Finds class references in Java code"
    (let [code "class MyClass {
                  private OtherClass field;
                  public void method(ThirdClass param) {
                    String str = \"FakeClass\";
                    Integer count = 0;
                  }
                }"
          refs (analyze/find-class-references code "MyClass")]
      (is (= #{"OtherClass" "ThirdClass"} refs))
      (is (not (contains? refs "String")))
      (is (not (contains? refs "Integer")))
      (is (not (contains? refs "MyClass")))
      (is (not (contains? refs "FakeClass")))))
  (testing "Handles empty input"
    (is (empty? (analyze/find-class-references "" "MyClass")))))

(deftest test-parse-source
  (testing "Parses Java source with package and imports"
    (let [content "package com.example;
                   import java.util.List;
                   import com.other.Thing;
                   
                   public class MyClass {
                     private OtherClass field;
                   }"
          result (analyze/parse-source content "MyClass.java")]
      (is (= "com.example" (:package result)))
      (is (= "MyClass" (:class-name result)))
      (is (= #{"java.util.List" "com.other.Thing"} (:imports result)))
      (is (= #{"OtherClass"} (:class-refs result)))))
  (testing "Handles source without package declaration"
    (let [result (analyze/parse-source "public class Test {}" "Test.java")]
      (is (nil? (:package result)))
      (is (= "Test" (:class-name result)))
      (is (empty? (:imports result)))
      (is (empty? (:class-refs result)))))
  (testing "Falls back to filename for class name"
    (let [result (analyze/parse-source "// Just a comment" "Simple.java")]
      (is (= "Simple" (:class-name result))))))

(deftest test-build-dependency-graph
  (testing "Builds dependency graph with imports and class references"
    (let [parsed-files [{:class "com.example.A"
                         :package "com.example"
                         :imports #{"java.util.List" "com.example.B"}
                         :class-refs #{"C"}}
                        {:class "com.example.B"
                         :package "com.example"
                         :imports #{}
                         :class-refs #{"D"}}
                        {:class "com.example.C"
                         :package "com.example"
                         :imports #{}
                         :class-refs #{}}
                        {:class "com.example.D"
                         :package "com.example"
                         :imports #{}
                         :class-refs #{}}]
          result (analyze/build-dependency-graph parsed-files "com.example")]
      (testing "Forward dependencies"
        (is (= #{"com.example.B" "com.example.C"}
               (get-in result [:dependencies "com.example.A"])))
        (is (= #{"com.example.D"}
               (get-in result [:dependencies "com.example.B"]))))
      (testing "Reverse dependencies"
        (is (= #{"com.example.A"}
               (get-in result [:reverse-dependencies "com.example.B"])))
        (is (= #{"com.example.B"}
               (get-in result [:reverse-dependencies "com.example.D"])))))))

(deftest test-format-dependency-data
  (testing "Formats dependency data with both forward and reverse deps"
    (let [data {:dependencies {"com.example.A" #{"com.example.B"}
                               "com.example.B" #{}}
                :reverse-dependencies {"com.example.B" #{"com.example.A"}}}
          result (analyze/format-dependency-data data)]
      (is (string? result))
      (is (.contains result "Class: com.example.A"))
      (is (.contains result "Dependencies: com.example.B"))
      (is (.contains result "Used by: com.example.A"))))
  (testing "Handles empty dependency data"
    (let [data {:dependencies {} :reverse-dependencies {}}]
      (is (= "" (analyze/format-dependency-data data))))))
