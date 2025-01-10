(ns javadeps.core-test
  (:require [clojure.test :refer :all]
            [javadeps.core :as core]
            [clojure.java.io :as io]))

(deftest parse-java-file-basic-test
  (let [temp-file (java.io.File/createTempFile "Test" ".java")]
    (try
      (spit
        temp-file
        "package com.example;
import java.util.List;
import java.util.Map;
import java.util.*;  // Should be ignored
public class Test {
}")
      (let [result (core/parse-java-file temp-file)]
        (is (= "com.example.Test" (:class result)))
        (is (= #{"java.util.List" "java.util.Map"} (:imports result)))
        (is (not (contains? (:imports result) "java.util.*"))))
      (finally (.delete temp-file)))))

(deftest parse-java-file-annotations-test
  (let [temp-file (java.io.File/createTempFile "AnnotatedTest" ".java")]
    (try
      (spit
        temp-file
        "package com.example;
import java.util.List;
import static java.util.Collections.emptyList;
@Deprecated
public abstract class Test<T> {
    private class Inner {}
}")
      (let [result (core/parse-java-file temp-file)]
        (is (= "com.example.Test" (:class result)))
        (is (= #{"java.util.List" "java.util.Collections"} (:imports result))))
      (finally (.delete temp-file)))))

(deftest parse-java-file-complex-test
  (let [temp-file (java.io.File/createTempFile "ComplexTest" ".java")]
    (try
      (spit
        temp-file
        "package com.example;
import java.util.List;
public abstract class Test<T> {
    private class Inner {}
}")
      (let [result (core/parse-java-file temp-file)]
        (is (= "com.example.Test" (:class result)))
        (is (= #{"java.util.List"} (:imports result))))
      (finally (.delete temp-file)))))
