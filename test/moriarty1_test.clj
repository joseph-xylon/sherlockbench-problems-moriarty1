(ns moriarty1-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [moriarty1 :as moriarty]))

(deftest complex-sequence-generator-test
  (let [func (:function (first moriarty/problems))]
    (testing "base cases"
      (is (= 0 (func 0)))
      (is (= 1 (func 2))))
    
    (testing "non-verification cases"
      (is (= 1 (func 3)))
      (is (= 1 (func 4)))
      (is (= 3 (func 6)))
      (is (= 10922 (func 16)))
      (is (= 18724 (func 18))))))

(deftest quantum-state-analyzer-test
  (let [func (:function (nth moriarty/problems 1))]
    (testing "non-verification cases"
      (is (= 36 (func 3 2 1)))
      (is (= 13 (func 6 1 4)))
      (is (= 36 (func 3 3 3)))
      (is (= -32 (func -2 2 3)))
      (is (= -31 (func -3 -3 -3))))))

(deftest planetary-zones-test
  (let [func (:function (nth moriarty/problems 2))]
    (testing "non-verification cases"
      (is (= "mercury" (func 1 1)))
      (is (= "venus" (func 5 5)))
      (is (= "mars" (func 8 13)))
      (is (= "mars" (func 15 8)))
      (is (= "fire" (func -3 -1)))
      (is (= "water" (func -7 -7)))
      (is (= "uranus" (func -10 8))))))

(deftest elemental-phase-analyzer-test
  (let [func (:function (nth moriarty/problems 3))]
    (testing "non-verification cases"
      (is (= "Liquid" (func 90 "C")))
      (is (= "Liquid" (func 100 "F")))
      (is (= "Solid" (func 1000 "K")))
      (is (= "Solid" (func -10 "C")))
      (is (= "Liquid" (func 500 "F"))))
    
    (testing "error cases"
      (is (= "Invalid scale - must be C, F, K, or R" (func 100 "Y"))))))

(deftest multi-layered-cipher-system-test
  (let [func (:function (nth moriarty/problems 4))]
    (testing "non-verification cases"
      (is (= "fgyoj" (func "basic" 5)))
      (is (= "ZQD" (func "BOX" 2)))
      (is (= "edcbghijonmlqrs" (func "abcdefghijklmno" 1)))
      (is (= "yaxpabowwk" (func "programmer" 9)))
      (is (= "ettu" (func "test" 0))))))

(defn run-all-tests []
  (run-tests 'moriarty1-test))
