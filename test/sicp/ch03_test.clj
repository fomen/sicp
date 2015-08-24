(ns sicp.ch03-test
  (:require [clojure.test :refer :all]
            [sicp.ch03 :refer :all]))

(deftest make-accumulator-test
  (testing "make-accumulator"
    (let [a (make-accumulator -1)]
      (is (= (a 0) -1))
      (is (= (a 1) 0))
      (is (= (a 1) 1))
      (is (= (a 19) 20))
      (is (= (a 80) 100))
      (is (= (a -100) 0)))))

(deftest make-monitored-test
  (testing "make-monitored"
    (let [f (fn [x] x)
          m (make-monitored f)]
      (is (= (m :how-many-calls) 0))
      (is (= (m 1) 1))
      (is (= (m :how-many-calls) 1))
      (is (= (m 2) 2))
      (is (= (m 3) 3))
      (is (= (m :how-many-calls) 3))
      (is (= (m :reset-count) 0))
      (is (= (m :how-many-calls) 0)))))

(deftest make-monitored-generic-test
  (testing "make-monitored-generic"
    (let [f (fn [] nil)
          m (make-monitored-generic f)]
      (is (= (m :how-many-calls) 0))
      (is (nil? (m)))
      (is (= (m :how-many-calls) 1))
      (is (= (m :reset-count) 0)))
    (let [f (fn [x y] (+ x y))
          m (make-monitored-generic f)]
      (is (= (m :how-many-calls) 0))
      (is (= (m 1 2) 3))
      (is (= (m 0 0) 0))
      (is (= (m -1 1) 0))
      (is (= (m :how-many-calls) 3))
      (is (= (m :reset-count) 0)))))
