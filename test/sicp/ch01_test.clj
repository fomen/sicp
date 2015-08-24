(ns sicp.ch01-test
  (:require [clojure.test :refer :all]
            [sicp.ch01 :refer :all]))

(deftest my-sum-sq-test
  (testing "my-sum-sq"
    (is (= (my-sum-sq 0 0) 0))
    (is (= (my-sum-sq 1 3) 10))
    (is (= (my-sum-sq 2 3) 13))))

(deftest my-sum-sq-max-test
  (testing "my-sum-sq-max"
    (is (= (my-sum-sq-max 1 2 3) 13))))
