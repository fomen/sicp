(ns sicp.ch02-test
  (:require [clojure.test :refer :all]
            [sicp.ch02 :refer :all]))

(deftest equal-rat-test
  (testing "equal-rat-test"
    (is (equal-rat? (make-rat 1 3)
                    (make-rat 1 3)))))

(deftest add-rat-test
  (testing "add-rat-test"
    (is (equal-rat? (add-rat (make-rat 1 3)
                             (make-rat 2 3))
                    (make-rat 3 3)))))

(deftest my-reverse-test
  (testing "my-reverse-test"
    (is (= (my-reverse (list 1 2 3 4 5))
           (list 5 4 3 2 1)))))

(deftest my-reduce-reverse-test
  (testing "my-reduce-reverse-test"
    (is (= (my-reduce-reverse (list 5 4 3 2 1))
           (list 1 2 3 4 5)))))

(deftest my-reverse-reduce-reverse-test
  (testing "my-reverse-reduce-reverse-test"
    (let [coll (list 1 2 3 4 5 6 7)]
      (is (= (my-reverse coll)
             (my-reduce-reverse coll))))))

(deftest same-parity-test
  (testing "same-parity"
    (is (= (same-parity 2 2 4 6 7) (list 2 4 6)))
    (is (= (same-parity 1 3 4 5 6 7) (list 3 5 7)))))

(deftest full-reverse-test
  (testing "full-reverse"
    (is (= (full-reverse (list 1 2 3 (list 4 5 (list 6 7))))
           (list (list (list 7 6) 5 4) 3 2 1)))))

(deftest fringe-test
  (testing "fringe"
    (is (= (fringe (list 1 2)) (list 1 2)))
    (is (= (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4)))
    (is (= (fringe (list (list (list 1 2) 3 4) 5 6)) (list 1 2 3 4 5 6)))))

(deftest count-leaves2-test
  (testing "count-leaves-2"
    (is (= (count-leaves2 ()) 0))
    (is (= (count-leaves2 (list 1 2 3 (list 4 5))) 5))
    (is (= (count-leaves2 [1 2 3]) 3))
    (is (= (count-leaves2 [1 2 3 [4 5] [6 [7 8 9]]]) 9))))

(deftest count-leaves3-test
  (testing "count-leaves-3"
    (is (= (count-leaves3 ()) 0))
    (is (= (count-leaves3 (list 1 2 3 (list 4 5))) 5))
    (is (= (count-leaves3 [1 2 3]) 3))
    (is (= (count-leaves3 [1 2 3 [4 5] [6 [7 8 9]]]) 9))))

(deftest count-leaves4-test
  (testing "count-leaves-4"
    (is (= (count-leaves4 ()) 0))
    (is (= (count-leaves4 (list 1 2 3 (list 4 5))) 5))
    (is (= (count-leaves4 [1 2 3]) 3))
    (is (= (count-leaves4 [1 2 3 [4 5] [6 [7 8 9]]]) 9))))

(deftest square-tree-test
  (testing "square-tree"
    (is (= (square-tree ()) ()))
    (is (= (square-tree (list 1 2 3 4 5))
           (list 1 4 9 16 25)))
    (is (= (square-tree (list 1 2 3 (list 4 5)))
           (list 1 4 9 (list 16 25))))
    (is (= (square-tree (list (list (list 1 2 3) 4 5) 6 7))
           (list (list (list 1 4 9) 16 25) 36 49)))))

(deftest square-tree-map-test
  (testing "square-tree-map"
    (is (= (square-tree-map ()) ()))
    (is (= (square-tree-map (list 1 2 3 4 5))
           (list 1 4 9 16 25)))
    (is (= (square-tree-map (list 1 2 3 (list 4 5)))
           (list 1 4 9 (list 16 25))))
    (is (= (square-tree-map (list (list (list 1 2 3) 4 5) 6 7))
           (list (list (list 1 4 9) 16 25) 36 49)))))

(deftest my-map-test
  (testing "my-map"
    (is (= (my-map - (list 1 2 3 4 5)) (map - (list 1 2 3 4 5))))
    (is (= (my-map (fn [x] x) [1 2 3 4 5]) (map (fn [x] x) [1 2 3 4 5])))))

(deftest append-test
  (testing "append"
    (is (= (append (list 1 2 3) (list 4 5 6))
           (concat (list 1 2 3) (list 4 5 6))))
    (is (= (append () (list 1 2 3 4 5))
           (concat (list 1 2 3 4 5) ())))
    (is (= (append (append [1] [2]) [3])
           (concat [1] (concat [2] [3]))))))

(deftest my-length-test
  (testing "my-length"
    (let [coll (list 1 2 3 4 5)]
      (is (= (my-length coll)
             (count coll))))))

(deftest horner-eval-test
  (testing "horner-eval"
    (is (= (horner-eval 2 (list 1 3 0 5 0 1)) 79))))

(deftest count-leaves-test
  (testing "count-leaves"
    (is (= (count-leaves ()) 0))
    (is (= (count-leaves (list 1 2 3)) 3))
    (is (= (count-leaves [1 2 [3 [4 [5] 6 7] 8] 9]) 9))))

(deftest binary-search-test
  (testing "binary-search"
    (is (nil? (binary-search [] nil)))
    (is (nil? (binary-search (list 1 2 3) 0)))
    (is (= (binary-search (list 1 2 3 4 5) 1) 0))
    (is (= (binary-search [1 3 5 7] 7) 3))))

(deftest enumerate-interval-test
  (testing "enumerate-interval"
    (is (= (enumerate-interval 1 10)
           (range 1 11)))
    (is (= (enumerate-interval 1 1)
           (range 1 2)))))

(deftest my-eq?-test
  (testing "my-eq?"
    (is (true? (my-eq? () ())))
    (is (true? (my-eq? (list 1 2 3 4 5)
                       (list 1 2 3 4 5))))
    (is (false? (my-eq? [1 2 3] ())))
    (is (false? (my-eq? [1 2 3] (list 1 2 3 4 5))))
    (is (true? (my-eq? nil nil)))
    (is (true? (my-eq? 1 1)))))
