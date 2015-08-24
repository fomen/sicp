(ns sicp.ch02
  (:use sicp.ch01))

; rat

(defn make-rat [n d]
  (list n d))

(defn numer [x]
  (first x))

(defn denom [x]
  (second x))

(defn print-rat [x]
  (println (str (numer x) "/" (denom x))))

(defn add-rat [x y]
  (make-rat
    (+
      (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat
    (-
      (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (=
    (* (numer x) (denom y))
    (* (numer y) (denom x))))

; reverse ex. 2.18

(defn my-reverse [coll]
  (loop [c coll r ()]
    (if (empty? c)
      r
      (recur (rest c) (conj r (first c))))))

(defn my-reduce-reverse [coll]
  (reduce conj () coll))

; same parity ex. 2.20

(defn same-parity [f & coll]
  (loop [r coll res ()]
    (let [ff (first r)]
      (if (empty? r)
        res
        (recur
          (rest r)
          (if (= (mod f 2) (mod ff 2))
            (concat res (list ff))
            res))))))

; ex. 2.22

(defn square-list [coll]
  (loop [c coll r ()]
    (if (empty? c)
        r
        (recur (rest c)
               (concat r
                       (list (my-sqr (first c))))))))

; ex. 2.27

(defn full-reverse [coll]
  (letfn [(fr [c r]
            (if (empty? c)
              r
              (if (coll? (first c))
                (recur (rest c) (conj r (fr (first c) (empty c))))
                (recur (rest c) (conj r (first c))))))]
  (fr coll (empty coll))))

; no sicp

(defn vec-to-tree [v]
  (if (vector? v)
    (let [[val left right] v]
      {:val val
       :left (vec-to-tree left)
       :right (vec-to-tree right)})
    v))

; ex. 2.28

(defn fringe [coll]
  (letfn [(fringe-fun [c r]
            (if (empty? c)
              r
              (if (coll? (first c))
                (recur (rest c) (concat r (fringe-fun (first c) (empty c))))
                (recur (rest c) (concat r (list (first c)))))))]
  (fringe-fun coll (empty coll))))

; ex. 2.30

(defn square-tree [tree]
  (if (coll? tree)
    (if (empty? tree)
      tree
      (cons (square-tree (first tree)) (square-tree (rest tree))))
    (my-sqr tree)))

(defn square-tree-map [tree]
  (map (fn [sub-tree]
         (if (coll? sub-tree)
           (square-tree-map sub-tree)
           (my-sqr sub-tree)))
       tree))

; no ex.

(reduce max
        (map (fn [employee] (:salary employee))
             (filter (fn [employee]
                       (if (= (:post employee) "programmer")
                         true))
                     (list {:name "Joe" :salary 1000 :post "programmer"}
                           {:name "Hloey" :salary 1200 :post "programmer"}
                           {:name "Matt" :salary 2200 :post "effective-manager"}))))

; ex. 2.33

(defn my-map [p coll]
  (let [s (seq coll)]
    (reduce
     (fn [x y]
       (concat x (list (p y))))
     (empty s)
     s)))

(defn append [coll1 coll2]
  (let [s1 (seq (reverse coll1))
        s2 (seq coll2)]
    (reverse (reduce conj s1 s2))))

(defn my-length [coll]
  (reduce (fn [x y] (+ x 1)) 0 coll))

; ex. 2.34

(defn horner-eval [x params]
  (reduce
   (fn [y z]
     (+ (* y x) z))
   (reverse params)))

; ex. 2.35

(defn fringe2 [coll]
  (letfn [(fringe-fun [c r]
            (if (empty? c)
              r
              (let [frst (first c)
                    rst (rest c)]
                (if (coll? frst)
                  (recur rst (concat (fringe-fun frst (empty c)) r))
                  (recur rst (cons frst r))))))]
  (fringe-fun coll (empty coll))))

(defn count-leaves [tree]
  (reduce + 0 (map (fn [x] 1) (fringe2 tree))))

(defn count-leaves2 [tree]
  (reduce +
          0
          (map (fn [x]
                 (if (coll? x)
                   (count-leaves2 x)
                   1)) tree)))

(defn count-leaves3 [tree]
  (count (flatten tree)))

(defn reduce-flat [tree]
  (reduce (fn [x y]
            (concat x
                    (if (coll? y)
                      (reduce-flat y)
                      (list y))))
          ()
          tree))

(defn count-leaves4 [tree]
  (reduce +
          0
          (map (fn [x] 1)
               (reduce-flat tree))))

; no sicp

(defn binary-search [coll item]
  (letfn [(binary-search-iter [u l]
            (if (< l u)
              nil
              (let [m (quot (+ u l) 2)
                    v (nth coll m)]
                (cond (= v item) m
                      (> v item) (recur u (dec m))
                      (< v item) (recur (inc m) l)))))]
  (binary-search-iter 0 (dec (count coll)))))

; no ex.

(defn enumerate-interval [from to]
  (if (< to from)
    []
    (conj (enumerate-interval from (dec to)) to)))

; ex. 2.54

(defn my-eq? [x y]
  (if (and (coll? x) (coll? y))
    (and (my-eq? (first x) (first y))
         (my-eq? (next x) (next y)))
    (= x y)))

; no ex.

(defn set-intersect2 [set1 set2]
  (letfn [(iter [r s1 s2]
            (println "r = " r " s1 = " s1 " s2 = " s2)
            (if-not (and (empty? s1) (empty? s2))
              (let [f1 (first s1)
                    f2 (first s2)]
                (if (and (not (nil? f1)) (<= f1 f2))
                  (iter (cons f1 r) (rest s1) (rest s2))
                  (iter (cons f2 r) (rest s1) (rest s2))))
              r))]
  (iter (empty set1) set1 set2)))

(defn set-intersect [set1 set2]
  (if (or (empty? set1) (empty? set2))
    ()
    (let [f1 (first set1)
          f2 (first set2)]
      (cond (= f1 f2) (cons f1 (set-intersect (rest set1) (rest set2)))
            (< f1 f2) (set-intersect (rest set1) set2)
            (< f2 f1) (set-intersect set1 (rest set2))))))
