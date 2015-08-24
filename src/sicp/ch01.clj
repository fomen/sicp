(ns sicp.ch01)

(defn my-sum-sq [x y]
  (+ (* x x) (* y y)))

(defn my-sum-sq-max [x y z]
  (cond (and (< x y) (< x z)) (my-sum-sq y z)
        (and (< y x) (< y z)) (my-sum-sq x z)
        :else (my-sum-sq x y)))

; Newton

(defn my-sqr [x]
  (* x x))

(defn my-abs [x]
  (if (< x 0)
    (- x)
    x))

(defn my-average [x y]
  (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (my-abs (- (my-sqr guess) x)) 0.001))

(defn not-good-enough? [guess x]
  (>= (my-abs (- (my-sqr guess) x)) 0.001))

(defn improve-guess [guess x]
  (my-average guess (/ x guess)))

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
    :else else-clause))

(defn my-sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (my-sqrt-iter (improve-guess guess x) x)))

(defn my-sqrt [x]
  (my-sqrt-iter 1.0 x))

(defn my-sqrt2 [x]
  (loop [guess 1.0]
    (if (not-good-enough? guess x)
      (recur (improve-guess guess x))
      guess)))

(defn my-sqrt3 [x]
  (loop [guess 1.0]
    (let [new-guess (improve-guess guess x)]
      (if (> (my-abs (- guess new-guess)) 0.001)
        (recur new-guess)
        new-guess))))

; Akkerman

(defn akkerman [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (akkerman (- x 1) (akkerman x (- y 1)))))

; root

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn close-enough? [x y]
  (< (abs (- x y)) 0.001))

(defn changes-sign [x y]
  (or
    (and (< x 0) (> y 0))
    (and (< y 0) (> x 0))))

(defn find-root [f a b]
  (let [m (/ (+ a b) 2)]
    (if (close-enough? a b)
      m
      (let [fm (f m)]
        (cond
          (pos? fm) (recur f a fm)
          (neg? fm) (recur f fm b)
          :else fm)))))
