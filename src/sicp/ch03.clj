(ns sicp.ch03
  (:use sicp.ch01)
  (:use sicp.ch02))

; ex. 3.1

(defn make-accumulator [initial]
  (let [a (atom initial)]
    (fn [x] (swap! a + x))))

(defn make-monitored [f]
  (let [runs (atom 0)]
    (fn [x]
      (cond (= x :how-many-calls) @runs
            (= x :reset-count) (swap! runs * 0)
            :else (do
                    (swap! runs + 1)
                    (f x))))))

(defn make-monitored-generic [f]
  (let [runs (atom 0)]
    (fn [& args]
      (cond (= (nth args 0) :how-many-calls) @runs
            (= (nth args 0) :reset-count) (swap! runs * 0)
            :else (do
                    (swap! runs + 1)
                    (apply f args))))))
