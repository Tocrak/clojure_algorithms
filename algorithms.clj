(require '[clojure.repl :refer :all])

Power set:
(defn power-set [s]
 (if (empty? s) #{#{}}
  (let [subsets (map #(disj s %) s)]
   (conj (set (mapcat power-set subsets)) s))))

(use '[clojure.set])
(defn powerset [ls]
 (if (empty? ls)
  '(())
  (union (powerset (next ls))
   (map #(conj % (first ls))
    (powerset (next ls))))))

(defn powerset2 [coll]
 (reduce (fn [a x]
          (into a (map #(conj % x)) a))
  #{#{}} coll))

Classwork:
(defn printList [lst]
 (if (not (empty? lst))
  (do (println (first lst))
   (printList (rest lst)))))
(defn getFromList [l n] (if (= n 0) (first l) (getFromList (rest l) (- n 1))))

Fibonacci sequence:
(defn fibonacci [nr]
 (if (= nr 0) 0
  (if (= nr 1) 1
   (+ (fibonacci (- nr 1))
    (fibonacci (- nr 2))))))

(defn fibl-helper [h1 h2 i n]
 (if (= i n) (+ h1 h2)
  (fibl-helper h2 (+ h1 h2) (+ i 1) n)))
(defn fibonacci [nr]
 (if (= nr 0) 0
  (if (= nr 1) 1
   (fibl-helper 0N 1N 2 nr))))

Factorial:
(defn factorial [nr]
 (if (< nr 2) 1N
  (* nr (factorial (- nr 1)))))

Measure time:
(defn nano-time []
 (. System (nanoTime)))
(defn measure-function [func]
 (def start-time (nano-time))
 (func)
 (def end-time (nano-time))
 (- end-time start-time))
(defn measure-function-print [func n]
 (def start-time (nano-time))
 (def result (func n))
 (def end-time (nano-time))
 (def duration (- end-time start-time))
 (println n result duration))
(measure-function (fn [] (factorial 8)))
(defn analyze-function [func n]
 (if (> n 0)
  (do
   (println n
    (measure-function (fn [] (func n))))
   (analyze-function func (- n 1)))))
(defn analyze-function-forward [func n n-max]
 (if (<= n n-max)
  (do
   (println n
    (measure-function (fn [] (func n))))
   (analyze-function func (+ n 1) n-max))))
(defn analyze-function-complete [func n n-max]
 (if (<= n n-max)
  (do
   (measure-function-print func n)
   (analyze-function func (+ n 1) n-max))))

Loops:
(loop [x 10] (when (>= x 1) (statement) (recur (- x 1))))
(loop [i 1] (println i) (if (<i 10) (recur (+ i 1))))

(for [x (range 0 10)] (println x))

(dotimes [x 10] (println x))



PowerSet:
(defn exp2 [n]
    (if (zero? n) 1
        (* 2 (exp2 (dec n)))))
(defn powerSet [lst]
    (def vect (vec lst))
    (def size (count vect))
    (def pwrSize (exp2 size))
    (loop [i pwrSize] (when (>= i 0)
                       (loop [j size] (when (>= j 0)
                                       (if (> (bit-and i (exp2 j)) 0)
                                           (print (get vect j)))
                                       (recur (dec j))))
                       (println)
                       (recur (dec i)))))
(powerSet '(1 2 3))
