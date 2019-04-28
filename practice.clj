(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn powerSet [lst]
 (def vect (vec lst))
 (def size (count vect))
 (def pwrSize (exp 2 size))
 (loop [i pwrSize] (when (>= i 0)
                    (loop [j size] (when (>= j 0)
                                    (if (> (bit-and i (exp 2 j)) 0)
                                     (print (get vect j)))
                                    (recur (- j 1))))
                    (println)
                    (recur (- i 1)))))
====================================================================================
(defn powerSetHelper1 [i size pwrSize vect]
 (if (< i pwrSize)
  (powerSetHelper2 i 0 size pwrSize vect)))

(defn powerSetHelper2 [i j size pwrSize vect]
 (if (< j size)
  (if (> (bit-and i (exp2 j)) 0)
   (print (get vect j))
   (powerSetHelper2 i (+ j 1) size pwrSize vect))
  (do (println)
   (powerSetHelper1 (+ i 1) size pwrSize vect))))

(defn powerSet [lst]
 (def vect (vec lst))
 (def size (count vect))
 (def pwrSize (exp2 size))
 (powerSetHelper1 0 size pwrSize vect))
====================================================================================
(ref value)
(deref reference)
(dosync (ref-set reference new-value))









(defn sListIterRemoveFirst [lst func]
 (if (not (sListEmpty lst))
  (do (func (sListFirst lst))
   (sListIter (sListRemoveFirst lst) func))))


(defn sListIterRest [lst func]
 (if (not (sListEmpty lst))
  (do (func (sListFirst lst))
   (sListIter (sListRest lst) func))))






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
