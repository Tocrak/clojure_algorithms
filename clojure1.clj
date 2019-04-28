(def myArr (int-array '(1 20 400 300 5 70 2 99 107)))
(count arr)
(aget arr 3)
(aget arr 3 4)

(defn printArray [arr]
  (loop [x 0]
    (when (< x (count arr))
      (println (aget arr x))
      (recur (+ x 1)))))

(let [a 1 b 2] (println a) (println b))
(let* [a 1 b (+ a 6)] (println a) (println b))

"deref = @"

(with-local-vars [a 1 b 2]
  (println @a @b)
  (var-set a 10)
  (println @a @b))

(defn bubbleSortOnce [arr elm]
  (loop [nr 1 i 0]
      (when (< nr elm)
        (if (> (aget arr (dec nr)) (aget arr nr))
          (do
            (let [x (aget arr nr)]
              (aset arr nr (aget arr (dec nr)))
              (aset arr (dec nr) x))
            (recur (inc nr) (inc i))))))
  (= 0 i))

(defn bubbleSort [arr]
  (loop [done false elm (count arr)]
    (if (not done)
      (recur (bubbleSortOnce arr elm) (dec elm)))))

(defn calcSum [arr]
  (loop [i 0 acc 0]
    (if (< i (count arr))
      (recur (inc i) (+ acc (aget arr i)))
      acc)))
(defn calcAvg [arr] (/ (calcSum arr) (count arr)))

(if (> (aget arr (dec nr)) (aget arr nr))
  True False)


(defn splitArr [arr s f]
  (if (not (= (- f s) 1))
    (let [cnt f
          cnt2 (quot cnt 2)]
      (splitArr arr s cnt2)
      (splitArr arr cnt2 cnt))))

(defn splitArrStart [arr]
  (let [cnt (count arr)
        cnt2 (quot cnt 2)]
    (splitArr arr 0 cnt2)
    (splitArr arr cnt2 cnt)))


(defn printSplitArr [arr s f]
  (loop [i s]
    (when (< i f)
      (print (aget arr i) " ")
      (recur (+ i 1))))
  (newline))

(defn splitHelper [arr s f]
  (splitHelper arr s f)
  (when (< s f)
    (let [cnt (inc (- f s))
          cnt2 (quot cnt 2)
          mid (+ s cnt2)]
      (splitHelper arr s (dec mid))
      (splitHelper arr mid f))))

(defn splitPrint [arr]
  (splitHelper arr 0 (dec (count arr))))

(defn printParallel [arr]
  (loop [i 0]
        j (quot (count arr))
        k (count arr)))

(defn printTwoArr [arr s1 e1 s2 e2]
  (loop [i1 s1 i2 s2]
    (if (and (<= i1 e1)
            (<= i2 e2))
        (do (print (aget arr i1) "/" (aget arr i2) "  ")
          (recur (inc i1) (inc i2)))
      (if (<= i1 e1)
        (do (print (aget arr i1) "/ -" "  ")
          (recur (inc i1) i2))
        (if (<= i2 e2)
          (do (print "- /" (aget arr i2) "  ")
            (recur i1 (inc i2)))))))
  (newline))

(defn mergeTwoArr [arr s1 e1 s2 e2]
  (let tempArr (int-array (inc (- e2 s1)))
    (loop [i1 s1 i2 s2 j 0]
      (if (and (<= i1 e1)
               (<= i2 e2))
          (do (let [val1 1])
            (recur (inc i1) (inc i2) (inc j)))
        (if (<= i1 e1)
          (do (print (aget arr i1) "/ -" "  ")
            (recur (inc i1) i2 (inc j)))
          (if (<= i2 e2)
            (do (print "- /" (aget arr i2) "  ")
              (recur i1 (inc i2) (inc j)))))))))





(defn sListSearch [lst val]
  (loop [node (deref (:head lst))]
    (if (not (nil? node))
      (if (= (:data node) val)
        true
        (recur (deref (:next node))))
      false)))

(defn cartesianProduct [lst]
  (loop [firstNode (deref (:head lst))]
    (when (not (nil? firstNode))
      (loop [secondNode (deref (:head lst))]
        (when (not (nil? secondNode))
          (println (str "(" (:data firstNode) " " (:data secondNode) ")"))
          (recur (deref (:next secondNode)))))
      (recur (deref (:next firstNode))))))
