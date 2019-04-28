(defrecord dictRoot [root])
(defrecord dictNode [key value left right])

(defn createDict [] (dictRoot. (ref nil) 0))

(defn isEmpty [dict] (nil? @dict))

(defn insertDict [dict key val]
  (dosync
    (if (isEmpty dict)
      (ref-set dict (dictNode. key (ref val) (ref nil) (ref nil)))
      (if (< key (:key @dict))
        (insertDict (:left @dict) key val)
        (if (> key (:key @dict))
          (insertDict (:right @dict) key val)
          (ref-set (:value dict) val))))))

(defn iterDict [dict func]
  (when (not (isEmpty dict))
    (iterDict (:left @dict) func)
    (func (:key @dict) (:value @dict))
    (iterDict (:right @dict) func)))

(defn reduceDict [dict acc func]
  (if (isEmpty dict)
    acc
    (let [accLeft (reduceDict (:left @dict) acc func)]
      (reduceDict (:right @dict) (func accLeft (:key @dict)) func))))

(defn countDict [dict] (reduceDict dict 0 (fn [a v] (inc a))))

(defn sumDict [dict] (reduceDict dict 0 +))

(defn avgDict [dict] (/ (sumDict dict) (countDict dict)))

(defn minDict1 [dict]
  (reduceDict dict nil (fn [a v] (if (nil? a) v (if (< v a) v a)))))

(defn maxDict1 [dict]
  (reduceDict dict nil (fn [a v] (if (nil? a) v (if (> v a) v a)))))

(defn minDict2 [dict]
  (if (isEmpty dict)
    nil
    (if (nil? (:left @dict))
      (:key @dict)
      (minDict2 (:left @dict)))))

(defn maxDict2 [dict]
  (if (isEmpty dict)
    nil
    (if (nil? (:right @dict))
      (:key @dict)
      (maxDict2 (:right @dict)))))

(defn searchKey [dict key]
  (if (isEmpty dict)
    false
    (if (= (:key @dict) key)
      true
      (if (< key (:key @dict))
        (searchKey (:left @dict) key)
        (searchKey (:right @dict) key)))))

(defn dictGet [dict key]
  (if (isEmpty dict)
    nil
    (if (= (:key @dict) key)
      (:value @dict)
      (if (< key (:key @dict))
        (dictGet (:left @dict) key)
        (dictGet (:right @dict) key)))))

(defn depthDict [dict]
  (if (isEmpty dict)
    0
    (let [leftDepth (depthDict (:left @dict))
          rightDepth (depthDict (:right @dict))]
      (if (< leftDepth rightDepth)
        (inc rightDepth)
        (inc leftDepth)))))

(defn freeDepthHelper [dict counter]
  (if (isEmpty dict)
    counter
    (if (or (isEmpty (:left @dict))
            (isEmpty (:right @dict)))
      (inc counter)
      (let [leftDepth (freeDepthHelper (:left @dict) (inc counter))
            rightDepth (freeDepthHelper (:right @dict) (inc counter))]
        (if (< leftDepth rightDepth)
          leftDepth
          rightDepth)))))

(defn freeDepthDict [dict]
  (freeDepthHelper dict 0))

(defn checkBalanceDict [dict] (- (freeDepthDict dict) (depthDict dict)))

(defn minDeleteHelp [node]
  (if (nil? (:left @node))
    node
    (minDeleteHelp (:left @node))))

(defn deleteNodeDict [node]
  (dosync
    (if (and (nil? @(:left @node))
             (nil? @(:right @node)))
      (ref-set node nil)
      (if (nil? @(:right @node))
        (ref-set node @(:left @node))
        (let [leftBranch (:left @node)
              rightBranch (:right @node)]
          (ref-set node @(:right @node))
          (ref-set (minDeleteHelp rightBranch) @leftBranch))))))

(defn deleteSpecificValueDict [dict key]
  (if (isEmpty dict)
    "Value not present"
    (if (= (:key @dict) key)
      (deleteNodeDict dict)
      (if (< key (:key @dict))
        (deleteSpecificValueDict (:left @dict) key)
        (deleteSpecificValueDict (:right @dict) key)))))

(defn printDict [dict]
  (with-local-vars [c 0]
    (iterDict dict (fn [x y]
                    (if (= @c 0)
                      (do (print (str x " : " y))
                          (var-set c 1))
                      (print (str " " x " : " y)))))
    (newline)))

(defn printDict1 [dict]
  (with-local-vars [c 0]
    (iterPreOrderDict dict (fn [x])
                    (print (str (if (> @c 0) "_" "") x))
                    (var-set c (inc @c)))
    (newline)))

(defn iterPreOrderDict [dict func]
  (when (not (isEmpty dict))
    (func (:value @dict))
    (iterDict (:left @dict) func)
    (iterDict (:right @dict) func)))

(defn prettyPrintDictHelp [dict pref firstPref]
    (when (not (isEmpty dict))
      (print (str firstPref pref (:key @dict) " : " (:value @dict)))
      (newline)
      (prettyPrintDictHelp (:left @dict) "[L]" (str " " pref))
      (prettyPrintDictHelp (:right @dict) "[R]" (str " " pref))))

(defn prettyPrintDict [dict]
  (prettyPrintDictHelp dict "" "[/]"))

======================================================

(def t1 (createDict))
(isEmpty t1)

(def t1 (createDict))
(insertDict t1 5 "E")
(insertDict t1 3 "C")
(insertDict t1 4 "D")
(insertDict t1 2 "B")
(insertDict t1 1 "A")
(insertDict t1 7 "G")
(insertDict t1 8 "H")
(insertDict t1 6 "F")
(iterDict t1 println)

(printDict t1)

(prettyPrintDict t1)

(checkBalanceDict t1)

(deleteSpecificValueDict t1 3)
(iterDict t1 println)

"key value binary search dict"
