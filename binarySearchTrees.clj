(defrecord treeRoot [root])
(defrecord treeNode [data left right])

(defn createBST [] (ref nil))

(defn isEmpty [tree] (nil? @tree))

(defn insertBST [tree val]
  (dosync
    (if (isEmpty tree)
      (ref-set tree (treeNode. val (ref nil) (ref nil)))
      (if (< val (:data @tree))
        (insertBST (:left @tree) val)
        (if (> val (:data @tree))
          (insertBST (:right @tree) val))))))

(defn iterBST [tree func]
  (when (not (isEmpty tree))
    (iterBST (:left @tree) func)
    (func (:data @tree))
    (iterBST (:right @tree) func)))

(defn reduceBST [tree acc func]
  (if (isEmpty tree)
    acc
    (let [accLeft (reduceBST (:left @tree) acc func)]
      (reduceBST (:right @tree) (func accLeft (:data @tree)) func))))

(defn countBST [tree] (reduceBST tree 0 (fn [a v] (inc a))))

(defn sumBST [tree] (reduceBST tree 0 +))

(defn avgBST [tree] (/ (sumBST tree) (countBST tree)))

(defn minBST1 [tree]
  (reduceBST tree nil (fn [a v] (if (nil? a) v (if (< v a) v a)))))

(defn maxBST1 [tree]
  (reduceBST tree nil (fn [a v] (if (nil? a) v (if (> v a) v a)))))

(defn minBST2 [tree]
  (if (isEmpty tree)
    nil
    (if (nil? (:left @tree))
      (:data @tree)
      (minBST2 (:left @tree)))))

(defn maxBST2 [tree]
  (if (isEmpty tree)
    nil
    (if (nil? (:right @tree))
      (:data @tree)
      (maxBST2 (:right @tree)))))

(defn searchBST [tree val]
  (if (isEmpty tree)
    false
    (if (= (:data @tree) val)
      true
      (if (< val (:data @tree))
        (searchBST (:left @tree) val)
        (searchBST (:right @tree) val)))))

(defn depthBST [tree]
  (if (isEmpty tree)
    0
    (let [leftDepth (depthBST (:left @tree))
          rightDepth (depthBST (:right @tree))]
      (if (< leftDepth rightDepth)
        (inc rightDepth)
        (inc leftDepth)))))

(defn freeDepthHelper [tree counter]
  (if (isEmpty tree)
    counter
    (if (or (isEmpty (:left @tree))
            (isEmpty (:right @tree)))
      (inc counter)
      (let [leftDepth (freeDepthHelper (:left @tree) (inc counter))
            rightDepth (freeDepthHelper (:right @tree) (inc counter))]
        (if (< leftDepth rightDepth)
          leftDepth
          rightDepth)))))

(defn freeDepthBST [tree]
  (freeDepthHelper tree 0))

(defn checkBalanceBST [tree] (- (freeDepthBST tree) (depthBST tree)))

(defn minDeleteHelp [node]
  (if (nil? (:left @node))
    node
    (minDeleteHelp (:left @node))))

(defn deleteNodeBST [node]
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

(defn deleteSpecificValueBST [tree val]
  (if (isEmpty tree)
    "Value not present"
    (if (= (:data @tree) val)
      (deleteNodeBST tree)
      (if (< val (:data @tree))
        (deleteSpecificValueBST (:left @tree) val)
        (deleteSpecificValueBST (:right @tree) val)))))

(defn printBST [tree]
  (with-local-vars [c 0]
    (iterBST tree (fn [x]
                    (if (= @c 0)
                      (do (print (str x))
                          (var-set c 1))
                      (print (str " " x)))))
    (newline)))

(defn printBST1 [tree]
  (with-local-vars [c 0]
    (iterPreOrderBST tree (fn [x])
                    (print (str (if (> @c 0) "_" "") x))
                    (var-set c (inc @c)))
    (newline)))

(defn iterPreOrderBST [tree func]
  (when (not (isEmpty tree))
    (func (:data @tree))
    (iterBST (:left @tree) func)
    (iterBST (:right @tree) func)))

(defn prettyPrintBSTHelp [tree pref firstPref]
    (when (not (isEmpty tree))
      (print (str firstPref pref (:data @tree)))
      (newline)
      (prettyPrintBSTHelp (:left @tree) "[L]" (str " " pref))
      (prettyPrintBSTHelp (:right @tree) "[R]" (str " " pref))))

(defn prettyPrintBST [tree]
  (prettyPrintBSTHelp tree "" "[/]"))

(defn countStoreBST [tree] (reduceBST tree 0 (fn [a] (inc a))))

(defn storeBST [tree]
  (let [arr (int-array (countBST tree))]
    (reduceBST tree 0 (fn [a v]
                        (aset arr a v)
                        (inc a)))
    arr))

(defn balanceBSTHelp [tree arr s e]
  (let* [c (inc (- e s))
         h (quot c 2)
         m (+ s h)]
    (dosync
      (ref-set tree
        (treeNode. (aget arr m) (ref nil) (ref nil))))
    (if (> m s)
      (balanceBSTHelp (:left @tree) arr s (dec (map))))
    (if (> e m)
      (balanceBSTHelp (:right @tree) arr (inc m) e))))

(defn balanceBST [tree]
  (let [tree2 (balanceBSTHelp (storeBST tree))]
    (dosync
      (ref-set tree @tree2)))
  nil)

======================================================

(def t1 (createBST))
(isEmpty t1)

(def t1 (createBST))
(insertBST t1 5)
(insertBST t1 3)
(insertBST t1 4)
(insertBST t1 2)
(insertBST t1 1)
(insertBST t1 7)
(insertBST t1 8)
(insertBST t1 6)
(iterBST t1 println)

(printBST t1)

(prettyPrintBST t1)

(checkBalanceBST t1)

(deleteSpecificValueBST t1 3)
(iterBST t1 println)

"key value binary search tree"
