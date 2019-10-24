(defrecord sListNode [next data])
(defrecord sList [head])

(defn makeSList []
  (sList. (ref nil)))

(defn sListPrepend [lst val]
  (dosync
    (ref-set (:head lst)
             (sListNode. (ref (deref (:head lst))) val))))

(defn sListIter [lst func]
  (loop [node (deref (:head lst))]
    (when (not (nil? node))
      (func (:data node))
      (recur (deref (:next node))))))

(defn sListSearch [lst val]
  (loop [node (deref (:head lst))]
    (if (not (nil? node))
      (if (= (:data node) val)
        true
        (recur (deref (:next node))))
      false)))

(defn sListCartesianProduct [lst]
  (loop [firstNode (deref (:head lst))]
    (when (not (nil? firstNode))
      (loop [secondNode (deref (:head lst))]
        (when (not (nil? secondNode))
          (println (str "(" (:data firstNode) " " (:data secondNode) ")"))
          (recur (deref (:next secondNode)))))
      (recur (deref (:next firstNode))))))

(defn sListCartesianProduct [lst]
  (with-local-vars [cartProd ()]
    (loop [firstNode (deref (:head lst))]
      (when (not (nil? firstNode))
        (loop [secondNode (deref (:head lst))]
          (when (not (nil? secondNode))
            (var-set cartProd (conj (var-get cartProd)
                                    (list (:data firstNode) (:data secondNode))))
            (recur (deref (:next secondNode)))))
        (recur (deref (:next firstNode)))))
    (var-get cartProd)))

(def list1 (makeSList))
(sListPrepend list1 1)
(sListPrepend list1 2)
(sListPrepend list1 3)
(sListPrepend list1 4)
(sListPrepend list1 5)
(sListIter list1 println)
(sListSearch list1 3)
(sListSearch list1 9)
(sListCartesianProduct list1)
