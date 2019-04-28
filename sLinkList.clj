(defrecord sListNode [next data])

(defrecord sList [head])

(defn makeSList []
  (sList. (ref nil)))

(defn sListEmpty? [lst]
  (nil? (deref (:head lst))))

(defn sListPrepend [lst val]
  (dosync
    (ref-set (:head lst)
             (sListNode. (ref (deref (:head lst))) val))))

(defn sListFirst [lst]
  (:data (deref (:head lst))))


(defn sListRest [lst]
  (sList. (:next (deref (:head lst)))))


(defn sListRemFirst [lst]
  (dosync
    (ref-set (:head lst)
             (deref (:next (deref (:head lst)))))))


(defn sListIterHelper [node func]
  (if (not (nil? node))
    (do
      (func (:data node))
      (sListIterHelper (deref (:next node)) func))))

(defn sListIterOrig [lst func]
  (sListIterHelper (deref (:head lst)) func))

(defn sListIter [lst func]
  (loop [node (deref (:head lst))]
    (when (not (nil? node))
      (func (:data node))
      (recur (deref (:next node))))))

(defn sListAppend [lst val]
  (def new-node (sListNode. (ref nil) val))
  (if (nil? (deref (:head lst)))
    (ref-set (:head lst) new-node)
    (loop [node (deref (:head lst))]
      (if (nil? (deref (:next node)))
        (ref-set (:next node) new-node)
        (recur (deref (:next node)))))))

(defn sListRemLast [lst]
 (def first-node (deref (:head lst)))
 (if (not (nil? first-node))
   (if (nil? (deref (:next first-node)))
     (ref-set (:head lst) nil)
     (loop [node first-node]
       (if (nil? (deref (:next (deref (:next node)))))
         (dosync (ref-set (:next node) nil))
         (recur (deref (:next node))))))))

======================================================

(def list1 (makeSList))
(sListPrepend list1 1)
(sListPrepend list1 2)
(sListPrepend list1 3)
(sListPrepend list1 4)
(sListPrepend list1 5)
(sListIter list1 println)
