(defrecord dListNode [next data prev])

(defrecord dList [head tail])

(defn makeDList []
  (dList. (ref nil) (ref nil)))

(defn dListEmpty [lst]
  (nil? (deref (:tail lst))))

(defn dListPrepend [lst val]
  (dosync
    (ref-set (:head lst)
      (dListNode. (ref (deref (:head lst))) val (ref nil)))
    (if (not (nil? (deref (:tail lst))))
        (ref-set (:prev (deref (:next (deref (:head lst)))))
          (deref (:head lst)))
        (ref-set (:tail lst)
          (deref (:head lst))))))

(defn dListAppend [lst val]
  (dosync
    (ref-set (:tail lst)
      (dListNode. (ref nil) val (ref (deref (:tail lst)))))
    (if (not (nil? (deref (:head lst))))
        (ref-set (:next (deref (:prev (deref (:tail lst)))))
          (deref (:tail lst)))
        (ref-set (:head lst)
          (deref (:tail lst))))))

(defn dListFirst [lst]
  (:data (deref (:head lst))))

(defn dListLast [lst]
  (:data (deref (:tail lst))))

(defn dListRemFirst [lst]
  (if (not (nil? (deref (:next (deref (:head lst))))))
    (dosync
      (ref-set (:head lst)
        (deref (:next (deref (:head lst)))))
      (ref-set (:prev (deref (:head lst))) nil))
    (dosync
      (ref-set (:head lst) nil)
      (ref-set (:tail lst) nil))))

(defn dListRemLast [lst]
  (if (not (nil? (deref (:prev (deref (:tail lst))))))
    (dosync
      (ref-set (:tail lst)
        (deref (:prev (deref (:tail lst)))))
      (ref-set (:next (deref (:tail lst))) nil))
    (dosync
      (ref-set (:head lst) nil)
      (ref-set (:tail lst) nil))))

(defn dListRest [lst]
  (dosync
    (ref-set (:head lst) (deref (:next (deref (:head lst)))))
    (ref-set (:prev (deref (:head lst))) nil)))

(defn dListRestRev [lst]
  (dosync
    (ref-set (:tail lst) (deref (:prev (deref (:tail lst)))))
    (ref-set (:next (deref (:tail lst))) nil)))

(defn dListIterForw [lst func]
  (loop [node (deref (:head lst))]
    (when (not (nil? node))
      (func (:data node))
      (recur (deref (:next node))))))

(defn dListIterRev [lst func]
  (loop [node (deref (:tail lst))]
    (when (not (nil? node))
      (func (:data node))
      (recur (deref (:prev node))))))

(defn dListIterForwAcc [lst func init]
  (loop [node (deref (:head lst)) sum init]
    (if (not (nil? node))
      (recur
        (deref (:next node))
        (func sum (:data node)))
      sum)))
======================================================

(def list1 (makeDList))

(dListEmpty list1)

(dListPrepend list1 123)
(dListPrepend list1 456)
(dListPrepend list1 789)
(dListEmpty list1)

(dListAppend list1 "abc")

(dListFirst list1)

(dListLast list1)

(dListRemFirst list1)
(dListFirst list1)

(dListRemLast list1)
(dListLast list1)

(dListRest list1)
(dListFirst list1)

dListRestRev list1
(dListLast list1)

(def list2 (makeDList))
(dListPrepend list2 1)
(dListPrepend list2 2)
(dListPrepend list2 3)
(dListPrepend list2 4)
(dListPrepend list2 5)
(dListIterForw list2 println)
(dListIterRev list2 println)

(dListIterForwAcc list2)

(if (nil? (deref (:next (deref (:head list1)))))
 (:data (deref (:head list1)))
 (println "Empty"))
