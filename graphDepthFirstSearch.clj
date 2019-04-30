;; !!! for information about the functions, look the breadthFirstSearch.clj
;; the only difference is that the new vertices are prepended to the queue

;; !!! DOUBLE LINKED LIST PART !!!

(defrecord dListNode [next data prev])
(defrecord dList [head tail])

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

(defn dListFirst [lst]
  (:data (deref (:head lst))))

(defn dListRemFirst [lst]
  (if (not (nil? (deref (:next (deref (:head lst))))))
    (dosync
      (ref-set (:head lst)
        (deref (:next (deref (:head lst)))))
      (ref-set (:prev (deref (:head lst))) nil))
    (dosync
      (ref-set (:head lst) nil)
      (ref-set (:tail lst) nil))))

;; !!! GRAPH PART !!!
;; !!! vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap edges])
(defrecord vertex [label status neighbours]) ;;index (not implemented)
(defrecord edge [firstVert secondVert])

(defn makeGraph []
  (graph. (ref []) (ref {}) (ref [])))

(defn graphAddVertex [g label]
  (dosync
    (ref-set (:vertexMap g)
      (assoc @(:vertexMap g) label (count @(:vertices g))))
    (ref-set (:vertices g)
      (conj @(:vertices g) (vertex. label (ref 0) (ref ()))))))

(defn graphAddEdge [g l1 l2]
  (dosync
    (ref-set (:edges g)
      (conj @(:edges g)
            (edge. (get @(:vertexMap g) l1)
                   (get @(:vertexMap g) l2))))
    (ref-set (:neighbours (get @(:vertices g) (get @(:vertexMap g) l1)))
      (cons l2 @(:neighbours (get @(:vertices g) (get @(:vertexMap g) l1)))))
    (ref-set (:neighbours (get @(:vertices g) (get @(:vertexMap g) l2)))
      (cons l1 @(:neighbours (get @(:vertices g) (get @(:vertexMap g) l2)))))))

(defn statusReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0))))

(defn searchForUnseen [g]
  (loop [indx 0]
    (if (= 0 @(:status (get @(:vertices g) indx)))
      (:label (get @(:vertices g) indx))
      (recur (inc indx)))))

(defn dfsMain [g opQueue vertCount groupCount]
  (if (not (dListEmpty opQueue))
    (let [currentNode (dListFirst opQueue)]
      (dosync
        (dListRemFirst opQueue)
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentNode)))
          2)
        ;; some code to process the graph node if necessary
        (println currentNode)
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentNode)))
          3)
        (doseq [neighbour @(:neighbours (get @(:vertices g) (get @(:vertexMap g) currentNode)))]
          (when (= 0 @(:status (get @(:vertices g) (get @(:vertexMap g) neighbour))))
            (dListPrepend opQueue neighbour)
            (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) neighbour)))
              1))))
      (dfsMain g opQueue (inc vertCount) groupCount))
    (if (< vertCount (count @(:vertices g)))
      (do (dListPrepend opQueue (searchForUnseen g))
          (dfsMain g opQueue vertCount (inc groupCount)))
      (if (= 1 groupCount)
        (str "Graph is connected")
        (str "Graph is disconnected. Number of connected components is: " groupCount)))))

(defn dfs [g & [label]]
  (statusReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        vertCount 0
        groupCount 0]
    (if (nil? label)
      (dListAppend opQueue (:label (get @(:vertices g) 0)))
      (dListAppend opQueue label))
    (dfsMain g opQueue vertCount (inc groupCount))))

======================================================

(dfs g)
