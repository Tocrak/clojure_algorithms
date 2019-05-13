;; !!! DOUBLE LINKED LIST PART !!!

(defrecord dListNode [next data prev])
(defrecord dList [head tail])

(defn dListEmpty [lst]
  (nil? (deref (:tail lst))))

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

;; !!! GRAPH PART !!!
;; !!! vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap edges])
(defrecord vertex [label status neigbors hops]) ;;index (not implemented)
(defrecord edge [firstVert secondVert])

(defn makeGraph []
  (graph. (ref []) (ref {}) (ref [])))

(defn graphAddVertex [g label]
  (dosync
    (ref-set (:vertexMap g)
      (assoc @(:vertexMap g) label (count @(:vertices g))))
    (ref-set (:vertices g)
      (conj @(:vertices g) (vertex. label (ref 0) (ref ()) (ref nil))))))

(defn graphAddEdge [g l1 l2]
  (dosync
    (ref-set (:edges g)
      (conj @(:edges g)
            (edge. (get @(:vertexMap g) l1)
                   (get @(:vertexMap g) l2))))
    (ref-set (:neigbors (get @(:vertices g) (get @(:vertexMap g) l1)))
      (cons l2 @(:neigbors (get @(:vertices g) (get @(:vertexMap g) l1)))))
    (ref-set (:neigbors (get @(:vertices g) (get @(:vertexMap g) l2)))
      (cons l1 @(:neigbors (get @(:vertices g) (get @(:vertexMap g) l2)))))))

(defn verticesReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:hops vertex) nil))))

(defn searchForUnseen [g]
  (loop [indx 0]
    (if (= 0 @(:status (get @(:vertices g) indx)))
      (:label (get @(:vertices g) indx))
      (recur (inc indx)))))

(defn dijikstraMain [g opQueue path start finish]
  (if (not (dListEmpty opQueue))
    (let [currentNode (dListFirst opQueue)]
      (dosync
        (dListRemFirst opQueue)
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentNode)))
          2)
        ;; some code to process the graph node if necessary
        ;;(println currentNode @(:hops (get @(:vertices g) (get @(:vertexMap g) currentNode))))
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentNode)))
          3)
        (doseq [neigbor @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentNode)))]
          (when (= 0 @(:status (get @(:vertices g) (get @(:vertexMap g) neigbor))))
            (dListAppend opQueue neigbor)
            (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) neigbor)))
              1)
            (ref-set (:hops (get @(:vertices g) (get @(:vertexMap g) neigbor)))
              (inc @(:hops (get @(:vertices g) (get @(:vertexMap g) currentNode))))))))
      (dijikstraMain g opQueue path start finish))
    (if (nil? @(:hops (get @(:vertices g) (get @(:vertexMap g) start))))
      (str "There is no path to the destination")
      (do
        (dListAppend path start)
        (println start)
        (loop [currentNode start]
          (when (not (= finish (dListLast path)))
            (doseq [neigbor @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentNode)))]
              (when (= (dec @(:hops (get @(:vertices g) (get @(:vertexMap g) currentNode)))
                         @(:hops (get @(:vertices g) (get @(:vertexMap g) neigbor)))))
                (dListAppend path neigbor)
                (println neigbor)))
            (recur (dListLast path))))))))

(defn dijikstra [g start finish]
  (verticesReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        path (dList. (ref nil) (ref nil))]
    (dListAppend opQueue finish)
    (dosync
      (ref-set (:hops (get @(:vertices g) (get @(:vertexMap g) finish)))
        0))
    (dijikstraMain g opQueue path start finish)))

======================================================

(dijikstra g "Prague" "Paris")
