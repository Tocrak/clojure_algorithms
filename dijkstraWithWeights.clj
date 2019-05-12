;; !!! DOUBLE LINKED LIST PART !!!

(defrecord dListVertex [next data prev])
(defrecord dList [head tail])

(defn dListEmpty [lst]
  (nil? @(:tail lst)))

(defn dListAppend [lst val]
  (dosync
    (ref-set (:tail lst)
      (dListVertex. (ref nil) val (ref @(:tail lst))))
    (if (not (nil? @(:head lst)))
        (ref-set (:next @(:prev @(:tail lst)))
          @(:tail lst))
        (ref-set (:head lst)
          @(:tail lst)))))

(defn dListFirst [lst]
  (:data @(:head lst)))

(defn dListLast [lst]
  (:data @(:tail lst)))

(defn dListDeleteElement [lst element]
  (loop [node @(:head lst)]
    (when (not (nil? node))
      (if (= (:data node) element)
        (dosync
          (if (and (nil? @(:prev node))
                   (nil? @(:next node)))
            (do
              (ref-set (:head lst)
                nil)
              (ref-set (:tail lst)
                nil))
            (if (nil? @(:prev node))
              (do
                (ref-set (:prev @(:next node))
                  nil)
                (ref-set (:head lst)
                  @(:next node))
                (ref-set (:next node)
                  nil))
              (if (nil? @(:next node))
                (do
                  (ref-set (:next @(:prev node))
                    nil)
                  (ref-set (:tail lst)
                    @(:prev node))
                  (ref-set (:prev node)
                    nil))
                (do
                  (ref-set (:next @(:prev node))
                    @(:next node))
                  (ref-set (:prev @(:next node))
                    @(:prev node))))))))
      (recur @(:next node)))))

(defn dListIntoClojureList [dList]
  (with-local-vars [resultList ()]
    (loop [node (deref (:head dList))]
      (when (not (nil? node))
        (var-set resultList (conj @resultList (:data node)))
        (recur (deref (:next node)))))
    @resultList))

;; !!! GRAPH PART !!!
;; !!! vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap])
(defrecord vertex [label status neighbours distance])
(defrecord neighbour [index weight])

(defn makeGraph []
  (graph. (ref []) (ref {})))

(defn graphAddVertex [g label]
  (dosync
    (ref-set (:vertexMap g)
      (assoc @(:vertexMap g) label (count @(:vertices g))))
    (ref-set (:vertices g)
      (conj @(:vertices g) (vertex. label (ref 0) (ref ()) (ref 0))))))

(defn graphAddEdge [g l1 l2 w]
  (dosync
    (ref-set (:neighbours (get @(:vertices g) (get @(:vertexMap g) l1)))
      (conj @(:neighbours (get @(:vertices g) (get @(:vertexMap g) l1)))
        (neighbour. (get @(:vertexMap g) l2) w)))
    (ref-set (:neighbours (get @(:vertices g) (get @(:vertexMap g) l2)))
      (conj @(:neighbours (get @(:vertices g) (get @(:vertexMap g) l2)))
        (neighbour. (get @(:vertexMap g) l1) w)))))

(defn verticesReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:distance vertex) 0))))

(defn searchForUnseen [g]
  (loop [indx 0]
    (if (= 0 @(:status (get @(:vertices g) indx)))
      (:label (get @(:vertices g) indx))
      (recur (inc indx)))))

(defn selectFromQueue [opQueue g]
  (with-local-vars [selectedVertex (dListFirst opQueue)]
    (loop [node @(:head opQueue)]
      (when (not (nil? node))
        (if (< @(:distance (get @(:vertices g) (get @(:vertexMap g) (:data node))))
               @(:distance (get @(:vertices g) (get @(:vertexMap g) @selectedVertex))))
          (var-set selectedVertex (:data node)))
        (recur @(:next node))))
    @selectedVertex))

(defn selectMinDistance [currentVertex g]
  (with-local-vars [selectedVertex (first @(:neighbours (get @(:vertices g) (get @(:vertexMap g) currentVertex))))]
    (doseq [neighbour @(:neighbours (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
      (if (< @(:distance (get @(:vertices g) (:index neighbour)))
             @(:distance (get @(:vertices g) (:index @selectedVertex))))
        (var-set selectedVertex neighbour)))
    @selectedVertex))

(defn processNeighbors [currentVertex opQueue g]
  (doseq [neighbour @(:neighbours (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
    (if (= 0 @(:status (get @(:vertices g) (:index neighbour))))
      (do
        (dListAppend opQueue (:label (get @(:vertices g) (:index neighbour))))
        (ref-set (:status (get @(:vertices g) (:index neighbour)))
          1)
        (if (= 0 @(:distance (get @(:vertices g) (:index neighbour))))
          (ref-set (:distance (get @(:vertices g) (:index neighbour)))
            (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
               (:weight neighbour)))))
      (if (= 1 @(:status (get @(:vertices g) (:index neighbour))))
        (if (< (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
                  (:weight neighbour))
               @(:distance (get @(:vertices g) (:index neighbour))))
          (ref-set (:distance (get @(:vertices g) (:index neighbour)))
            (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
               (:weight neighbour))))))))

(defn dijikstraMain [g opQueue path start finish]
  (if (not (dListEmpty opQueue))
    (let [currentVertex (selectFromQueue opQueue g)]
      (dosync
        (dListDeleteElement opQueue currentVertex)
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
          2)
        ;; some code to process the graph Vertex if necessary
        ;;(println currentVertex @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex))))
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
          3)
        (processNeighbors currentVertex opQueue g))
      (dijikstraMain g opQueue path start finish))
;;may cause some bugs if start = finish
    (if (= 0 @(:distance (get @(:vertices g) (get @(:vertexMap g) start))))
      (str "There is no path to the destination")
      (do
        (dListAppend path start)
        (println start)
        (loop [currentVertex start]
          (when (not (= finish (dListLast path)))
            (let [neighbour (selectMinDistance currentVertex g)]
                (dListAppend path (:label (get @(:vertices g) (:index neighbour))))
                (println (:label (get @(:vertices g) (:index neighbour)))))
            (recur (dListLast path))))))))


(defn dijikstra [g start finish]
  (verticesReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        path (dList. (ref nil) (ref nil))]
    (dListAppend opQueue finish)
    (dijikstraMain g opQueue path start finish)))

======================================================

(dijikstra g "Prague" "Alessandria")

(dijikstra g "Prague" "Catania")
