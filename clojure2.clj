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


(defn dListIntoClojureList [dList]
  (with-local-vars [resultList ()]
    (loop [node (deref (:head dList))]
      (when (not (nil? node))
        (var-set resultList (conj @resultList (:data node)))
        (recur (deref (:next node)))))
    @resultList))




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
             (:weight neighbour)))))))




;; !!! GRAPH PART !!!
;; !!! vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap])
(defrecord vertex [label status neigbors distance])
(defrecord neigbor [index weight])

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
    (ref-set (:neigbors (get @(:vertices g) (get @(:vertexMap g) l1)))
      (conj @(:neigbors (get @(:vertices g) (get @(:vertexMap g) l1)))
        (neigbor. (get @(:vertexMap g) l2) w)))
    (ref-set (:neigbors (get @(:vertices g) (get @(:vertexMap g) l2)))
      (conj @(:neigbors (get @(:vertices g) (get @(:vertexMap g) l2)))
        (neigbor. (get @(:vertexMap g) l1) w)))))

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
  (with-local-vars [selectedVertex (first @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentVertex))))]
    (doseq [neigbor @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
      (if (< @(:distance (get @(:vertices g) (:index neigbor)))
             @(:distance (get @(:vertices g) (:index @selectedVertex))))
        (var-set selectedVertex neigbor)))
    @selectedVertex))

(defn processNeighbors [currentVertex opQueue g]
  (doseq [neigbor @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
    (if (= 0 @(:status (get @(:vertices g) (:index neigbor))))
      (do
        (dListAppend opQueue (:label (get @(:vertices g) (:index neigbor))))
        (ref-set (:status (get @(:vertices g) (:index neigbor)))
          1)
        (if (= 0 @(:distance (get @(:vertices g) (:index neigbor))))
          (ref-set (:distance (get @(:vertices g) (:index neigbor)))
            (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
               (:weight neigbor)))))
      (if (= 1 @(:status (get @(:vertices g) (:index neigbor))))
        (if (< (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
                  (:weight neigbor))
               @(:distance (get @(:vertices g) (:index neigbor))))
          (ref-set (:distance (get @(:vertices g) (:index neigbor)))
            (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
               (:weight neigbor))))))))

(defn dijikstraMain [g opQueue path start finish]
  (if (not (dListEmpty opQueue))
    (let [currentVertex (selectFromQueue opQueue g)]
      (dosync
        (dListDeleteElement opQueue currentVertex)
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
          2)
        ;; some code to process the graph Vertex if necessary
        (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
          3)
        (processNeighbors currentVertex opQueue g))
      (dijikstraMain g opQueue path start finish))
    (if (= 0 @(:distance (get @(:vertices g) (get @(:vertexMap g) start))))
      (if (= start finish)
        (str "You are searching for the same city")
        (str "There is no path to the destination"))
      (do
        (loop [currentVertex start]
          (when (not (= finish (dListLast path)))
            (let [neigbor (selectMinDistance currentVertex g)]
              (dListAppend path (:label (get @(:vertices g) (:index neigbor)))))
            (recur (dListLast path))))
        (dListIntoClojureList path)))))

(defn dijikstra [g start finish]
  (verticesReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        path (dList. (ref nil) (ref nil))]
    (dListAppend opQueue finish)
    (dListAppend path start)
    (dijikstraMain g opQueue path start finish)))
