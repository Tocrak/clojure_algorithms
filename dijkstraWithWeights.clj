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
    (loop [node (deref (:tail dList))]
      (when (not (nil? node))
        (var-set resultList (conj @resultList (:data node)))
        (recur (deref (:prev node)))))
    @resultList))

;; !!! GRAPH PART !!!
;; !!! vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap])
(defrecord vertex [label status neighbors distance])
(defrecord neighbor [index weight])

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
    (ref-set (:neighbors (get @(:vertices g) (get @(:vertexMap g) l1)))
      (conj @(:neighbors (get @(:vertices g) (get @(:vertexMap g) l1)))
        (neighbor. (get @(:vertexMap g) l2) w)))
    (ref-set (:neighbors (get @(:vertices g) (get @(:vertexMap g) l2)))
      (conj @(:neighbors (get @(:vertices g) (get @(:vertexMap g) l2)))
        (neighbor. (get @(:vertexMap g) l1) w)))))

(defn verticesReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:distance vertex) 0))))

;;may be combined with selectMinDistance?
(defn selectFromQueue [opQueue g]
  (with-local-vars [selectedVertex (dListFirst opQueue)]
    (loop [node @(:head opQueue)]
      (when (not (nil? node))
        (if (< @(:distance (get @(:vertices g) (get @(:vertexMap g) (:data node))))
               @(:distance (get @(:vertices g) (get @(:vertexMap g) @selectedVertex))))
          (var-set selectedVertex (:data node)))
        (recur @(:next node))))
    @selectedVertex))

;;may be combined with selectFromQueue?
(defn selectMinDistance [currentVertex g]
  (with-local-vars [selectedVertex (first @(:neighbors (get @(:vertices g) (get @(:vertexMap g) currentVertex))))]
    (doseq [neighbor @(:neighbors (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
      (if (< @(:distance (get @(:vertices g) (:index neighbor)))
             @(:distance (get @(:vertices g) (:index @selectedVertex))))
        (var-set selectedVertex neighbor)))
    @selectedVertex))

(defn processUnseenNeighbor [currentVertex neighbor opQueue g]
  (dListAppend opQueue (:label (get @(:vertices g) (:index neighbor))))
  (ref-set (:status (get @(:vertices g) (:index neighbor)))
    1)
  (if (= 0 @(:distance (get @(:vertices g) (:index neighbor))))
    (ref-set (:distance (get @(:vertices g) (:index neighbor)))
      (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
         (:weight neighbor)))))

(defn processOpenNeighbor [currentVertex neighbor g]
  (if (< (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
            (:weight neighbor))
         @(:distance (get @(:vertices g) (:index neighbor))))
    (ref-set (:distance (get @(:vertices g) (:index neighbor)))
      (+ @(:distance (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
         (:weight neighbor)))))

(defn processNeighbors [currentVertex opQueue g]
  (doseq [neighbor @(:neighbors (get @(:vertices g) (get @(:vertexMap g) currentVertex)))]
    (if (= 0 @(:status (get @(:vertices g) (:index neighbor))))
      (processUnseenNeighbor currentVertex neighbor opQueue g)
      (if (= 1 @(:status (get @(:vertices g) (:index neighbor))))
        (processOpenNeighbor currentVertex neighbor g)))))

(defn processCurrentVertex [opQueue g]
  (let [currentVertex (selectFromQueue opQueue g)]
    (dosync
      (dListDeleteElement opQueue currentVertex)
      (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
        2)
      ;; some code to process the graph Vertex if necessary
      (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) currentVertex)))
        3)
      (processNeighbors currentVertex opQueue g))))

(defn createPathList [path start finish g]
  (loop [currentVertex start]
    (when (not (= finish (dListLast path)))
      (let [neighbor (selectMinDistance currentVertex g)]
        (dListAppend path (:label (get @(:vertices g) (:index neighbor)))))
      (recur (dListLast path))))
  (dListIntoClojureList path))

(defn dijikstraMain [g opQueue path start finish]
  (if (not (dListEmpty opQueue))
    (do
      (processCurrentVertex opQueue g)
      (dijikstraMain g opQueue path start finish))
    (if (= 0 @(:distance (get @(:vertices g) (get @(:vertexMap g) start))))
      (str "There is no path to the destination")
      (createPathList path start finish g))))

(defn dijikstra [g start finish]
  (verticesReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        path (dList. (ref nil) (ref nil))]
    (dListAppend opQueue finish)
    (dListAppend path start)
    (if (= start finish)
      (dListIntoClojureList path)
      (dijikstraMain g opQueue path start finish))))

======================================================

(dijikstra g "Prague" "Prague") ;;for debugging

(dijikstra g "Prague" "Alessandria") ;;for ICA 2

(dijikstra g "Prague" "Catania") ;;for ICA 2
