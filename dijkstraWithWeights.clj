;;  DOUBLE LINKED LIST PART

(defrecord dListNode [next data prev])
(defrecord dList [head tail])

(defn dListEmpty? [lst]
  (nil? @(:tail lst)))

(defn dListAppend [lst val]
  (dosync
    (ref-set (:tail lst)
      (dListNode. (ref nil) val (ref @(:tail lst))))
    (if (not (nil? @(:head lst)))
        (ref-set (:next @(:prev @(:tail lst)))
          @(:tail lst))
        (ref-set (:head lst)
          @(:tail lst)))))

(defn dListFirst [lst]
  (:data @(:head lst)))

(defn dListLast [lst]
  (:data @(:tail lst)))

(defn dListRemFirst [node lst]
  (ref-set (:prev @(:next node))
    nil)
  (ref-set (:head lst)
    @(:next node))
  (ref-set (:next node)
    nil))

(defn dListRemLast [node lst]
  (ref-set (:next @(:prev node))
    nil)
  (ref-set (:tail lst)
    @(:prev node))
  (ref-set (:prev node)
    nil))

(defn dListRemMiddle [node]
  (ref-set (:next @(:prev node))
    @(:next node))
  (ref-set (:prev @(:next node))
    @(:prev node))
  (ref-set (:prev node)
    nil)
  (ref-set (:next node)
    nil))

(defn dListRemAll [lst]
  (ref-set (:head lst)
    nil)
  (ref-set (:tail lst)
    nil))

(defn dListDeleteElement [node lst]
  (dosync
    (if (and (nil? @(:prev node))
             (nil? @(:next node)))
      (dListRemAll lst)
      (if (nil? @(:prev node))
        (dListRemFirst node lst)
        (if (nil? @(:next node))
          (dListRemLast node lst)
          (dListRemMiddle node))))))

(defn dListIntoClojureList [dList]
  (with-local-vars [resultList ()]
    (loop [node (deref (:tail dList))]
      (when (not (nil? node))
        (var-set resultList (conj @resultList (:data node)))
        (recur (deref (:prev node)))))
    @resultList))

;;  GRAPH PART
;;  vertex states: unseen(0) open(1) current(2) visited(3)

(defrecord graph [vertices vertexMap])
(defrecord vertex [label status neighbors distance])
(defrecord neighbor [index weight])

(defn makeGraph []
  (graph. (ref []) (ref {})))

(defn callVertexLabel [label g]
  (get @(:vertices g) (get @(:vertexMap g) label)))

(defn callVertexIndex [index g]
  (get @(:vertices g) index))

(defn graphAddVertex [g label]
  (dosync
    (ref-set (:vertexMap g)
      (assoc @(:vertexMap g) label (count @(:vertices g))))
    (ref-set (:vertices g)
      (conj @(:vertices g) (vertex. label (ref 0) (ref ()) (ref 0))))))

(defn graphAddEdge [g l1 l2 w]
  (dosync
    (ref-set (:neighbors (callVertexLabel l1 g))
      (conj @(:neighbors (callVertexLabel l1 g))
        (neighbor. (get @(:vertexMap g) l2) w)))
    (ref-set (:neighbors (callVertexLabel l2 g))
      (conj @(:neighbors (callVertexLabel l2 g))
        (neighbor. (get @(:vertexMap g) l1) w)))))

(defn verticesReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:distance vertex) 0))))

;;may be combined with selectMinDistance?
(defn selectFromQueue [opQueue g]
  (with-local-vars [selectedVertex @(:head opQueue)]
    (loop [node @(:head opQueue)]
      (when (not (nil? node))
        (if (< @(:distance (callVertexLabel (:data node) g))
               @(:distance (callVertexLabel (:data @selectedVertex) g)))
          (var-set selectedVertex node))
        (recur @(:next node))))
    (dListDeleteElement @selectedVertex opQueue)
    (:data @selectedVertex)))

;;may be combined with selectFromQueue?
(defn selectMinDistance [currentVertex g]
  (with-local-vars [selectedVertex
                    (first @(:neighbors (callVertexLabel currentVertex g)))]
    (doseq [neighbor @(:neighbors (callVertexLabel currentVertex g))]
      (if (< @(:distance (callVertexIndex (:index neighbor) g))
             @(:distance (callVertexIndex (:index @selectedVertex) g)))
        (var-set selectedVertex neighbor)))
    @selectedVertex))

(defn processUnseenNeighbor [currentVertex neighbor opQueue g]
  (dListAppend opQueue (:label (callVertexIndex (:index neighbor) g)))
  (ref-set (:status (callVertexIndex (:index neighbor) g))
    1)
  (if (= 0 @(:distance (callVertexIndex (:index neighbor) g)))
    (ref-set (:distance (callVertexIndex (:index neighbor) g))
      (+ @(:distance (callVertexLabel currentVertex g))
         (:weight neighbor)))))

(defn processOpenNeighbor [currentVertex neighbor g]
  (if (< (+ @(:distance (callVertexLabel currentVertex g))
            (:weight neighbor))
         @(:distance (callVertexIndex (:index neighbor) g)))
    (ref-set (:distance (callVertexIndex (:index neighbor) g))
      (+ @(:distance (callVertexLabel currentVertex g))
         (:weight neighbor)))))

(defn processNeighborUsingHops [currentVertex neighbor opQueue g]
  (dListAppend opQueue (:label (callVertexIndex (:index neighbor) g)))
  (ref-set (:status (callVertexIndex (:index neighbor) g))
    1)
  (ref-set (:distance (callVertexIndex (:index neighbor) g))
    (inc @(:distance (callVertexLabel currentVertex g)))))

(defn processNeighbors [currentVertex opQueue g hops]
  (if hops
    (doseq [neighbor @(:neighbors (callVertexLabel currentVertex g))]
      (when (= 0 @(:status (callVertexIndex (:index neighbor) g)))
        (processNeighborUsingHops currentVertex neighbor opQueue g)))
    (doseq [neighbor @(:neighbors (callVertexLabel currentVertex g))]
      (if (= 0 @(:status (callVertexIndex (:index neighbor) g)))
        (processUnseenNeighbor currentVertex neighbor opQueue g)
        (if (= 1 @(:status (callVertexIndex (:index neighbor) g)))
          (processOpenNeighbor currentVertex neighbor g))))))

(defn processCurrentVertex [opQueue g hops]
  (let [currentVertex (selectFromQueue opQueue g)]
    (dosync
      (ref-set (:status (callVertexLabel currentVertex g))
        2)
      (processNeighbors currentVertex opQueue g hops)
      (ref-set (:status (callVertexLabel currentVertex g))
        3))))

(defn dijkstraMarkingGraph [opQueue g hops]
  (when (not (dListEmpty? opQueue))
    (processCurrentVertex opQueue g hops)
    (dijkstraMarkingGraph opQueue g hops)))

(defn dijkstraCreatePath [path g]
  (loop [currentVertex (dListFirst path)]
    (when (not (= 0 @(:distance (callVertexLabel currentVertex g))))
      (let [neighbor (selectMinDistance currentVertex g)]
        (dListAppend path (:label (callVertexIndex (:index neighbor) g))))
      (recur (dListLast path))))
  (dListIntoClojureList path))

(defn dijkstra [g start finish & [hops]]
  (verticesReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        path (dList. (ref nil) (ref nil))]
    (dListAppend opQueue finish)
    (dListAppend path start)
    (if (= start finish)
      (dListIntoClojureList path)
      (do
        (if hops
          (dijkstraMarkingGraph opQueue g true)
          (dijkstraMarkingGraph opQueue g false))
        (if (= 0 @(:distance (callVertexLabel start g)))
          (list)
          (dijkstraCreatePath path g))))))

======================================================

(dijkstra g "Prague" "Prague") ;;for debugging

(dijkstra g "Prague" "Alessandria") ;;for ICA 2

(dijkstra g "Prague" "Catania") ;;for ICA 2

(comment
 to add easier the vertices use a list then convert to a vector
 vertexMap may be created using a BST
 neigbors list may be created using single/double linked list
 BST dict for open set
 each key of the disctionary will have a list of elements with the same value)

(comment to do list:
  1. separate the marking and backtracking part (y)
  2. make it work with both hops and weights (y)
  3. optimise the calling of values (y)
  4. transform the neigbors simple list into single linked list (n) /optional
  5. make default distance infinity/big value (n) /optional
  6. keep the open queue sorted / combine finding and deleting function (y)
  7. general optimisation, if possible (n))

(comment
  the queue creation may be improved using single linked list
  create a single linked list that stores [distance index nextRef]
  functions needed queueInsert queueTakeFirst)

;;for debugging
(defn graphPrint [g]
  (doseq [vertex @(:vertices g)]
    (println (str "Label: " (:label vertex) ";"))
    (println (str "Status: " @(:status vertex) ";"))
    (println (str "Distance: " @(:distance vertex) ";"))
    (println (str "Neighbors: "))
    (doseq [neighbor @(:neighbors vertex)]
      (println "  Index: " (:index neighbor) ";")
      (println "  Weight: " (:weight neighbor) ";"))
    (newline)))

(def g1 (makeGraph))
(graphAddVertex g1 "Home")
(graphAddVertex g1 "Shop")
(graphAddVertex g1 "Guild")
(graphAddVertex g1 "Dungeon")
(graphAddVertex g1 "Port")
(graphAddVertex g1 "Town")
(graphAddVertex g1 "Fields")
(graphAddEdge g1 "Shop" "Fields" 1)
(graphAddEdge g1 "Town" "Fields" 4)
(graphAddEdge g1 "Guild" "Town" 9)
(graphAddEdge g1 "Guild" "Dungeon" 6)
(graphAddEdge g1 "Shop" "Dungeon" 2)
(graphAddEdge g1 "Shop" "Home" 3)
(graphAddEdge g1 "Guild" "Port" 5)
(graphPrint g1)
