;; !!! DOUBLE LINKED LIST PART !!!

;;creates necessary templates for double linked list implementation
(defrecord dListNode [next data prev])
(defrecord dList [head tail])

;;looks if the  double linked list is empty
(defn dListEmpty [lst]
  (nil? (deref (:tail lst))))

;;adds to the end of the  double linked list
(defn dListAppend [lst val]
  (dosync
    (ref-set (:tail lst)
      (dListNode. (ref nil) val (ref (deref (:tail lst)))))
    (if (not (nil? (deref (:head lst))))
        (ref-set (:next (deref (:prev (deref (:tail lst)))))
          (deref (:tail lst)))
        (ref-set (:head lst)
          (deref (:tail lst))))))

;;outputs the first element of the  double linked list
(defn dListFirst [lst]
  (:data (deref (:head lst))))

;;removes the first element of the double linked list
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

;;creates necessary templates for graph implementation
(defrecord graph [vertices vertexMap edges])
(defrecord vertex [label status neigbors]) ;;index (not implemented)
(defrecord edge [firstVert secondVert])

;;creates an empty graph
(defn makeGraph []
  (graph. (ref []) (ref {}) (ref [])))

;;adds new vertices to the graph
(defn graphAddVertex [g label]
  (dosync
    (ref-set (:vertexMap g)
      (assoc @(:vertexMap g) label (count @(:vertices g))))
    (ref-set (:vertices g)
      (conj @(:vertices g) (vertex. label (ref 0) (ref ()))))))

;;adds new edges to the graph
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

;;resets the status of all vertices to unseen
(defn statusReset [g]
  (doseq [vertex @(:vertices g)]
    (dosync
      (ref-set (:status vertex) 0))))

;;searches and outputs the first unseen vertex it founds
(defn searchForUnseen [g]
  (loop [indx 0]
    (if (= 0 @(:status (get @(:vertices g) indx)))
      (:label (get @(:vertices g) indx))
      (recur (inc indx)))))

;;the main bfs function
;;if the open queue is not empty
;;it creates a local variable of the first vertex from the queue
;;then it removes the vertex from queue and changes its status to current
;;processes it if commands are written at specified line
;;then changes vertex's status to visited
;;searches among vertex's neoghbours for unseen ones
;;and puts them to open queue while changing their status to open
;;repeats the function
;;if the queue is empty it compares the number of iterated vertices with
;;their total number
;;if its smaller it uses the searchForUnseen function and adds the output to the queue
;;then it repeats the function while increasing the number of connected groups
;;if it is smaller it gives one of the two outputs based on the number of groups
(defn bfsMain [g opQueue vertCount groupCount]
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
        (doseq [neigbor @(:neigbors (get @(:vertices g) (get @(:vertexMap g) currentNode)))]
          (when (= 0 @(:status (get @(:vertices g) (get @(:vertexMap g) neigbor))))
            (dListAppend opQueue neigbor)
            (ref-set (:status (get @(:vertices g) (get @(:vertexMap g) neigbor)))
              1))))
      (bfsMain g opQueue (inc vertCount) groupCount))
    (if (< vertCount (count @(:vertices g)))
      (do (dListAppend opQueue (searchForUnseen g))
          (bfsMain g opQueue vertCount (inc groupCount)))
      (if (= 1 groupCount)
        (str "Graph is connected")
        (str "Graph is disconnected. Number of connected components is: " groupCount)))))

;;the starter function that takes the graph and a label of an vertex
(defn bfs [g & [label]]
  (statusReset g)
  (let [opQueue (dList. (ref nil) (ref nil))
        vertCount 0
        groupCount 0]
    (if (nil? label)
      (dListAppend opQueue (:label (get @(:vertices g) 0)))
      (dListAppend opQueue label))
    (bfsMain g opQueue vertCount (inc groupCount))))

======================================================

(bfs g)
