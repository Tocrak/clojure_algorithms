;; !!! OUTDATED !!!
;; creates verticle template with verticle's name and metadata if necessary
;; if necessary it can store neighbours too
;;if it is required it can allow storage of verticle position
(defrecord verticle [name]) ;; positionX positionY]) ;; metadata])
;; creates edge template that requires two verticles
;; edgeType (-> for direct and <-> for undirected) weight and other metadata
;; edgeType is may not be necessary
(defrecord edge [firstVert secondVert edgeType weight]) ;; metadata])

(def vert1 (verticle. a))
(def vert2 (verticle. b))
(def edg1 (edge. vert1 vert2 "<->" 100))
;; or
(def edg2 (edge. vert1 vert2 "->" 100))
(def edg3 (edge. vert2 vert1 "->" 100))
;; store them using different dictionary, vector or other methods
(def vertGroupVect [vert1 vert2])
(def vertGroupDict {:a vert1
                    :b vert2})
(def egdeGroupVect1 [egd1])
(def egdeGroupDict1 {:a=b edg1})

(def egdeGroupVect2 [egd2 edg3])
(def egdeGroupDict2 {:a-b edg2}
                    :b-a edg3)

(defn graphPrint [g]
  (loop [i 0]
    (when (< i (count @(:edges g)))
      (println (str (:label (get @(:vertices g) (:firstVert (get @(:edges g) i))))
                    " -- "
                    (:label (get @(:vertices g) (:secondVert (get @(:edges g) i))))))
      (recur (inc i)))))

========================================================
;; !!! BEGINNING !!!
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

(defn graphPrint [g]
  (doseq [edge @(:edges g)]
    (println (str (:label (get @(:vertices g) (:firstVert edge)))
                  " -- "
                  (:label (get @(:vertices g) (:secondVert edge)))))))

(defn graphPrint [g]
  (doseq [vertex @(:vertices g)]
    (println (str "Label: " (:label vertex))
             (str "Status: " @(:status vertex))
             (str "Distance: " @(:distance vertex))
             (str "Neighbors: " @(:neighbors vertex)))))

(get @(:vertexMap g) l1)
(get @(:vertexMap g) label)
========================================================

(def g1 (makeGraph))
(graphAddVertex g1 "a")
(graphAddVertex g1 "b")
(graphAddVertex g1 "c")
(graphAddEdge g1 "a" "b")
(graphAddEdge g1 "b" "c")
(graphAddEdge g1 "a" "c")
(graphPrint g1)

(def g1 (makeGraph))
(graphAddVertex g1 "d")
(graphAddVertex g1 "e")
(graphAddVertex g1 "f")
(graphAddEdge g1 "d" "e")
(graphAddEdge g1 "e" "f")
(graphAddEdge g1 "d" "f")
(graphPrint g1)

(def g1 (makeGraph))
(graphAddVertex g1 "d")
(graphAddVertex g1 "a")
(graphAddVertex g1 "b")
(graphAddVertex g1 "c")
(graphAddVertex g1 "e")
(graphAddVertex g1 "g")
(graphAddVertex g1 "f")
(graphAddEdge g1 "a" "f" 1)
(graphAddEdge g1 "g" "f" 4)
(graphAddEdge g1 "b" "g" 3)
(graphAddEdge g1 "b" "c" 1)
(graphAddEdge g1 "a" "c" 2)
(graphAddEdge g1 "a" "d" 3)
(graphAddEdge g1 "b" "e" 5)
(graphPrint g1)
