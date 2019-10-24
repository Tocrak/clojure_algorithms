
(defn dListDeleteElement [node lst]
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

(defn processCurrentVertex [opQueue g hops]
  (let [currentVertex (selectFromQueue opQueue g)]
    (dosync
      (ref-set (:status (callVertexLabel currentVertex g))
        2)
      (processNeighbors currentVertex opQueue g hops)
      (ref-set (:status (callVertexLabel currentVertex g))
        3))))
