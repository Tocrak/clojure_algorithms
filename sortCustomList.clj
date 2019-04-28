"Bubble Sort Single Linked List"
(defn compareSListData [node]
  (< (:data node) (:data (deref (:next node)))))

(defn swapSListNodes [node prevNode lst]
  (dosync
    (let [nextNode (deref (:next node))]
      (ref-set (:next node)
        (deref (:next nextNode)))
      (ref-set (:next nextNode)
        node)
      (if (not (nil? prevNode))
        (ref-set (:next prevNode)
          nextNode)
        (ref-set (:head lst)
          nextNode)))))

(defn sListIterSort [lst]
  (loop [node (deref (:head lst))
         prevNode nil
         swap 0]
    (let [nextNode (deref (:next node))]
      (if (and (not (nil? node))
               (not (nil? nextNode)))
        (if (compareSListData node)
          (recur nextNode node swap)
          (do (swapSListNodes node prevNode lst)
              (recur node nextNode (inc swap))))
        (= swap 0)))))

(defn sListBubbleSort [lst]
  (loop [done false]
    (if (not done)
      (recur (sListIterSort lst)))))

"Bubble Sort Double Linked List"
(defn compareDListData [node]
  (< (:data node) (:data (deref (:next node)))))

(defn swapDListNodes [node lst]
  (dosync
    (let [nextNode (deref (:next node))
          prevNode (deref (:prev node))]
      (if (not (nil? (deref (:next nextNode))))
        (ref-set (:prev (deref (:next nextNode)))
          node)
        (ref-set (:tail lst)
          node))
      (ref-set (:next node)
        (deref (:next nextNode)))
      (ref-set (:next nextNode)
        node)
      (ref-set (:prev node)
        nextNode)
      (ref-set (:prev nextNode)
        prevNode)
      (if (not (nil? prevNode))
        (ref-set (:next prevNode)
            nextNode)
        (ref-set (:head lst)
            nextNode)))))

(defn dListIterSort [lst]
  (loop [node (deref (:head lst))
         swap 0]
    (let [nextNode (deref (:next node))
          prevNode (deref (:prev node))]
      (if (and (not (nil? node))
               (not (nil? nextNode)))
        (if (compareSListData node)
          (recur nextNode swap)
          (do (swapDListNodes node lst)
              (recur node (inc swap))))
        (= swap 0)))))

(defn dListBubbleSort [lst]
  (loop [done false]
    (if (not done)
      (recur (dListIterSort lst)))))

"MergeSort Single Linked List"

(defn countSList [lst]
    (loop [node (deref (:head lst))
           nr 0]
      (if (not (nil? node))
        (do (inc nr)
            (recur (deref (:next node))
                   (inc nr)))
        nr)))

"Class Work"
"Bubble Sort sList Simplified"

(defn compare1 [node order]
  (order (:data @node) (:data @(:next node))))

(defn swap1 [node]
  (dosync
    (let* [currNode node
           nextNode (:next @currNode)
           next2Node (:next @nextNode)
           valNode @currNode
           val2Node @nextNode
           val3Node @next2Node]
      (ref-set currNode val2Node)
      (ref-set nextNode val3Node)
      (ref-set next2Node valNode))))

(defn compareSwap1 [node order]
  (if (not (compare1 node order))
    (do (swap1 node)
        1)
    0))

(defn canIter1 [node]
  (if (and (not (nil? @node))
           (not (nil? @(:next @node))))))

(defn iterSort1 [lst order]
  (loop [node (:head lst)
         swap 0]
    (when (canIter1 node)
      (recur (:next @node)
             (+ (compareSwap1 node order) swap)))
    swap))

(defn bubbleSort1 [lst order]
  (loop [done false]
    (if (not done)
      (recur (= (iterSort1 lst order) 0)))))

"Merge Sort"
