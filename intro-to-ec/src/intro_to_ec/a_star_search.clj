(ns intro-to-ec.a-star-search
  (:require [clojure.set :as cset]
            [intro-to-ec.grid-problem-with-walls :as walls]))

;;========================================================================================================

(defn generate-path
  [came-from node]
  
  (if (= :start-node (get came-from node)) ;; Check the 'came-from' map and see where 'node' came-from
    [node]
    (conj 
     (generate-path came-from (get came-from node)) 
     node
     )
    )
  ) ;; Return a recursive conjugation of all the nodes

;=========================================================================================================

;; Filter out states that have already been visited
(defn remove-previous-states
  [new-states visited]
  (remove (set visited) new-states)
  )

;;=====================================================================================================

;; Manhatten distance
(defn heuristic-distance [goal curr]
  (+ (Math/abs (- (get curr 0) (get goal 0)))
     (Math/abs (- (get curr 1) (get goal 1)))))

;;======================================================================================================

;; A sum of the manhatten distance and 'cost-so-far'
(defn a-star-distance [goal curr came-from cost-so-far]
  (print came-from)
  (+ (heuristic-distance goal curr)
     (+ (cost-so-far (came-from curr)) 1)
     )
  )

;;======================================================================================================

;; Sorts the frontier with better scores first. Used instead of priority map
(defn sortFrontier [frontier]
  (into (sorted-map-by (fn [key1 key2]
                         (compare [(get frontier key1) key1]
                                  [(get frontier key2) key2])))
        frontier))

;;============================================================================================

;; This checks the frontier for nodes in common with the children. If any exist, it checks to see which one
;; has the better score, and returns a map of them.
(defn filterFrontier [frontier children]
  (let [common-nodes (vec
                      (cset/intersection (set (keys frontier)) (set (keys children))))]
    ;; ABOVE: the nodes that frontier has in common with the children.

    (loop
     [x (- (count common-nodes) 1) ;; A count for iterating through the common nodes using 'nth'
      returnNodes {} ;; this will contain the nodes that should now be added to the frontier 
      ]

      (cond

        (< x 0) returnNodes

        :else
        (let [curr-node (nth common-nodes x) ;; define the current node to check
              children-node-score (children curr-node) ;; define the score of that node in the children
              frontier-node-score (frontier curr-node) ;; also do that for the frontier
              ]

          (recur ;; Now for each entry...
           (dec x) ;; go down an index for x
           (if
            (< children-node-score frontier-node-score)
             (assoc returnNodes curr-node children-node-score)
             (assoc returnNodes curr-node frontier-node-score))
           ;; ABOVE: if the child node has a better score than the one on the frontier, add it.
           ;;    Otherwise, add the frontier node back.
           ))))))

;;=====================================================================================================
(def a-star-search

  ;; Always performed on a sorted frontier
  {:get-next-node first

   ;; Use the filterFrontier function to create a NEW frontier.
   :add-children #(conj %1 (filterFrontier %1 %2))}) ;; [frontier, children]
;;========================================================================================================

(defn search

  [{:keys [get-next-node add-children]}   
   {:keys [goal? make-children goal-pos]}  
   start-node                              
   max-calls]

  (loop
   [came-from {start-node start-node} ;; Came-from is a MAP. It originally says, "starting node came from itself."   
    cost-so-far {start-node 0}          ;; Start node is a map that tracks 'steps-so-far'
    frontier (hash-map start-node 
                       (a-star-distance goal-pos start-node came-from cost-so-far)) 
    ;; ABOVE: The frontier originally only contains the starting node (and score)
    
    visited [] ;; Visited is originally empty.          
    num-calls 0] 

    (let [current-node (get-next-node (sortFrontier frontier))]

      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached

        :else

        (let [children (remove-previous-states
                        (make-children current-node) (keys came-from))]
          (

          (recur

           (into {} (map #(assoc came-from % current-node) children)) ;; Update came-from with children/parent entries
           (into {} (map
                     #(assoc cost-so-far % (+ 1 (cost-so-far current-node)))
                     children)) ;; Update the cost-so-far with children and 'cost-so-far' entries
           
           (add-children frontier children) ;; Filter out children with bad scores
           (conj visited current-node) ;; add current-node to visited list
           (inc num-calls) ;; increment max-calls
           ))))))
)