(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(defn heuristic-distance [goal curr]
  "Finds the distance from the given node to the goal"
  (+ (Math/abs (- (get curr 0)(get goal 0)))
     (Math/abs (- (get curr 1)(get goal 1)))))

(defn pair [goal child]
  "Pairs nodes to distance from goal"
  (pm/priority-map child (heuristic-distance goal child)))

(def heuristic-search
  {:get-next-node first
   :add-children #(assoc %2 (for [child %1 (pair %3 child)]))})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn search

  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children goal-pos]}
   start-node max-calls]

  (loop [frontier (pm/priority-map start-node (heuristic-distance goal-pos start-node))
         came-from {start-node :start-node}
         num-calls 0]

    (println num-calls ": " frontier)
    (println came-from)(reduce (fn [cf child] (assoc cf child current-node)) came-from kids)

    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children
            kids
            (rest frontier)
            goal-pos)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
