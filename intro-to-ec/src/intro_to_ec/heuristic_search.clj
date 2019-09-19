(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def helper-map (pm/priority-map))

(defn heuristic-distance [goal curr]
  "Finds the distance from the given node to the goal"
  (+ (Math/abs (- (get curr 0)(get goal 0)))
     (Math/abs (- (get curr 1)(get goal 1)))))

(defn pair [goal child map]
  "Pairs nodes to distance from goal"
  (assoc map child (heuristic-distance goal child)))

(defn make-p-map
  [children goal]
  "For child in children, make a priority map of their distance to the goal"
  (for [child children]
        (pair goal child helper-map)))

(def heuristic-search
  {:get-next-node first
   :add-children #(concat (for [x (make-p-map %1 %3)]
                          (first (first x)))
                          %2)})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn search

  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children goal-pos]}
   start-node max-calls]

  (loop [frontier [start-node]
         came-from {start-node :start-node}
         num-calls 0]

    (println num-calls ": " frontier)
    (println came-from)
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
