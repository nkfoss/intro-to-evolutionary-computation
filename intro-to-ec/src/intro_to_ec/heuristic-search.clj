(ns intro-to-ec.search-with-solutions
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

  (defn heuristic-distance [goal curr]
    "Finds the distance from the given node to the goal"
    (+ (abs(- (get curr 0)(get goal 0)))
       (abs(- (get curr 1)(get goal 1)))))

(def depth-first-search
  {:get-next-node first
   :add-children concat})
frontier [start-node]
(def breadth-first-search
  {:get-next-node first
   :add-children #(concat %2 %1)})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})

(def heuristic-search
  {:get-next-node first
   :add-children #(pm (get %1 0) (heuristic-distance %2 %1))})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]remove-previous-states
    (conj (generate-path came-from (get came-from node)) node)))

(defn search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children goal-pos]}
   start-node max-calls]
  (loop [frontier (pm start-node (heuristic-distance goal start-node))
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
            (rest frontier))
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
