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

(defn a-star-distance [start goal curr]
  (+ (heuristic-distance start curr)
     (heuristic-distance goal curr)))

(defn pair-heuristic [goal child map]
  "Pairs nodes to distance from goal"
  (assoc map child (heuristic-distance goal child)))

(defn make-p-map-heuristic
  [children goal map start]
  (if (not (empty? children))
    (do
      ; (pair goal
      ;   (first children)
      ;   map)
      (make-p-map-heuristic (rest children) goal
            (pair-heuristic goal
              (first children)
              map)
                            start))
    map))

(defn pair-a-star [goal child map start]
  "Pairs nodes to distance from goal"
  (assoc map child (a-star-distance start goal child)))

(defn make-p-map-a-star
  [children goal map start]
  (if (not (empty? children))
    (do
      ; (pair goal
      ;   (first children)
      ;   map)
      (make-p-map-a-star (rest children) goal
                         (pair-a-star goal
                               (first children)
                               map
                               start)
                         start))
    map))

(def heuristic-search
  {:get-next-node first
   :add-children #(conj %4 (first (first (make-p-map-heuristic %1 %2 %3 %5))))})

(def a-star-search
  {:get-next-node first
   :add-children #(conj %4 (first (first (make-p-map-a-star %1 %2 %3 %5))))})

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

    (println num-calls ": " frontier "a-star-distance: "(a-star-distance start-node goal-pos (get-next-node frontier)))
    ; (println came-from)
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
            goal-pos
            helper-map
            (rest frontier)
            start-node)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
