(ns mondrian.graphs
  (:use [clojure [set :only [union]]]))

(defprotocol Graph
  "Defines operations supported by a weighted graph."
  (points    [this] "Yields a set of points within the graph.")
  (edges     [this] "Yields a set of edges within the graph.")
  (add-point [this point] "Adds a point to the graph.")
  (add-edge  [this n1 n2] [this p1 p2 w] "Adds an edge, possibly weighted, to the graph.  Edges without weights are added with a default weight of 1.")
  (has-point? [this point] "Returns logical true if the given point is contained within the graph.")
  (has-edge?  [this n1 n2] "Returns logical true if the given edge is contained within the graph.")
  (directed? [this] "Returns logical true if the graph is directed, and false otherwise.")
  (neighbors [this point] "Returns a set of points adjacent to the given point in the graph.")
  (weights [this p1 p2] "Returns a set of the weights of each edge connecting p1 and p2."))

(defn- make-edge
  [n1 n2 w]  
  (if (< (hash n1) (hash n2))
    [n1 n2 w]
    [n2 n1 w]))

(defrecord UndirectedGraph [points edges adjacencies weights]
  Graph  
  (points
    [this]
    points)

  (edges
    [this]
    edges)
  
  (add-point
    [this point]
    (update-in this [:points] conj point))

  (add-edge
    [this p1 p2]
    (add-edge this p1 p2 1))
  
  (add-edge
    [this p1 p2 w]
    (-> this
        (update-in [:points] conj p1 p2)
        (update-in [:edges] conj (make-edge p1 p2 w))
        (update-in [:adjacencies p1] (fnil conj #{}) p2)
        (update-in [:adjacencies p2] (fnil conj #{}) p1)
        (update-in [:weights  p1 p2] (fnil conj #{}) w)
        (update-in [:weights  p2 p1] (fnil conj #{}) w)))

  (has-point?
    [this p]
    (contains? points p))
  
  (has-edge?
    [this p1 p2]
    (contains? (adjacencies p1) p2))

  (neighbors
    [this point]
    (if-let [ns (adjacencies point)]
      ns
      #{}))

  (weights
    [this p1 p2]
    (get-in this [:weights p1 p2] #{})))

(defn empty-graph
  "Creates an empty graph"
  {:added "0.10"
   :static true}
  []
  (UndirectedGraph. (hash-set) (hash-set) (hash-map) (hash-map)))

(defn- get-weight
  [[_ _ w]]
  w)

(defn spanning-tree
  "Computes the minimum spanning tree of a given graph and
  returns it as a vector of edges in the form [[from to weight]].

  Uses Kruskal's algorithm."
  {:static true}
  [graph]
  (loop [sorted-edges (vec (sort-by get-weight (edges graph)))
         trees (reduce conj (hash-set) (map hash-set (points graph)))
         span  []]
    (cond
     (<= (count trees) 1)  span
     (empty? sorted-edges) nil
     :else
     (let [e  (first sorted-edges)
           es (rest sorted-edges)
           n1 (first e)
           n2 (second e)
           t1 (first (filter #(contains? % n1) trees))
           t2 (first (->> trees
                          (filter #(contains? % n2))
                          (remove #(= t1 %))))]
       (if (and (and t1 t2) (not= t1 t2))
         (let [t3 (union t1 t2)
               forest (conj (disj (disj trees t1) t2) t3)]
           (recur es forest (conj span e)))
         (recur es trees span))))))

(defn points-of-spanning-tree
  "Converts a spanning tree to a set of constituent points"
  {:static true}
  [span]
  (->> span
       (map (fn [[p1 p2 _]] [p1 p2]))
       (flatten)
       (set)))

(defn weight-of-spanning-tree
  "Calculates the weight of a spanning tree."
  {:static true}
  [span]
  (->> span
       (map (fn [[_ _ w]] w))
       (reduce + 0)))

(defn adjacencies-of-spanning-tree
  "Derives an adjacency map from a spanning tree, in the form of {from #{to}}."
  {:static true}
  [span]
  (let [f (fn [adjs [x y _]]
            (-> adjs
                (update-in [x] (fnil conj #{}) y)
                (update-in [y] (fnil conj #{}) x)))]
    (reduce f {} span)))

(defn make-queue
  {:static true}
  ([from adjacencies]
     (make-queue from adjacencies identity))
  ([from adjacencies f]
     (let [q (clojure.lang.PersistentQueue/EMPTY)
           mapped (map f (adjacencies from))]
       (reduce conj q mapped))))

(defn cycle-neighbors
  [from adjacencies visited]
  (remove #(contains? visited %) (adjacencies from)))

(defn make-cycle
  {:static true}
  [from to adjacencies]
  (loop [q (make-queue from adjacencies (fn [p] [[from p] p]))
         visited #{from}]
    (if (empty? q)
      nil
      (let [[path current] (peek q)
            new-visited (conj visited current)]
        (if (= current to)
          path
          (let [next-neighbors (cycle-neighbors current adjacencies new-visited)
                next-entries (map (fn [p] [(conj path p) p]) next-neighbors)]
            (recur (reduce conj (pop q) next-entries)
                   new-visited)))))))

(defmacro abs
  "Yields the absolute value of a numeric argument."
  [x]
  `(if (< ~x 0) (- ~x) ~x))

;;; A cycle is split by and edge iff from and to are
;;; contained in cycle and are non-adjacent (i.e. the
;;; edge isn't already in the cycle.
;;;
;;; We determine this by computing the indices in the
;;; cycle of from and to.  Given ix-from and ix-to,
;;; the check becomes:
;;;   a) ix-from and ix-to exist and
;;;   b) ix-from != 0 and ix-to != (last-ix cycle)
;;;   c) |ix-from - ix-to| > 1
(defn splits-cycle?
  {:static true}
  [from to cycle]
  (let [ixs (->> cycle
                 (keep-indexed #(when (or (= %2 from) (= %2 to)) %1))
                 (vec))
        ix1 (first ixs)
        ix2 (second ixs)
        len (count cycle)]
    (cond (not= 2 (count ixs)) false
          (and (= 0 ix1) (= (- len 1) ix2)) false
          (<= (abs (- ix1 ix2)) 1) false
          :else true)))

(defn split-cycle
  {:static true}
  [from to c]
  (assert (splits-cycle? from to c) "Should only be called when [from to] splits cycle c.")
  (let [cs (cycle c)
        c1 (conj (vec (->> cs
                           (drop-while #(not= % from))
                           (take-while #(not= % to))))
                 to)
        c2 (conj (vec (->> cs
                           (drop-while #(not= % to))
                           (take-while #(not= % from))))
                 from)]
    [c1 c2]))

(defn split-cycle-if-necessary
  {:static true}
  [from to cycles]
  (reduce (fn [[has-split cs] c]
            (if (splits-cycle? from to c)
              (let [[c1 c2] (split-cycle from to c)]
                [true (conj (conj cs c1) c2)])
              [has-split (conj cs c)]))
          [false []]
          cycles))

(defn add-edge-to-adjacencies
  {:static true}
  [adjs [p1 p2 & _]]
  (-> adjs
      (update-in [p1] (fnil conj #{}) p2)
      (update-in [p2] (fnil conj #{}) p1)))

;;; Algorithm:
;;; Start with the minimum spanning tree  of the graph.
;;; Find all edges not in the MST, and add them one by one.
;;; Each edge so added will add exactly one cycle; either
;;; a new cycle is created, or an existing cycle is split
;;; in two.  Through this process we maintain a list of
;;; cycles - in the latter case, the original cycle is
;;; removed from the list and its components are added in
;;; its place.  After all edges have been so added, we are
;;; left with a list of all fundamental cycles in the graph.
(defn fundamental-cycle-basis
  "Gets a list of all cycles in a graph's fundamental cycle basis."
  {:static true
   :added "0.10"}
  [graph]
  (let [span (spanning-tree graph)]
    (loop [edges (let [spanset (set span)
                       remover #(contains? spanset %)
                       es (edges graph)]
                   (vec (remove remover es)))
           adjs (reduce add-edge-to-adjacencies {} span)
           cycles []]
      (if (empty? edges)
        cycles
        (let [[e & es] edges
              [from to & _] e
              [did-split cs] (split-cycle-if-necessary from to cycles)
              new-cycles (if did-split
                           cs
                           (conj cs (make-cycle from to adjs)))
              new-adjs (add-edge-to-adjacencies adjs e)]
          (recur es new-adjs new-cycles))))))