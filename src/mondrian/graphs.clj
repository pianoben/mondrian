(ns mondrian.graphs)

(defrecord graph [points edges directed?])
(defrecord edge  [from to weight])

(defn empty-graph
  []
  (graph.
   (hash-set) (hash-set) true))

(defn add-point
  "Adds a point to a graph"
  [{ps :points es :edges d :directed?} point]
  (graph. (conj ps point) es d))

(defn add-edge
  "Adds an edge to a graph"
  [graph, edge]
  (let [ps (:points graph)]
    (assert
     (and (contains? ps (:from edge)) (contains? ps (:to edge)))
     "Edges can only exist between points in a graph.")
    (graph. ps (conj (:edges graph) edge) (:directed? graph))))

(defn spanning-tree
  [graph]
  (loop [es (sorted-set-by (fn [{w1 :weight} {w2 :weight}]
                             (- w1 w2))
                           (:edges graph))
         vs (map 'hash-set (:points graph))]
    (if (or (empty? vs) (empty? es))
      vs
      (let [e (first es)
            rest (rest es)
            vfrom (:from e)
            vto (:to e)
            from (first (filter #(contains? % vfrom) es))
            to (first (filter #(contains? % vto) es))]
        (if (and (and vfrom vto) (not= vfrom vto))
          (let [node (reduce conj from to)
                forest (conj (disj (disj vs from) to) node)]
            (reduce forest rest))
          (reduce vs rest))))))

(defn- adjacencies
  [point edges]
  (let [es (vec (filter #(or (= (:from %) point) (= (:to %) point)) edges))]
    {point es}))

(defn- make-cycle
  [starting-edge starting-edge span adjs]
  (let [from (:from starting-edge)
        to   (:to   starting-edge)]
    (loop [visited (hash-set from)
           stack  '(starting-edge)
           cycle  '(from)]
      (let [next-edge  (first stack)
            cycle-head (first cycle)]
        (if (nil? next-edge)
          nil
          (let [n (if (not= cycle-head (:from next-edge)) (:from next-edge) (:to next-edge))
                c (conj cycle n)]
            (if (= n to)
              c
              (recur
               (conj visited n)
               (reduce conj (rest stack) (get adjs n))
               c))))))))
         
         
(defn cycle-basis
  [graph]
  (let [span (spanning-tree graph)
        adjs (hash-map (map #(adjacencies % span) (:points graph)))
        basis (filter #(not (contains? span %)) (:edges graph))]
    ;; do something
    ))