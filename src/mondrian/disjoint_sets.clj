(ns mondrian.disjoint-sets)

(defprotocol UnionFind
  "Represents the union-find algorithm."
  (union [u this that] "Joins two sets together.")
  (find  [u member]    "Finds the representative of the set to which member belongs.")
  (ranks [u]           "Returns the number of unique ranks."))

(defn- find-aux
  [field index]
  (let [fi (nth field index)]
    (if (= fi index)
      [field, index]
      (let [[f, r] (find-aux field fi)
            f2 (assoc f index r)]
        [f2, r]))))

(deftype DisjointSet
    [^:private nmap
     ^:private ranks
     ^:volatile-mutable links
     ^:private nranks]
  
  UnionFind

  (find
    [ds member]
    ;(locking ds
      (let [nmem (get nmap member)
            [f, cx] (find-aux links nmem)]
        (set! links f)
        cx))

  (union
    [ds r1 r2]
    ;(locking ds
      (let [cx (find ds r1)
            cy (find ds r2)]
        (if (= cx cy)
          ds
          (let [rx (nth ranks cx)
                ry (nth ranks cy)]
            (cond
             (> rx ry) (DisjointSet.
                        nmap
                        ranks
                        (assoc links cy cx)
                        (- nranks 1))
             (< rx ry) (DisjointSet.
                        nmap
                        ranks
                        (assoc links cx cy)
                        (- nranks 1))
             :else     (DisjointSet.
                        nmap
                        (assoc ranks cx (+ rx 1))
                        (assoc links cy cx)
                        (- nranks 1)))))))
  
  (ranks [_] nranks))

(defn make-disjoint-set
  [coll]
  (let [v (vec coll)
        len (count v)
        ranks (vec (take len (repeat 0)))
        links (vec (take len (iterate inc 0)))
        nmap  (zipmap v links)]
    (DisjointSet. nmap ranks links len)))
