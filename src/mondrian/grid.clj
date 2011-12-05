(ns mondrian.grid
  (:use mondrian.intervals)
  (:use mondrian.graphs))

(defprotocol Line
  (x   [line] "Gets the x coordinate of a line's origin.")
  (y   [line] "Gets the y coordinate of a line's origin.")
  (dir [line] "Gets a line's direction.")
  (len [line] "Gets the length of a line.")
  (intersects? [line other] "Determines if a line intersects with another."))

(defprotocol Grid
  (width    [grid] "Gets the width of a grid.")
  (height   [grid] "Gets the height of a grid.")
  (padding  [grid] "Gets the padding between lines of a grid.")) 

(defrecord GridLine [xval yval direction length]
  Line
  (x   [_] xval)
  (y   [_] yval)
  (dir [_] direction)
  (len [_] length)

  (intersects?
    [this that]
    (let [h  (if (= :h direction) this that)
          v  (if (= :v direction) this that)
          hx (x h)
          hy (y h)
          hl (len h)
          vx (x v)
          vy (y v)
          vl (len v)]
      (if (= direction (dir that))
        false
        (and (<= hx vx)
             (<= vy hy)
             (>= (+ hx hl) vx)
             (>= (+ vy vl) hy))))))

(defrecord MGrid [lines hpadding vpadding hlines vlines width height padding])

(defn empty-grid
  [width height padding]
  (let [hmax (- height 1)
        wmax (- width  1)]
    (MGrid.
     [(assoc (GridLine. 0 0    :h width)  :nodraw true)
      (assoc (GridLine. 0 wmax :h width)  :nodraw true)
      (assoc (GridLine. 0 0    :v height) :nodraw true)
      (assoc (GridLine. hmax 0 :v height) :nodraw true)]
     
     ; Horizontal padding itree
     (empty-tree)
     ; Vertical padding itree
     (empty-tree)

     ; FIXME: Find a better data structure for ix lists
     ; Horizontal intersection itree
     (-> (empty-tree)
         (add-interval 0 wmax #{0 hmax}))
     ; Vertical intersection itree
     (-> (empty-tree)
         (add-interval 0 hmax #{0 wmax}))
     width height padding)))

(defn random-point
  ([min max]
     (+ min (rand-int (- max min))))
  ([min max padding]
     (first (remove #(contains-point? padding %) (repeatedly #(random-point min max)))))) 

(defn select-random-endpoints
  [ixs]
  (let [v  (vec ixs)
        x  (rand-int (count v))
        y  (first (filter #(not= x %) (repeatedly #(rand-int (count v)))))
        vx (get v x)
        vy (get v y)]
    (if (< vx vy)
      [vx vy]
      [vy vx])))

(defn add-vertical-line
  [grid]
  (let [padding  (:vpadding grid)
        npadding (:padding grid)
        x        (random-point 0 (:width grid) padding)
        ixs      (flatten (map (comp vec tag) (stab (:hlines grid) x)))
        [a b]    (select-random-endpoints ixs)
        line     (GridLine. x a :v (- b a))]
    (-> grid
        (update-in [:lines] conj line)
        (update-in [:vpadding] add-interval (- x npadding) (+ x npadding) nil)
        (update-in [:vlines]  add-interval a b x))))

(defn add-horizontal-line
  [grid]
  (let [padding  (:hpadding grid)
        npadding (:padding  grid)
        y        (random-point 0 (:height grid) padding)
        ixs      (flatten (map (comp vec tag) (stab (:vlines grid) y)))
        [a b]    (select-random-endpoints ixs)
        line     (GridLine. a y :h (- b a))]
    (-> grid
        (update-in [:lines] conj line)
        (update-in [:hpadding] add-interval (- y npadding) (+ y npadding) nil)
        (update-in [:hlines]   add-interval a b y))))
  
(defn add-random-line
  [grid]
  (if (= 0 (rand-int 2))
    (add-vertical-line   grid)
    (add-horizontal-line grid)))

(defn make-grid
  [width height padding nlines]
  (loop [grid (empty-grid width height padding)
         n 0]
    (if (= n nlines)
      grid
      (recur (add-random-line grid) (inc n)))))

(defn points-of-line
  [grid line]
  (let [dir (:dir line)
        ixs ((if (= :h dir) :vlines :hlines) grid)
        sel (if (= :h dir) :x :y)
        min (sel line)
        max (+ min (:len line))
        vs  (->> (stab ixs (sel line))
                 (map (comp seq tag))
                 (flatten))
        ps  (if (= :h dir)
              (map #(vector [% (:y line)]) vs)
              (map #(vector [(:x line) %]) vs))
        kfn (if (= :h dir) first second)
        es  (partition 2 1 (sort-by kfn ps))]
    [ps es]))

(defn reduce-line
  [graph grid line]
  (let [[ps es] (points-of-line grid line)]
    (reduce #(fn [g [p1 p2]] (add-edge g p1 p2)) graph es)))

                  