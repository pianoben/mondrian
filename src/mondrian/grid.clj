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
         (add-ix 0 wmax #{0 hmax}))
     ; Vertical intersection itree
     (-> (empty-tree)
         (add-ix 0 hmax #{0 wmax}))
     width height padding)))

(defn random-point
  ([min max]
     (+ min (rand-int (- max min))))
  ([min max padding]
     (let [p (first
              (remove #(contains-point? padding %)
                                        ; Instead of an infinite seq, try ten times - if after ten attempts we can't find an appropriate point, call the grid full.
                      (take 10 (repeatedly #(random-point min max)))))]
       (if p
         p
         (throw (java.lang.RuntimeException. "Full grid."))))))

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
        ixs      (get-ixs (:hlines grid) x)
        [a b]    (select-random-endpoints ixs)
        line     (GridLine. x a :v (- b a))]
    (-> grid
        (update-in [:lines] conj line)
        (update-in [:vpadding] add-interval (- x npadding) (+ x npadding) nil)
        (update-in [:vlines] add-ix a b x))))

(defn add-horizontal-line
  [grid]
  (let [padding  (:hpadding grid)
        npadding (:padding  grid)
        y        (random-point 0 (:height grid) padding)
        ixs      (get-ixs (:vlines grid) y)
        [a b]    (select-random-endpoints ixs)
        line     (GridLine. a y :h (- b a))
	with-line (update-in grid [:lines] conj line)
	with-pad  (update-in with-line [:hpadding] add-interval (- y npadding) (+ y npadding) nil)
	with-int  (update-in with-pad [:hlines] add-ix a b y)]
    with-int))

(defn add-random-line
  [grid]
  (try
    (if (= 0 (rand-int 2))
      (add-vertical-line   grid)
      (add-horizontal-line grid))
    (catch java.lang.RuntimeException e
      nil)))

(defn make-grid
  [width height padding nlines]
  (loop [grid (empty-grid width height padding)
         n 0]
    (if (= n nlines)
      grid
      (if-let [g (add-random-line grid)]
        (recur g (inc n))
        grid))))

(defn gridline-to-line2d
  [line]
  (let [d   (dir line)
        x1  (x line)
        y1  (y line)
        x2  (if (= :h d) (+ x1 (len line)) x1)
        y2  (if (= :v d) (+ y1 (len line)) y1)]
    (java.awt.geom.Line2D$Double. x1 y1 x2 y2)))

(defn draw-grid
  [grid]
  (let [lines (map gridline-to-line2d (:lines grid))
        panel (proxy [javax.swing.JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (doseq [l lines]
                    (.draw g l))))]

    (doto (javax.swing.JFrame.)
      (.setContentPane panel)
      (.setSize (:width grid) (:height grid))
      (.show))))

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