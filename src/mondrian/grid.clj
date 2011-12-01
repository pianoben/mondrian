(ns mondrian.grid
  (:use [mondrian.intervals]))

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

(defrecord Grid [hpadding vpadding hixs vixs width height padding])

(defn empty-grid
  [width height padding]
  (let [hmax (- height 1)
        wmax (- width  1)]
    (Grid.
     ; Horizontal padding itree
     (empty-tree)
     ; Vertical padding itree
     (empty-tree)

     ; FIXME: Find a better data structure for ix lists
     ; Horizontal intersection itree
     (-> (empty-tree)
         (add-interval 0 0 :nodraw)
         (add-interval wmax wmax :nodraw))
     ; Vertical intersection itree
     (-> (empty-tree)
         (add-interval 0 0 :nodraw)
         (add-interval hmax hmax :nodraw))
     width height padding)))

(defn add-line
  [grid dir padding intersecting-lines]
  
  
(defn add-random-line
  [grid]
  (if (= 0 (rand-int 2))
    (add-vertical-line grid)
    (add-horizontal-line grid)))    

(defn make-grid
  [width height padding num-of-lines]
  (iterate add-random-line (