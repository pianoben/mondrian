(ns mondrian.intervals)

(defrecord itree [low high max color left right])

(defmacro max-or-cur [node cur]
  `(if-let [m# (:max ~node)] m#  ~cur))

(defn overlaps?
  [ll lh rl rh]
  (and
   (not (< lh rl))
   (not (< rh ll))))

(defn balance-case-one
  "Balances the case where a black node has red left child and left-left grandchild."
  [tree]
  (let [l  (:left tree)
	ll (:left l)
	a  (:left ll)
	b  (:right ll)
	c  (:right l)
	d  (:right tree)
	xl (:low ll)
	xh (:high ll)
	xm (:max ll)
	yl (:low l)
	yh (:high l)
	ym (:max l)
	zl (:low tree)
	zh (:high tree)
	zm (:max tree)]
    (itree. yl yh zm :red (itree. xl xh xm :black a b) (itree. zl zh zm :black c d))))

(defn balance-case-two
  "Balances the case where a black node has a red left child and left-right grandchild."
  [tree]
  (let [l  (:left tree)
	lr (:right l)
	a  (:left l)
	b  (:left lr)
	c  (:right lr)
	d  (:right tree)
	xl (:low l)
	xh (:high l)
	xm (:max l)
	yl (:low lr)
	yh (:high lr)
	ym (:max lr)
	zl (:low tree)
	zh (:high tree)
	zm (:max tree)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b) (itree. zl zh zm :black c d))))

(defn balance-case-three [tree]
  (let [r  (:right tree)
	rl (:left r)
	a  (:left tree)
	b  (:left rl)
	c  (:right rl)
	d  (:right r)
	xl (:low tree)
	xh (:high tree)
	xm (:max tree)
	yl (:low rl)
	yh (:high rl)
	ym (:max rl)
	zl (:low r)
	zh (:high r)
	zm (:max r)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b) (itree. zl zh zm :black c d))))

(defn balance-case-four [tree]
  (let [r  (:right tree)
	rr (:right r)
	a  (:left tree)
	b  (:left r)
	c  (:left rr)
	d  (:right rr)
	xl (:low tree)
	xh (:high tree)
	xm (:max tree)
	yl (:low r)
	yh (:high r)
	ym (:max r)
	zl (:low rr)
	zh (:high rr)
	zm (:max rr)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b) (itree. zl zh zm :black c d))))

(defn balance
  "A perhaps too-literal implementation of Okasaki's red-black balance fn."
  [tree]
  (let [ll-color (:color (:left (:left tree)))
	lr-color (:color (:right (:left tree)))
	rl-color (:color (:left (:right tree)))
	rr-color (:color (:right (:right tree)))
	l-color  (:color (:left tree))
	r-color  (:color (:right tree))
	color    (:color tree)]
    (cond
     (and (= color :black) (= l-color ll-color :red)) (balance-case-one tree)
     (and (= color :black) (= l-color lr-color :red)) (balance-case-two tree)
     (and (= color :black) (= r-color rl-color :red)) (balance-case-three tree)
     (and (= color :black) (= r-color rr-color :red)) (balance-case-four tree)
     :else tree)))

(defn insert [tree low high]
  (if tree
    (let [color (:color tree)
	  curh  (:high  tree)
	  curl  (:low   tree)
	  left  (:left  tree)
	  right (:right tree)]
      (cond
       (< low  curl) (balance (assoc tree :left (insert left low high)))
       (> low  curl) (balance (assoc tree :right (insert right low high)))
       (< high curh) (balance (assoc tree :left (insert left low high)))
       (> high curh) (balance (assoc tree :right (insert right low high)))
       :else tree))
    (itree. low high high :red nil nil)))

(defn empty-tree
  []
  nil)

(defn add-interval
  "Adds an interval to an itree."
  [tree low high]
  (let [root (insert tree low high)]
    (assoc root :color :black)))

(defn contains-point?
  "Indicates whether a given point falls within an interval in the given itree."
  [tree point]
  (loop [node tree]
    (if-not node
      false
      (let [low  (:low node)
            high (:high node)
            left (:left node)]
        (cond
         (and (>= point low) (<= point high)) true
         (and left (< point (:max left))) (recur (:left node))
         :else (recur (:right node)))))))

(defn get-overlapping-intervals
  "Returns a vector of intervals which overlap a given interval."
  [tree low high]
  (loop [node tree
	 ixs  []]
    (if-not node
      ixs
      (let [nlow  (:low node)
	    nhigh (:high node)
	    left  (:left node)
	    ixss  (if (overlaps? low high nlow nhigh) (conj ixs node) ixs)]
	(if (and left (< low (:max left)))
	  (recur left ixss)
	  (recur (:right node) ixss))))))