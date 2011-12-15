(ns mondrian.intervals
  (:use clojure.set))

(defrecord itree [low high max color left right tag])

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
        xt (:tag ll)
	yl (:low l)
	yh (:high l)
	ym (:max l)
        yt (:tag l)
	zl (:low tree)
	zh (:high tree)
	zm (:max tree)
        zt (:tag tree)]
    (itree. yl yh zm :red (itree. xl xh xm :black a b xt) (itree. zl zh zm :black c d zt) yt)))

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
        xt (:tag l)
	yl (:low lr)
	yh (:high lr)
	ym (:max lr)
        yt (:tag lr)
	zl (:low tree)
	zh (:high tree)
	zm (:max tree)
        zt (:tag tree)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b xt) (itree. zl zh zm :black c d zt) yt)))

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
        xt (:tag tree)
	yl (:low rl)
	yh (:high rl)
	ym (:max rl)
        yt (:tag rl)
	zl (:low r)
	zh (:high r)
	zm (:max r)
        zt (:tag r)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b xt) (itree. zl zh zm :black c d zt) yt)))

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
        xt (:tag tree)
	yl (:low r)
	yh (:high r)
	ym (:max r)
        yt (:tag r)
	zl (:low rr)
	zh (:high rr)
	zm (:max rr)
        zt (:tag rr)]
    (itree. yl yh zm :red (itree. xl xh (max-or-cur b xh) :black a b xt) (itree. zl zh zm :black c d zt) yt)))

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

(defn update-tag
  [tree value update-fn]
  (update-in tree [:tag] update-fn value))

(defn insert [tree low high tag update-fn]
  (if tree
    (let [color (:color tree)
	  curh  (:high  tree)
	  curl  (:low   tree)
	  left  (:left  tree)
	  right (:right tree)]
      (cond
       (< low  curl) (balance (assoc tree :left  (insert left  low high tag update-fn)))
       (> low  curl) (balance (assoc tree :right (insert right low high tag update-fn)))
       (< high curh) (balance (assoc tree :left  (insert left  low high tag update-fn)))
       (> high curh) (balance (assoc tree :right (insert right low high tag update-fn)))
       :else (update-in tree [:tag] update-fn tag)))
    (itree. low high high :red nil nil tag)))

(defn empty-tree
  []
  nil)

(defn ^:private replace-value
  [_ newValue]
  newValue)

(defn add-interval
  "Adds an interval to an itree."
  ([tree low high]
     (add-interval tree low high nil replace-value))
  ([tree low high tag]
   (let [root (insert tree low high tag replace-value)]
     (assoc root :color :black)))
  ([tree low high tag update-fn]
     (let [root (insert tree low high tag update-fn)]
       (assoc root :color :black))))

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

(defn stab
  [tree point]
  (get-overlapping-intervals tree point point))

(defn tag
  [tree]
  (:tag tree))

(defprotocol IxTree
  (add-ix [tree p1 p2 ix])
  (get-ixs [tree point]))

(defn ensure-set
  [ix]
  (if (instance? clojure.lang.PersistentHashSet ix)
    ix
    #{ix}))

(extend-type nil
  IxTree
  (add-ix [_ p1 p2 ix] (insert (empty-tree) p1 p2 (ensure-set ix) (fnil union #{})))
  (get-ix [_ _] []))

(extend-type itree
  IxTree
  (add-ix [tree p1 p2 ix] (insert tree p1 p2 (ensure-set ix) (fnil union #{})))
  (get-ixs [tree point] (flatten (map (comp vec tag) (stab tree point)))))