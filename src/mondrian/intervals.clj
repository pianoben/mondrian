(ns mondrian.intervals
  (:use [clojure.set])
  (:use [clojure.core.match :only [match]]))

(defrecord itree [low high max color left right tag])

(defmacro max-or-cur [node cur]
  `(if-let [m# (:max ~node)] m#  ~cur))

(defmacro defn-match [name & forms]
  `(defn name [x#]
     (match [x#]
            ~@forms)))

(defn make-red
  [tree]
  (assoc tree :color :r))

(defn make-black
  [tree]
  (assoc tree :color :b))

(defn make-double-black
  [tree]
  (assoc tree :color :bb))

(defn incr-blackness
  [color]
  (match color
         :-b :r
         :r  :b
         :b  :bb))

(defn nodemax
  [h left right]
  (let [l (max-or-cur left h)
        r (max-or-cur right h)]
    (max h l r)))

(defn overlaps?
  [ll lh rl rh]
  (and
   (not (< lh rl))
   (not (< rh ll))))

(defn balance
  [tree]
  (match [tree]
         (:or [{:color (:or :b :bb) :left {:color :r :left {:color :r :left a :right b :low xl :high xh :max xm :tag xt} :right c :low yl :high yh :max ym :tag yt} :right d :low zl :high zh :max zm :tag zt}]
              [{:color (:or :b :bb) :left {:color :r :left a :right {:color :r :left b :right c :low yl :high yh :max ym :tag yt} :low xl :high xh :max xm :tag xt} :right d :low zl :high zh :max zm :tag zt}]
              [{:color (:or :b :bb) :left a :right {:color :r :left {:color :r :left b :right c :low yl :high yh :max ym :tag yt} :right d :low zl :high zh :max zm :tag zt} :low xl :high xh :max xm :tag xt}]
              [{:color (:or :b :bb) :left a :right {:color :r :left b :right {:color :r :left c :right d :low zl :high zh :max zm :tag zt} :low yl :high yh :max ym :tag yt} :low xl :high xh :max xm :tag xt}])
         (let [x (itree. xl xh (nodemax xh a b) :b a b xt)
               z (itree. zl zh (nodemax zh c d) :b c d zt)]
           (itree. yl yh (nodemax yh x z) :r x z yt))

         [{:color :bb :left a :right {:color :-b :left {:color :b :left b :right c :low yl :high yh :max ym :tag yt} :right ({:color :b} :as d) :low zl :high zh :max zm :tag zt} :low xl :high xh :max xm :tag xt}]
         (let [x (itree. xl xh (nodemax a b) :b a b xt)
               z (balance (itree. zl zh (nodemax c d) :b c (make-red d) zt))]
           (itree. yl yh (nodemax x z) :b x z yt))

         [{:color :bb :left {:color :-b :left ({:color :b} :as a) :right {:color :b :left b :right c :low yl :high yh :max ym :tag yt} :low xl :high xh :max xm :tag xt} :right d :low zl :high zh :max zh :tag zt}]
         (let [x (balance (itree. xl xh (nodemax xh a b) :b (make-red a) b xt))
               z (itree. zl zh (nodemax zh c d) :b c d zt)]
           (itree. yl yh (nodemax x z) :b x z yt))

         :else
         tree))

(defn bubble
  [tree]
  (println "bubbling"))

(defn-match remove-max
  [tree]
  [{:right nil}] (remove tree)
  :else (bubble (assoc tree :right (remove-max tree))))
         
(defn remove
  [tree]
  (match [tree]
         ; Leaves
         [{:color :r :left nil :right nil}] nil
         [{:color :b :left nil :right nil}] (assoc tree :color :bb)

         ; Single-child
         (:or [{:color :r :left child :right nil}]
              [{:color :r :left nil :right child}])
         child

         ; Black node with one (red) child
         (:or [{:color :b :left {:color :r :left cl :right cr :low l :high h :max m :tag t} :right nil}]
              [{:color :b :left nil :right {:color :r :left cl :right cr :low l :high h :max m :tag t}}])
         (itree. l h m :b cl cr t)

         ; Black node with one (black) child
         (:or [{:color :b :left ({:color :b} :as child) :right nil}]
              [{:color :b :left nil :right ({:color :b} :as child)}])
         (make-double-black child)

         [{:left l :right r}]
         

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
    (itree. low high high :r nil nil tag)))

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
     (make-black (insert tree low high tag replace-value)))
  ([tree low high tag update-fn]
     (make-black (insert tree low high tag update-fn))))

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