(ns mondrian.test.intervals
  (:use clojure.test)
  (:use mondrian.intervals)
  (:import (mondrian.intervals itree)))

(deftest test-c1
  "Tests balancing case one - black node with red left child and red left-left grandchild."
  []
  (let [t (itree. 4 8 8 :black
		  (itree. 2 6 6 :red
			  (itree. 1 5 5 :red nil nil nil)
			  nil
                          nil)
		  nil
                  nil)
	t2 (balance-case-one t)
	left (:left t2)
	right (:right t2)]
    (is (= :red (:color t2)))
    (is (= 2    (:low t2)))
    (is (= :black (:color left)))
    (is (= :black (:color right)))
    (is (= 1 (:low left)))
    (is (= 4 (:low right)))
    (is (= 8 (:max t2)))))

(deftest test-c2
  "Tests balancing case two - black node with red left child and red left-right grandchild."
  []
  (let [t1 (itree. 4 8 8 :black
                   (itree. 2 6 6 :red
                           :a
                           (itree. 3 7 7 :red
                                   :b
                                   :c
                                   :t)
                           :t)
                   :d
                   :t)
        t2 (balance-case-two t1)
        left (:left t2)
        right (:right t2)]
    (is (= :red   (:color t2)))
    (is (= :black (:color left)))
    (is (= :black (:color right)))

    (is (= 3 (:low t2)))
    (is (= 7 (:high t2)))
    (is (= 8 (:max t2)))
    (is (= left (:left t2)))
    (is (= right (:right t2)))

    (is (= 2 (:low left)))
    (is (= 6 (:high left)))
    (is (= 6 (:max left)))
    (is (= :a (:left left)))
    (is (= :b (:right left)))

    (is (= 4 (:low right)))
    (is (= 8 (:high right)))
    (is (= 8 (:max right)))
    (is (= :c (:left right)))
    (is (= :d (:right right)))))

(deftest test-c3
  "Tests balancing case three - black node with red right child and red right-left grandchild."
  []
  (let [t1 (itree. 1 5 8 :black
                   :a
                   (itree. 4 8 8 :red
                           (itree. 2 6 6 :red
                                   :b
                                   :c
                                   :t)
                           :d
                           :t)
                   :t)
        t2 (balance-case-three t1)
        left (:left t2)
        right (:right t2)]
    (is (= :red   (:color t2)))
    (is (= :black (:color left)))
    (is (= :black (:color right)))

    (is (= 2 (:low t2)))
    (is (= 6 (:high t2)))
    (is (= 8 (:max t2)))
    (is (= left (:left t2)))
    (is (= right (:right t2)))

    (is (= 1 (:low left)))
    (is (= 5 (:high left)))
    (is (= 5 (:max left)))
    (is (= :a (:left left)))
    (is (= :b (:right left)))

    (is (= 4 (:low right)))
    (is (= 8 (:high right)))
    (is (= 8 (:max right)))
    (is (= :c (:left right)))
    (is (= :d (:right right)))))

(deftest test-c4
  "Tests balancing case four - black node with red right child and red right-right grandchild."
  []
  (let [t1 (itree. 1 5 9 :black
                   :a
                   (itree. 3 7 9 :red
                           :b
                           (itree. 5 9 9 :red
                                   :c
                                   :d
                                   :t)
                           :t)
                   :t)
        t2 (balance-case-four t1)
        left (:left t2)
        right (:right t2)]
    (is (= :red (:color t2)))
    (is (= :black (:color left)))
    (is (= :black (:color right)))

    (is (= 3 (:low t2)))
    (is (= 7 (:high t2)))
    (is (= 9 (:max t2)))
    (is (= left (:left t2)))
    (is (= right (:right t2)))
    
    (is (= 1 (:low left)))
    (is (= 5 (:high left)))
    (is (= 5 (:max left)))
    (is (= :a (:left left)))
    (is (= :b (:right left)))

    (is (= 5 (:low right)))
    (is (= 9 (:high right)))
    (is (= 9 (:max right)))
    (is (= :c (:left right)))
    (is (= :d (:right right)))))