(ns mondrian.test.graphs
  (:use mondrian.graphs)
  (:use clojure.test)
  (:import (mondrian.graphs UndirectedGraph)))

(defn make-basic-test-graph
  []
  (let [ps [:a :c :b :f :d :e]
        es [[:f :e 4]
            [:b :d 5]
            [:b :a 1]
            [:b :c 6]
            [:b :e 1]
            [:a :d 3]
            [:c :e 4]
            [:d :e 1]
            [:c :f 2]]
        fe (fn [g [p1 p2 w]] (add-edge g p1 p2 w))]
    (reduce fe (reduce add-point (empty-graph) ps) es)))
    
(defn make-graph
  [points]
  (loop [g  (empty-graph)
         ps points]
    (if (empty? ps)
      g
      (recur (add-point g (first ps)) (rest ps)))))

(deftest has-point-test
  []
  (let [g (make-graph [:a :b])
        ps (set (.points g))]
    (testing "Testing has-point?"
      (is (contains? ps :a) ":a wasn't added.")
      (is (contains? ps :b) ":b wasn't added.")
      (is (= 2 (count ps)) "Expected two points.")
      (is (has-point? g :a) ":a not reported as member")
      (is (has-point? g :b) ":b not reported as member")
      (is (not (has-point? g :c)) ":c reported as member"))))

(deftest add-points-test
  []
  (let [g (-> (empty-graph)
              (add-point :x)
              (add-point :y)
              (add-point :z))
        ps (set (.points g))]
    (testing "Testing the addition of points"
      (is (= 3 (count ps))  "3 points should have been added.")
      (is (contains? ps :x) ":x was not added.")
      (is (contains? ps :y) ":y was not added.")
      (is (contains? ps :z) ":z was not added."))))

(deftest add-edges-base-test
  []
  (let [g (-> (make-graph [:a :b :c :d :e :f])
              (add-edge :a :b)
              (add-edge :b :c)
              (add-edge :a :c)
              (add-edge :a :d 2))]
    (testing "Base functionality of add-edge:"
      (testing "point adding"
        (is (has-point? g :a))
        (is (has-point? g :b))
        (is (has-point? g :c))
        (is (has-point? g :d))
        (is (has-point? g :e))
        (is (has-point? g :f)))
      (testing "edge adding"
        (is (has-edge? g :a :b))
        (is (has-edge? g :b :c))
        (is (has-edge? g :a :c))
        (is (has-edge? g :a :d))
        (is (not (has-edge? g :d :e))))
      (testing "undirected edge adding"
        (is (has-edge? g :b :a))
        (is (has-edge? g :c :b))
        (is (has-edge? g :c :a))
        (is (has-edge? g :d :a)))
      (testing "default weights"
        (is (= #{1} (weights g :a :b)))
        (is (= #{1} (weights g :b :c)))
        (is (= #{1} (weights g :a :c)))
        (is (= #{}  (weights g :d :e))))
      (testing "given weights"
        (is (= #{2} (weights g :a :d)))))))

(deftest spanning-tree-test
  []
  (let [g (make-basic-test-graph)
        s (spanning-tree g)
        spanset (points-of-spanning-tree s)]
    (is (= 5 (count s)) "Should only need 5 edges for 6 points")
    (is (= (count (points g)) (count spanset)))
    (is (every? #(and (has-point? g (first %))
                      (has-point? g (second %)))
                s))
    (is (every? #(contains? spanset %) (points g)))
    (is (= 9 (weight-of-spanning-tree s)) "The miminum tree should have a weight of 9")))

(defmacro is-adjacent
  [as x y]
  `(contains? (~as ~x) ~y))

(deftest adjacency-map-test
  []
  (let [g (make-basic-test-graph)
        s (spanning-tree g)
        a (adjacencies-of-spanning-tree s)]
    (testing "Testing valid adjacencies"
      (is (is-adjacent a :a :b) ":a -> :b")
      (is (is-adjacent a :b :e) ":b -> :e")
      (is (is-adjacent a :e :d) ":e -> :d")
      (is (is-adjacent a :e :f) ":e -> :f")
      (is (is-adjacent a :f :c) ":f -> :c"))
    (testing "Testing invalid adjacencies"
      (is (not (is-adjacent a :a :d)))
      (is (not (is-adjacent a :b :c)))
      (is (not (is-adjacent a :b :d)))
      (is (not (is-adjacent a :c :e))))))

(deftest make-cycle-test
  []
  (let [g  (make-basic-test-graph)
        s  (spanning-tree g)
        a  (adjacencies-of-spanning-tree s)
        c1 (make-cycle :c :d a)
        c2 (make-cycle :a :x a)]
    (testing "Testing valid cycle detection"
      (is (not (nil? c1)) "The cycle [:c :f :e :d] was reported as nil.")
      (is (= 4 (count c1)) "c1 should have have four points.")
      (is (= [:c :f :e :d] c1) "c1 was not nil but was wrong.")
      (testing "Testing cycle validity"
        (for [[x y] (partition 2 1 c1)]
          (is (contains? (a x) y) "each consecutive point in the cycle should be adjacent with its neighbors.")))
    (testing "Testing the negative case"
      (is (nil? c2) "c2 should have been nil; no path from :a to :x.")))))