(ns mondrian.test.graphs
  (:use mondrian.graphs)
  (:use clojure.test)
  (:import (mondrian.graphs UndirectedGraph)))

(defn make-graph
  [points]
  (loop [g  (empty-graph)
         ps points]
    (if (empty? ps)
      g
      (recur (add-point g (first ps)) (rest ps)))))

(deftest has-point-test
  []
  (let [g (make-graph [:a :b])]
    (testing "Points added"
      (is (has-point? g :a) ":a not reported as member")
      (is (has-point? g :b) ":b not reported as member")
      (is (not (has-point? g :c)) ":c reported as member"))))

(deftest add-points-test
  []
  (let [g (-> (empty-graph)
              (add-point :x)
              (add-point :y)
              (add-point :z))]
    (is (has-point? g :x))
    (is (has-point? g :y))
    (is (has-point? g :z))
    (is (not (has-point? g :a)))))

(deftest add-edges-base-test
  []
  (let [g (-> (make-graph [:a :b :c :d :e :f])
              (add-edge :a :b)
              (add-edge :b :c)
              (add-edge :a :c)
              (add-edge :a :d))]
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
        (is (= #{1} (weights g :a :d)))
        (is (= #{}  (weights g :d :e)))))))

(defn set-of-span
  "Converts a spanning tree to a set of constituent points"
  [span]
  (->> span
       (map (fn [[p1 p2 _]] [p1 p2]))
       (flatten)
       (set)))

(defn weight-of-span
  [span]
  (->> span
       (map (fn [[_ _ w]] w))
       (reduce + 0)))

(deftest spanning-tree-test
  []
  (let [g (-> (make-graph [:a :b :c :d :e :f])
              (add-edge :a :b 1)
              (add-edge :a :d 3)
              (add-edge :b :c 6)
              (add-edge :b :d 5)
              (add-edge :b :e 1)
              (add-edge :c :e 4)
              (add-edge :c :f 2)
              (add-edge :d :e 1)
              (add-edge :e :f 4))
        s (spanning-tree g)
        spanset (set-of-span s)]
    (is (= 5 (count s)) "Should only need 5 edges for 6 points")
    (is (= (count (points g)) (count spanset)))
    (is (every? #(and (has-point? g (first %))
                      (has-point? g (second %)))
                s))
    (is (every? #(contains? spanset %) (points g)))
    (is (= 9 (weight-of-span s)) "The miminum tree should have a weight of 9")))