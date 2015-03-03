(ns clojure-gol.core-test
  (:require [clojure.test :refer :all]
            [clojure-gol.core :refer :all]
  )
)

(def space1 {[0,0] :d,[0,1] :l,[0,2] :l,[0,3] :l,[1,0] :d,[1,1] :d,[1,2] :l,[1,3] :l, [2,0] :l,[2,1] :d,[2,2] :d,[2,3] :d, [3,0] :d,[3,1] :d, [3,2] :d, [3,3] :l})
(def blinker {[1,0] :l,[1,1] :l,[1,2] :l})

(deftest count-neighbours
  (is (= (live-neighbours [1,1] {[0,0] :d,[0,1] :d,[0,2] :d,[1,0] :d,[1,1] :l,[1,2] :d,[2,0] :d,[2,1] :d,[2,2] :d}) 0))
  (is (= (live-neighbours [1,1] {[0,0] :d,[0,1] :d,[0,2] :l,[1,0] :d,[1,1] :l,[1,2] :d,[2,0] :d,[2,1] :d,[2,2] :d}) 1))
  (is (= (live-neighbours [1,1] {[0,0] :l,[0,1] :d,[0,2] :l,[1,0] :d,[1,1] :l,[1,2] :d,[2,0] :l,[2,1] :l,[2,2] :l}) 5))
)
(deftest pad-cells
  (is (= (apply sorted-map (pad-cell [1,1])) {[0 0] :d, [0 1] :d, [0 2] :d, [1 0] :d, [1 2] :d, [2 0] :d, [2 1] :d, [2 2] :d}))
  (is (= (apply sorted-map (pad-cell [0,0])) {[-1 -1] :d, [-1 0] :d, [-1 1] :d, [0 -1] :d, [0 1] :d, [1 -1] :d, [1 0] :d, [1 1] :d}))
)         
(deftest pad-the-boards
  (is (= (pad-the-board {}) {}))
  (is (= (into (sorted-map) (pad-the-board {[1 1] :l})) {[0 0] :d, [0 1] :d, [0 2] :d, [1 0] :d, [1 1] :l, [1 2] :d, [2 0] :d, [2 1] :d, [2 2] :d}))
  (is (= (into (sorted-map) (pad-the-board {[0 0] :l})) {[-1 -1] :d, [-1 0] :d, [-1 1] :d, [0 -1] :d, [0 0] :l, [0 1] :d, [1 -1] :d, [1 0] :d, [1 1] :d}))
  (is (= (into (sorted-map) (pad-the-board {[15 15] :d,[0 0] :l,[2,2] :l,[1 1] :d})) {[-1 -1] :d, [-1 0] :d, [-1 1] :d, [0 -1] :d, [0 0] :l, [0 1] :d, [1 -1] :d, [1 0] :d, [1 1] :d, [1 2] :d, [1 3] :d, [2 1] :d, [2 2] :l, [2 3] :d, [3 1] :d, [3 2] :d, [3 3] :d}))
) 
(deftest blinkers 
  (is (= (run 1 blinker) '([[1 1] :l] [[2 1] :l] [[0 1] :l])))
  (is (= (run 2 blinker) '([[1 0] :l] [[1 1] :l] [[1 2] :l])))       
  (is (= (run 3 blinker) '([[1 1] :l] [[2 1] :l] [[0 1] :l])))
  (is (= (run 4 blinker) '([[1 0] :l] [[1 1] :l] [[1 2] :l])))       
)         
