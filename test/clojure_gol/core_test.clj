(ns clojure-gol.core-test
  (:require [clojure.test :refer :all]
            [clojure-gol.core :refer :all]
  )
)

(def oscillator-blinker {[2 2]:l [2 3]:l [2 4]:l })
(def oscillator-beacon {[2 5]:l [2 6]:l [3 5]:l [3 6]:l [4 3]:l [4 4]:l [5 3]:l [5 4]:l  })
(def spaceship-glider {[1 1]:l [2 1]:l [3 1]:l [3 2]:l [2 3]:l})
(def oscillator-toad {[1,2] :l,[2,2] :l,[2,3] :l,[3,3] :l, [3,2] :l,[4,3] :l})
(def diehard {[3,2] :l [3,3] :l [2,3] :l [7,2] :l  [8,2] :l,[9,2] :l [8 4] :l}) ; Goes away after 130 generations
(def blinker1-on {[1,0] :l,[1,1] :l,[1,2] :l})
(def blinker1-off {[1 1] :l [2 1] :l [0 1] :l})
(def blinker2-on {[1,2] :l,[2,2] :l,[2,3] :l,[3,3] :l, [3,2] :l,[4,3] :l})
(def blinker2-off {[4 3] :l, [3 4] :l, [4 2] :l, [1 3] :l, [2 1] :l, [1 2] :l})

(deftest pad-the-board-test
  (is (= (pad-the-board {[2 2]:l [2 3]:l [2 4]:l }) {[2 2] :l, [2 3] :l, [2 5] :d, [3 3] :d, [1 1] :d, [3 4] :d, [1 4] :d, [1 3] :d, [1 5] :d, [2 4] :l, [3 1] :d, [2 1] :d, [1 2] :d, [3 5] :d, [3 2] :d})))

(deftest fast-forward-test
  (is (=  (fast-forward 100 {[1 1]:l [2 1]:l [3 1]:l [3 2]:l [2 3]:l} (fn [_] nil))
       {[27 -23] :d, [29 -23] :d, [27 -25] :d, [26 -21] :d, [27 -22] :l, [29 -22] :d, [28 -23] :l, [27 -21] :d, [25 -25] :d, [25 -23] :d, [26 -25] :d, [25 -24] :d, [28 -21] :d, [28 -24] :l, [26 -22] :d, [26 -24] :l, [27 -24] :l, [26 -23] :d, [29 -24] :d, [28 -22] :d, [28 -25] :d, [29 -25] :d})))

(deftest live-neighbours-around-test
  (is (= (live-neighbours-around  [0 0]  {[1 1]:l [2 1]:l [3 1]:l [3 2]:l [2 3]:l}) [[1 1]] ))
  (is (= (live-neighbours-around  [1 2]  {[1 1]:l [2 1]:l [3 1]:l [3 2]:l [2 3]:l}) [[1 1] [2 3] [2 1]] )))

(deftest select-alive-test
  (is (= (select-alive (fast-forward 1 blinker1-on (fn [_] nil)))) blinker1-off)
  (is (= (select-alive (fast-forward 2 blinker1-on (fn [_] nil)))) blinker1-on)
  (is (= (select-alive (fast-forward 3 blinker1-on (fn [_] nil)))) blinker1-off)
  (is (= (select-alive (fast-forward 4 blinker1-on (fn [_] nil)))) blinker1-on)
  (is (= (select-alive (fast-forward 1 blinker2-on (fn [_] nil)))) blinker2-off)
  (is (= (select-alive (fast-forward 2 blinker2-on (fn [_] nil)))) blinker2-on)
  (is (= (select-alive (fast-forward 3 blinker2-on (fn [_] nil)))) blinker2-off)
  (is (= (select-alive (fast-forward 4 blinker2-on (fn [_] nil)))) blinker2-on))

(deftest diehard-test
  (is (= (select-alive (fast-forward 130 diehard (fn [_] nil))) {})))
