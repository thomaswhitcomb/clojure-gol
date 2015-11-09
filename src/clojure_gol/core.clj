(ns clojure-gol.core
  (:gen-class)
  (:require [clojure.test :refer :all])
)

(def square \u25FB)
(defn display
  [space]
  (def margin 5)
  (def minx (apply min (map (fn [[x y]] x) (keys space))))
  (def maxx (apply max (map (fn [[x y]] x) (keys space))))
  (def miny (apply min (map (fn [[x y]] y) (keys space))))
  (def maxy (apply max (map (fn [[x y]] y) (keys space))))
  (doseq [
          y (range (+ maxy margin) (- miny margin) -1)
          x (range (- minx margin) (+ maxx margin) 1) ]
    (cond 
      (= (space [x y]) :l) (print square)
      :else (print " "))

    (if 
      (= x (+ maxx (- margin 1))) (println))))

(defn surrounding-peers [x y] [[(+ x 1) y] [(- x 1) y] [x (+ y 1)][x (- y 1)][(- x 1) (- y 1)][(+ x 1) (+ y 1)][(- x 1) (+ y 1)][(+ x 1) (- y 1)]])
(defn live-neighbours [[x y] space]
  (let [
        values (map #(space % :d) (surrounding-peers x y))
        ]
    (count (filter #(= % :l) values))
  )
)

(defn evolve [ [k v] space]
  (let [
        live (live-neighbours k space)
        to   (if (= v :l)
               (cond 
                 (< live 2) [k :d]
                 (or (= live 2) (= live 3)) [k :l]
                 :else [k :d]
               )
              (cond
                (= live 3) [k :l]
                :else [k :d]
              )
            )  
       ] 
    to
  )  
)

(defn pad-cell [[x y]]
  (apply concat (for [a (surrounding-peers x y) ] [a :d]))
)

(defn select-alive [space] (into {} (filter (fn [[k v]] (= v :l)) space)))

(defn pad-the-board [space]
  (let [
        alive (select-alive space)
        pads (map (fn [[k v]] (pad-cell k)) alive)
        f (apply concat pads)
        deads (apply hash-map f)
       ] 
     (merge deads alive)
  )
)  
(defn run1 [tick space ] 
  (loop [tick1 tick space1 space]
    (display space1)
    (println (select-alive space1))
    (let [padded-space (pad-the-board space1)]
      (cond 
        (<= tick1 0) padded-space
        :else (recur (- tick1 1) (into {} (map (fn [[k v]](evolve [k v] padded-space)) padded-space)))
      )  
    )
  )  
)

(defn run [n space] 
  (select-alive (run1 n space ))
)  

(defn -main
  [& args]
  (def oscillator-blinker {[2 2]:l [2 3]:l [2 4]:l })
  (def oscillator-beacon {[2 5]:l [2 6]:l [3 5]:l [3 6]:l [4 3]:l [4 4]:l [5 3]:l [5 4]:l  })
  (def spaceship-glider {[1 1]:l [2 1]:l [3 1]:l [3 2]:l [2 3]:l})
  (def oscillator-toad {[1,2] :l,[2,2] :l,[2,3] :l,[3,3] :l, [3,2] :l,[4,3] :l})
  (def diehard {[3,2] :l [3,3] :l [2,3] :l [7,2] :l  [8,2] :l,[9,2] :l [8 4] :l}) ; Goes away after 130 generations
  (run 130 diehard)
)
