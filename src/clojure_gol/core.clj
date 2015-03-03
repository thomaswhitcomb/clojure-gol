(ns clojure-gol.core
  (:gen-class)
  (:require [clojure.test :refer :all])
)

(defn live-neighbours [[x y] space]
  (let [x [
          (space [(+ x 1) y] :d) 
          (space [(- x 1) y] :d) 
          (space [x (+ y 1)] :d) 
          (space [x (- y 1)] :d) 
          (space [(- x 1) (- y 1)] :d) 
          (space [(+ x 1) (+ y 1)] :d) 
          (space [(- x 1) (+ y 1)] :d) 
          (space [(+ x 1) (- y 1)] :d) 
         ]]
    (count (filter #(= % :l) x))
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
  [[(+ x 1) y] :d,[(- x 1) y] :d,[x (+ y 1)] :d,[x (- y 1)] :d,[(- x 1) (- y 1)] :d,[(+ x 1) (+ y 1)] :d,[(- x 1) (+ y 1)] :d,[(+ x 1) (- y 1)] :d ]
)

(defn select-alive [space] (filter (fn [[k v]] (= v :l)) space))

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
  (println (select-alive space))
  (let [padded-space (pad-the-board space)]
    (cond 
      (<= tick 0) padded-space
      :else (run1 (- tick 1) (into {} (map (fn [[k v]](evolve [k v] padded-space)) padded-space)))
    )  
  )  
)
(defn run [n space] 
  (select-alive (run1 n space ))
)  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
)
