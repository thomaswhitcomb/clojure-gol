(ns clojure-gol.core
  (:gen-class)
  (:require [clojure.test :refer :all])
)
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
  (loop [tick1 tick space1 space]
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
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println "yo mamma")
)
