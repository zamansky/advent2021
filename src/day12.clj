(ns day12
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))



(def data
  (->> 
   (slurp "data/sample12-1.dat")
   ;;(aocd/input 2021 12)
   string/split-lines
   (map #(string/split % #"-"))
   ))

(defn build-graph [data]
  (reduce (fn [graph [a b]] (assoc graph a (into #{} b)))
          {}
          (reduce (fn [graph [a b]]
                    
                    (update
                     ( update graph a conj   b )
                     b conj a)
                    )

                  {} data)) )


(def graph (build-graph data))

(def count (atom 0))
(defn part1-bfs [graph current solset used]
  (cond
    (=  current "end")  (do
                          (swap! count inc)
                          solset)

    :else (let [raw-neighbors (into [] (get graph current))
                neighbors   (filter #(not (contains?  used %))  raw-neighbors)
                solset (into solset current)
                used (if (= current (string/lower-case current))
                       (set/union used #{current})
                       used)
                ]
            (loop [cn (first neighbors) neighbors neighbors
                   solset solset
                   ]
              (cond (empty? neighbors) solset
                    
                    :else (recur  (first (rest neighbors))
                                  (rest neighbors)
                                  (part1-bfs graph cn solset used)))
              )
            ) ))



(reset! count 0)
(part1-bfs graph "start" #{} #{})

@count




(defn part2-bfs [graph current solset used twice]
  (cond
    (=  current "end")  (do
                          (swap! count inc)
                          solset)
    (and (not (empty solset)) (= current "start"))
    solset
    

    :else (let [
                
                raw-neighbors (into [] (get graph current))
                neighbors   (filter #(not (= "start" %))  raw-neighbors)
                neighbors   (filter #(not (contains?  used %))  neighbors)





                twice (if (and
                           (not (= current "start"))
                           (contains? solset current)
                           (= (string/lower-case current) current))
                        true
                        twice
                        )
                
                used (cond  (and  twice
                                  (= current (string/lower-case current))
                                  )
                            (set/union used #{current})
                            :else
                            used
                            )
                
                solset (into solset #{current})

                ]
            (loop [cn (first neighbors) neighbors neighbors
                   solset solset
                   ]
              (cond (empty? neighbors) solset
                    
                    :else (recur  (first (rest neighbors))
                                  (rest neighbors)
                                  (part2-bfs graph cn solset used  twice)))
              )
            ) ))



( reset! count 0)

(part2-bfs graph "start" #{} #{}  false)

@count
