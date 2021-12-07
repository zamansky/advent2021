(ns day05
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]

            [clojure.string :as str]))


(def data
  (->> 
   ;;(slurp "data/sample05.dat")
   (aocd/input 2021 5)
   string/split-lines
   (map #(string/split % #"( -> )|,"))
   (map  #(map u/parse-int %))
   )
  )

(defn add-points [grid [x0 x1] [y0 y1]]
  (let [newpoints (for [x (range x0 (inc x1))
                        y (range y0 (inc y1))]
                    [x y]
                    )
        newgrid (reduce (fn [sofar next]
                          (update sofar next
                                  (fn [x] (if (nil? x) 1 (+ x 1))))
                          )grid newpoints)
        ]
    newgrid
    ) )

(defn build-grid-part1 [data]
  (reduce (fn [sofar [x0 y0 x1 y1] ]
            (let [xs (sort [x0 x1])
                  ys (sort [y0 y1])]
              (if (or
                   (and (= x0 x1)
                        (< (first ys) (second ys)))
                   (and (= y0 y1)
                        (< (first xs) (second xs))))
                (add-points sofar xs ys )
                sofar
                )))
          {} data
          ))

(defn part1 [data]
  (count (filter #(>=  % 2 ) (vals (build-grid-part1 data)))))

(defn add-points-diagonal [grid [x0 y0 :as p0] [x1 y1 :as p1]]
  (let [dx (if (< x1 x0) -1 1)
        dy (if (< y1 y0) -1 1)
        newpoints (loop [x x0
                         y y0
                         result []
                         ]
                    (if (= x x1)
                      (conj  result [x y])
                      (recur (+ x dx) (+  y dy) (conj result [x y]))
                      )
                    )
        newgrid (reduce (fn [sofar next]
                          (update sofar next
                                  (fn [x] (if (nil? x) 1 (+ x 1))))
                          )grid newpoints)

        ]
    newgrid
    ))

(defn build-grid-part2 [data]
  (reduce (fn [sofar [x0 y0 x1 y1] ]
            (let [xs (sort [x0 x1])
                  ys (sort [y0 y1])]
              (cond
                (or (and (= x0 x1) (< (first ys) (second ys)))
                    (and (= y0 y1) (< (first xs) (second xs))))
                (add-points sofar xs ys )

                :else  ;; it's a diagonal
                
                (add-points-diagonal sofar [x0 y0] [x1 y1])
                )

              ))
          {} data
          ))
(defn part2 [data]
  (count (filter #(>=  % 2 ) (vals (build-grid-part2 data)))))
