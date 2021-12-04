(ns day04
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            ))


(def data
  (->> 
   ;;(slurp "data/sample04.dat")
   (aocd/input 2021 4)
   string/split-lines))

(def balls (->> data
                first
                (#(string/split % #","))))


(defn split-board [board]
  (->> board
       (map #(string/split % #"[ \n]")) 
       (map (fn [x]  (filter #(not (= % ""))x )) )
       (reduce concat)))

(def boards
  (->> data
       (drop 1)
       (partition 6)
       (map #(drop 1 %))
       (map #(split-board %))))


(defn add-mark [board mark]
  (map #(if (= % mark) "X" %) board))

(defn has-five? [board]
  (>  (->> board
           (map #(filter (fn [x] (= x "X"))%)  )
           (map count)
           (filter #(= 5 % ))
           count) 0))



(defn check-board [board]
  (let [horiz (partition 5 board)
        vert  (apply mapv vector horiz)]
    (or  (has-five? horiz)
         (has-five? vert))))

(defn find-winner-part1 [balls boards]
  (loop [balls balls
         boards boards]
    (let [ ball (first balls)
          newboards (map #(add-mark % ball) boards )
          winners (map check-board newboards)
          winner? (>  (count  (filter true? winners)) 0)
          ]
      (if winner?
        [ball newboards]
        ( recur (rest balls) newboards))
      )
    ))
(defn part1 [balls boards]
  (let [[ball boards] (find-winner-part1 balls boards)
        board (first (filter check-board boards))
        b (map u/parse-int (filter #(not (= "X" %)) board))
        ]
    (* (u/parse-int ball) (apply + b))
    ))



(defn part2 [balls boards]
  (loop [balls balls
         boards boards]
    (let [ ball (first balls)
          newboards (map #(add-mark % ball) boards )
          n (filter #(not ( check-board %) ) newboards) 
          ]
      (if (= 1 (count  n))
        (part1 (rest balls) n)
        ( recur (rest balls) n))
      )
    ))


