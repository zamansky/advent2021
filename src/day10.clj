(ns day10
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))
(def data
  (->> 
   ;;(slurp "data/sample10.dat")
   (aocd/input 2021 10)
   ( #(string/split % #"\n"))
   (map #(string/split % #""))))
(def opens #{"(" "[" "{" "<" })
(def matches-by-close {")" "(" "]" "[" "}" "{" ">" "<"})
(def matches-by-open {"(" ")" "[" "]" "{" "}" "<" ">"})
(def part1-values {")" 3 "]" 57 "}" 1197 ">" 25137})
(def part2-values {")" 1 "]" 2 "}" 3 ">" 4})

(defn part1-parse-line [line]
  (loop [line line stack '()]
    (let [next  (first line)]
      (cond
        (empty? line) 0 
        (contains?  opens next)  (recur (rest line) (conj stack next))
        
        (=  (get matches-by-close  next)  (peek stack)) 
        (recur (rest line) (pop stack))

        (empty? stack) 0
        :else (get part1-values next)
        )))
  )



(defn part2-score [l]
  (let [closes  (map #(get matches-by-open %) l)]
    (reduce (fn [score next]
              (+  (* score 5) (get part2-values next))
              ) 0 closes )
    ))

(defn part2-parse-line [line]
  (loop [line line stack '()]
    (let [next  (first line)]
      (cond
        (empty? line)  (part2-score stack) 
        (contains?  opens next)  (recur (rest line) (conj stack next))
        
        (=  (get matches  next)  (peek stack)) 
        (recur (rest line) (pop stack))

        (empty? stack) stack 
        :else 0;;( part2-score stack ))
        )))
  )

(defn part1 [data]
(apply + (map part1-parse-line data)))

(part1 data)

(part2-parse-line (first data))

(map part2-parse-line data)

(defn part2 [data]
  (let [scores (->> data
                    (map part2-parse-line)
                    (filter #(> % 0))
                    sort)
        ]
    (nth scores (/ (count scores) 2))
    ))

(part2 data)
