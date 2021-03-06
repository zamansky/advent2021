(ns day08
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))


(def data
  (->> 
   (slurp "data/sample08.dat")
   ;;(aocd/input 2021 8)
   (#(string/split % #"\n"))
   (map #(string/split % #" \| "))
   (map (fn [ [a b] ] [(string/split a #" ") (string/split b #" ")]))))

;;------------------ Part 1 --------------------------------------

(defn myfilter [x]
  (filter #(contains? #{2 4 3 7} %) x))


(defn part1 [  data]
  (let [results (map second data)
        counted (map #(map count %) results)
        f (map #(myfilter %) counted)
        total (map #(count %) f)
        ]
    (apply +  total)))

(part1 data)

;;-------------- Part 2 ----------------------------------

;; build a map where the keys are the digis and the entries are sets with the edges
;; this does the 4 edges that we know because they each have a unique # of edges
(defn build-map-1478 [[a b] mapsofar]
  (let [line (into a b)
        result (loop [result mapsofar
                      digit (first line)
                      line line
                      ]
                 (cond
                   (nil? digit)
                   result

                   (= (count digit) 2)
                   (recur (assoc result 1 (set digit)) (first (rest line)) (rest line))

                   (= (count digit) 4)
                   (recur (assoc result 4 (set digit)) (first (rest line)) (rest line))

                   (= (count digit) 3)
                   (recur (assoc result 7 (set digit)) (first (rest line)) (rest line))

                   (= (count digit) 7)
                   (recur (assoc result 8 (set digit)) (first (rest line)) (rest line))

                   
                   :else
                   (recur result (first (rest line)) (rest line))
                   
                   ))
        ]
    result))

;; build a map for the numbers that have 5 segments 
(defn build-map-235 [[ a b ] mapsofar]
  (let [line (into a b)
        data (filter #(= (count %) 5) line)
        four-one   (set/difference  (get mapsofar  4)  (get mapsofar  1))
        result (loop [result mapsofar
                      digit  (first data)
                      line data
                      ]
                 (cond
                   (nil?  digit)
                   result

                   (=  (set/intersection   (set digit)  (get result 1)) (get result 1)) ;; 3
                   (recur (assoc result 3  (set digit)) (first (rest line)) (rest line))

                   (= 2 (count (set/intersection (set digit)  four-one))) 
                   (recur (assoc result 5 (set digit)) (first (rest line)) (rest line))
                   
                   :else
                   (recur (assoc result 2 (set digit))  (first (rest line)) (rest line))
                   
                   ))

        ]
    result
    ))

;; build the numbers that have 6 segments 
(defn build-map-069 [[ a b ] mapsofar]
  (let [line (into a b)
        data (filter #(= (count %) 6) line)
        result (loop [result mapsofar
                      digit (first data)
                      line data
                      ]
                 (cond
                   (nil?  digit)
                   result

                   (and
                    (= 2  (count (set/intersection (set digit) (get result 1))))
                    (= 4  (count (set/intersection (set digit) (get result 4)))))
                   (recur (assoc result 9 (set digit)) (first (rest line)) (rest line))

                   (= 5 (count (set/intersection (set digit) (get result 5)))) ;; 
                   (recur (assoc result 6 (set digit)) (first (rest line)) (rest line))

                   :else
                   (recur (assoc result 0 (set digit))  (first (rest line)) (rest line))
                   
                   ))

        ]
    result
    ))



;; build a dictionary with the keys being the digits and the entries the set for it's pattern
(defn build-map [line]
  (->> {}
       (build-map-1478 line)
       (build-map-235 line )
       (build-map-069 line)
       ))

;; decode a single line by
;; 1. invert the map so that we can look up by the edge set
;; 2. get the digit for each item in the line 
(defn decode [line]
  (let [key  (build-map line)
        datamap (set/map-invert key)
        ;;datamap  (zipmap (vals key) (keys key))

        data (second line)
        ]
    (map #(get datamap (set %))  data)
    ))


;; convert a list of numbers to a number ex: (9 5 2 3) -> 9523
(defn list->num [x]
  (reduce (fn [sofar next] (+  (* sofar  10 )0  next) ) x))


;; put it all together:
;; 1. for each line, build the map and decode it 
;; 2. convert them to numbers
;; 3. Add them up
(defn part2 [data] (apply + (map list->num (map decode data ))))

(part2 data )

