(ns day02
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            ))
(def data
  (->> 
   ;;(slurp "data/sample01.dat")
   (aocd/input 2021 1)
   string/split-lines
   (map u/parse-int)
   ))
