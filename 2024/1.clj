(ns aoc1
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing run-test]]
   [clojure.edn :as edn]
   [clojure.core.matrix :as matrix]))

(def input-lines (str/split-lines (slurp "1.txt")))

(def test-sample
  (str/split-lines
   "3   4
4   3
2   5
1   3
3   9
3   3"))

(defn sample-to-matrix [lines]
  (->> lines
       (mapv #(str/split % #" +"))
       matrix/transpose
       (mapv (fn [xs] (mapv #(Integer/parseInt %) xs)))))

(defn generate-sums [lists]
  (->> lists
       (mapv (comp vec sort))
       matrix/transpose
       (mapv #(apply (comp abs -) %))))

(defn resultsA [input]
  (->> input
       sample-to-matrix
       generate-sums
       (apply +)
       ))

(println (resultsA test-sample))
(println (resultsA input-lines))

(defn resultsB [input]
  (let [[listA listB] (sample-to-matrix input)
        freq (frequencies listB)]
    (->> listA
         (mapv (fn [n]
                 (* n
                    (or (get freq n)
                        0))))
         (apply +))))

(println (resultsB test-sample))
(println (resultsB input-lines))

