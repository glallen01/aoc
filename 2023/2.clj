(ns aoc2
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing run-tests]]
   [clojure.java.io :as io]))

(def input-lines (str/split-lines (slurp "2.txt")))

(def maxes {:red 12,
            :green 13,
            :blue 14})

(defn line->game [line]
  (last (first (re-seq #"^Game (\d+):" line))))

(defn color-exceeds-max? [max color line]
  (let [regex (re-pattern (str " (\\d+) (" color ")"))]
    (->>
     (re-seq regex line)
     (map #(Long/parseLong (nth % 1)))
     (map #(< max %))
     (some true?))))

(defn max-of-color [color line]
  (let [regex (re-pattern (str " (\\d+) (" color ")"))]
    (->>
     (re-seq regex line)
     (map #(Long/parseLong (nth % 1)))
     (apply max))))

(defn check-colors-vs-maxes [maxes line]
  (let [game (Long/parseLong (line->game line))]
    (if-not
        (some true?
              (for [[colorkey max] maxes]
                (let [color (name colorkey)]
                  (color-exceeds-max? max color line))))
        game)))

(defn aoc2a [maxes lines]
  (->> lines
       (map #(check-colors-vs-maxes maxes %))
       (remove nil?)
       (reduce +)))

(defn aoc2b [lines]
  (->> lines
       (map (fn [line]
              (for [[colorkey max] maxes]
                (let [color (name colorkey)]
                  (max-of-color color line)))))
       (map #(reduce * %))
       (reduce +)))

(def test-lines (str/split-lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(deftest test-aoc2a
  (testing
      (is (= true
             (color-exceeds-max? 4 "blue" (first test-lines))))
    (is (= nil
           (color-exceeds-max? 4 "red" (first test-lines))))
    (is (= 1
           (check-colors-vs-maxes maxes (first test-lines))))
    (is (= nil
           (check-colors-vs-maxes maxes (nth test-lines 3))))
    (is (= 8
           (aoc2a maxes test-lines))))
  (testing
      (is (= 2278
             (aoc2a maxes input-lines)))))

(deftest test-aoc2b
  (testing
      (is (= (+ 48 12 1560 630 36)
             (aoc2b test-lines))))
  (testing
      (is (= 67953
             (aoc2b input-lines)))))

(do
  (println "part1: " (aoc2a maxes input-lines))
  (println "part2: " (aoc2b input-lines)))
