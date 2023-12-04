(ns aoc1
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing run-tests]]
   [clojure.java.io :as io]))

(defn parse-line-1 [line]
  (->> (str/split line #"[a-z]")
       (filter #(try
                  (Long/parseLong %)
                  (catch NumberFormatException e)))
       (mapcat #(map (fn [s]
                       (Character/getNumericValue s))
                     %))
       (apply (fn [& lst]
                (read-string
                 (str/join [(first lst) (last lst)]))))))

(deftest aoc1a
  (def short-samples '("1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"))
  (testing
      (is (= '(12 38 15 77)
             (map parse-line-1 short-samples))))
  (testing
      (is (= 142
             (apply + (map parse-line-1
                           short-samples)))))
  (testing
      (is (= '(12 89)
             (map parse-line-1
                  '("ab13cd42de" "ab832c56def8889g"))))))


(def string-numbers '(("one"   "1")
                      ("two"   "2")
                      ("three" "3")
                      ("four"  "4")
                      ("five"  "5")
                      ("six"   "6")
                      ("seven" "7")
                      ("eight" "8")
                      ("nine"  "9")))


;; Tried this first and went down the wrong path because it was replacing
;; strings with ints as it went, which broke overlapping numbers

;; (defn stringint-to-int-at-head
;;   "walks through a list of pairs of `(\"number\" int)` in `replacements` and
;;   replaces number if it's at the beginning of `s`"
;;   [s replacements]
;;   (if (empty? replacements)
;;     s
;;     (let [[[pattern replacement]] replacements]
;;       (recur
;;        (str/replace-first s
;;                           (re-pattern (str "^" pattern))
;;                           replacement)
;;        (rest replacements)))))

;; (defn apply-replacements [tail & head]
;;   "`tail` starts full and empties into `head` one char at a time via [[subs]],
;;   we check `/^number/` tail as we go via [[stringint-to-int-at-head]]`"
;;   (if (empty? tail)
;;     head
;;     (let [xtail (stringint-to-int-at-head tail string-numbers)]
;;       (recur
;;        (subs xtail 1)
;;        (str head (subs xtail 0 1))))))

(defn match-int-at-head [s]
  (loop [int nil
         replacements string-numbers]
    (if (or int
            (empty? replacements))
      int
      (recur
       (let [[name number] (first replacements)
             maybe-int (try
                         (Long/parseLong (subs s 0 1))
                         (catch NumberFormatException e))]
         (cond
           (some? maybe-int) maybe-int
           (str/starts-with? s name) (read-string number)))
       (rest replacements)))))

(defn line-to-ints [s & xs]
  (if (empty? s)
    (remove nil? xs)
    (let [x (match-int-at-head s)]
      (recur
       (subs s 1)
       (conj (vec xs) x)))))

(defn parse-line-2 [line]
  (let [[x & xs] (line-to-ints line)
        y (if (nil? xs)
            x
            (last xs))]
    (+ (* 10 x) y)))

(deftest aoc1b
  (def samples '("two1nine" "eightwothree" "abcone2threexyz" "xtwone3four"
                 "4nineeightseven2" "zoneight234" "7pqrstsixteen"))
  (def results '(29 83 13 24 42 14 76))
  (def total 281)
  (testing
      (is (= "a123b"
             (apply-replacements "aone2threeb"))))
  (testing
      (is (= results
             (map parse-line-2 samples))))
  (testing
      (is (= total
             (apply + (map parse-line-2 samples)))))
  (testing
      (let [samples ["onetwothree3foureightwo" "twoone4" "1eighthree2" "twoneeighthree" "2fourseven1oneights"]
            results '(12 24 12 23 28)]
        (is (= results
               (map parse-line-2 samples))))))

(run-tests 'aoc1)

(def data-file "1.txt")

(println "aoc1a: "
         (with-open [rdr (clojure.java.io/reader data-file)]
           (reduce +
                   (map parse-line-1 (line-seq rdr)))))
(println "aoc1b: "
         (with-open [rdr (clojure.java.io/reader data-file)]
           (reduce +
                   (map parse-line-2 (line-seq rdr)))))
