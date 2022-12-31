#!/usr/bin/env sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "alexandria" :silent t)
(defpackage :day3
  (:use :cl :alexandria))

(in-package :day3)

(defparameter *testdata* (list
                      "vJrwpWtwJgWrhcsFMMfFFhFp"
                      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                      "PmmdzqPrVvPwwTWBwg"
                      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                      "ttgJtRGJQctTZtZT"
                      "CrZsJsPPZsGzwwsLwLmpwMDw"))

(defparameter *data* (with-open-file (in "3.input.txt")
                       (loop for line = (read-line in nil)
                             while line
                             collect line)))


;; from: https://github.com/julian3ng/aoc-lisp/blob/master/2022/problem-3.lisp#L6
(defparameter *priorities*
  (alist-hash-table
   (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
         for i from 1 collect (cons c i))))

(defun get-priority (c)
  (gethash c *priorities* 0))

;; figured out this much on my own, but pulling out into func borrowing from
;; @julian3ng and adding the priorities
(defun common-chars (s1 s2)
  (intersection (coerce s1 'list) (coerce s2 'list)))

(defun part1 (data)
  (loop for i in data
        for j in (mapcar (lambda (s) (/ (length s) 2)) data)
        sum (let ((rucktop (subseq i 0 j))
                  (ruckbot (subseq i j)))
              (car (mapcar (lambda (x) (get-priority x)) (common-chars rucktop ruckbot))))))

(defun part2 (data)
  (loop for (x y z) on data by 'cdddr
        sum (get-priority
             (car (intersection
                   (intersection (common-chars z y)
                                 (common-chars y x))
                   (common-chars z x))))))

(format t "test1: ~A~%" (part1 *testdata*))
(format t "test2: ~A~%" (part2 *testdata*))
(format t "part1: ~A~%" (part1 *data*))
(format t "part2: ~A~%" (part2 *data*))
