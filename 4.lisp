#!/usr/bin/env sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "alexandria" :silent t)
(defpackage :day4
  (:use :cl :alexandria)
  (:import-from :uiop/utility :split-string))

(in-package :day4)

(defparameter *testdata* (list "2-4,6-8"
                               "2-3,4-5"
                               "5-7,7-9"
                               "2-8,3-7"
                               "6-6,4-6"
                               "2-6,4-8"))

(defun range-to-list (range)
  "turns a range into a list: (range-to-list \"2-4\") => (2 3 4) "
  (destructuring-bind (i j)
      (split-string range :separator "-")
    (loop for n from (parse-integer i) to (parse-integer j)
          collecting n)))

(defun true? (x)
  (not (null x)))

(defun pairs-to-contained (data)
  "splits the input lines into lists of ranges: (pairs-to-list data) => ((\"2-4\" \"6-8\") ...)"
  (loop for (i j)
          in (mapcar (lambda (x) (uiop:split-string x :separator ",")) data)
        for u = (range-to-list i)
        for v = (range-to-list j)
        for overlap = (intersection u v)
        collecting ;(format t "~A~A~A~%" u v overlap)
           (if (and overlap
                    ;; the difference will be nil if fully contained, so look
                    ;; for either set to return not nil => T
                    (or (null (set-difference u overlap))
                        (null (set-difference v overlap))))
               T
               nil)))
(defun pairs-to-overlap (data)
  "splits the input lines into lists of ranges: (pairs-to-list data) => ((\"2-4\" \"6-8\") ...)"
  (loop for (i j)
          in (mapcar (lambda (x) (uiop:split-string x :separator ",")) data)
        for u = (range-to-list i)
        for v = (range-to-list j)
        for overlap = (intersection u v)
        collecting ;(format t "~A~A~A~%" u v overlap)
           (if overlap
               T
               nil)))

(defparameter *data* (with-open-file (in "4.input.txt")
                       (loop for line = (read-line in nil)
                             while line
                             collect line)))
(defparameter *priorities*
  (alist-hash-table
   (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
         for i from 1 collect (cons c i))))

(defun part1 (data)
  (count-if #'true? (pairs-to-contained data)))

(defun part2 (data)
  (count-if #'true? (pairs-to-overlap data)))

(format t "test1: ~A~%" (part1 *testdata*))
(format t "test2: ~A~%" (part2 *testdata*))
(format t "part1: ~A~%" (part1 *data*))
(format t "part2: ~A~%" (part2 *data*))
