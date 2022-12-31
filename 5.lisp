#!/usr/bin/env sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "alexandria" :silent t)
(ql:quickload "cl-ppcre" :silent t)
(defpackage :day5
  (:use :cl :cl-ppcre :alexandria )
  (:import-from :uiop/utility :split-string))

(in-package :day5)

(defparameter *testdata* (with-open-file (in "5.example.txt")
                           (loop for line = (read-line in nil)
                                 while line
                                 collect line)))
(defparameter *data* (with-open-file (in "5.input.txt")
                           (loop for line = (read-line in nil)
                                 while line
                                 collect line)))

(defstruct header-columns
  (position-list () :type list)
  (length 0 :type integer))

(defun find-key-positions (data)
  (loop for line in data
        for lineno from 0
        for match = (all-matches "^[ 0-9]+$" line)
        when (not (null match))
          ;; do (print line)
          return  (make-header-columns
                   :position-list (loop for x in (all-matches "[0-9]" line) by 'cddr
                                        collecting x)
                   :length (- lineno 1))))

(defparameter *header*
  (find-key-positions *testdata*))

(defparameter *towers* '())

(defun find-towers (data)
  (mapcar (lambda (group) (delete #\  group)) ;; trim empty tops of columns
          (apply #'mapcar #'list ;; transpose (https://stackoverflow.com/a/3513158/222519)
                 (loop for line in data
                       with header = (find-key-positions data)
                       for idx from 0 to (header-columns-length header)
                       collect (loop 
                                 for pos in (header-columns-position-list header)
                                 collect (char line pos))))))

(setq *towers* (find-towers *testdata*))

(defun move (cmdline towers &key model9001)
  "in-place/destructive (move count from src to dst) against towers"
  (let* ((cmd (split " " cmdline))
         (count (parse-integer (nth 1 cmd)))
         (src (parse-integer (nth 3 cmd)))
         (dst (parse-integer (nth 5 cmd)))
         (tmp '()))
    (format *error-output* "==> ~A: ~{~A ~}~%" cmd towers)
    (loop for iter from 0 to (- count 1)
          if model9001
            do (progn
                 (push (pop (nth (- src 1) towers)) tmp)
                 (format *error-output* "~2A> ~A: ~{~A ~} tmp: ~A~%" iter cmd towers tmp))
          else
            do
               (progn
                 (push (pop (nth (- src 1) towers))
                       (nth (- dst 1) towers))
                 (format *error-output* "~2A> ~A: ~{~A ~}~%" iter cmd towers))
          finally (if model9001
                      (progn
                        (setf (nth (- dst 1) towers)
                              (append (reverse tmp)
                                      (nth (- dst 1) towers)))
                        (format *error-output* ">>> ~A: ~{~A ~}~%" cmd towers))))
    towers))

(defun process-tower-data (data &key model9001)
  (let ((preamble (find-key-positions data))
        (towers (find-towers data)))
    (loop for cmd in (subseq data (+ 3 (header-columns-length preamble)))
          do (move cmd towers :model9001 model9001))
    towers))

(defun part1 (data)
  (concatenate 'string (mapcar 'car  (process-tower-data data))))
(defun part2 (data)
  (concatenate 'string (mapcar 'car  (process-tower-data data :model9001 t))))

(format t "test1: ~A~%" (part1 *testdata*))
(format t "test2: ~A~%" (part2 *testdata*))
(format t "part1: ~A~%" (part1 *data*))
(format t "part2: ~A~%" (part2 *data*))

