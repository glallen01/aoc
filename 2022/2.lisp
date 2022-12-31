#!/usr/bin/env sbcl --script
(require 'uiop)
(defpackage :day2
  (:use :cl)
  (:import-from :uiop/utility :split-string))
(in-package :day2)

(defparameter *data* (with-open-file (in "2.input.txt")
                       (loop for line = (read-line in nil)
                             while line
                             collect line)))
;;  1 A X rock
;;  2 B Y paper
;;  3 C Z scissors
;;  0 loss
;;  3 draw
;;  6 win

(defun testfunc ()
  (loop for (i j) in (mapcar (lambda (x) (split-string x :separator " ")) *data*)
        do
           (let ((u (cond ((equal i "A") 1)
                          ((equal i "B") 2)
                          ((equal i "C") 3)))
                 (v (cond ((equal j "X") 1)
                          ((equal j "Y") 2)
                          ((equal j "Z") 3))))
             (cond
               ;; tie
               ((equal u v)
                (format t "TIE : ~A(~A) ~A(~A) => ~A~%" i u j v (+ v 3)))
               ;; win
               ((or
                 (equal '(1 2) (list u v))
                 (equal '(2 3) (list u v)) 
                 (equal '(3 1) (list u v)))
                (format t "WIN : ~A(~A) ~A(~A) => ~A~%" i u j v (+ v 6)))
               ;; lose
               ((or
                 (equal '(1 3) (list u v)) 
                 (equal '(2 1) (list u v)) 
                 (equal '(3 2) (list u v)))
                (format t "LOSE: ~A(~A) ~A(~A) => ~A~%" i u j v (+ v 0)))))))

(defun sum-rpc-scores ()
  (apply '+
         (loop for (i j) in (mapcar (lambda (x) (split-string x :separator " ")) *data*)
               collect (let
                           ((u (cond ((equal i "A") 1)
                                     ((equal i "B") 2)
                                     ((equal i "C") 3)))
                            (v (cond ((equal j "X") 1)
                                     ((equal j "Y") 2)
                                     ((equal j "Z") 3))))
                         (cond
                           ((equal u v)
                            (+ v 3)) ; tie
                           ((or
                             (equal '(1 2) (list u v))
                             (equal '(2 3) (list u v))
                             (equal '(3 1) (list u v)))
                            (+ v 6)) ; win
                           ((or
                             (equal '(1 3) (list u v))
                             (equal '(2 1) (list u v))
                             (equal '(3 2) (list u v)))
                            (+ v 0)) ; lose
                           )))))

(defun sum-rpc-scores2 ()
  (apply '+
         (loop for (i j) in (mapcar (lambda (x) (split-string x :separator " ")) *data*)
               collect (let
                           ((u (cond ((equal i "A") 1)
                                     ((equal i "B") 2)
                                     ((equal i "C") 3)))
                            (v (cond ((equal j "X") :lose)
                                     ((equal j "Y") :tie)
                                     ((equal j "Z") :win))))
                         (cond
                           ((equal v :tie)
                            (+ u 3))
                           ((equal v :win)
                            (+ 6 (cond
                                   ((equal u 1) 2)
                                   ((equal u 2) 3)
                                   ((equal u 3) 1))))
                           ((equal v :lose)
                            (+ 0 (cond
                                   ((equal u 1) 3)
                                   ((equal u 2) 1)
                                   ((equal u 3) 2)))))))))


(format t "sum of the scores1: ~A~%" (sum-rpc-scores))
(format t "sum of the scores2: ~A~%" (sum-rpc-scores2))
