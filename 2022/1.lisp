(defpackage :day1
  (:use :common-lisp)
  (:import-from :uiop/utility :split-string))
(in-package :day1)

(defparameter *data* (with-open-file (in "1.input.txt")
                 (loop for line = (read-line in nil)
                       while line
                       collect line)))

(defun elfsums (xs)
  (loop with sublist
        for x in xs
        if (equal "" x)
          collect (apply '+ sublist) into result and do (setf sublist nil)
        else
          do (push (parse-integer x) sublist)
        finally (return (nconc result (list (apply '+ sublist))))))

(defun maxelf (xs)
  (loop for x in xs
        maximizing x))

(print (format t "Max elf calories: ~A~%" (maxelf (elfsums *data*))))
(print (format t "Sum of top three elves: ~A~%"
               (apply '+ (subseq (sort (elfsums *data*) '>) 0 3))))
