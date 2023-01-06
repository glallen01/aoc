


(defun list-with-dup? (list)
  "checks for duplicates by comparing the list to it's self without duplicates"
  (not (= (length list)
          (length (delete-duplicates list)))))

(defun find-start-of-packet (msg)
  (loop for i from 0 to (- (length msg) 4)
        do (let* ((segment
                    (list (char msg (+ 0 i)) 
                          (char msg (+ 1 i)) 
                          (char msg (+ 2 i)) 
                          (char msg (+ 3 i))))
                  (test-key
                    (list-with-dup? segment)))
             (case test-key
               ('()
                (format t "no dupes ~a ~{~a~}~%" (+ i 4) segment)
                (return (+ i 4)))
               ((T) (format t ">> dupes ~a ~{~a~}~%" (+ i 4) segment))
               (otherwise test-key)))))

(defun find-start-of-message (msg)
  (loop for i from 0 to (- (length msg) 14)
        do (let* ((segment
                    (loop for j from 0 to 13
                          collecting (char msg (+ j i))))
                  (test-key
                    (list-with-dup? segment)))
             (case test-key
               ('()
                (format t "no dupes ~a ~{~a~}~%" (+ i 14) segment)
                (return (+ i 14)))
               ((T) (format t ">> dupes ~a ~{~a~}~%" (+ i 14) segment))
               (otherwise test-key)))))
