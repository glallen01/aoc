;; WIP

(defclass inode ()
  ((left  :accessor left  :initform nil)
   (right :accessor right :initform nil)
   (name  :accessor name :initarg :name)))

(defclass file (inode)
  ((size  :accessor size :initarg :size)))

(defclass dir (inode)
  ((contents :accessor contents :initform '() :initarg :contents)
   (parent   :accessor parent   :initform '() :initarg :parent)))

(defparameter *root* (make-instance 'dir :name "/"))

(defmethod print-object ((i inode) out)
  (with-slots (name) i
    (print-unreadable-object (i out :type t)
      (format out "~<~:_foo-slot = ~A~:>" (list name)))))

(defmethod print-object ((i dir) out)
  (with-slots (name contents parent) i
    (print-unreadable-object (i out :type t)
      (format out "~A ~A ~A"  "foo" "bar" "foo"))))

(defmethod print-inodes ((i inode) &optional (parent *top*))
  (if (eql (type-of i) 'DIR)
      (progn (format t "~20A ~20A (~10A)~%"
                     (name parent)
                     (name i)
                     "(size TBD)")
             (loop for ii in (contents i)
                   do (print-inode ii i)))
      (format t "~20t~A ~20t~A (~10t~A)~%"
              (name parent)
              (name i)
              (size i))))

(defun get-obj-by-name (targetname &optional (path *top*))
  (if (not (equalp targetname (name path)))
      (if (eql (type-of path) 'DIR)
          (loop for ii in (contents path)
                until (not (null path))
                return (get-obj-by-name targetname ii))))
  path)



(setf (contents *top*) (list
                        (make-instance 'dir :name "a")
                        (make-instance 'file :name "b.txt" :size 14848514)
                        (make-instance 'file :name "c.dat" :size 8504156)
                        (make-instance 'dir :name "d")))

;; $ cd /
(mkdir *root* "/")
(setpwd "/")
;; $ ls
;; dir a
(mkdir pwd a)
;; 14848514 b.txt
(mkfile pwd :name "b.txt" :size 14848514)
;; 8504156 c.dat
(mkfile pwd ...)
;; dir d
(mkdir ...)
;; $ cd a
(setpwd pwd "a")
;; $ ls
;; dir e
;; 29116 f
;; 2557 g
;; 62596 h.lst
;; $ cd e
;; $ ls
;; 584 i
;; $ cd ..
;; $ cd ..
;; $ cd d
;; $ ls
;; 4060174 j
;; 8033020 d.log
;; 5626152 d.ext
;; 7214296 k


(ql:quickload :access)
(access:accesses *top* 'contents)

(print-inode *top*)



;; from chatgpt:
;;;;;;;;;;;;;;;;;;;;;

(defun print-tree (node)
  (if (not (null node))
      (progn
        (print-tree (left node))
        (print (name node))
        (print-tree (right node)))))

;; write a function to insert a new node into the tree
(defun insert-node (node tree)
  (if (null tree)
      (setf tree node)
    (if (string&lt; (name node) (name tree))
        (insert-node node (left tree))
      (insert-node node (right tree))))))

;; write a function to create a new tree
(defun new-tree (node)
  (insert-node node nil))

;; write a function to create a new file
(defun new-file (name size)
  (make-instance 'file :name name :size size))

;; write a function to create a new directory
(defun new-dir (name)
  (make-instance 'dir :name name))

; write a common lisp function using the drakma library to read a file from a url and write it to a file on the local machine.

(defun read-url-to-file (url filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (drakma:http-request url :output stream)))

(read-url-to-file "http://www.google.com" "google.html")

(defun read-url-to-file (url filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (drakma:http-request url :output stream)))

(defun read-url-to-file-with-progress (url filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (drakma:http-request url :output stream
                             :progress-callback #'(lambda
                                                      (bytes-read)
                                                    (format t "~d bytes read~%" bytes-read)))))
