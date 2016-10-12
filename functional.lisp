;; 関数型
(defun add-widget (database widget)
  (cons widget database))


;; 非関数型
(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~a~%" *database*)))



;; 命令型
(defparameter *my-list* '(4 7 2 3))
(loop for n below (length *my-list*)
   do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))
*my-list*


;; 関数型
(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))))

(add-two '(4 7 2 3))

(mapcar (lambda (x)
          (+ x 2))
        '(4 7 2 3))
