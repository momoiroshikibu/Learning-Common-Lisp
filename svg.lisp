(defun print-tag (name alist closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alist)
  (princ #\>))

(print-tag 'mytag '((color . blue) (height . 9)) nil)

(defmacro tag (name attrs &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs attrs)))
                     nil)
          ,@body
          (print-tag ',name nil t)))


(macroexpand '(tag my-tag (color (color 'blue height (+ 4 5)))))

(tag myTag (color 'blue size 'big)
  (tag first_inner_tag ())
  (tag second_inner_tag ()))



(tag html ()
  (tag body ()
    (princ "Hello World")))


(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(html
  (body
    (princ "Hello")))
