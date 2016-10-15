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



(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink"
                   height ,height width ,width)
     ,@body))



(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

(brightness '(255 0 0) -100)
;; (155 0 0)


(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))


(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(svg 150 150
     (circle '(50 . 50) 50 '(255 0 0))
     (circle '(100 . 100) 50 '(0 0 255)))



(defun polygon (points color)
  (tag polygon points (format nil
                              "~{~a,~a !}"
                              (mapcan (lambda (tp)
                                        (list (car tp) (cdr tp)))
                                      points))
       style (svg-style color)))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

(random-walk 100 10)

