(defun 2= (n)
  (if (= 2 n)
      t
      nil))

(remove-if-not #'2= '(1 2 3))           ; (2)
(remove-if-not #'2= '(2 2 2))           ; (2 2 2)


;; from Practical Common Lisp
;; http://www.gigamonkeys.com/book/practical-a-simple-database.html
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
