(defun 2= (n)
  (if (= 2 n)
      t
      nil))

(remove-if-not #'2= '(1 2 3))           ; (2)
(remove-if-not #'2= '(2 2 2))           ; (2 2 2)
