;; truncate
;; 商, 余

(truncate 10 2)
;; 5 0

(truncate 2 3)
;; 0, 2

(multiple-value-bind (x y) (truncate 2 3)
  (print x)
  (print y))

