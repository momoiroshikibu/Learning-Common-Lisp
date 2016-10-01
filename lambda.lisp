;; lambda
;; 関数ではなくマクロ。関数を返す。

(lambda (n) (/ n 2))

(mapcar (lambda (n) (/ n 2)) '(1 2 3 4 5))
