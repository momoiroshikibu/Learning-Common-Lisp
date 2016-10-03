;; setf
;; setfの最初の引数は、汎変数と呼ばれる。

(setf foo (make-array 4))
(setf (aref foo 2) (list 'x 'y 'z))
;; #(0 0 (X Y Z) 0)

(setf (car (aref foo 2)) (make-hash-table))
;; #(0 0 (#<HASH-TABLE :TEST EQL :COUNT 0 {10032892F3}> Y Z) 0)

(setf (gethash 'zoink (car (aref foo 2))) 5)
;; #(0 0 (#<HASH-TABLE :TEST EQL :COUNT 1 {10032892F3}> Y Z) 0)

