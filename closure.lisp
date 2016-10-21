;; クロージャはlambdaで関数が作られる時に、外側の情報を補足したもの。

(defparameter *foo* (lambda ()
                      5))
(funcall *foo*)
;; 5


(defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))
(funcall *foo*)
;; 5


;; letにより作られた変数は必ずしもローカルとは限らない。
;; lambdaの中で参照されることで、クロージャに補足される可能性がある。




;; 呼ばれる度に現在の行番号を出力する
(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))

(my-print "this")
;; 0
;; "this"

(my-print "is")
;; 1
;; "is"

