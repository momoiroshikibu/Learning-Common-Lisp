(defmacro lazy (&body body)
  ;; 生成されたコードの中で2つの変数を使う必要があるので、gensymで命名。
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(lazy (+ 1 2))


(force (lazy (+ 1 2)))


(defun add (a b)
  (princ "I am adding now")
  (+ a b))
;; ADD

(defparameter *foo* (lazy (+ 1 2)))
;; *FOO*

(force *foo*)
;; I am adding now
;; 3


;; lazyによって作られた関数を呼び出すだけ
(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))


(defparameter *yeah* (lazy-cons 4 7))

(lazy-car *yeah*)
(lazy-cdr *yeah*)


;; すべての正の整数のリスト
;; lazy-consを通じて、数を1ずつ増やしながら無限にfを再帰呼び出ししている。
;; 一度この不可能に見える*integers*変数を定義したら、期待通りそこから数値を取り出すことができる。
(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))


;; lazyコマンドを使っている限り、この無限の整数リスト*integers*からほしいだけ数を取り出すことができる。
;; 取り出したいところまでの整数が、必要に応じて計算される。
(lazy-car *integers*)
(lazy-car (lazy-cdr *integers*))
(lazy-car (lazy-cdr (lazy-cdr *integers*)))

;; すべての正整数のリストのような無限リストばかりが遅延リストではない。
;; 終端のある遅延リストの終端を表現するために、lazy-nilが必要。
;; そしてまた、null関数が通常のリストの終端に達したかどうかを調べるのに対応する、遅延リストの終端を調べるlazy-null関数も。
(defun lazy-nil ()
  (lazy nil))
(defun lazy-null (x)
  (not (force x)))

