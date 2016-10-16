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



;; 通常のリストと遅延リストとの交換
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

;; make-lazyは再帰で与えられたリストを順に見ていき、それぞれのコンスをlazyマクロで包んでいるということになる。

;; 遅延リストを通常のリストに変換する
(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;; 遅延リストから通常のリストに変換する関数が2つ必要なのは、通常のリストと違って、遅延リストは無限に長い可能性があるから。
;; 指定した数の要素だけを取り出す関数があれば、無限リストに対しても使える。
;; take関数は取り出したい要素数を指定するn引数を取る。
;; すべての値が欲しいなら、take-all関数が使える。
;; take-all関数は無限リストには使えない。無限リストからすべての要素を取り出すには無限の時間がかかる。

(take 10 *integers*)
;; (1 2 3 4 5 6 7 8 9 10)

(take 10 (make-lazy '(q w e r t y u i o p a s d f)))
;; (Q W E R T Y U I O P)

(take-all (make-lazy '(q w e r t y u i o p a s d f)))
;; (Q W E R T Y U I O P A S D F)



;; 遅延リストに対するマッピングと検索
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f lazy-cdr lst-cur))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))


(take 10 (lazy-mapcar #'sqrt *integers*))
;; (1.0 1.4142135 1.7320508 2.0 2.236068 2.4494898 2.6457512 2.828427 3.0 3.1622777)

(lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
;; 7

(lazy-nth 4 (make-lazy '(a b c d e f g)))
;; E

