(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(add 2 3)



;; var
;; 最初の引数はローカル変数として定義される名前
;; マクロの中では引数varの値がその名前になっている。
;; foo
;;
;; val
;; 2番目の式はローカル変数の値を決めるコード。
;; (+ 2 3)
;;
;; body
;; let1の中で実行されるコードの本体。
;; let1が作る新しい変数(foo)を使うことが出来る。
;; マクロではこのコードが引数bodyの値として使える。
;;
;; &body
;; letコマンドは本体の中に複数の式をくことが出来る。
;; let1も同じように使えると良い。
;; &bodyはマクロ展開時に「マクロの使われている場所に出てくる残りの式をすべてリストにして次の引数に渡してくれ」ということ。
;; body引数に渡ってくる値はネストしたリスト((* foo foo))になっている。

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))


(let ((foo (+ 2 3)))
  (* foo foo))
;; 25

(let1 foo (+ 2 3)
  (* foo foo))


(defun add (a b)
  (let1 x (+ a b)
        (format t "The sum is ~a" x)
        x))

(macroexpand '(let1 foo (+ 2 3)
               (* foo foo)))
;; (LET1 FOO (+ 2 3) (* FOO FOO)), T




;; 16.2 もっと複雑なマクロ
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                    acc)))
    (f lst 0)))

;; リストを分割するマクロ
(split '(2 3)
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
;; This can be split into 2 and (3)

(split '()
       (format t "This can be split into ~a and ~a" head tail)
       (format t "This cannot be split."))
;; This cannot be split.

;; splitマクロは自動的に2つのローカル変数head, tailを作り、car, cdrを格納する。
;; リストが空だった場合は、3番目の引数に渡された式が実行される


;; bug version
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))


(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

;; 自動的に変数を作り出すマクロ => アナフォリックマクロ

;; マクロ中で式が繰り返し実行されるのを防ぐ

(macroexpand '(split (progn (princ "Lisp rocks!")
                            '(2 3))
               (format t "This can be split into ~a and ~a." head tail)
               (format t "This cannot be split.")))

(defmacro split (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

;; マクロの変数xとぶつかる。
(let1 x 100
  (split '(2 3)
         (+ x head)
         nil))


(macroexpand '(split '(2 3)
               (+ x head)
               nil))

;; 変数衝突を避けるために、gensymという関数がある
(gensym)
;; #:G659


(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))


;; アナフォリックマクロの性質として、予め決まっている変数を補足するのはバグではなく仕様
;; マクロの本体内でこれらの変数を使えるようにわざわざ補足している。
