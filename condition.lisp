;;; define-condition
;;; コンディションの型に応じてよりわかりやすいメッセージを表示することができる

(define-condition foo () ()
  (:report (lambda (condition stream)
             (princ "Stop FOOing around, numbskull!" stream))))

(error 'foo)
;; Stop FOOing around, numbskull!


;;; handler-case
;;; define-conditionでコンディション型を定義したときに名前(上の例ではfoo)を与えた。
;;; この名前を使えば、この型のコンディションが通知されたときに、プログラムを中断する代わりに実行する処理を、プログラムの上位層で書いておくことができる。
(defun bad-function ()
  (erorr 'foo))

(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))

;; handler-caseコマンドに最初に与えるのは、横取りしたいコンディションを通知するかもしれないコード
;; この例ではbad-functionの呼び出しがそれにあたる。
;; handler-caseの残りの部分は、特定のコンディションが通知されたときに何をすべきかを並べておく
;; このコードが実行されると、bad-functionがジックオサレ、その中の(error 'foo)によってfooコンディションが通知される。
;; handler-caseがなかったら、この時点でプログラムが中断されてREPLにエラープロンプトが表示されている。
;; この例ではhandler-caseがfooコンディションを横取りして、プログラムは中断されることなく"somebody signaled foo!"という結果が返ってくる。

