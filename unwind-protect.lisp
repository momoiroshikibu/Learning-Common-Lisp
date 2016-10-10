;;; unwind-protect
;;; このコードだけは何があっても実行してくれ

(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' matter what"))

;; I need to say 'h'flubyduby' matter what

;; unwind-protectの中でゼロ除算を行っていて、これはコンディションを通知する。
;; 実行の放棄を指示したあとで、重要なメッセージが表示される。

;; Common Lispのwith-マクロを使う場合、そのマクロが内部でunwind-protectを呼んでくれることが多いので、直接unwind-protectを呼ばなければならないことは多くない。
