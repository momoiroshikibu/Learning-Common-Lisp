;; symbol-function
;; 引数のシンボルに束縛されている関数を取り出す
;; 同じ名前の関数を再定義した後でも、以前のバージョンの定義にアクセスするため

(defun a ()
  'a)

(funcall (symbol-function 'a))
;; A
