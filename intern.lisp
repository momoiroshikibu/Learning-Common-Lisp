;;; intern
;;; 文字列をシンボルに変換する

(intern "hello")
;; |hello|, NIL

(intern "hello")
;; |hello|, :INTERNAL
