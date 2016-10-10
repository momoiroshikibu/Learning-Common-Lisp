;;; fresh-line
;;; 現在のカーソルが行頭にない場合に限って開業する
;;; "必要なら、新たな行を始めよ"
(progn (princ 22)
       (fresh-line)
       (princ 33))
;; 22
;; 33


;; 2つのprincの間に複数のfresh-lineを挟んでも、改行が1回しか行われない。
;; 最初のfresh-lineが実際の改行を行う。2番目のfresh-lineは何もしない。
(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))
;; 22
;; 33
