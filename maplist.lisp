;; maplist
;; mapcarと似ているが、リストの中の現在の要素だけでなく、リストの残りすべてを見ることができる

(mapcar #'print '(a b c))
;; (A B C)


(maplist #'print '(a b c))
;; (A B C)
;; (B C)
;; (C)


;; maplist関数はprintを呼ぶ度に、それに現在の要素とそこから先の要素全てを渡す。
