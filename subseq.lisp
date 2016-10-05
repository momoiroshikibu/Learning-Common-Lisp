;; subseq
;; 始点と終点を指定してシーケンスの一部分を取り出すのに使える

(subseq "america" 2 6)
;; eric

(subseq '(1 2 3 4 5 6 7) 2 6)
;; (3 4 5 6)


(subseq (make-array 7 :initial-contents '(1 2 3 4 5 6 7)) 2 6)
;; #(3 4 5 6)
