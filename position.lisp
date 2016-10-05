;; 特定の要素がシーケンスのどの位置にあるかを教えてくれる

(position #\4 "2kewl4skewl")
;; 5

(position 1 '(0 0 0 1 2))
;; 3

(position 1 (make-array 5))
;; NIL

