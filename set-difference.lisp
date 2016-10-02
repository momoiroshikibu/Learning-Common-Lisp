;; set-difference
;; 2つのリストを取り、最初のリストにあって2番目のリストに含まれない要素全てをリストにして返す
(set-difference '(1 2 3) '(1))
;; (3 2)


(set-difference '(1 2 3) '(1 2 3 4))
;; nil
