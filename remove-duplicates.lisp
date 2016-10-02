;; remove-duplicates
;; 重複を除く
;; デフォルトでeqlを比較に使う

(remove-duplicates '(1 2 3))
;; (1 2 3)


(remove-duplicates '(1 2 3 3))
;; (1 2 3)


;; :test指定なし => eql
(remove-duplicates '((1 . 2) (1 . 2)))
;; ((1 . 2) (1 . 2))

;; :test指定 equal
(remove-duplicates '((1 . 2) (1 . 2)) :test #'equal)
;; ((1 . 2))
