(cons 1 (cons 2 (cons 3 nil)))

(equal '(1 2 3) (cons 1 (cons 2 (cons 3 nil))))


(cons 1 (cons 2 3))
;; (1 2 . 3)

'(1 . (2 . (3 . nil)))
;; (1 2 3)

(equal '(1 2 3) '(1 . (2 . (3 . nil))))
;; T


;; 長さ2のドットリスト
(cons 2 3)
;; (2 . 3)
