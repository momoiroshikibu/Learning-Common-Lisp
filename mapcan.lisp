;; mapcan
;; mapcarの仲間。mapcarと違い、mapcanに渡す関数は必ずリストを返す必要がある。
;; mapcanは返されたリストを全てappendしたものを返す
;; 入力リストの項目と出力リストの項目が必ずしも1対1で対応するとは限らない場合に便利な関数。

(defun ingredients (order)
  (mapcan (lambda (burger)
            (case burger
              (single (list 'patty))
              (double (list 'patty 'patty))
              (double-cheese (list 'patty 'patty 'cheese))))
          order))

(ingredients '(single double-cheese double))


(mapcar (lambda (x)
          (list x x))
        '(1 2 3))
;;((1 1) (2 2) (3 3))
(apply #'append (mapcar (lambda (x)
          (list x x))
        '(1 2 3)))
;; (1 1 2 2 3 3)
