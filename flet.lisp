(flet ((f (n)
         (+ n 10)))
  (f 5))


(flet ((f (n)
         (+ n 10))
       (g (n)
         (- n 3)))
  (g (f 5)))


;; ローカル関数の中で同じスコープで定義されるローカル関数名を使いたい場合
;; ローカル関数が自分自身を呼ぶ場合にも使える
(labels ((a (n)
           (+ n 5))
         (b (n)
           (+ (a n) 6)))
  (b 10))
