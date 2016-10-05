;; map
;; 各要素を引数に渡した関数を呼んで結果を集める。
;; mapcarはリストにしか使えないが、mapは全てのシーケンスに使える。
;; 返り値としてどのシーケンス型の値を返すかという引数を取る。

(map 'list
     (lambda (x)
       (if (eq x #\s)
           #\s
           x))
     "This is a string")
;; (#\T #\h #\i #\s #\  #\i #\s #\  #\a #\  #\s #\t #\r #\i #\n #\g)

(map 'string
     (lambda (x)
       (if (eq x #\s)
           #\s
           x))
     "This is a string")
;; "This is a string"
