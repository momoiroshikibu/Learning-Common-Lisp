;; 与えた述語を満たす最初の要素を見つける

(find-if #'oddp '(2 4 5 6))
;; 5

(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number)
;; THERE-IS-AN-ODD-NUMBER

(find-if #'null '(2 4 nil 6))
;; NIL

;; nilを見つけ出しているが、条件判断では偽とされてしまう。

