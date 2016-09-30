;; memberはリストの中にある要素が含まれているかどうかを調べる
(if (member 1 '(3 4 1 5))
    'one-is-in-the-last
    'one-is-not-in-the-last)

(member 1 '(3 4 1 5))
;; (1 5)



(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list)
;; 'nil-is-in-the-list

;; もしmemberが見つけた値をそのまま返していたら、nilが返る。
;; これは偽と判断されるから、nilはリストにないという間違った答えになってしまう。
;; 探している要素が見つかった点から先のcdrを返すことで、探している値に関わらず、
;; 見つかった場合は真となる値が返ることが保証される。
