;; substitute-if
;; 与えられたテスト関数の結果に寄って値を書き換える
(substitute-if #\e #'digit-char-p "I'm a l33t hack3r!")
;; I'm a leet hacker!

;; テスト関数はdigit-char-p


;; リストも処理可能
(substitute-if 0 #'oddp '(1 2 3 4 5 6))
;; (0 2 0 4 0 6)


;; substitute-if関数は、様々なデータ型のオブジェクトを引数として取り、それぞれに適切な処理を行うことができる。
;; => ジェネリックな関数

