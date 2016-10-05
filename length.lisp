;; Lispで値の列(シーケンス)を表す3つの主要なデータ型、リスト、配列、文字列を統一的に扱うことができる。

(length '(a b c))
;; 3

(length "blub")
;; 4

(length (make-array 5))
;; 5

