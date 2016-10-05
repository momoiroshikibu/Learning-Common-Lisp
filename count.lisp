;; 特定の要素がいくつシーケンス中にあるかを数える

(count 1 '(1 2 3 1))
;; 2

(count #\s "mississippi")
;; 4

(count 0 (make-array 5))
;; 5
