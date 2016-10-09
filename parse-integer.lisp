;; parse-integer
(parse-integer "1")
;; 1, 1

(parse-integer "123")
;; 123, 3

(parse-integer "123m" :junk-allowed t)
;; 123, 3

(parse-integer "no-integer" :junk-allowed t)
;; NIL, 0
