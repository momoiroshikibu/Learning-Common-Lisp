;; pushnew

(setq x '(a (b c) d))
;; (A (B C) D)

(pushnew 5 (cadr x))
;; (5 B C)

(princ x)
;; (A (5 B C) D)

