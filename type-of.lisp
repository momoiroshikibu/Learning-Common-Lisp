;; type-of

(type-of 'foo)
;; SYMBOL

(type-of 5)
;; (INTEGER 0 4611686018427387903)

(type-of "foo")
;; (SIMPLE-ARRAY CHARACTER (3))

(defstruct bird name)
(type-of (make-bird))
;; BIRD

