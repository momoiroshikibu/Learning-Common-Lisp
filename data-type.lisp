(eq 'fooo 'FoOo)
; T

(+ 1 1.0)


(expt 53 53)

(/ 4 6)

(/ 4.0 6)

(princ "Tutti Frutti")

(princ "He yelled \"Stop that thief!\" from the busy street.")


'(expt 2 3)

(cons 'chicken 'cat)

(cons 'chicken 'nil)

(cons 'chicken ())

(cons 'pork '(beek chicken))
(cons 'beef (cons 'chicken ()))


(cons 'pork (cons 'beef (cons 'chicken ())))

(car '(pork beef chicken))
(cdr '(pork beef chicken))
(cdr '(pork beef chicken))
(car '(beef chicken))
(car (cdr '(pork beef chicken)))
(cadr '(pork beef chicken))
(caddr '(pork beef chicken))

;; 全て同じ
(cons 'pork (cons 'beef (cons 'chicken ())))
(list 'pork 'beef 'chicken)
'(pork beef chicken)

(car '((peas carrots tomatoes) (pork beef chicken)))
(cdr '(peas carrots tomatoes))
(cdr (car '((peas carrots tomatoes) (pork beef chicken))))
(cdar '((peas carrots tomatoes) (pork beef chicken)))


(cons (cons 'peas (cons 'carrots (cons 'tomatoes ())))
      (cons (cons 'pork (cons 'beef (cons 'chicken ()))) ()))


(cddr '((peas carrots tomatoes) (pork beef chicken) duck))
(cddar '((peas carrots tomatoes) (pork beef chicken) duck))
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck))
