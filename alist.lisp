(defparameter *drink-order* '((bill . double-expresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
(assoc 'lisa *drink-order*)
;; (LISA . SMALL-DRIP-COFFEE)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

(assoc 'lisa *drink-order*)
;; (LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM)
