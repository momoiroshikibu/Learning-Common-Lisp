;; inplicit progn
(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

;; ODD-NUMBER

*number-is-odd*
;; T
