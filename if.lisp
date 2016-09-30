(if (= (+ 1 2) 3)
    'yup
    'nope)
;; YUP

(if (= (+ 1 2) 4)
    'yup
    'nope)
;; NOPE

(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)
;; THE-LIST-HAS-STUFF-IN-IT

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)
;; THE-LIST-IS-EMPTY

(if (oddp 5)
    'odd-number
    'even-number)
;; ODD-NUMBER


;; ifの持つ2つの式のうち、どちらか一方だけが実際に評価される。
;; if文で出来ることは1つだけ。


;; prognフォームは、最後の式の評価値をフォーム全体の値として返す。

(defvar *number-was-odd* nil)
(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number)
;; ODD-NUMBER

*number-was-odd*
;; T
