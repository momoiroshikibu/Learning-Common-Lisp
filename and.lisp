(and (oddp 5) (oddp 7) (oddp 9))
;; T

(or (oddp 4) (oddp 7) (oddp 8))
;; T


;; ある数が偶数の場合だけ、グローバル変数を真にセットする
(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t))
*is-it-even*
;; T

(defparameter *is-it-even* nil)
(or (oddp 5) (setf *is-it-even* t))
*is-it-even*
;; nil


;; 以下の2つは同じ
(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))

(and *file-modified* (ask-user-about-saving) (save-file))


(if (and *file-modified*
         (ask-user-about-saving))
    (save-file))
