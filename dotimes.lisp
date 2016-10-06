;; dotimes
;; 指定した回数だけコードを実行する関数

(dotimes (i 10) (princ i))
;; 0123456789
;; NIL


(dotimes (i 3)
  (fresh-line)
  (princ i)
  (princ ". Hatchoo!"))
