;; 構造体のインスタンスを作る関数や、属性へのアクセス関数をすべて自動的に作ってくれる
(defstruct person
  name
  age
  waist-size
  favorite-color)

(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

(print *bob*)
;; #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")

(person-age *bob*)
;; 35

(setf (person-age *bob*) 36)
;; 36


(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
;; *THAT-GUY*

(person-age *that-guy*)
;; 35
