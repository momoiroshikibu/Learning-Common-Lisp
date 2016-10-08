;; copy-structure
;; 浅いコピーであるため、構造体が数値やシンボルと言った単純なデータではなく、
;; リストや他の構造体のような複雑なデータを持っていた場合、共有されてしまう。

(defstruct animal x y energy dir genes)
(defparameter *parent* (make-animal :x 0
                                    :y 0 
                                    :energy 0
                                    :dir 0
                                    :genes (list 1 1 1 1 1 1 1 1)))
;; *PARENT*

(defparameter *child* (copy-structure *parent*))
;; *CHILD*

(setf (nth 2 (animal-genes *parent*)) 10)
;; 10

*parent*
#S(ANIMAL :X 0 :Y 0 :ENERGY 0 :DIR 0 :GENES (1 1 10 1 1 1 1 1))

*child*
#S(ANIMAL :X 0 :Y 0 :ENERGY 0 :DIR 0 :GENES (1 1 10 1 1 1 1 1))
