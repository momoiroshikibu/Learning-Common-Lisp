;; mapcar 引数の皮下の関数とリストを受け取って、リストの要素一つ一つについてそれを引数として受け取った関数を呼び出す。

(mapcar #'sqrt '(1 2 3 4 5))

(mapcar #'car '((foo bar) (baz qux)))

(mapcar (function car) '((foo bar) (baz qux)))

(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux))))
;; ローカル変数ではなく、関数のcarを参照する => #'carとしているため
