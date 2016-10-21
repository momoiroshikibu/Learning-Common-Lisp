;; メモ化は関数型スタイルのコードにしか使えない
;; 関数が受け取った引数と、その結果を毎回記録しておく
;; 関数が以前受け取ったのと同じ引数でまた呼ばれたら、結果をもう一度計算せずに、計算済みの値を返す。

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))
