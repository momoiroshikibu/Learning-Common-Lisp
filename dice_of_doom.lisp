(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


(defun board-array (lst)
  "リストで表現されたゲーム盤を配列表現へと変える"
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  "ランダムなゲーム盤を作る"
  (board-array (loop for n below *board-hexnum*
                    collect (list (random *num-players*)
                                 (1+ (random *max-dice*))))))
;; 呼ばれるたびに、ランダムに決まる異なる結果を返すので、関数型ではない。


(defun player-letter (n)
  "プレイヤー番号を文字に変換する"
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (- *board-size* y)
                  do (princ " "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))


