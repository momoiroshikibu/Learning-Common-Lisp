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


(defun game-tree (board player spare-dice first-move)
  "ゲームツリーの作成"
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))


(defun add-passing-move (board player spare-dice first-move moves)
  "相手に手番を渡す"
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;; 指し手のリストに(それが許されていれば)自分の手番を終了するという動きを追加する。
;; 現在までに集められた可能な指し手が引数movesに渡され、この関数はそれに指し手を追加して返す。
;; ただし、これがプレーヤーの手番の最初の手のときは最低1回攻撃しないとならないので、movesをそのまま帰す。
;; そうでなければ、新たな手を加える。

