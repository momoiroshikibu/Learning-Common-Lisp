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


;; 指し手の情報
;; ・最初の要素は、手の記述。ここでは常に手番を終了するだけなので、記述はnilとしておく。
;; ・次の要素は、この手が指された後にあり得る全ての可能性を表すゲーム木。
;;   この部分はgame-treeを再帰的に呼び出すことで作れる。プレーヤーはこの手で手番を終了するので、
;;   add-new-dice関数を使って、獲得したサイコロの数に応じて受ける補給を反映した新たな盤面を作り出し、game-treeの引数に渡す。

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player
                                                            src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                 collect n))))

;; 各マスにつき占有者が誰かを調べる必要があるので、ゲーム版の指定場所の占有者を返すローカル関数playerを定義する。
;; 同様に、指定場所にあるサイコロの数を返すdiceも作っておく。
;; 各マスについて、それが現在のプレーヤーに専有されていれば、その場所から攻撃可能なマスを探す。
;; 1つのマスから攻撃可能なマスは複数有りえるので、ゲーム版の操作にはmapcanを使う。
;; => 各要素に対する計算結果をリストで得て、それをつなぎ合わせる関数
;; これによって各マスは0個以上の任意個の可能な手を返すことができる。
;; mapcanの中のlambda関数はゲーム版のマスごとに呼ばれる。最初にチェックするのは、現在のプレーヤーがそのマスを専有しているかどうか。
;; 次に、隣接するマスをすべて見て、攻撃が可能かどうかを調べる。
;; 後者にはまたmapcanを使う。隣接するマスを得るのは、これから各neighbors関数。
;; 隣のマスに攻撃が可能かどうか: まずそのマスは自分のものであってはならない。またおsのマスのサイコロの数が攻撃元のマスのサイコロの数より少なくなければならない
;; そのようなマスを見つけたら、それを有効な攻撃の手として記述する。
;; 攻撃の手の記述は、攻撃元のマスの番号と攻撃先のマスの番号をリストにしたもの。
;; 手番を終了するときとどうよう、この攻撃の手をさした後に可能なすべての手をゲーム木として再帰的に生成する。


(defun neighbors (pos)
  "隣接するマスを見つける"
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
         when (and (>= p 0) (< p *board-hexnum*))
       collect p)))
(neighbors 2)



(defun board-attack (board player src dst dice)
  "攻撃を行う. マスsrcからdstを攻撃したときに何が起きるかを計算する"
  (board-array (loop for pos from 0
                    for hex across board
                    collect (cond ((eq pos src) (list player 1))
                                  ((eq pos dst) (list player (1- dice)))
                                  (t hex)))))

