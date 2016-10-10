;; format
;; 高機能なテキスト表示関数

(format t "Add onion rings for only ~$ dollars more!" 1.5)
;; Add onion rings for only 1.50 dollars more!


;; t => 出力先(nil: 文字列, t: コンソール, その他ストリーム)
;; ~$ が制御シーケンス


;;; 出力先
;;; nil
;;; 出力する代わりに、生成されたテキストを文字列として返す
;;; t
;;; 結果をコンソールに出力する。返り値はnilとなる
;;; stream
;;; データを出力ストリームに書き出す。

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))
;; "!erom srallod 05.1 ylno rof sgnir noino ddA"



;;; 制御文字列
;;; テキストの成形方法
;;; 原則として、制御文字列中のテキストはそのまま出力される。制御シーケンスは出力形式に影響を与える。
;;; 制御シーケンスは常に~で始まる。



;;; 値引数
;;; 整形され表示されるべきデータ。
;;; 制御文字列に従って解釈されて整形される。


;;; 制御シーケンス~s, ~aを使えば、prin1とprincの出力と同じものをformatの出力中に埋め込むことができる。
;;; ~sはprin1とおあん軸、Lispが後から読み込めるような区切り文字が入った形式を出力する。
;;; ~aはprincと同じく、人間が読みやすい形式。
(format t "I am printing ~s in the middle of this sentence." "foo")
;; I am printing "foo" in the middle of this sentence.

(format t "I am printing ~a in the middle of this sentence." "foo")
;; I am printing foo in the middle of this sentence.



;;; ~10a
;;; 値の部分が10文字になるように、fooの右に7個の空白が追加される
(format t "I am printing ~10a within ten spaces of room." "foo")
;; I am printing foo        within ten spaces of room.

;;; 値の左側に空白を含める場合、@を加える
(format t "I am printing ~10@a within ten spaces of room." "foo")
;; I am printing        foo within ten spaces of room.


;;; 制御シーケンスへのパラメータを複数渡す場合は,で区切る
;;; 表示幅を埋める際に、1つではなく3つずつ空白を足すように指示する。空白は全体の表示幅が10文字に等しくなるかそれを超えるまで足される。
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
;; I am printing foo          within ten (or more) spaces of room.


;;; 少なくとも何文字かの空白(パディング文字数の下限)
(format t "I am printing ~,,4a in the middle of this sentence." "foo")
;; I am printing foo     in the middle of this sentence.

;;; 4番目の制御シーケンスパラメータはパディングに使われる文字
(format t "The word ~,,4,'!a feels very important." "foo")
;; The word foo!!!! feels very important.



;;; 整数の整形

;;; 16進数 => ~x
(format t "The number 1000 in hexadecimal is ~x" 1000)
;; The number 1000 in hexadecimal is 3E8


;;; 2進数
(format t "The number 1000 in binary is ~b" 1000)
;; The number 1000 in binary is 1111101000


;;; 10進数
(format t "The number 1000 in decimal is ~d" 1000)
;; The number 1000 in decimal is 1000

;;; ,による桁のグループ化 => :
(format t "Numbers with commas in them are ~:d times better." 1000000)
;; Numbers with commas in them are 1,000,000 times better.


;;; 表示幅の指定
(format t "I am printing ~10d within ten spaces of room" 10000)
;; I am printing      10000 within ten spaces of room
(format t "I am printing ~10,'xd within ten spaces of room" 10000)
;; I am printing xxxxx10000 within ten spaces of room


;;; 浮動小数点数の整形
;;; ~f制御シーケンス
;;; 指定された表示幅(小数点も含める)に収まるように丸められる
(format t "PI can be estimated as ~4f" 3.141593)
;; PI can be estimated as 3.14

;;; ~fの2番目のパラメータは、小数点以下に表示する桁数。
(format t "PI can be estimated as ~,4f" 3.141593)
;; PI can be estimated as 3.1416

;;; Common Lispにはpiという定数がある
(format t "PI can be estimated as ~,4f" pi)
;; PI can be estimated as 3.1416

;;; ~f制御シーケンスの3番目のパラメータは表示する数を10の指数倍するためのその指数
;; 2を渡すと表示する数は10の2乗倍され、%表示になる
(format t "Percentages are ~,,2f percent better than fractions" 0.77)
;; Percentages are 77.0 percent better than fractions


;;; 価格を表示するときには~fの代わりに~$を使うと良い
(format t "I wish I had ~$ dollars in my bank account." 1000000.2)
;; I wish I had 1000000.20 dollars in my bank account.



;;; 複数行
;; ~% => 常に改行する(terpri相当)
;; ~& => 必要なときだけ改行(fresh-line相当)


(progn (format t "this is on the line ~%")
       (format t "~%this is on another line"))
;; this is on the line 
;; 
;; this is on another line


(progn (format t "this is on one line~&")
       (format t "~&this is on another line"))
;; this is on one line
;; this is on another line


;;; 余分な~%があると空行が増えて出力が醜くなるが、同じ場所に~&を使えばそれを避けられる。
;;; これらのシーケンスは改行数を指定するパラメータを1つ取る。
(format t "this will print ~5%on two lines spread far apart")
;; this will print 
;;
;;
;;
;;
;; on two lines spread far apart



;;; テキストを揃える
(defun random-animal ()
  (nth (random 5) '(dog tick tiger walrus kangaroo)))

;;; 表を作るには~t制御シーケンスが便利
;;; パラメータは整形後のテキストが現れるべきカラム位置

;;; 5, 15, 25カラム目から3列で表示
(loop repeat 10
   do (format t "~5t~a ~15t~a ~25t~a~%"
              (random-animal)
              (random-animal)
              (random-animal)))
;;      TICK      KANGAROO  TICK
;;      TIGER     WALRUS    WALRUS
;;      KANGAROO  TIGER     DOG
;;      KANGAROO  TICK      TIGER
;;      TICK      TICK      WALRUS
;;      TIGER     TIGER     DOG
;;      TICK      KANGAROO  WALRUS
;;      KANGAROO  TICK      TIGER
;;      TICK      WALRUS    DOG
;;      KANGAROO  TIGER     DOG



;;; 1行の中で動物名がなるべく等しい距離を取って表示されるように
;;; ~< と ~> 制御シーケンスを使う。
(loop repeat 10
     do (format t "~30<~a~;~a~;~a~>~%"
                (random-animal)
                (random-animal)
                (random-animal)))
;; KANGAROO     WALRUS      TIGER
;; KANGAROO    TIGER     KANGAROO
;; WALRUS       TIGER        TICK
;; TIGER         DOG          DOG
;; TIGER    KANGAROO     KANGAROO
;; TICK         TICK         TICK
;; TIGER       TICK        WALRUS
;; TICK       DOG        KANGAROO
;; DOG       WALRUS        WALRUS
;; KANGAROO      TIGER       TICK

;; ~30< がテキスト揃えのブロックの開始を示す
;; パラメータの30はブロックが30も自分の幅を持つことを指定する
;; ~aが3つ並ぶ => 動物を3列に並べる。
;; それぞれの~aは~;で区切られていて、これは~<による字寄せの対象となる新たな値が次に来ることを示す。
;; (~;は値を適切に字寄せするための空白文字の挿入を指示する)
;; 字寄せの対象となる範囲の終了は~>制御シーケンスによって指示される。

;;; ~<制御シーケンスに:@フラグを与えると、値をセンタリングしてくれる。

(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))
;;              TICK             
;;             TIGER             
;;             WALRUS            
;;            KANGAROO           
;;              DOG              
;;             WALRUS            
;;              DOG              
;;              TICK             
;;              DOG              
;;            KANGAROO           


;;; 複数列の場合に:@を使うと、行の左右に空白を付け足して行全体をセンタリングしてくれる。
(loop repeat 10
     do (format t "~30:@<~a~;~a~;~a~;~>~%"
                (random-animal)
                (random-animal)
                (random-animal)))
;;   TIGER  KANGAROO  WALRUS     
;;   WALRUS   WALRUS   TICK      
;;    TICK   TICK   TIGER        
;;  KANGAROO KANGAROO  WALRUS    
;;   TIGER  TIGER  KANGAROO      
;;   TIGER  TIGER  KANGAROO      
;;   TIGER   WALRUS   TIGER      
;;   TICK   KANGAROO   TICK      
;;   TIGER   TIGER   WALRUS      
;;    TIGER   DOG   WALRUS       


;;; キレイに揃った3列組を実現するには、列ごとに独立して、10文字ずつのセンタリングを指定する

(loop repeat 10
     do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                (random-animal)
                (random-animal)
                (random-animal)))
;;    TICK     TIGER      TICK   
;;    DOG       DOG       DOG    
;;  KANGAROO    TICK     WALRUS  
;;  KANGAROO   WALRUS     DOG    
;;   TIGER      DOG      TIGER   
;;    TICK     WALRUS     DOG    
;;  KANGAROO    TICK     TIGER   
;;    TICK     WALRUS     DOG    
;;    DOG       TICK      DOG    
;;  KANGAROO   TIGER      TICK   



;;; 繰り返し
;;; formatでループを実現するのは~{~}の制御シーケンス
;;; このリストを与えると、formatはリスト中のデータをループで処理する。
;;; 与えられたリストの要素数分、その制御文字列が繰り返し使われる。

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animals*)
;; I see a KANGAROO! I see a KANGAROO! I see a TIGER! I see a DOG! I see a KANGAROO! I see a WALRUS! I see a WALRUS! I see a KANGAROO! I see a TIGER! I see a TIGER! 


;;; ループ1回につき複数の値をリストから取ることもできる
;;; ループ本体の中に2つの~a制御シーケンスが含まれる。
;;; 各~aにつき1匹の動物がリストから使われるので、ループ1回につき2匹ずつの動物が表示される。
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)
;; I see a KANGAROO... or was it a KANGAROO?
;; I see a TIGER... or was it a DOG?
;; I see a KANGAROO... or was it a WALRUS?
;; I see a WALRUS... or was it a KANGAROO?
;; I see a TIGER... or was it a TIGER?




