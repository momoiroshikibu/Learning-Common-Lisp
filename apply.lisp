;; apply: 関数とリストを渡すと、あたかもそのリストの各要素を引数として関数を呼び出したかのように動作する

(apply #'append '((mary had) (a) (little lamb)))

(apply #'append '((THERE IS A DOOR GOING WEST FROM HERE.)
                  (THERE IS A LADDER GOING UPSTAIRS FROM HERE.)))
