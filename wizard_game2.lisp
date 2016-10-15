(load 'wizard_game.lisp)

(defun have (object)
  (member object (cdr (inventory))))


(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))

(weld 'chain 'bucket)
;; (YOU CANNOT WELD LIKE THAT.)


(pushnew 'weld *allow-commands*)
;; (WELD LOOK WALK PICKUP INVENTORY)


