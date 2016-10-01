(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the cauch.))
                        (garden (you are in a beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

(assoc 'garden *nodes*)


(defun describe-location (location nodes)
  "場所を描写する"
  (cadr (assoc location nodes)))

(describe-location 'garden *nodes*)


(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  "必要なエッジの名前からテキストによる説明を得る"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))


(defun describe-paths (location edges)
  "与えられた場所から出ている全てのエッジを探し出して描写する"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(cdr (assoc 'living-room *edges*))
(mapcar #'describe-path '((garden west door) (attic upstairs ladder)))



(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))
(objects-at 'living-room *objects* *object-locations*)



(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
(describe-objects 'living-room *objects* *object-locations*)


(defparameter *location* 'living-room)
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
(look)

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way))))


(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(pickup 'whiskey)


(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
(inventory)
