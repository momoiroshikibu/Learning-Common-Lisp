(defparameter *house* '((walls
                         (mortar
                          (cement)
                          (water)
                          (sand))
                         (bricks))
                        (windows
                         (glass)
                         (frame)
                         (curtains))
                        (roof
                         (shingles)
                         (chimney))))

(defparameter *wizard-nodes* '((living-room
                                (you are in the living-room. a wizard is snoring loudly on the couch.))
                               (garden
                                (you are in a beautiful garden. there is a well in front of you.))
                               (attic
                                (you are in the attic. there is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room
                                (garden west door)
                                (attic upstairs ladder))
                               (garden
                                (living-room east door))
                               (attic
                                (living-room downstairs ladder))))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(dot-name 'living-room)
