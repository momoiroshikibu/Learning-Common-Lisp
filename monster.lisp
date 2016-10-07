(defstruct monster (health (random 10))) ; 既定値

(make-monster)
;; #S(MONSTER :HEALTH 6)

(make-monster)
;; #S(MONSTER :HEALTH 9)

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "| "))
      (progn (princ "You hit the ")
             (princ (type-of m))
             (princ ", knocking off ")
             (princ x)
             (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (typeof m)))

(defmethod monster-attack (m))


(defstruct (orc (:include monster)) (club-level (random 8))
;(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club")

(defmethod monster-attack ((m orc))
  (let ((x (random (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


(defstruct (hydra (:include monster)))
(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (random (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head!")
    (incf (monster-health m))
    (decf *player-health* x)))



(defstruct (slime-mold (:include monster)) (slimeness (random 5)))
(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a slimeness of ")
  (princ (slime-mold-slimeness m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (random (slime-mold-slimeness m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point!")
      (decf *player-health*))))

