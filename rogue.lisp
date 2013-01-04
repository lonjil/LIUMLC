(defparameter *hight* 10)
(defparameter *width* 10)
(defparameter *entities* nil)
(setq *world* (progn
		(let ((world nil))
		  (loop for x to *hight*
			do (loop for y to *width*
				 do (setq world (acons (list x y) 'floor world))))
		  world)))
(defstruct entity
  (hp 10)
  (name "Casimir")
  (x nil)
  (y nil)
  (type 'mob)
  (strength 10)
  (dexterity 10)
  (consitution 10)
  (intelligence 10)
  (wisdom 10)
  (charisma 10)
  (luck 10)
  (easter-egg -10)
  (magic 0))
(defun register-entity (entity &body keywords)
  (setf *entities* (acons entity `(,make-entity ,@keybords) *entities*)))
(defun get-entity-val (entity val)
  (funcall val (cdr (assoc *entities* entity))))
(defun player-gen (&optional (name "Casimir")) (make-entity :hp 20 :magic 16 :type 'player :x (random *hight*) :y (random *width*) :name name))
(defun create-entites ()
  (setf player (player-gen))
  (setf mob (make-entity)))
(defun draw-map (entities world)
  (loop for x in *hight*
	do (loop for y in *width*
		 do (print (cond (()))))))
(defun set-coord (entity coord)
  (setf (entity-x entity) (car coord))
  (setf (entity-y entity) (cdr coord)))
(defun set-coord-relative (entity dir &optional (x 1))
  (case dir 
	('up (decf (entity-y entity) x))
	('down (incf (entity-y entity) x))
	('left (decf (entity-x entity) x))
	('right (incf (entity-x entity) x))))
(defun walk (entity dir &optional (x 1))
  (cond ((<= (entity-dexterity entity) x)
	 (set-coord-relative entity dir x))))
(defun rule (move gamestate) ())
(defun next-state (current-state player-move rules) ())