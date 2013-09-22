(defun main ()
  (defvar back-point nil)
  (defvar height 10)
  (defvar width 10)
  (ql:quickload 'cl-charms)
  (defstruct mons
    (name 'kkk)
    (xp 1)
    (mana 1)
    (hp 1)
    (agi 1)
    (str 1)
    (x (random 10))
    (y (random 10)))
  (
  (tagbody
   init
   (defvar world (make-hash-table :test 'equal))
   (defvar entities (make-hash-table))
   (loop for x from 1 to width  do
	 (loop for y from 1 to height do
	       (setf (gethash `(,x . ,y) world) 'floor)))
   (setf (gethash 'player entities) (new-mons :name 'player :hp 10 :mana 10 :hp 10))
   (setf (gethash 'mob entities) (new-mons :name 'mob))
   (
   draw
   (loop for key beign the hash-keys
