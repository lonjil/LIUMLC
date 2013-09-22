;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Yadril 2: The Not So Great Remake: Revenge of Common Decency
;;;; Copyright Birk Hirdman (AKA lonjil, AKA bik1230) 2013
;;;;
;;;; TODO:
;;;;	Add stuff to this pre-code comment
;;;;	Implement features
;;;;	Write clean code
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defclass* in part copyright Xerox Corporation.
;;;; Copyright statement reproduced below:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Debugger symbols
(defvar *swank-debug* nil)
(defvar *swank-port* 4006)

;;; Library loading
#-quicklisp (error "Got quicklisp?")
#+quicklisp (progn #-cl-charms (ql:quickload :cl-charms) 
		   #-swank (when *swank-debug* (ql:quickload :swank) (swank:create-server :port *swank-port*)))


;;; Package defining
#-(or yadril yadril2)
(progn
  (defpackage :yadril2
    (:nicknames :yadril)
    (:documentation "Contains yadril code 'n stuff")
    (:use :common-lisp :cl-charms))
  (push 'yadril cl:*features*)
  (push 'yadril2 cl:*features*))

;;; Variable defining
(defvar *height* 10)
(defvar *width* 10)
(defvar *dlvl* 1)

;;; Dumb shit
(defmacro defclass* (&environment env name direct-superclasses direct-slots &rest options)
  (let (sb-pcl::*initfunctions-for-this-defclass*
        sb-pcl::*readers-for-this-defclass* ;Truly a crock, but we got
        sb-pcl::*writers-for-this-defclass* ;to have it to live nicely.
        sb-pcl::*slot-names-for-this-defclass*)
    ;; FIXME: It would be nice to collect all errors from the
    ;; expansion of a defclass and signal them in a single go.
    (multiple-value-bind (metaclass canonical-options)
        (sb-pcl::canonize-defclass-options name options)
      (let ((canonical-slots (sb-pcl::canonize-defclass-slots name direct-slots env))
	    ;; DEFSTRUCT-P should be true if the class is defined
	    ;; with a metaclass STRUCTURE-CLASS, so that a DEFSTRUCT
	    ;; is compiled for the class.
	    (defstruct-p (and (eq sb-pcl::**boot-state** 'complete)
			      (let ((mclass (find-class metaclass nil)))
				(and mclass
				     (sb-pcl::*subtypep
				      mclass
				      sb-pcl::*the-class-structure-class*))))))
	(let* ((defclass-form
		 `(let ,(mapcar #'cdr sb-pcl::*initfunctions-for-this-defclass*)
		    (sb-pcl::load-defclass ,name
					   ',metaclass
					   ,direct-superclasses
					   (list ,@canonical-slots)
					   (list ,@(apply #'append
							  (when defstruct-p
							    '(:from-defclass-p t))
							  canonical-options))
					   ',sb-pcl::*readers-for-this-defclass*
					   ',sb-pcl::*writers-for-this-defclass*
					   ',sb-pcl::*slot-names-for-this-defclass*
					   (sb-c:source-location)
					   ',(sb-pcl::safe-code-p env)))))
	  (if defstruct-p
	      (progn
		;; FIXME: (YUK!) Why do we do this? Because in order
		;; to make the defstruct form, we need to know what
		;; the accessors for the slots are, so we need already
		;; to have hooked into the CLOS machinery.
		;;
		;; There may be a better way to do this: it would
		;; involve knowing enough about PCL to ask "what will
		;; my slot names and accessors be"; failing this, we
		;; currently just evaluate the whole kaboodle, and
		;; then use CLASS-DIRECT-SLOTS. -- CSR, 2002-06-07
		(eval defclass-form)
		(let* ((include (or (and direct-superclasses
					 (find-class (car direct-superclasses) nil))
				    (and (not (eq name 'structure-object))
					 sb-pcl::*the-class-structure-object*)))
		       (defstruct-form (sb-pcl::make-structure-class-defstruct-form
					name (sb-pcl::class-direct-slots (find-class name))
					include)))
		  `(progn
		     (eval-when (:compile-toplevel :load-toplevel :execute)
		       ,defstruct-form) ; really compile the defstruct-form
		     (eval-when (:compile-toplevel :load-toplevel :execute)
		       ,defclass-form))))
            `(progn
               ;; By telling the type system at compile time about
               ;; the existence of a class named NAME, we can avoid
               ;; various bogus warnings about "type isn't defined yet"
               ;; for code elsewhere in the same file which uses
               ;; the name of the type.
               ;;
               ;; We only need to do this at compile time, because
               ;; at load and execute time we write the actual
               ;; full-blown class, so the "a class of this name is
               ;; coming" note we write here would be irrelevant.
               (eval-when (:compile-toplevel)
                 (%compiler-defclass ,name
                                     ',sb-pcl::*readers-for-this-defclass*
                                     ',sb-pcl::*writers-for-this-defclass*
                                     ',sb-pcl::*slot-names-for-this-defclass*))
               (eval-when (:load-toplevel :execute)
                 ,defclass-form))))))))

;;; Other stuff
(defun gen-floor (&optional (dlvl *dlvl*))
  (if level
      (let ((world (make-hash-table :test 'equal)))
	(loop for x to *height*
	      do (loop for y to *width*
		       do (setf (gethash (cons x y) world) 'floor)))
	(setf (gethash (cons (random *height*) (random *width*)) 
		       world)
	      'down)
	(setf (gethash (cons (random *height*) (random *width*)) 
		       world)
	      'up))))
(defun cat-sym/string (&rest blargh)
  (intern (apply #'concatenate 'string (loop for x in blargh collect (string-upcase (string x))))))

(defclass monster ()
  ((name		:accessor name
			:initform "I've been a bad programmer"
			:initarg :name)
   (hp		        :accessor hp
			:initform 10
			:initarg :hp)
   (str                 :accessor str
			:initform 10
			:initarg :str)
   (mag                 :accessor mag
		        :initform 10
		        :initarg :mag)
   (x			:accessor x
			:initarg :x)
   (y			:accessor y
			:initarg :y)
   (dlvl		:accessor dlvl
			:initform *dlvl*
			:initarg :dlvl)))
(defclass human (monster)
  ())
(defclass ooze (monster)
  ((hp  :initform 4)
   (str :initform 3)
   (mag :initform 1)))

(defun gen-monsters ()
  (list (find-class 'human)
	(find-class 'ooze)))
(defun gen-items ()
  ())

(defclass dlvl ()
  ((floor		:accessor	lvl-floor
			:initform	(gen-floor)
			:initarg	:floor)
   (entities	        :accessor	entites
			:initform	(make-hash-table)
			:initarg	:entities)))
(defclass world ()
  ((levels	:accessor	levels
		:initform	(make-hash-table)
		:initarg 	:levels)
   (monsters	:accessor	monsters
		:initform	(gen-monsters)
		:initarg	:monsters)
   (entities    :accessor       entities
		:initform       (make-hash-table)
		:initarg        :entities
   (items	:accessor	items
		:initform	(gen-items)
		:initarg 	:items)
   (misc	:accessor       misc
		:initform       nil
		:initarg        misc)))


(defun get-level (&optional (lvl *dlvl*) (world world))
  (gethash lvl (levels world)))
(defmethod get-monster ((obj symbol) &optional (world *world))
  (gethash obj (entities *world*)))
(defmethod get-coord-of ((obj monster))
  (list :x (x obj) :y (y obj) :dlvl (dlvl obj)))
(defmethod get-coord-of ((obj symbol))
  (get-coord-of (get-monster obj)))

(defun draw-map (&optional (y1 2) (x1 0))
  (loop for y to *height* and y-curs from y1 to (+ *height* y1 -1)
	do (loop for x to *width* and x-curs from x1 to (+ *width* x1 -1)
		 do (move y-curs x-curs)
		 (printw (string
			  (cond ((equal `(,x . ,y) (get-coord 'player)) #\@)
				((equal `(,x . ,y) (get-coord 'mob)) #\M)
				((equal (cdr (assoc (cons x y) *level* :test #'equal)) 'up) #\<)
				((equal (cdr (assoc (cons x y) *level* :test #'equal)) 'down) #\>)
				((equal (cdr (assoc `(,x . ,y) *level* :test #'equal)) 'floor)  #\.)
				(t #\Space)))))))
