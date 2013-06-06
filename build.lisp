(declaim (optimize (debug 0) (safety 0) (space 3) (speed 0)))

(load #P"rogue.lisp")
(setf *version* (write-to-string (get-universal-time)))
#+sbcl (sb-ext:save-lisp-and-die (concatenate 'string "yadril-" *version* ".exe")
									:toplevel #'main
									:executable t
									:purify t)
#+sbcl (quit)
#+clisp (EXT:SAVEINITMEM (concatenate 'string "yadril-" (write-to-string (get-universal-time)) ".exe")
							:QUIET t
							:INIT-FUNCTION 'main
							:EXECUTABLE t
							:NORC t)
#+clisp (exit)
(format t "I don't have settings for your lisp implementation")