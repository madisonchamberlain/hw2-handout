

(defun checkStates (start final)
  (cond
    ; if one of the state options = the final option; return true
    ((eq final start) t)

    ; if not return null 
    (t nil)
  )
)

(defun nfa (transition start final input)
  (cond
    ; if no input; start must = final for true
    ((null input) (car (mapcar #'(lambda (state) (checkStates state final)) start)))

    (T nil
      (setq states (funcall transition start (car input)))
      (car (mapcar #'(lambda (state) (reachable transition (list state) final (cdr input))) states))
    )
  )
)

(defun reachable (transition start final input)
  ;(print (list start))
  ;(setq start (list start))
  (cond
    ; if no input; start must = final for true
    ((and (null input)  (equal start final))  T)

    ; if no input and not equal return nil
    ((and (null input)  (not (equal start final))) nil)

    ; otherwise pass to nfa function with start state in list form 
    (T

      ; get next states with funcall on the transition 
      ;(print "start")
      ;(print start)
      ;(print input)
      ;(print transition)
      (setq states (funcall transition start (car input)))
      ;(print "next state options: ")
      ;(print states)
      (car (mapcar #'(lambda (state) (reachable transition state final (cdr input))) states))
    
    )
  )
)


