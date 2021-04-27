(defun reachable (transition start final input)
  (cond
    ; if no input; start must = final for true
    ((and (null input)  (equal start final))  T)

    ; if no input and not equal return nil
    ((and (null input)  (not (equal start final))) nil)

    ; otherwise pass to nfa function with start state in list form 
    (T
      ; get next states with funcall on the transition 
      (setq states (funcall transition start (car input)))
      (car(mapcar #'(lambda (state) (reachable transition state final (cdr input))) states))
    )
  )
)