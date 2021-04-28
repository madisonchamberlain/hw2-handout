(defun flatten (xs)
  ; if no list return null
  (if (null xs) nil
    ; if the first item is an atom; flatten the rest 
    (if (atom (car xs))
      (cons (car xs) (flatten (cdr xs)))
    ; if first item is a list; flatten first item and last item 
    (append (flatten (car xs)) (flatten (cdr xs)))
    )
  )
)

(defun anyTrue (xs)
  (cond
    ; if first item is T return T
    ((not (null (car xs))) T)

    ; if the list is empty return nil
    ((null xs) nil)

    ; otherwise, recurse on the rest of the list
    (T (anyTrue (cdr xs)))
  )
)

(defun helper (transition start final input)
  (cond
    ; if no input; start must = final for true
    ((and (null input)  (equal start final))  T)

    ; if no input and not equal return nil
    ((and (null input)  (not (equal start final))) nil)

    ; otherwise pass to nfa function with start state in list form 
    (T
      ; get next states with funcall on the transition 
      (setq states (funcall transition start (car input)))
      ; if all answers are nil return nil else return T
      (setq answer (mapcar #'(lambda (state) (helper transition state final (cdr input))) states))
    )
  )
)


(defun reachable (transition start final input)
  (cond
    ; if no input; start must = final for true
    ((and (null input)  (equal start final))  T)

    ; if no input and not equal return nil
    ((and (null input)  (not (equal start final))) nil)

    ; otherwise pass to nfa function with start state in list form 
    (T
      ;call helper 
      (setq answer (helper transition start final input))
      (anyTrue (flatten answer))
    )
  )
)