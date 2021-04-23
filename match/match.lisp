; add 1 for each element in list
(defun len (list)
    (cond
        ; null list; return 0
        ((null list) 0)

        ; if list has 1 element return 1
        ((null (cdr list)) 1)

        ; otherwise return 1 + length(rest of list)
        (t (+ 1 (len(cdr list)))))) 


; T if x is in list
(defun xInList (x list) 
  (cond
    ; if list null return 
    ((null list) nil) 

    ; if list[0] == x return T
    ((equal (car list) x) T) 

    ; if not, recurse on rest of list
    (t (member x (cdr list)))
  )
) 

; return true if lists are equal excluding ?
(defun questionMark (pattern assertion)
  (cond
    ; if both lists null; return true
    ((and (null pattern) (null assertion)) T)

    ; if first element of pattern is ?, continue onto next elements
    ((equal (car pattern) '?)  (questionMark (cdr pattern) (cdr assertion)))

    ; if the first element of pattern = the first of assertion continue
    ((equal (car pattern) (car assertion))  (questionMark (cdr pattern) (cdr assertion)))

    ; otherwise it doesn't match, return false
    (t nil)
  )
)

; return true if lists are equal excluding ?
(defun exclamationMark (pattern assertion)
  (cond
    ; if both lists null; return true
    ((and (null pattern) (null assertion)) T)

    ; if the first element of pattern = the first of assertion continue
    ((equal (car pattern) (car assertion))  (exclamationMark (cdr pattern) (cdr assertion)))


    ; if first element of pattern is !
    ((equal (car pattern) '!)  
      (cond
        ; if the next two elements are equal; continue to the next two
        ((equal (car (cdr pattern)) (car (cdr assertion)))  (exclamationMark (cdr pattern) (cdr assertion)))

        ; if on the last element of assertion; proceed on cdr of both
        ((null (cdr assertion)) (exclamationMark (cdr pattern) (cdr assertion)))

        ; otherwise continue on the same chunk of pattern, but progress on assertion
        (t (exclamationMark pattern (cdr assertion)))   
      )
    )

    ; if next item is ?


    ; otherwise it doesn't match, return false
    (t nil)
  )
)



(defun match(pattern assertion)
  (cond 
    ; if they are exact matches; return true
    ((equal pattern assertion) t)

    ; if ! in pattern...
    ((xInList '! pattern)  (exclamationMark pattern assertion))

    ; if ? in pattern...
    ((xInList '? pattern)  (questionMark pattern assertion))

    ; otherwise they don't match
    (t nil)
  )
)
