; add 1 for each element in list
(defun len (list)
  (cond
    ; null list; return 0
    ((null list) 0)

    ; if list has 1 element return 1
    ((null (cdr list)) 1)

    ; otherwise return 1 + length(rest of list)
    (t (+ 1 (len(cdr list))))
  )
) 


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
(defun exclamationMark (pattern assertion)
  (cond
    ; if the next item of each are the same proceed on those
    ((equal (car (cdr pattern))  (car (cdr assertion)))  (patternMatch (cdr pattern) (cdr assertion)) )

    ; if the next item of pattern is ?, proceed on both
    ((equal (car (cdr pattern))  '?)  (patternMatch (cdr pattern) (cdr assertion)) )

    ; if on the last element of assertion; proceed on cdr of both
    ((null (cdr assertion)) (patternMatch (cdr pattern) (cdr assertion)))

    ; otherwise continue on the same chunk of pattern, but progress on assertion
    (t (patternMatch pattern (cdr assertion)))   
  )
)


(defun patternMatch (pattern assertion)
  (cond
    ; if both lists null; return true
    ((and (null pattern) (null assertion)) T)

    ; if nothing left in assertion but something left in pattern return nil
    ((and (not (null pattern)) (null assertion)) nil)

    ; if the next element of pattern = the next of assertion continue
    ((equal (car pattern) (car assertion))  (patternMatch (cdr pattern) (cdr assertion)))

    ; if next item of pattern is ! go to exmark
    ((equal (car pattern) '!) (exclamationMark pattern assertion))

    ; if next item is ?, continue with queston mark one
    ((equal (car pattern) '?)  (patternMatch (cdr pattern) (cdr assertion)))

    ; otherwise it doesn't match, return false
    (t nil)
  )
)



(defun match(pattern assertion)
  (cond 
    ; if they are exact matches; return true
    ((equal pattern assertion) t)

    ; if ! or ? in pattern see if they match with patternMatch
    ((or (xInList '! pattern) (xInList '? pattern)) (patternMatch pattern assertion))

    ; otherwise they don't match
    (t nil)
  )
)
