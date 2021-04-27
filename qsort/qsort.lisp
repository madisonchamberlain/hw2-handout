; add all list elements
(defun sum (list)
  (reduce '+ list))  

; add 1 for each element in list
(defun len (list)
    (cond
        ; null list; return 0
        ((null list) 0)

        ; if list has 1 element return 1
        ((null (cdr list)) 1)

        ; otherwise return 1 + length(rest of list)
        (t (+ 1 (len(cdr list)))))) 

(defun mean (list)
  (/ (sum list) (len list))
)


(defun generate_pivot(n list less more)
  (cond
    ; if list null return the less and more lists
    ((null list) (list less more))

    ; if first element < n append to less list and continue on cdr
    ((< (car list) n)
      (generate_pivot n (cdr list) (append less (list (car list))) more)
    )

    ; otherwise append first element to greater list and continue on cdr 
    (t
      (generate_pivot n (cdr list) less (append more (list (car list))))
    )
  )
)

(defun pivot (n xs)
  (generate_pivot n xs nil nil)
)

(defun ordered (xs)
  (cond
    ; if list is null it is ordered; return T
    ((null xs) T)
  
    ; if the list contails 1 element it is ordered; return T
    ((null (cdr xs)) T)

    ; if the next element is less than current element; return nul its not ordered
    ((> (car xs) (car (cdr xs))) nil)

    ; otherwise continue to the next element
    (T (ordered (cdr xs)))
  )
)


(defun quicksort (xs)
  (cond
    ; if list empty return none
    ((null xs) nil)

    ; if list already sorted return list
    ((not (null (ordered xs))) xs)

    ; if list not already sorted split based on pivot (pivot = mean)
    (t
      (append
        ; quicksort on the lower half
        (quicksort (car (pivot (mean xs) xs)))

        ; quicksort on the upper half
        (quicksort (car (cdr (pivot (mean xs) xs))))
      )
    )
  )
)