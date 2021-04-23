
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


(defun quicksort (xs)
  (cond 
    ; if no list return null
    ((null xs) nil)

    ; otherwise recursively quicksort
    (t
      ; set n to the first element of xs
      (setq n (car xs))
      
      ; set pivotAnswer to the result from pivot 
      (setq pivotAnswer (pivot n xs))

      ; set lower to the first list returned 
      (setq lower (car pivotAnswer))
      
      ; set upper to the second list returned 
      (setq upper (car (cdr pivotAnswer)))
      (append 
        ; quicksort on the first element of pivots return 
        (quicksort lower) 
        ; list containing first element of list and nothing
        (cons n nil) 
        ; quicksort on the second element revurned by call to pivot
        (quicksort upper) 
      )
    )
  )
)
