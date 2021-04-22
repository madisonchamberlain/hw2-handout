(defun minimum (list)
    (cond
        ; if list null return nill
        ((null list) nil)

        ; if list has 1 element return element
        ((null (cdr list)) (car list))

        ; if first element smaller than second element continue call without second
        ((< (car list) (car(cdr list)))
            (minimum (cons (car list) (cdr (cdr list)))))

        ; if the second is smaller; call min on the rest of the list 
        (t (minimum (cdr list)))))    

; exactly the same as minimum but with the < sign flipped to >
(defun maximum (list)
    (cond
        ; if list null return nill
        ((null list) nil)

        ; if list has 1 element return element
        ((null (cdr list)) (car list))

        ; if first element LARGER than second element continue call without second
        ((> (car list) (car(cdr list)))
            (maximum (cons (car list) (cdr (cdr list)))))

        ; if the second is smaller; call min on the rest of the list 
        (t (maximum (cdr list)))))    

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

 

(defun min-mean-max (xs)
    ;the mean is the sum/the len
    (list (minimum xs)  (/ (sum xs) (len xs))  (maximum xs))
)
