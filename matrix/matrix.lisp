(defun row-add (l1 l2)
  ; add each number of each row
  (mapcar #'+ l1 l2)
)

(defun matrix-add (mat1 mat2)
  ; call add on each row of each matrix
  (mapcar #'row-add  mat1 mat2)
)


(defun matrix-transpose (mat)
  (cond
    ; if at row of the matrix; make each first element into a list
    ((null (cdr mat))  (mapcar #'list (car mat)))

    ; otherwise pass each number from the row to be converted to a list
    (t (apply #'mapcar #'list mat))
  )
)

(defun row-multiply (mat1 mat2)
  ; multiply each element of the matricies and add the result
  ; mat1 is a row of mat1; mat2 is the corresponding col of mat2
  (mapcar #'(lambda (mat2_col)(apply '+ (mapcar #'* mat1 mat2_col))) mat2)
)


(defun matrix-multiply (mat1 mat2)
  ; call multiply on each row of mat1 X each col of mat2
  (mapcar #'(lambda (mat1_row) (row-multiply mat1_row (matrix-transpose mat2))) mat1)
)

