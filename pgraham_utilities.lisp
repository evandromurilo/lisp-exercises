(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (reverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

          
(defun most (scorer lst)
  (labels ((most-rec (score elt lst)
             (if (null lst)
                 (values elt score)
                 (let ((new-score (funcall scorer (car lst))))
                   (if (> new-score score)
                       (most-rec new-score (car lst) (cdr lst))
                       (most-rec score elt (cdr lst)))))))
    (most-rec 0 nil lst)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))
                
  
