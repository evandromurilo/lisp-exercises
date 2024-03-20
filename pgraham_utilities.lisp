(defun single? (lst)
  (and (listp lst)
       (car lst)
       (null (cdr lst))))

(defun append1 (lst elt)
  (append lst (list elt)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n (reverse acc))
      (push (funcall fn i) acc))))

(defun filter (predicate lst)
  (let ((acc nil))
    (dolist (elt lst)
      (when (funcall predicate elt)
        (push elt acc)))
    (reverse acc)))

          
(defun most (scorer lst)
  (labels ((most-rec (score elt lst)
             (if (null lst)
                 (values elt score)
                 (let ((new-score (funcall scorer (car lst))))
                   (if (> new-score score)
                       (most-rec new-score (car lst) (cdr lst))
                       (most-rec score elt (cdr lst)))))))
    (most-rec 0 nil lst)))

  
