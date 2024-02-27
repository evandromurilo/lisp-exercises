(defun compress (lst)
  "Compress a list of atoms grouping repeating sequences: (a a a b c c) becomes ((3 a) b (2 c))."
  (if (consp lst)
      (compr (cdr lst) (car lst) 1)
      lst))

(defun compr (lst last n)
  "Do the hard work for compress, should not be called isolated."
  (if (null lst)
      (list (n-elts last n)) ; end of list, compress last atom
      (let ((next (car lst)))
        (if (equal next last)
            (compr (cdr lst) last (+ 1 n)) ; same as previous atom, keep going
            (cons (n-elts last n)
                  (compr (cdr lst) next 1)))))) ; new atom, compress previous atom and begin compressing the new one
         
(defun n-elts (at n)
  "Return representation for atom at repeated n times."
  (cond ((equal 1 n) at)
        (t (list n at))))

(defun uncompress (lst)
  "Revert a compressed list to it's original state."
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))
        
(defun list-of (n elt)
  "Repeat elt n times."
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))
      
