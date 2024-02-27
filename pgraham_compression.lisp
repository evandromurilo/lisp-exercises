(defun compress (lst)
  "Compress a list of atoms grouping repeating sequences: (a a a b c c) becomes ((3 a) b (2 c))."
  (if (consp lst)
      (compr (cdr lst) (car lst) 1)
      lst))

(defun compr (lst last n)
  "Do the hard work for compress, should not be called isolated."
  (cond ((null lst)
         (list (n-elts last n))) ; end of list, compress last atom
        ((equal (first lst) last)
         (compr (rest lst) last (+ 1 n))) ; same as previous atom, keep going
        (t
         (cons (n-elts last n)
               (compr (rest lst) (first lst) 1))))) ; new atom, compress previous atom and begin compressing the new one
         

(defun n-elts (at n)
  "Return representation for atom at repeated n times."
  (cond ((equal 1 n) at)
        (t (list n at))))
