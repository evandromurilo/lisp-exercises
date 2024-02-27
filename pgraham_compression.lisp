(defun run-length-encode (lst)
  "Compress a list of atoms grouping repeating sequences: (a a a b c c) becomes ((3 a) b (2 c))."
  (if (null lst)
      nil
      (compr (cdr lst) (car lst) 1)))

(defun compr (lst last n)
  "Do the hard work for run-length-encode, should not be called isolated."
  (cond ((null lst)
         (list (compr-atom last n))) ; end of list, compress last atom
        ((equal (first lst) last)
         (compr (rest lst) last (+ 1 n))) ; same as previous atom, keep going
        (t
         (cons (compr-atom last n)
               (compr (rest lst) (first lst) 1))))) ; new atom, compress previous atom and begin compressing the new one
         

(defun compr-atom (at n)
  "Return representation for atom at repeated n times."
  (cond ((equal 1 n) at)
        (t (list n at))))
