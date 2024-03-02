(defun binary-search (elt arr)
  (finder elt arr 0 (- (length arr) 1)))

(defun average (a b)
  (/ (+ a b) 2))

(defun finder (elt arr lo hi)
  (if (equal lo hi)
      (if (equal (svref arr lo) elt)
          elt
          nil)
      (let* ((mid (floor (average lo hi)))
             (felt (svref arr mid)))
        (if (> elt felt)
            (finder elt arr (+ 1 mid) hi)
            (if (< elt felt)
                (finder elt arr lo (- mid 1))
                elt)))))
