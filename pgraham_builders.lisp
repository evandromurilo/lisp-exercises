(defun compose (&rest fs)
  (destructuring-bind (fn1 . rest) (reverse fs)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  (if (null nfs)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null nfs)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(defun curry (f &rest args)
  #'(lambda (&rest other-args)
      (apply f (append args other-args))))

(defun rcurry (f &rest args)
  #'(lambda (&rest other-args)
      (apply f (append other-args args))))
