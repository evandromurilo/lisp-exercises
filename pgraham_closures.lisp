(defun combiner (x)
  (typecase x
    (number #'+)
    (list   #'append)
    (t      #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
         args))

(defun make-adder (n)
  #'(lambda (x)
      (+ x n)))

(let ((counter 0))
  (defun c-reset ()
    (setf counter 0))
  (defun c-stamp ()
    (setf counter (+ counter 1))))

(defun our-complement (f)
  '#(lambda (&rest args)
     (not (apply f args))))
  
