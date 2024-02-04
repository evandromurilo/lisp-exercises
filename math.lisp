(defclass fraction ()
  ((numerator :initarg :numerator :reader fraction-numerator)
   (denominator :initarg :denominator :reader fraction-denominator)))

(defclass mixed-fraction (fraction)
  ((whole :initarg :whole :reader fraction-whole)))

(defun make-fraction (numerator denominator)
  (make-instance 'fraction :numerator numerator :denominator denominator))

(defun make-mixed-fraction (whole numerator denominator)
  (make-instance 'mixed-fraction :whole whole :numerator numerator :denominator denominator))

(defgeneric quotient (fraction))

(defmethod quotient ((f fraction))
  (/ (fraction-numerator f) (fraction-denominator f)))

(defmethod quotient ((f mixed-fraction))
  (/ (* (fraction-whole f) (fraction-numerator f)) (fraction-denominator f)))

(defgeneric prime-factors (numbers))

(defmethod prime-factors ((n integer))
  (prime-factors-helper (prime-table n) n))

(defmethod prime-factors ((f fraction))
  (list (prime-factors (fraction-numerator f)) (prime-factors (fraction-denominator f))))

(defgeneric simplify (f))

(defmethod simplify ((f fraction))
  (let ((gcd (gcd (fraction-numerator f) (fraction-denominator f))))
    (make-fraction (/ (fraction-numerator f) gcd)
                   (/ (fraction-denominator f) gcd))))

(defun prime-factors-helper (table n)
  (cond ((equal n 1) nil)
        ((factorp (car table) n) (cons (car table) (prime-factors-helper table (/ n (car table)))))
        (t (prime-factors-helper (cdr table) n))))

(defun factorp (a b)
  (zerop (rem b a)))

(defun primep (n)
  (primep-helper n (- n 1)))

(defun primep-helper (x n)
  (cond ((< n 2) t)
        (t (and (> (rem x n) 0) (primep-helper x (- n 1))))))

(defun prime-table (n)
  (cond ((zerop n) nil)
        ((primep n) (cons n (prime-table (- n 1))))
        (t (prime-table (- n 1)))))

;; to-mixed-fraction
;; proper-fraction-p
;; improper-fraction-p
