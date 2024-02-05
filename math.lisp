(defpackage :math
  (:use :cl))

(in-package :math)

(defclass fraction ()
  ((numerator :initarg :numerator
              :reader fraction-numerator)
   (denominator :initarg :denominator
                :reader fraction-denominator))
  (:documentation "Uma fração simples com numerador e denominador."))

(defclass mixed-fraction (fraction)
  ((whole :initarg :whole :reader fraction-whole))
  (:documentation "Uma fração mista com um número inteiro, numerador e denominador."))

(defun make-fraction (numerator denominator)
  (make-instance 'fraction :numerator numerator :denominator denominator))

(defun make-mixed-fraction (whole numerator denominator)
  (make-instance 'mixed-fraction :whole whole :numerator numerator :denominator denominator))

(defgeneric quotient (fraction)
  (:documentation "Retorna o quociente calculado da fração."))

(defmethod quotient ((f fraction))
  (/ (fraction-numerator f) (fraction-denominator f)))

(defmethod quotient ((f mixed-fraction))
  (/ (* (fraction-whole f) (fraction-numerator f)) (fraction-denominator f)))

(defun factorp (a b)
  "Diz se a é um fator de b."
  (zerop (rem b a)))

(defun primep-helper (x n)
  (cond ((< n 2) t)
        (t (and (> (rem x n) 0) (primep-helper x (- n 1))))))

(defun primep (n)
  "Diz se n é um número primo."
  (primep-helper n (- n 1)))

(defun prime-table (n)
  "Retorna uma lista com todos os números primos até n."
  (cond ((zerop n) nil)
        ((primep n) (cons n (prime-table (- n 1))))
        (t (prime-table (- n 1)))))

(defun prime-factors-helper (table n)
  "Table é inicialmente a tabela dos números primos até n, e vai sendo consumida progressivamente."
  (cond ((equal n 1) nil)
        ((factorp (car table) n) ;; encontramos um fator, agora procuramos os demais fatores após dividir n pelo fator
         (cons (car table) (prime-factors-helper table (/ n (car table)))))
        (t (prime-factors-helper (cdr table) n))))

(defgeneric prime-factors (numbers)
  (:documentation "Retorna os fatores primos de um número no formato de lista."))

(defmethod prime-factors ((n integer))
  (prime-factors-helper (prime-table n) n))

(defmethod prime-factors ((f fraction))
  (list (prime-factors (fraction-numerator f)) (prime-factors (fraction-denominator f))))

(defgeneric simplify (f)
  (:documentation "Simplifica uma fração."))

(defmethod simplify ((f fraction))
  "Encontra o máximo divisor comum  entre o numerador e o denominador, e divide ambos por esse fator."
  (let ((gcd (gcd (fraction-numerator f) (fraction-denominator f))))
    (make-fraction (/ (fraction-numerator f) gcd)
                   (/ (fraction-denominator f) gcd))))

(defmethod print-object ((obj fraction) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a / ~a" (fraction-numerator obj) (fraction-denominator obj))))

(defmethod print-object ((obj mixed-fraction) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a / ~a)" (fraction-whole obj) (fraction-numerator obj) (fraction-denominator obj))))

(defgeneric to-mixed-fraction (f)
  (:documentation "Converte uma fração para uma fração mista."))

(defmethod to-mixed-fraction ((f fraction))
  (make-mixed-fraction
   (floor (/ (fraction-numerator f) (fraction-denominator f)))
   (rem (fraction-numerator f) (fraction-denominator f))
   (fraction-denominator f)))

(defmethod to-mixed-fraction ((f mixed-fraction))
  f)

(defgeneric proper-fraction-p (f)
    (:documentation "Diz se uma fração é própria."))

(defmethod proper-fraction-p ((f fraction))
  (not (> (fraction-numerator f) (fraction-denominator f))))

(defgeneric improper-fraction-p (f)
    (:documentation "Diz se uma fração é imprópria."))

(defmethod improper-fraction-p ((f fraction))
  (not (proper-fraction-p f)))

