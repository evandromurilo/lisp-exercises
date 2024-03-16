(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(setf mona #(31 28 31 30 31 30 31 31 30 31 30 31))

;; do version
(do* ((i 0 (+ i 1))
      (n (aref mona i) (aref mona i))
      (acc n (+ acc n))
      (lst (list n) (cons acc lst)))
     ((= i 11) (reverse lst)))

;; dolist version
(let ((lst nil)
      (acc 0))
  (dolist (n mon)
    (incf acc n)
    (push acc lst))
  (reverse lst))

;; recursive version
(defun seq-sum (lst acc)
  (if (null lst)
      nil
      (cons (+ (car lst) acc)
            (seq-sum (cdr lst) (+ (car lst) acc)))))

(seq-sum mon 0)

;; maplist version
(reverse (maplist #'(lambda (x)
                      (apply #'+ x))
                  (reverse mon)))

(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defun date->int (y m d)
  (+ (year-num y)
     (month-num m)
     d))

(defun year-num (y)
  (* (- y 2000) 365))

(defun month-num (m)
  (aref month (- m 1)))

(defun int->date (n)
  (multiple-value-bind (yquotient yrem) (floor n 365)
    (multiple-value-bind (m d) (num-month yrem)
      (list (+ 2000 yquotient) m d))))

(defun num-month (n)
  (do* ((mon 1 (+ mon 1)))
       ((> (aref month mon) n)
        (values mon (- n (aref month (- mon 1)))))))



  
