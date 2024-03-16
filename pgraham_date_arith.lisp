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

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (y m d)
  (+ (year-num y)
     (month-num m y)
     d))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y) 366 365))

(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values y m d))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t        (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun date+ (y m d n)
  (num->date (+ (date->num y m d) n)))
    




  
