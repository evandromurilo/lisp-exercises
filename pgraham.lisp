;; 1.7 Using only operators introduced in this chapter, define a function that takes a list as an argument and returns true if one of its elements is a list.

(defun has-listp (lst)
           (if (null lst) nil
               (if (listp (car lst)) t
                   (has-listp (cdr lst)))))

;; 1.8 Give iterative and recursive definitions of a function that (a) takes a positive integer and prints that many dots. (b) takes a list and returns the number of times the symbol A occurs in it.

(defun rec-dots (n)
  (if (> n 0)
      (progn
        (format t ".")
        (rec-dots (- n 1)))))

(defun it-dots (n)
  (do ((i n (- i 1)))
      ((zerop i) 'DONE)
    (format t ".")))

(defun rec-count-a (lst)
  (cond ((null lst) 0)
        ((equal (car lst) 'a)
         (+ 1 (rec-count-a (cdr lst))))
        (t (rec-count-a (cdr lst)))))

(defun it-count-a (lst)
  (let ((cnt 0))
    (dolist (obj lst)
      (if (equal 'a obj)
          (setf cnt (+ cnt 1))))
    cnt))
