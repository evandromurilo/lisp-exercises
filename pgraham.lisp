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

;; Palindrome example from chapter 3

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

;; Nth gratest element example from chapter 3

(defun nthmost (n lst)
  (nth (- n 1)
       (sort lst #'>)))

;; Iterative reverse example from chapter 3

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

;; 3.2 Write a version of union that preserves the order of the elements in the original lists.

(defun new-union (a-list b-list)
  (if (null b-list)
      a-list
      (let ((elt (car b-list))
            (rest (cdr b-list)))
        (if (member elt a-list)
            (new-union a-list rest)
            (new-union (append a-list (list elt)) rest)))))

;; 3.3 Define a function that takes a list and returns a list indicating the number of times each (eql) element appears, sorted from most common element to least common.

(defun occurrences (lst)
  (let ((cnt nil))
    (dolist (elt lst)
      (let ((cnt-elt (assoc elt cnt)))
        (if cnt-elt
            (setf (cdr cnt-elt) (+ 1 (cdr cnt-elt)))
            (push (cons elt 1) cnt))))
    (sort cnt #'> :key #'cdr)))
    

;; 3.5 Suppose the function pos+ takes a list and returns a list of each element plus its position: (post+ '(7 5 1 4)) -> (7 6 3 7). Define this function using (a) recursion, (b) iteration, (c) mapcar.

(defun rec-pos+ (lst)
  (helper-rec-pos+ lst 0))

(defun helper-rec-pos+ (lst n)
  (if (null lst)
      nil
      (cons (+ n (car lst)) (helper-rec-pos+ (cdr lst) (+ 1 n)))))

(defun iter-pos+ (lst)
  (let ((i 0)
        (res nil))
    (dolist (elt lst)
      (push (+ elt i) res)
      (incf i))
    (reverse res)))

(defun map-pos+ (lst)
  (let ((i 0))
    (mapcar #'(lambda (elt)
                (let ((new (+ elt i)))
                  (incf i)
                  new))
            lst)))

;; 3.6 After years of deliberation, a government comission has decided that lists should be represented by using the cdr to point to the first element and the car to point to the rest of the list. Define the government versions of the following functions: (a) cons (b) list (c) length (d) member.

(defun gov-cons (a b)
  (cons b a))

(defun gov-list (lst)
  (let ((new nil))
    (dolist (elt lst)
      (setf new (gov-cons elt new)))
    new))

(defun gov-length (lst)
  (if (null lst)
      0
      (+ 1 (gov-length (car lst)))))

(defun gov-member (elt lst)
  (if (null lst)
      nil
      (if (equal (cdr lst) elt)
          lst
          (gov-member elt (car lst)))))

;; 3.7 Modify the program in Figure 3.6 (compression) to use fewer cons cells.

(defun compress (lst)
  (if (consp lst)
      (compr (cdr lst) (car lst) 1)
      lst))

(defun compr (lst last n)
  (if (null lst)
      (list (n-elts last n))
      (let ((next (car lst)))
        (if (equal next last)
            (compr (cdr lst) last (+ 1 n))
            (cons (n-elts last n)
                  (compr (cdr lst) next 1))))))
         
(defun n-elts (at n)
  (cond ((equal 1 n) at)
        (t (cons n at))))

;; 3.8 Define a function that takes a list and prints it in dot notation:

(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (progn
        (format t "(~S . " (car lst))
        (showdots (cdr lst))
        (format t ")"))))

;; 3.9 Write a program to find the longest finite path through a network represented as in Section 3.15. The network may contain cycles.

(defvar mynet '((a b d)
                (b d c)
                (d f)
                (c e g)
                (f e c)
                (g e)))

(defvar myothernet '((a b j)
                     (b a c)
                     (c d)
                     (d e g)
                     (e)
                     (f d)
                     (g f)
                     (h g)
                     (i h d f)
                     (j i)))

(defun longest-path (start end net)
  (find-path start end net nil))

(defun find-path (start end net tested)
  "Encontra o maior caminho sem repetição entre start e end (quase não acredito que funcionou)."
  (if (equal start end)
      (list start)
      (let ((queue (set-difference (cdr (assoc start net)) tested))) ;; the queue has all the paths from start that have not been tested already
        (let ((paths (mapcar
                      #'(lambda (elt) (find-path elt end net (cons start tested))) ;; we add teh start node to the tested list, so we do not check it again
                      queue)))
          (let ((longest (car (sort paths #'> :key #'length))))
            (if (null longest) ;; there was no path
                nil
                (cons start longest)))))))
              
;; 4.1 Define a function to take a square array (an array whose dimensions are (n n)) and rotate it 90º clockwise: (quarter-turn #2A((a b) (c d)) -> #2A((C A) (D B))

(defun get-pos (n s)
  (let* ((top (- s 1))
        (right (+ top (- s 1)))
        (bottom (+ right (- s 1)))
        (end (+ bottom (- s 1))))
    (cond ((< n top)
           (list 0 n))
          ((< n right)
           (list (- n top) (- s 1)))
          ((< n bottom)
           (list (- s 1) (- bottom n)))
          ((< n end)
           (list (- end n) 0))
          (t
           '(0 0)))))

(defun perimeter (s)
  (+ (* 2 s) (* 2 (- s 2))))

(defun quarter-turn (arr)
  (let* ((new-arr (make-array (array-dimensions arr)))
        (s (first (array-dimensions arr))))
    (dotimes (i (perimeter s))
      (setf (apply #'aref (cons new-arr (get-pos (+ 1 i) s)))
            (apply #'aref (cons arr (get-pos i s)))))
    new-arr))

;; 4.2 Read the description of reduce, then use it to define (a) copy-list (b) reverse (for lists).

(defun my-copy-list (lst)
  (reduce #'cons
          lst
          :initial-value nil
          :from-end t))

(defun my-reverse (lst)
  (reduce #'(lambda (a b) (cons b a))
          lst
          :initial-value nil))

;; 4.3 Define a structure to represent a tree where each node contains some data and has up to three children. Define (a) a function to copy such a tree (so that no node in the copy is eql to a node in the original) (b) a function that takes an object and such a tree, and returns true if the object is eql to the data field of one of the nodes.

(defstruct ttree elt l c r)

(defvar my-tree (make-ttree
                 :elt 5
                 :l (make-ttree
                     :elt 3
                     :l (make-ttree :elt 9)
                     :c (make-ttree :elt 12)
                     :r (make-ttree :elt 90))
                 :r (make-ttree
                     :elt 87
                     :c (make-ttree :elt 91
                                    :r (make-ttree :elt 102)))))

(defun copy-ttree (ttree)
  (if (null ttree)
      nil
      (make-ttree :elt (ttree-elt ttree)
                  :l (copy-ttree (ttree-l ttree))
                  :c (copy-ttree (ttree-c ttree))
                  :r (copy-ttree (ttree-r ttree)))))
                  
(defun member-ttree (obj ttree)
  (if (null ttree)
      nil
      (if (eql obj (ttree-elt ttree))
          t
          (or (member-ttree obj (ttree-l ttree))
              (member-ttree obj (ttree-r ttree))
              (member-ttree obj (ttree-c ttree))))))
