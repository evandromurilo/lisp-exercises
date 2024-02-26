;; 1.1 [m] Define a version of last-name that handles "Rex Morgan MD", "Morton Downey, Jr.", and whatever other cases you can think of.

(defparameter *titles*
  '(MISS MR MD JR JR. MR. DOCTOR DOC. SENSEI ADMIRAL COMMANDER LADY KING ST ST. SAINT))

(defun last-name (name)
  (first-name (reverse name)))

(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

;; 1.2 [m] Write a function to exponentiate, or raise a number to an integer power.

(defun power (base exp)
  "Power raises x to the nth power. N must be an integer >= 0."
  (if (zerop exp)
      1
      (* base (power base (- exp 1)))))

;; 1.3 [m] Write a function that counts the number of atoms in an expression. For example: (count-atoms '(a (b) c)) = 3.

(defun count-atoms (exp &optional (if-null 1))
  "Count the number of atoms in the list, including nil on non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp) 1)
              (count-atoms (rest exp) 0)))))

;; 1.4 [m] Write a function that counts the number of times an expression occurs anywhere within another expression. Example: (count-anywhere 'a '(a ((a) b) a)) = 3.

(defun count-anywhere (item exp &optional (if-null 1))
  "Count the number of times the item occurs anywhere within the expression. Does not count nil on tail position."
  (cond ((atom exp) (if (equal item exp) if-null 0))
        (t (+ (count-anywhere item (first exp) 1)
              (count-anywhere item (rest exp) 0)))))

;; 1.5 [m] Write a function to compute the dot product of two sequences of numbers, represented as lists.

(defun dot-product (lst-a lst-b)
  "Compute the mathematical dot product of two vectors."
  (cond ((null lst-a) 0)
        (t (+ (* (first lst-a)
                 (first lst-b))
              (dot-product (rest lst-a) (rest lst-b))))))
