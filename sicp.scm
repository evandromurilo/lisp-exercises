;; 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (sum-of-larger a b c)
  (cond ((smallest c a b) (sum-of-squares a b))
        ((smallest b a c) (sum-of-squares a c))
        (else (sum-of-squares b c))))

(define (smallest a b c)
  (and (<= a b)
       (<= a c)))
  
(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (square a)
  (* a a))

;; 1.8 Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value (x/y^2+2y)/3. Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve-guess guess x) x)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-guess y x)
  (/ (+ (/ x (square y))
        (* 2 y))
     3))

(define (cbrt x)
  (cbrt-iter 1.0 x))
