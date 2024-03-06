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

;; 1.11 A function f is defined by the rule that f(n) = n if n < 3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n ≥ 3. Write a procedure that computes f by means of recursive process. Write a procedure that computes f by menas of an interative process.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(define (fb n)
  (if (<= n 3)
      n
      (f-iter 0 1 2 (- n 2))))

(define (f-iter a b c n)
  (if (= 0 n)
      c
      (f-iter b c (+ a b c) (- n 1))))

;; 1.12 Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(define (pascal n c)
  (if (or (= n 0)
          (= c 0)
          (= c n))
      1
      (+ (pascal (- n 1) (- c 1)) (pascal (- n 1) c))))
