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
 
;; 1.16 Define a procedure that evolves an iterative exponentiation process that uses successive squareing and uses a logarithmic number of steps.

(define (square b)
  (* b b))

(define (cube b)
  (* b b b))

(define (exp b n)
  (exp-iter b n 1))

(define (exp-iter b n extra)
  (cond ((= n 0) (* 1 extra))
        ((= n 1) (* b extra))
        ((even? n) (exp-iter (square b) (/ n 2) extra))
        (else (exp-iter (square b) (/ (- n 1) 2) (* b extra)))))

;; 1.17 Using addition, double, and halve, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (times a b)
  (cond ((= b 0) 0)
        ((even? b) (times (double a) (halve b)))
        (else (+ a (times (double a) (halve (- b 1)))))))
  
;; 1.18 Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling and halving and uses a logarithmic number of steps.

(define (times a b)
  (times-iter a b 0))

(define (times-iter a b extra)
  (cond ((= b 0) (+ 0 extra))
        ((even? b) (times-iter (double a) (halve b) extra))
        (else (times-iter (double a) (halve (- b 1)) (+ extra a)))))

;; 1.30 The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; 1.31 a. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in termos of product.

(define (identity a) a)

(define (inc a) (+ a 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term
                           (next a)
                           next
                           b))))

(define (factorial n)
  (product identity 1 inc n))

;; 1.31 b. If your product procedure generates a recursive process, write one that generates an iterative process. If it genrates an iterative process, write one that generates a recursive process.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; 1.32 a. Show that sum and product are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
;; (accumulate combiner null-value term a next b)
;; Write accumulate and show how sum and product can both be defined as simple calls to accumulate.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; 1.32 b. If your accumulate procedure genrates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; 1.33 You can obtain an even more general version of accumulate by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction take the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered accumulate as a procedure.

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (sum-even n)
  (filtered-accumulate + 0 even? identity 1 inc n))

;; 1.33 a. Show how to express the sum of the squares of prime numbers in the interval a to b (assuming you have a prime? predicate already written).

(define (sum-square-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))


;; 1.33 b. Show how to express the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1)

