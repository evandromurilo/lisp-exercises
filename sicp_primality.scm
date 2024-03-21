(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))


;; fermat's test

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n start-time)
  (display n)
  (start-prime-test n start-time)
  (newline))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (define (do-search i start-time)
    (cond ((> i to) 'DONE)
          (else
           (timed-prime-test i start-time)
           (do-search (+ i 1) start-time))))
  (do-search from (current-time)))

