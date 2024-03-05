;; recursive

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; iteration

(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (= counter max-count)
      product
      (fact-iter (* product counter)
                 (+ counter 1)
  factorial               max-count)))
