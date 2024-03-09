(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-b b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

(define (expt-c b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-c b (/ n 2))))
        (else (* b (expt-c b (- n 1))))))
