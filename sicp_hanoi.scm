(define (move n from to spare)
  (cond ((= n 0) "DONE")
        (else
         (move (- n 1) from spare to)
         (print-move from to)
         (move (- n 1) spare to from))))

(define (print-move from to)
  (display "MOVE ")
  (display from)
  (display " ")
  (display to)
  (newline))
