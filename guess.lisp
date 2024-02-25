(defun start-game ()
  (play (+ 1 (random 100)) 0))

(defun play (n turn)
  (let ((guess (read-guess)))
    (cond ((equal guess n)
           (format t "Você acertou em ~A tentativas!~%" turn))
          ((< guess n)
           (format t "Chutou baixo! Tente novamente.~%")
           (play n (+ 1 turn)))
          (t
           (format t "Chutou alto! Tente novamente.~%")
           (play n (+ 1 turn))))))

(defun read-guess ()
  (format t "~&Digite um número: ")
  (let ((n (read)))
    (if (numberp n)
        n
        (read-guess))))
               
