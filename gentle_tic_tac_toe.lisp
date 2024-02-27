(defparameter *player* 1)
(defparameter *computer* 10)
(defparameter *triplets* '((1 2 3) (4 5 6) (7 8 9)
                           (1 4 7) (2 5 8) (3 6 9)
                           (1 5 9) (3 5 7)))
(defparameter *initial-board* '(board 0 0 0 0 0 0 0 0 0))

(defun init-board ()
  "Initialize the board for the game. 0 is empty, 1 is player, 10 is computer."
    (copy-list *initial-board*))

(defun start-game ()
  "Start a new tic-tac-toe game."
  (if (y-or-n-p "Want to start first? ")
      (player-turn (init-board))
      (computer-turn (init-board))))

(defun check-game-over (board)
  "Check if the game is over and print a message if that is the case."
  (when (or (game-won-p board)
            (board-full-p board))
    (format t "~&Game over!~%")
    t))

(defun board-full-p (board)
  "Check if the board is full."
  (not (member 0 board)))

(defun game-won-p (board)
  "Check if player or computer has won the game."
  (find-if
   #'(lambda (triplet)
       (let ((sum (triplet-sum triplet board)))
         (or (equal sum 30)
             (equal sum 3))))
   *triplets*))

(defun triplet-sum (triplet board)
  "Returns the sum of the triplet on the board."
  (apply
   #'+
   (mapcar #'(lambda (pos) (get-square pos board)) triplet)))

(defun player-turn (board)
  "The player takes a turn."
  (format-board board)
  (unless (check-game-over board)
    (let ((pos (read-valid-pos board)))
      (computer-turn (play *player* pos board)))))

(defun computer-turn (board)
  "The computer takes a turn."
  (format-board board)
  (unless (check-game-over board)
    (let ((move (calculate-move board)))
      (format t "~&~A~%" (move-explanation move))
      (player-turn
       (play *computer* (move-pos move) board)))))

(defun move-explanation (move)
  "Return the explanation for the move."
  (first move))

(defun move-pos (move)
  "Return the position for the move."
  (second move))

(defun calculate-move (board)
  "Calculate a move for the computer with explanation."
  (cond ((winning-move *computer* board)
         (list "Winning move" (winning-move *computer* board)))
        ((winning-move *player* board)
         (list "Stop player from winning" (winning-move *player* board)))
        (t (list "Random move" (random-move board)))))

(defun winning-move (player board)
  "Return a winning position for player on board."
  (let ((triplet (winning-triplet player board)))
    (if triplet
        (find-if #'(lambda (pos) (zerop (get-square pos board))) triplet))))

(defun winning-triplet (player board)
  "Return a winning triplet for player on board."
  (find-if #'(lambda (triplet)
               (equal (triplet-sum triplet board) (* 2 player)))
           *triplets*))

(defun random-move (board)
  "Calculate a random move on the board"
  (let ((pos (+ 1 (random 9))))
    (if (available-square-p pos board)
        pos
        (random-move board))))

(defun play (player pos board)
  "Plays position for player on board."
  (setf (nth pos board) player)
  board)

(defun read-valid-pos (board)
  "Reads a valid position for the current board."
  (format t "~&Where are you going to play? ")
  (let ((pos (read)))
    (cond ((and (numberp pos)
                (in-rangep pos 1 9)
                (available-square-p pos board)
                pos)))))

(defun get-square (pos board)
  "Gets the value of a square on the board."
  (nth pos board))

(defun available-square-p (pos board)
  "Says if the given square on the board is empty."
  (zerop (get-square pos board)))

(defun in-rangep (n lower upper)
  "Says if n is between lower and upper (inclusive)."
  (and (>= n lower)
       (<= n upper)))

(defun format-board (board)
  "Prints the tic tac toe board."
  (format-row (list (nth 1 board) (nth 2 board) (nth 3 board)))
  (format-separator)
  (format-row (list (nth 4 board) (nth 5 board) (nth 6 board)))
  (format-separator)
  (format-row (list (nth 7 board) (nth 8 board) (nth 9 board))))

(defun format-separator ()
  "Prints the board horizontal line."
  (format t "---+---+---~%"))

(defun format-row (row)
  "Prints a row of the tic tac toe board."
  (format t " ~A | ~A | ~A~%"
          (player-str (first row))
          (player-str (second row))
          (player-str (third row))))

(defun player-str (p)
  "Returns the string representation of p."
  (cond ((equal p 0) " ")
        ((equal p 1) "X")
        ((equal p 10) "O")))
