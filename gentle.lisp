;; 3.23. Write each of the following functions in Church's lambda notation:
;; DOUBLE
(lambda (x) (* x 2))
;; SQUARE
(lambda (x) (* x x))
;; ONEMOREP
(lambda (x y) (equal x (+ y 1)))

;; 4.1 Write a function MAKE-EVEN
(defun make-even (x) (if (oddp x) (+ x 1) x))

;; 4.2 Write a function FURTHER
(defun further (x) (if (> 0 x) (- x 1) (+ x 1)))

;; 4.3 Write a function MY-NOT
(defun my-not (x) (if x nil t))

;; 4.4 Write a function ORDERED
(defun ordered (x y) (if (> x y) (list y x) (list x y)))

;; 4.6 Write a version of MY-ABS using COND
(defun my-abs (x) (cond ((< x 0) (- x))
                        (t x)))

;; 4.8 Write EMPHASIZE3 which is like EMPHASIZE2 but adds the symbol VERY onto the list if it doesn' know how to emphasize it.
(defun emphasize3 (x) (cond ((equal 'good (car x)) (cons 'great (cdr x)))
                            ((equal 'bad (car x)) (cons 'awful (cdr x)))
                            (t (cons 'very x))))

;; 4.9 Fix this suspicious function definition.
(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
        (t x)))

;; 4.10 Wrihte a function CONSTRAIN
(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun constrain2 (x min max)
  (if (< x min) min (if (> x max) max x)))

;; 4.11 Write a function FIRSTZERO
(defun firstzero (x)
  (cond ((zerop (first x)) 'first)
        ((zerop (second x)) 'second)
        ((zerop (third x)) 'third)
        (t 'none)))

;; 4.12 Write a function CYCLE
(defun cycle (x)
  (if (< x 99) (+ x 1) 1))

;; 4.13 Write a function HOWCOMPUTE
(defun howcompute (a b x)
  (cond ((equal (+ a b) x) 'sum-of)
        ((equal (* a b) x) 'product-of)
        ((equal (- a b) x) 'subtraction-of)
        (t '(beats me))))

(defun how-alike (a b)
  (cond ((equal a b) 'the-same)
        ((and (oddp a) (oddp b)) 'both-odd)
        ((and (not (oddp a)) (not (oddp b))) 'both-even)
        ((and (< a 0) (< b 0)) 'both-negative)
        (t 'not-alike)))

(defun same-sign (x y)
  (or (and (zerop x) (zerop y))
      (and (< x 0) (< y 0))
      (and (> x 0) (> y 0))))

;; 4.15 Write a predicate qcalled GEQ that returns T if its first input
;; is greater than or equal to its second input
(defun geq (a b)
  (or (> a b) (equal a b)))

;; 4.16 Write a function that squares  number if it is odd and positive,
;; doubles if it is odd and negative, and otherwise divides the number by
;; 2
(defun e4-16 (a)
  (cond ((and (oddp a) (> a 0)) (* a a))
        ((and (oddp a) (< a 0)) (* a 2))
        (t (/ a 2))))

;; 4.17 Write a predicate that returns T if the first input is either BOY
;; or GIRL and the second input is CHILD, or the first input is either
;; MAN or WOMAN and the second input is ADULT
(defun e4-17 (a b)
  (cond ((and (or (equal a 'BOY) (equal a 'GIRL)) (equal b 'CHILD)) t)
        ((and (or (equal a 'WOMAN) (equal a 'MAN)) (equal b 'ADULT)) t)
        (t nil)))

;; 4.18 Write a function to act as referee in the Rock-Scissors-Paper game
(defun e4-18 (a b)
  (cond ((and (equal a 'ROCK) (equal b 'SCISSORS)) 'FIRST-WINS)
        ((and (equal a 'ROCK) (equal b 'PAPER)) 'SECOND-WINS)
        ((and (equal a 'SCISSORS) (equal b 'ROCK)) 'SECOND-WINS)
        ((and (equal a 'SCISSORS) (equal b 'PAPER)) 'FIRST-WINS)
        ((and (equal a 'PAPER) (equal b 'SCISSORS)) 'SECOND-WINS)
        ((and (equal a 'PAPER) (equal b 'ROCK)) 'FIRST-WINS)
        (t 'TIE)))

;; 4.19 Show how to write (AND X Y Z W) using COND instead of AND. Then
;; show how to write it using nested IFs instead of AND.
(defun e4-19a (x y z w)
  (cond (x (cond (y (cond (z (cond (w w)
                                   (t nil)))
                          (t nil)))
                 (t nil)))
        (t nil)))

(defun e4-19b (x y z w)
  (if x (if y (if z w))))

;; 4.20 Write a version of the COMPARE function using IF instead of COND.
;; Also write a version using AND and OR.
(defun compare-a (x y)
  (if (equal x y)
      'numbers-are-the-same
      (if (< x y)
          'first-is-smaller
          'first-is-bigger)))

(defun compare-b (x y)
  (or (and (equal x y) 'numbers-are-the-same)
       (and (< x y) 'first-is-smaller)
       'first-is-bigger))

;; 4.22 Use COND to write a predicate BOILINGP that takes two inputs, TEMP
;; and SCALE, and returns T if the temperature is above the boiling point
;; of water on the specified scale. If the scale is FAHRENHEIT, the
;; boiling point is 212 degrees; if CELSIUS, the boiling point is 100
;; degrees. Also write versions using IF and AND/OR instead of COND.

(defun boilingp-a (temp scale)
  (cond ((equal scale 'FAHRENHEIT) (> temp 212))
        ((equal scale 'CELSIUS) (> temp 100))))

(defun boilingp-b (temp scale)
  (if (equal scale 'FAHRENHEIT)
      (> temp 212)
      (if (equal scale 'CELSIUS)
          (> temp 100))))

(defun boilingp-c (temp scale)
  (or (and (equal scale 'FAHRENHEIT) (> temp 212))
      (and (equal scale 'CELSIUS) (> temp 100))))
          
;; 4.29 Write versions of LOGICAL-AND using IF and COND instead of AND
(defun logical-and (a b)
  (and a b t))
  
(defun logical-and-a (a b)
  (if a (if b t)))

(defun logical-and-b (a b)
  (cond (a (cond (b t)))))

;; 4.30 Write LOGICAL-OR. Make sure it returns only T or NIL.
(defun logical-or (a b)
  (or (and a t) (and b t)))


;; 4.14 DEMORGAN'S THEOREM
(defun demorgan-and (x y)
  (not (or (not x) (not y))))

(defun demorgan-or (x y)
  (not (and (not x) (not y))))

;; 4.35 write down the DeMorgan equations for the three-input verions of AND and OR
(defun demorgan-and-3 (x y z)
  (not (or (not x) (not y) (not z))))

(defun demorgan-or-3 (x y z)
  (not (and (not x) (not y) (not z))))

;; 4.37 Construct versions of LOGICAL-AND and LOGICAL-OR by putting together NANDS
(defun nand (x y)
  (not (and x y)))

(defun and2 (x y)
  (nand (nand x y) (nand x y)))

(defun or2 (x y)
  (nand (nand x x) (nand y y)))

;; 4.38 Consider the NOR function. Can you write versions of NOT, LOGICAL-AND, NAND, and LOGICAL-OR by putting NORs together?
(defun nor (x y)
  (not (or x y)))

(defun not3 (x)
  (nor x x))

(defun and3 (x y)
  (nor (nor x x) (nor y y)))

(defun nand3 (x y)
  (nor (nor (nor x x) (nor y y)) (nor (nor x x) (nor y y))))

(defun or3 (x y)
  (nor (nor x y) (nor x y)))

;; keyboard exercise dice

(defun throw-die ()
  "Returns the value of a die roll (1 to 6)"
  (+ (random 6) 1))

(defun throw-dice ()
  "Returns the value of two dice rolls, as a list"
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "When throwing dice, 2 ones are called snake eyes"
  (equal '(1 1) throw))

(defun boxcars-p (throw)
  "When throwing dice, 2 sixes are called boxcars"
  (equal '(6 6) throw))

(defun instant-win-p (throw)
  "In playing craps, a throw of 7 or 11 is an instant win"
  (let ((sum (+ (first throw) (second throw))))
    (cond ((equal 7 sum) t)
          ((equal 11 sum) t)
          (t nil))))

(defun instant-loss-p (throw)
  "In playing craps, a throw of 2, 3, or 12 is an instant loss"
  (let ((sum (+ (first throw) (second throw))))
    (cond ((equal 2 sum) t)
          ((equal 3 sum) t)
          ((equal 12 sum) t)
          (t nil))))

(defun say-throw (throw)
  "Returns either the sum of the two dice or the symbols SNAKE-EYES or BOXCARS"
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (+ (first throw) (second throw)))))

(defun craps ()
  "Reproduces a throw of dice in scraps"
  (let ((throw (throw-dice)))
    (cond ((instant-win-p throw)
           (list 'THROW (first throw) 'AND (second throw) '-- (say-throw throw) '-- 'YOU 'WIN))
          ((instant-loss-p throw)
           (list 'THROW (first throw) 'AND (second throw) '-- (say-throw throw) '-- 'YOU 'LOSE))
          (t (list 'THROW (first throw) 'AND (second throw) '-- 'YOUR 'POINT 'IS (say-throw throw))))))

(defun try-for-point (prev)
  "Reproduces a throw of dice in scraps, when the previous throw was a point"
  (let* ((throw (throw-dice))
         (sum (+ (first throw) (second throw))))
    (cond ((equal pre sum)
           (list 'THROW (first throw) 'AND (second throw) '-- sum '-- 'YOU 'WIN!))
          ((equal sum 7)
           (list 'THROW (first throw) 'AND (second throw) '-- sum '-- 'YOU 'LOSE))
          (t (list 'THROW (first throw) 'AND (second throw) '-- sum '-- 'THROW 'AGAIN)))))

;; 6.6 Use the LAST function to write a function called LAST_ELEMENT that returns the last element of a list instead of the last cons cell. Write another version of LAST-ELEMENT using REVERSE instead of LST. Wwrite another version using NTH and LENGTH.

(defun last-element-a (list)
  (car (last list)))

(defun last-element-b (list)
  (car (reverse list)))

(defun last-element-c (list)
  (nth (- (length list) 1) list))
                       

;; 6.7 Use REVERSE to write a NEXT-TO-LAST function that returns the next-to-last element of a list. Write another version using NTH.

(defun next-to-last-a (list)
  (cadr (reverse list)))

(defun next-to-last-b (list)
  (nth (- (length list) 2) list))

;; 6.8 Write a function MY-BUTLAST that returns a list with the last element removed.

(defun my-butlast (list)
  (reverse (cdr (reverse list))))

;; 6.10 A palindrome is a sequence that reads the same forwards and backwards. The list (A B C D C B A) is a palindrome; (A B C A B C) is not. Write a function PALIDROMEP that returns T if its input is a palindrome.

(defun palindromep (list)
  (equalp (reverse list) list))

;; 6.11 Write a function MAKE-PALINDROME that makes a paindome out of a list, for example, given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU)

(defun make-palindrome (list)
  (append list (reverse list)))

;; 6.15 Suppose we want a predicate CONTAINS-ARTICLE-P that returns a true value if a sentence contains any article, such as "the", "a" or "an". Write a version of this predicate using INTERSECTION. Write another version using MEMBER and OR. Could you solve this problem with AND instead of OR?

(defun a-contains-article-p (list)
  (intersection list '(a the an)))

(defun b-contains-article-p (list)
  (or (member 'a list) (member 'an list) (member 'the list)))

;; 6.18 Write a function ADD-VOWELS that takes a set of letters as input and adds vowels (A E I O U) to the set. For example, calling ADD-VOWELS on the set (X A E Z) should produce the set (X A E Z I O U).

(defun add-vowels (set)
  (union '(a e i o u y) set))

;; 6.21 Write MY-SUBSETP, a version of the SUBSETP predicate that returns T if its first input is a subset of its second input.
(defun my-subsetp (x y)
  (equalp nil (set-difference x y)))

;; 6.24 Write a SET-EQUAL predicate that returns T if two things are equals as sets.
(defun set-equal (x y)
  (and (my-subsetp x y) (my-subsetp y x)))

;; 6.25 Write the PROPER-SUBSETP predicate, which returns T if its first input is a proper subset of its second input.
(defun proper-subsetp (x y)
  (and (my-subsetp x y) (not (my-subsetp y x))))

;; 6.26 Keyboard exercise common features
;; (compare '(small red metal cube -vs- red plastic small cube))
(defun right-side (x)
  (cdr (member '-vs- x)))

(defun left-side (x)
  (right-side (reverse x)))

(defun count-common (x)
  (length (intersection (right-side x) (left-side x))))

(defun compare (x)
  (cons (count-common x) '(COMMON FEATURES)))

;; 6.30 Make a table called BOOKS of five books and their authors.
(defvar books '((WAR-AND-PEACE LEO-TOLSTOY)
                (A-GAME-OF-THRONES GEORGE-R-R-MARTIN)
                (THE-HOBBIT J-R-R-TOLKIEN)
                (THE-BOOK-OF-GOD WALTER-WANGERIN)
                (PHANTASTES GEORGE-MACDONALD)))

;; 6.31 Write the function WHO-WROTE that take the name of a book as input and returns the book's author.
(defun who-wrote (x) (second (assoc x books)))

;; 6.35 keyboard exercise nerdus americanis
(defvar nerd-states '((sleeping eating)
                      (eating waiting-for-a-computer)
                      (waiting-for-a-computer programming)
                      (programming debugging)
                      (debugging sleeping)))

(defun nerdus (state) (second (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (if (equalp (nerdus state) 'sleeping)
      'eating
      (nerdus state)))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

;; 6.36 Write a function to swap the first and last elements of any list.
(defun swap-extremes (list)
  (let ((f (car list))
        (l (car (last list))))
    (reverse (cons f (cdr (reverse (cons l (cdr list))))))))
    
;; 6.37 ROTATE-LEFT and ROTATE-RIGHT are functions that rotate the elements of a list. Write these functions.
(defun rotate-left (list)
  (append (cdr list) (cons (car list) nil)))

(defun rotate-right (list)
  (cons (car (last list)) (reverse (cdr (reverse list)))))

;; 6.41 keyboard exercise robot
(defvar rooms '((library
                 (south back-stairs)
                 (east upstairs-bedroom))
                (upstairs-bedroom
                 (west library)
                 (south front-stairs))
                (front-stairs
                 (south living-room)
                 (north upstairs-bedroom))
                (living-room
                 (north front-stairs)
                 (east kitchen)
                 (south dining-room))
                (kitchen
                 (west living-room)
                 (south pantry))
                (pantry
                 (west dining-room)
                 (north kitchen))
                (dining-room
                 (east pantry)
                 (north living-room)
                 (west downstairs-bedroom))
                (downstairs-bedroom
                 (east dining-room)
                 (north back-stairs))
                (back-stairs
                 (north library)
                 (south downstairs-bedroom))))

(defun choices (room) (cdr (assoc room rooms)))

(defun look (direction room)
  (cadr (assoc direction (choices room))))
                               
(defvar loc 'pantry)

(defun set-robbie-location (place)
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (place)
  (if (member place '(library upstairs-bedroom)) t nil))

(defun onstairsp (place)
  (if (member place '(front-stairs back-stairs)) t nil))

(defun where ()
  (cond ((upstairsp loc) (append '(ROBBIE IS UPSTAIRS IN THE) (list loc)))
        ((onstairsp loc) (append '(ROBBIE IS ON THE) (list loc)))
        (t (append '(ROBBIE IS DOWNSTAIRS IN THE) (list loc)))))

(defun move (direction)
  (let ((next (look direction loc)))
    (cond (next (setf loc next) (where))
          (t '(OUCH! ROBBIE HIT A WALL)))))

;; 6.42 Write a function called ROYAL-WE that changes every occurrence of the symbol I in a list to the symbol WE.

(defun royal-we (list)
  (subst 'WE 'I list))

;; 7.1 Write an ADD1 function that adds one to its input. Then write an expression to add one to each element of the list (1 3 7 9).

(defun add1 (n)
  (+ 1 n))

(mapcar #'add1 '(1 3 7 9))

;; 7.2 Let the global variable DAILY-PLANET contain the following table.

(defvar daily-planet
  '((olsem jimmy 1234-6-4535 club-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))

;; Each table entry consists of a last name, a first name, a social security number, and a job title. Use MAPCAR on this table to extract a list of social security numbers.

(mapcar #'third daily-planet)

;; 7.3 Write an expression to apply the ZEROP predicate to each element of the list (2 0 3 4 0 5 6).

(mapcar #'zerop '(2 0 3 4 0 5 6))

;; 7.4 Suppose we want to solve a problem similar to the preceding one, but testing whether an element is greater than five.

(defun greater-than-five-p (n)
  (> n 5))

(mapcar #'greater-than-five-p '(2 0 3 4 0 5 6))

;; 7.5 Write a lambda expression to subtract seven from  number.
#'(lambda (n) n - 7)

;; 7.6 Write a lambda expresion that returns T if its input is T or NIL, but NIL for any other input.
#'(lambda (v)
    (if (or (eq v 't) (eq v nil)) t nil))

;; 7.7 Write a function that takes a list such as (UP DOWN UP UP) and flips each element, returning (DOWN UP DOWN DOWN).
(defun flipdir (list)
  (mapcar #'(lambda (direction)
              (if (eq direction 'up) 'down 'up)) list))

;; 7.8 Write a function that takes two inputs, X and K, and returns the first number is the list X that is roughly equal to K.
(defun roughly (x k)
  (find-if #'(lambda (n) (<= (difference x n) 10)) k))

(defun difference (a b)
  (abs (- a b)))

;; 7.9 Write a function find-nested that returns the first element of a list that is itself a non-NIL list.
(defun find-nested (list)
  (find-if #'(lambda (n) (equal (type-of n) 'CONS)) list))

;; 7.10 Keyboard exercise notes

(defvar note-table '((c 1)
                     (c-sharp 2)
                     (d 3)
                     (d-sharp 4)
                     (e 5)
                     (f 6)
                     (f-sharp 7)
                     (g 8)
                     (g-sharp 9)
                     (a 10)
                     (a-sharp 11)
                     (b 12)))

(defun numbers (notes)
  (mapcar #'(lambda (n) (second (assoc n note-table))) notes))

(defun notes (numbers)
  (mapcar #'(lambda (n)
              (first (my-rassoc n note-table)))
          numbers))

(defun my-rassoc (x list)
  (find-if #'(lambda (n) (equal (second n) x)) list))

(defun raise (n numbers)
  (mapcar #'(lambda (x) (+ n x)) numbers))

(defun normalize (numbers)
  (mapcar #'(lambda (x)
              (cond ((< x 1) (+ x 12))
                    ((> x 12) (- x 12))
                    ('t x))) numbers))

(defun transpose (n notes)
  (notes (normalize (raise n (numbers notes)))))

;; 7.11 Write a function to pick out those numbers in a list that are greater than one and less than five.

(defun pickup (list)
  (remove-if-not #'(lambda (n) (and (> n 1) (< n 5))) list))

;; 7.12 Write a function that counts how many times the word "the" appears in a sentence.

(defun count-the (list)
  (length (remove-if-not #'(lambda (word) (equal word 'the)) list)))

;; 7.13 Write a function that picks from a list of lists those of exactly length two.

(defun pick-two (list)
  (remove-if-not #'(lambda (n) (= (length n) 2)) list))

;; 7.14 Write the INTERSECTION and UNION functions using REMOVE-IF and REMOVE-IF-NOT

(defun my-intersection (a b)
  (remove-if-not #'(lambda (x) (member x b)) a))

(defun my-union (a b)
  (append a (remove-if #'(lambda (x) (member x a)) b)))

;; 7.15 playing cards keyboard exercise

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(defvar my-hand
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (card) (equal (suit card) suit)) hand)))

(defvar colors
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))

(defun color-of (card)
  (second (assoc (suit card) colors)))

(defun first-red (hand)
  (find-if #'(lambda (card) (equal (color-of card) 'red)) hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card) (equal (color-of card) 'black)) hand))

(defun what-ranks (suit hand)
  (mapcar #'rank (remove-if-not #'(lambda (card) (equal (suit card) suit)) hand)))

(defvar all-ranks
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (a b)
  (equal (find-if #'(lambda (rank) (or (equal rank a)
                                       (equal rank b)))
                  all-ranks) b))

(defun high-card (hand)
  (find-if #'(lambda (rank) (member rank (mapcar #'rank hand))) (reverse all-ranks)))

;; 7.16 Write a function that reduces a list of sets to one big set.

(defun set-collapse (list)
  (reduce #'union list))

;; 7.17 Write a function that, given a list of lists, returns the total length of all the lists.

(defun deep-length (list)
  (reduce #'(lambda (a b) (+ a (length b))) list :initial-value 0))

(defun deep-length-b (list)
  (reduce #'+ (mapcar #'length list)))

;; 7.19 Write a function ALL-ODD that returns T if every element of a list of numbers is odd.

(defun all-odd (list)
  (every #'oddp list))

;; 7.20 Write a function NONE-ODD that returns T if every element of a list of numbers is not odd.

(defun none-odd (list)
  (every #'evenp list))

;; 7.21 Write a function NOT-ALL-ODD that returns T if not every element of a list of numbers is ODD.

(defun not-all-odd (list) ;; some-even
  (not (all-odd list)))

;; 7.22 Write a function NOT-NONE-ODD that returns T if it is not the case that a list of numbers contains no odd elements.

(defun not-none-odd (list) ;; some-odd
  (not (none-odd list)))

;; 7.26 Show how to write FIND-IF given REMOVE-IF-NOT.

(defun my-find-if (operator list)
  (first (remove-if-not operator list)))

;; 7.27 Show how to write EVERY given REMOVE-IF.

(defun my-every (operator list)
  (equal 0 (length (remove-if operator list))))

;; 7.29 keyboard exercise block world

(defvar database
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b1 material wood)
    (b2 shpe brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b2 material plastic)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)))

(defun match-element (a b)
  (or (equal a b)
      (equal b '?)))

(defun match-triple (assertion pattern)
  (every #'match-element assertion pattern))

(defun fetch (pattern)
  (remove-if-not #'(lambda (x) (match-triple x pattern)) database))

(defun color-pattern (bname)
  (list bname 'color '?))

(defun supporters (bname)
  (mapcar #'first (fetch (list '? 'supports bname))))

(defun supp-cube (bname)
  (find-if #'(lambda (b) (fetch (list b 'shape 'cube))) (supporters bname)))

(defun desc1 (bname)
  (fetch (list bname '? '?)))

(defun desc2 (bname)
  (mapcar #'cdr (desc1 bname)))

(defun description (bname)
  (reduce #'append (desc2 bname)))

;; 8.2 Show how to write ANYODDP using IF instead of COND

(defun anyoddp (x)
  (if (null x)
      nil
      (or (oddp (first x))
          (anyoddp (rest x)))))

;; 8.4 Write a function called LAUGH that takes a number as input and returns a list of that many HAs.

(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

;; 8.5 Write a function called ADD_UP to add up all the numbers in a list.

(defun add-up (list)
  (cond ((null list) 0)
        (t (+ (first list) (add-up (rest list))))))

;; 8.6 Write ALLODDP, a recursive function that returns T if all the numbers in a list are odd.

(defun allodp (list)
  (cond ((null list) t)
        (t (and (oddp (first list)) (allodp (rest list))))))

;; 8.7 Write a recursive version of MEMBER. Call it REC-MEMBER.

(defun rec-member (item list)
  (cond ((null list) nil)
        ((equal (first list) item) t)
        (t (rec-member item (rest list)))))

;; 8.8 Write a recursive version of ASSOC. Call it REC-ASSOC.

(defun rec-assoc (item alist)
  (cond ((null alist) nil)
        ((equal (caar alist) item) (first alist))
        (t (rec-assoc item (rest alist)))))

;; 8.9 Write a recursive version of NTH. Call it REC-NTH

(defun rec-nth (n list)
  (cond ((null list) nil)
        ((zerop n) (first list))
        (t (rec-nth (- n 1) (rest list)))))
         

;; 8.10 For x a nonnegative integer and y a positive integer, x+y equals x+1+(y-1). If y is zero then x+y equals x. Use these equations to build a recursive version of + called REC-PLUS out of ADD1, SUB1, COND and ZEROP.

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

;; 8.11 Write the FIB function.

(defun fib (n)
  (cond ((or (zerop n) (equal 1 n)) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

;; 8.14 Write the very shortest infinite recursion function you can.

(defun forever () (forever))

;; 8.17 Use double-test tail recursion to write FIND-FIRST-ODD, a function that returns the first odd number in a list. or NIL if there are none.

(defun find-first-odd (list)
  (cond ((null list) nil)
        ((oddp (first list)) (first list))
        (t (find-first-odd (rest list)))))

;; 8.18 Use single-test tail recursion to write LAST-ELEMENT, a function taht should recursively travel down the list until it reaches the last cons cell (a cell whose cdr is an atom); then it should return the car of this cell.

(defun last-element (list)
  (cond ((atom (cdr list)) (car list))
        (t (last-element (cdr list)))))

;; 8.21 Write a recursive function ADD-NUMS that adds up the numbers N, N-1, N-2, and so on, down to 0, and returns the result.

(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (- n 1))))))

;; 8.22 Write a recursive function ALL-EQUAL that returns T if the first element of a list is equal to the second, the second is equl to the third, and so on. ALL-EQUAL should return T for lists with less than two elements.

(defun all-equal (list)
  (cond ((< (length list) 2) t)
        ((not (equal (first list) (second list))) nil)
        (t (all-equal (rest list)))))

;; 8.24 Write COUNT-DOWN, a function that counts down from n using list-consing recursion.

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

;; 8.25 How could COUNT-DOWN be used to write an applicative version of FACT?

(defun fact (n)
  (reduce #'(lambda (a b) (* a b)) (count-down n)))

;; 8.26 Suppose we wanted to modify COUNT-DOWN so that the list it constructs ends in zero Show two ways this can be done.

(defun count-down-a (n)
  (cond ((< n 0) nil)
        (t (cons n (count-down-a (- n 1))))))

;; 8.27 Write SQUARE-LIST, a recursive function that takes a list of numbers as input and returns a list of their squares.

(defun square-list (n)
  (cond ((null n) nil)
        (t (cons (* (first n) (first n)) (square-list (rest n))))))

;; 8.31 Write a recursive function COMPARE-LENGTHS that takes two lists as input and returns one of the following symbols: SAME-LENGTH, FIRST-IS-LONGER, or SECOND-IS-LONGER.

(defun compare-lengths (a b)
  (cond ((and (null a) (null b)) 'SAME-LENGTH)
        ((null a) 'SECOND-IS-LONGER)
        ((null b) 'FIRST-IS-LONGER)
        (t (compare-lengths (rest a) (rest b)))))

;; 8.32 Write the function SUM-NUMERIC-ELEMENTS, which adds up all the numbers in the list and ignores the non-numbers.

(defun sum-numeric-elements (list)
  (cond ((null list) 0)
        ((numberp (first list)) (+ (first list) (sum-numeric-elements (rest list))))
        (t (sum-numeric-elements (rest list)))))

;; 8.33 Write MY-REMOVE, a recusive version of the REMOVE function

(defun my-remove (a list)
  (cond ((null list) nil)
        ((equal a (first list)) (my-remove a (rest list)))
        (t (cons (first list) (my-remove a (rest list))))))

;; 8.34 Write MY-INTERSECTION, a recursive version of the INSERSECTION function

(defun my-intersection (a b)
  (cond ((null a) nil)
        ((not (member (first a) b)) (my-intersection (rest a) b))
        (t (cons (first a) (my-intersection (rest a) b)))))
        
;; 8.35 Write MY-SET-DIFFERENCE, a recursive version of SET-DIFFERENCE.

(defun my-set-difference (a b)
  (cond ((null a) nil)
        ((not (member (first a) b)) (cons (first a) (my-set-difference (rest a) b)))
        (t (my-set-difference (rest a) b))))

;; 8.36 Write the function COUNT-ODD using conditional augmentation and regular augmentation.

(defun count-odd (list)
  (cond ((null list) 0)
        ((oddp (first list)) (+ 1 (count-odd (rest list))))
        (t (count-odd (rest list)))))

;; 8.37 Define a simple function COMBINE that takes two numbers as input and returns their sum. Now replace the occurrence of + in FIB with COMBINE.

(defun combine (a b)
  (+ a b))

(defun fib (n)
  (cond ((equal n 0) 1)
         ((equal n 1) 1)
         (t (combine (fib (- n 1)) (fib (- n 2))))))

;; 8.39 Write a function COUNT-ATOMS that returns the number of atoms in a tree. (COUNT-ATOMS '(a (b) c)) should return five, since in addition to A, B and C there are two NILs in the tree.

(defun count-atoms (tree)
  (cond ((atom tree) 1)
        (t (+ (count-atoms (car tree)) (count-atoms (cdr tree))))))

;; 8.40 Write COUNT-CONS, a function that returns the nubmer of cons cells in a tree (COUNT-CONS '(FOO) should return one. (COUNT-CONS '(FOO BAR)) should return two. (COUNT-CONS '((FOO))) should also return two. (COUNT-CONS 'FRED) should return zero.

(defun count-cons (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (count-cons (car tree)) (count-cons (cdr tree))))))

;; 8.41 Write a function SUM-TREE that returns the sum of all the numbers appearing in a tree. Nonnumber should be ignored. (SUM-TREE '((3 BEARS) (3 BOWLS) (1 BIRD))) should return seven.

(defun sum-tree (tree)
  (cond ((numberp tree) tree)
        ((atom tree) 0)
        (t (+ (sum-tree (car tree)) (sum-tree (cdr tree))))))

;; 8.42 WRITE MY-SUBST, a recursive version of the SUBST function.

(defun my-subst (new old tree)
  (cond ((equal tree old) new)
        ((atom tree) tree)
        (t (cons (my-subst new old (car tree)) (my-subst new old (cdr tree))))))

;; 8.43 Write FLATTEN, a function that returns al lthe elements of an arbitrarily nested list in a single-level list. (FLATTEN '((A B (R)) A C (A D ((A (B)) R) A))) should return (ABRACADABRA).

(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree)) (flatten (cdr tree))))))

;; 8.44 Write a function TREE-DEPTH that returns the maximum depth of a binary tree. (TREE-DEPTH '(A . B)) should return one. (TREE-DEPTH '((A B C D))) should return five, and (TREE-DEPTH '((A . B) . (C . D))) should return two.

(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (max (tree-depth (car tree)) (tree-depth (cdr tree)))))))

;; 8.45 Write a function PAREN-DPTH that returns the amximum depth of nested parentheses in a list.

(defun paren-depth (tree)
  (cond ((atom tree) 0)
        (t (max (+ 1 (paren-depth (car tree))) (paren-depth (cdr tree))))))

;; 8.46 Write the COUNT-UP function.

(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1)) (list n)))))

;; 8.47 Write MAKE-LOAF, a function that returns a loaf of size N. (MAKE-LOAF 4) should return (XXXX). Use IF instead of COND.

(defun make-loaf (n)
  (if (zerop n) nil (cons 'x (make-loaf (- n 1)))))

;; 8.48 Write a recursive function BURY that iries an item under n levels of parentheses. (BURY 'FRED 2) should return ((FRED)), while (BURY 'FRED 5) should return (((((FRED))))).

(defun bury (item n)
  (cond ((zerop n) item)
        (t (list (bury item (- n 1))))))

;; 8.49 Write PAIRINGS, a function that pairs the element of two lists. (PAIRINGS '(A B C) '(1 2 3)) should return ((A 1) (B 2) (C 3)). You may assume that the two lists will be of equal length.

(defun pairings (la lb)
  (cond ((null la) nil)
        (t (cons (list (car la) (car lb)) (pairings (cdr la) (cdr lb))))))

;; 8.50 Write SUBLISTS, a function that returns the successive sublists of a list. (SUBLISTS '(FEE FIE FOE)) should return ((FEE FIE FOE (FIE FOE) (FOE))).

(defun sublists (l)
  (cond ((null l) nil)
        (t (cons l (sublists (cdr l))))))

;; 8.51 The simplest way to write MY-REVERSE, a recursive version of REVERSE, is with a helping function plus a recursive function of two inputs. Write this version of MY-REVERSE.

(defun my-reverse (l)
  (reverse-recursively nil l))

(defun reverse-recursively (l rest)
  (cond ((null rest) l)
        (t (reverse-recursively (cons (car rest) l) (cdr rest)))))

;; 8.52 Write MY-UNION, a recursive version of UNION

(defun my-union (la lb)
  (my-union-recursively nil (append la lb)))

(defun my-union-recursively (r l)
  (cond ((null l) r)
        ((member (car l) r) (my-union-recursively r (cdr l)))
        (t (my-union-recursively (cons (car l) r) (cdr l)))))

;; 8.53 Write LARGEST-EVEN, a recursive function that returns the largest even number in a list of nonnegative integers. (LARGEST-EVEN '(5 2 4 3)) should return four. (LARGEST-EVEN NIL) should return zero. Use the built-in MAX function, which returns the largest of its inputs.

(defun largest-even (l)
  (cond ((null l) 0)
        ((evenp (car l)) (max (car l) (largest-even (cdr l))))
        (t (largest-even (cdr l)))))

;; 8.54 Write a recursive function HUGE that raises  number to its own power. (HUGE 2) should return 2^2, (HUGE 3) should return 3^3, and so on. Do not use REDUCE.

(defun huge (n)
  (huge-recursively n n))

(defun huge-recursively (remaining n)
  (cond ((zerop remaining) 1)
        (t (* n (huge-recursively (- remaining 1) n)))))

;; 8.55 Write EVERY-OTHER, a recursive function that returns every other element of a list.

(defun every-other (l)
  (every-other-recursively 0 l))

(defun every-other-recursively (n l)
  (cond ((null l) nil)
        ((oddp n) (every-other-recursively (+ 1 n) (cdr l)))
        (t (cons (car l) (every-other-recursively (+ 1 n) (cdr l))))))

(defun every-other-b (l)
  (cond ((null l) nil)
        (t (cons (car l) (every-other-b (cddr l))))))

;; 8.57 Write LEFT-HALF, a recursive function in two parts that returns the first n/2 elements ofa list of length n. Write your function so that the list does not have to be of even length. (LEFT-HALF '(A B C D E)) should return (A B C). You may use LENGTH but not REVERSE in your definition.

(defun left-half (l)
  (left-half-recursively nil l))

(defun left-half-recursively (l r)
  (cond ((null r) l)
        ((> (length r) (length l)) (left-half-recursively (append l (list (car r))) (cdr r)))
        (t l)))

(defun left-half-b (l)
  (left-half-helper l (/ (length l) 2)))

(defun left-half-helper (l n)
  (cond ((not (plusp n)) nil)
        (t (cons (car l) (left-half-helper (cdr l) (- n 1))))))

;; 8.58 Write MERGE-LISTS, a function that takes two lists of numbers, each in increasing order, as input. The function should return a list that is a merger of the elements in its inputs, in order. (MERGE-LISTS '(1 2 6 8 10 12) '(2 3 5 9 13)) should return '(1 2 3 ... 13).

(defun merge-lists (la lb)
  (cond ((null la) lb)
        ((null lb) la)
        ((< (car la) (car lb)) (cons (car la) (merge-lists (cdr la) lb)))
        (t (cons (car lb) (merge-lists la (cdr lb))))))
