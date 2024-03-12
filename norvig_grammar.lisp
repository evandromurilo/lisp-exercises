;; code from the chapter 2

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article > the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))
     

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, but we can switch to other grammars.")

(defun generate (phrase)
  "Generate a random sentence of phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((listp phrase)
         (outer-permutate (mapcar #'generate-all phrase)))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun outer-permutate (lst)
  (permutate (car lst) (cdr lst)))

(defun permutate (a lst)
  "Permutates the list of generations a with the lists of generations lst."
  (cond ((null a) nil)
        ((null lst) a)
        (t (append (perm (car a) (permutate (car lst) (cdr lst)))
              (permutate (cdr a) lst)))))
         
(defun perm (a lst)
  "Permutates the generation a with the list of generations lst."
  (if (null lst)
      nil
      (cons (append a (car lst))
            (perm a (cdr lst)))))
         
  
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun terminalp (category)
  "Return t if the category is terminal, that is, if there are no possible rewrites."
  (null (rewrites category)))

(defun one-of (set)
  "Return one random element of set as a list."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

;; Exercise 2.1 [m] Write a version of gnerate that uses cond but avoids calling rewrite twice.
;; (só consegui pensar em colocar um let em volta do cond)

;; Exercise 2.2 [m] Write a version of generate that explicitly differentiates between terminal symbols (those with no rewrite rules) and nonterminal symbols.

(defun generate-b (phrase)
  "Generate a random sentence of phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((terminalp phrase)
         (list phrase))
        (t
         (generate (random-elt (rewrites phrase))))))
