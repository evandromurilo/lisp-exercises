;; code from the chapter 2

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, but we can switch to other grammars.")

(defun generate (rule-name)
  "Generate a valid sequence for the given rule on the grammar."
  (let ((rule (cddr (assoc rule-name *grammar*))))
    (if (listp (car rule))
        (apply #'append (mapcar #'generate (car rule)))
        (one-of rule))))

(defun one-of (lst)
  "Return one random element of lst as a list."
  (list (elt lst (random (length lst)))))

    
