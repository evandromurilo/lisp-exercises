(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(defvar male-first-names '(john kim richard fred george))

(defvar female-first-names '(jane mary wanda barbara kim))

(defun malep (name)
  (and (member name male-first-names)
       (not (member name female-first-names))))

(defun femalep (name)
  (and (member name female-first-names)
       (not (member name male-first-names))))

(defun give-title (name)
  (let ((fname (first name)))
    (cond ((titledp name) name)
          ((malep fname) (cons 'mr name))
          ((femalep fname) (cons 'ms name))
          ('t (append '(mr or ms) name)))))
