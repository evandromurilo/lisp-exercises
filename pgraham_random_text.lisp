(setf word-table (make-hash-table :test #'equal))

(defun reset-table ()
  (setf word-table (make-hash-table :test #'equal)))

(defun read-text (path)
  (reset-table)
  (with-open-file (str path)
    (let ((word (make-string 100))
          (prev nil)
          (i 0))
      (do ((ch (read-char str nil 'eof) (read-char str nil 'eof)))
          ((eql ch 'eof) nil)
        (cond ((alpha-char-p ch)
               (setf (char word i) ch)
               (incf i))
              ((> i 0)
               (let ((new-word (subseq word 0 i)))
                 (add-word new-word prev)
                 (setf prev new-word)
                 (setf i 0)
                 (when (punc-p ch)
                   (add-word (string ch) prev)
                   (setf prev (string ch)))))))))
  (show-table))

(defun show-table ()
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) word-table))

(defun add-word (word prev)
  (let ((word-assoc (gethash word word-table)))
    (if (null word-assoc)
        (setf (gethash word word-table) (list (cons prev 1)))
        (let ((pair (assoc prev word-assoc)))
          (if (null pair)
              (setf (gethash word word-table) (push (cons prev 1) word-assoc))
              (incf (cdr pair)))))))

(defun punc-p (ch)
  (case ch ((#\. #\, #\! #\?) t)
            (otherwise nil)))

(defun generate (n)
  (implode " "  (reverse (gen nil n))))

(defun implode (separator lst)
  (let ((str (car lst)))
    (dolist (elt (cdr lst))
      (setf str (concatenate 'string str separator elt)))
    str))

(defun gen (prev n)
  (if (= n 0)
      nil
      (let ((word (gen-word prev)))
        (cons word (gen word (- n 1))))))

(defun gen-word (prev)
  (if (null prev)
      (random-word)
      (let ((related (cdr (gethash prev word-table))))
        (if (null related)
            (random-word)
            (random-elt (distribute related))))))

(defun distribute (lst)
  (if (null lst)
      nil
      (let ((pair (car lst)))
        (append (dist (car pair) (cdr pair))
                (distribute (cdr lst))))))

(defun dist (elt n)
  (if (zerop n)
      nil
      (cons elt (dist elt (- n 1)))))

(defun random-word ()
  (random-elt (keys word-table)))

(defun random-elt (lst)
  (nth (random (length lst)) lst))

(defun keys (hash)
  (let ((keys nil))
    (maphash #'(lambda (k v) (push k keys)) hash)
    keys))

  
