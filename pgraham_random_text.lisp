(setf word-table (make-hash-table :test #'equal))

(defun reset-table ()
  (setf word-table (make-hash-table :test #'equal)))

(defun read-text (path)
  (reset-table)
  (with-open-file (str path)
    (let ((word (make-string 100))
          (i 0))
      (do ((ch (read-char str nil 'eof) (read-char str nil 'eof)))
          ((eql ch 'eof) nil)
        (if (punc-p ch)
            (progn
              (add-word (subseq word 0 i))
              (setf i 0))
            (progn
              (setf (char word i) ch)
              (incf i))))))
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
  (case ch ((#\  #\. #\, #\! #\?) t)
            (otherwise nil)))
