(setf word-table (make-hash-table :test #'equal))

(defun read-text (path)
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
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) word-table))

(defun add-word (word)
  (incf (gethash word word-table 0)))

(defun punc-p (ch)
  (case ch ((#\  #\. #\, #\! #\?) t)
            (otherwise nil)))
