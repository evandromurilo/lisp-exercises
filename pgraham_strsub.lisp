(defun strsub (subject search replace)
  (list-to-str (sub (seq-to-list subject)
                    (seq-to-list search)
                    (seq-to-list replace))))

(defun seq-to-list (seq)
  (do* ((i (length seq) (- i 1))
        (lst nil (push (aref seq i) lst)))
       ((= i 0) lst)))

(defun list-to-str (lst)
  (let ((str (make-string (length lst))))
    (dotimes (i (length str))
      (setf (aref str i) (car lst))
      (setf lst (cdr lst)))
    str))

(defun sub (subject search replace)
  (if (null subject)
      nil
      (multiple-value-bind (rest found) (match subject search)
        (if found
            (append replace (sub rest search replace))
            (cons (car subject) (sub (cdr subject) search replace))))))

(defun match (subject search)
  (cond ((null search)
         (values subject t))
        ((null subject)
         (values nil nil))
        ((eql (car subject) (car search))
         (match (cdr subject) (cdr search)))
        (t (values subject nil))))
