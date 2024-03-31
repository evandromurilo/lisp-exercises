(defun fsub (path search replace)
  (with-open-file (f path :direction :input)
    (let ((in-match 0)
          (slen (length search))
          (buffer nil))
      (do ((c (read-char f nil 'eof) (read-char f nil 'eof)))
          ((eql c 'eof))
        (if (eql c (aref search in-match))
            (if (eql (+ in-match 1) slen)
                (progn
                  (format t replace)
                  (setf in-match 0))
                (progn
                  (push buffer c)
                  (incf in-match)))
            (if (null buffer)
                (format t (string c))
                (progn
                  (setf in-match 0)
                  (format t (list-to-str (reverse buffer)))
                  (format t (string c)))))))))

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

;; -----------

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
      (setf (buf-used b) (buf-start b)
            (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new   b) -1 (buf-end  b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))
    
        
