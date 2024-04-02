(defun fsub (path search replace)
  (with-open-file (f path :direction :input)
    (let ((in-match 0)
          (slen (length search))
          (buffer nil))
      (do ((c (read-char f nil 'eof) (read-char f nil 'eof)))
          ((eql c 'eof))
        (push c buffer)
        (if (eql c (aref search in-match))
            (if (eql (+ in-match 1) slen)
                (progn
                  (princ replace)
                  (setf buffer nil)
                  (setf in-match 0))
                (incf in-match))
            (progn
              (setf in-match 0)
              (princ (list-to-str (reverse buffer)))
              (setf buffer nil))))
      (unless (null buffer)
        (princ (list-to-str (reverse buffer)))))))

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

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new   b) -1 (buf-end  b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))
    
(defun file-subst (search replace inpath outpath)
  (with-open-file (instr inpath)
    (with-open-file (outstr outpath :direction :output :if-exists :supersede)
      (stream-subst search replace instr outstr))))

;; my take
(defun stream-subst (search replace instr outstr)
  (let* ((slen (length search))
         (buf (new-buf slen))
         (i 0))
    (do ((c (read-char instr nil 'eof) (read-char instr nil 'eof)))
        ((eql c 'eof))
      (buf-insert c buf)
      (if (eql c (aref search i))
          (if (eql (+ i 1) slen)
              (progn
                (princ replace outstr)
                (buf-clear buf)
                (setf i 0))
              (incf i))
          (progn
            (buf-flush buf outstr)
            (buf-clear buf)
            (setf i 0))))
    (buf-flush buf outstr)))
                
;; pg's
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((match-char (nth pos old) c)
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                            ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))
         
(defun match-char (a b)
  (cond ((typep a 'character)
         (char= a b))
        ((eql a 'd)
         (digit-char-p b))
        ((eql a 'a)
         (alphanumericp b))
        ((eql a '+)
         t)
        (t
         nil)))
