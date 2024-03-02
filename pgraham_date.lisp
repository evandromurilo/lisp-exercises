(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (read-integer (first toks))
          (read-month   (second toks))
          (read-integer (third toks)))))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))

(defconstant month-names #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun read-month (str)
  (let ((p (position str month-names :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))

(defun read-integer (str)
  (let ((len (length str))
        (val 0))
    (do ((factor 1 (* factor 10))
         (pos (- len 1) (- pos 1)))
        ((< pos 0))
      (incf val (* factor (char-as-int (aref str pos)))))
    val))

(defun char-as-int (char)
  (- (char-code char) (char-code #\0)))
