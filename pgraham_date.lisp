(setf months (vector "Jan"
                     "Feb"
                     "Mar"
                     "Apr"
                     "May"
                     "Jun"
                     "Jul"
                     "Aug"
                     "Sep"
                     "Oct"
                     "Nov"
                     "Dec"))

(defun parse-date (str)
  (let ((date (split #\  str)))
    (list (str-as-int (first date))
          (month-as-int (second date))
          (str-as-int (third date)))))

(defun split (char str)
  (if (zerop (length str))
      nil
      (let ((pos1 (position char str)))
        (if pos1
            (cons (subseq str 0 pos1)
                  (split char (subseq str (+ 1 pos1))))
            (list str)))))

(defun month-as-int (str)
  (+ 1 (position str months :test #'string-equal)))

(defun str-as-int (str)
  (let ((len (length str))
        (val 0))
    (do ((factor 1 (* factor 10))
         (pos (- len 1) (- pos 1)))
        ((< pos 0))
      (incf val (* factor (char-as-int (aref str pos)))))
    val))

(defun char-as-int (char)
  (- (char-code char) (char-code #\0)))
