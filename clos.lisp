(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "The foundation of all locks."))

(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "A lock that is either free or busy."))

(defclass null-lock (lock)
  ()
  (:documentation "A lock that is always free."))

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

(defgeneric seize (lock)
  (:documentation
   "Seizes the lock.
Returns the lock when the operation succeeds.
Some locks simply wait until they can succeed, while
other locks return NIL if they fail."))

(defgeneric release (lock &optional failure-mode)
  (:documentation
   "Releases the lock if it is currently owned y this process.
Retunrs T if the operation succeeds.
If unsuccessful and failure-mode is :no-error, returns NIL.
If unsuccessful and failure-mode is :error, signals an error.
The default for failure-mode is :no-error."))

(defmethod seize ((l null-lock))
  l) ; return lock, no waiting

(defmethod release ((l null-lock) &optional failure-mode)
  (declare (ignore failure-mode)) ; never fails for null
  t)
