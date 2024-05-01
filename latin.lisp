(defun declinar (raiz caso num decl)
  (find (list num caso raiz)
        (funcall decl raiz)
        :key #'(lambda (x) (subseq x 0 3))
        :test #'equal))


(defun declinacao (raiz s-nom s-voc s-gen s-dat s-abl s-ac
                      p-nom p-voc p-gen p-dat p-abl p-ac)
  `((sing nom ,raiz ,s-nom)
    (sing voc ,raiz ,s-voc)
    (sing gen ,raiz ,s-gen)
    (sing dat ,raiz ,s-dat)
    (sing abl ,raiz ,s-abl)
    (sing ac  ,raiz ,s-ac)
    (plur nom ,raiz ,p-nom)
    (plur voc ,raiz ,p-voc)
    (plur gen ,raiz ,p-gen)
    (plur dat ,raiz ,p-dat)
    (plur abl ,raiz ,p-abl)
    (plur ac  ,raiz ,p-ac)))

(defun 1-declinacao (raiz)
  (declinacao raiz 'a  'a  'ae   'ae 'a  'am
                   'ae 'ae 'arum 'is 'is 'as))
                 
(defun 2-declinacao-m (raiz)
  (declinacao raiz 'us 'e 'i    'o  'o  'um
                   'i  'i 'orum 'is 'is 'os))

(defun 2-declinacao-n (raiz)
  (declinacao raiz 'um 'um 'i    'o  'o  'um
                   'a  'a  'orum 'is 'is 'a))
