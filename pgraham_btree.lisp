(defstruct node elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :r (node-r bst)
                 :l (bst-insert obj (node-l bst) <))
                (make-node
                 :elt elt
                 :l (node-l bst)
                 :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (equal obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))
