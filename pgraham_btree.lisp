(defstruct node obj left right)

(defun bst-insert (obj bst order-fn)
  (if (funcall order-fn (node-obj bst) obj)
      (if (null (node-right bst))
          (setf (node-right bst) (make-node :obj obj))
          (bst-insert obj (node-right bst) order-fn))
      (if (null (node-left bst))
          (setf (node-left bst) (make-node :obj obj))
          (bst-insert obj (node-left bst) order-fn))))

