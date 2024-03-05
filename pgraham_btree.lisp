(defstruct node obj left right)

(defun bst-insert (obj bst order-fn)
  (insert-node (make-node :obj obj) bst order-fn))

(defun insert-node (node bst order-fn)
  (if (funcall order-fn (node-obj bst) (node-obj node))
      (if (null (node-right bst))
          (setf (node-right bst) node)
          (insert-node node (node-right bst) order-fn))
      (if (null (node-left bst))
          (setf (node-left bst) node)
          (insert-node node (node-left bst) order-fn))))

