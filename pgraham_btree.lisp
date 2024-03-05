(defstruct node obj left right)

(defun bst-insert (node obj order-fn)
  (let ((new ))
    (if (funcall order-fn (node-obj node) obj)
        (if (null (node-right node))
            (setf (node-right node) (make-node :obj obj))
            (bst-insert (node-right node) obj order-fn)))
    (if (null (node-left node))
        (setf (node-left node) (make-node :obj obj))
        (bst-insert (node-left node) obj order-fn))))

