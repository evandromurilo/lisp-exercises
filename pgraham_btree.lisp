(defstruct node obj left right)

(defun bst-insert (obj bst order-fn)
  (if (null bst)
      (make-node :obj obj)
      (if (funcall order-fn obj (node-obj bst))
          (make-node :obj (node-obj bst)
                     :right (node-right bst)
                     :left (bst-insert obj
                                       (node-left bst)
                                       order-fn))
          (make-node :obj (node-obj bst)
                     :left (node-left bst)
                     :right (bst-insert obj
                                        (node-right bst)
                                        order-fn)))))

(defun bst-member (obj bst order-fn)
  (if (null bst)
      nil
      (if (equal obj (node-obj bst))
          bst
          (if (funcall order-fn obj (node-obj bst))
              (bst-member obj (node-left bst) order-fn)
              (bst-member obj (node-right bst) order-fn)))))
