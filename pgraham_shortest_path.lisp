;; Network shortest-path example from chapter 3

(setf minnet '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
    (bfs end (list (list start)) net))

(defun bfs (end queue net)
  "Breadth-first search"
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (let ((nps (new-paths path node net)))
                (if (member end nps :key #'car)
                    (reverse (cons end path))
                    (bfs end
                         (append (cdr queue) nps)
                         net))))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
