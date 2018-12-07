(defun fibonacci3(item)
  (do ((p 0 (1+ p))
       (curr 0 next)
       (next 1 (+ curr next)))
    ((= item p) curr)))