(defun PREFIX-EXP(a)
	(cond
	((numberp a) a)
	((< (length a) 3) nil)
		((let((first (car a))
			(sign (cadr a))
			(second (caddr a)))
				(list sign (prefix-exp first) (prefix-exp second))))))