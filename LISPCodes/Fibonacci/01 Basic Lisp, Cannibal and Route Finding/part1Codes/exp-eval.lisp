(defun exp-eval (exp)
	(cond ((numberp exp) exp)
		(t (funcall (second exp) 	
			(exp-eval (first exp)) 
			(exp-eval (third exp))))))
