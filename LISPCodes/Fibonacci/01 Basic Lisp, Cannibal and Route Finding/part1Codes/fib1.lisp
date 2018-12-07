(defun fibonacci1 (N)
		 (if (or (zerop N) (= N 1))
			1	
			 (+ (fibonacci (- N 1)) (fibonacci (- N 2)))))
