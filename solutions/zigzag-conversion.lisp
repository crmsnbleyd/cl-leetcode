(defun solve (strinput)
  (let ((bucket0 (list))
	(bucket1 (list))
	(bucket2 (list))
	(counter 0)
	(fn '+))
    (loop :for c :across strinput
	  :do
	     (cond
	       ((= counter 0)
		(setf bucket0 (cons c bucket0)))
	       ((= counter 1)
		(setf bucket1 (cons c bucket1)))
	       ((= counter 2)
		(setf bucket2 (cons c bucket2))))
	     (cond
	       ((and (zerop counter) (eq '- fn))
		(setf fn '+))
	       ((and (= 2 counter) (eq '+ fn))
		(setf fn '-)))
	     (setf counter (funcall fn counter 1)))
    (solver-join bucket0 bucket1 bucket2)))

(defun solver-join (&rest lists)
  (apply 'concatenate 'string
	 (map
	  'list
	  (lambda (l) (concatenate 'string (reverse l)))
	  lists)))
