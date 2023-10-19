(defun zigzag (strinput)
  (let ((bucket0 (list))
	(bucket1 (list))
	(bucket2 (list)))
    (loop :for c :across strinput
	  :for counter :in '#1=(0 1 2 1 . #1#)
	  :do
	     (cond
	       ((= counter 0)
		(setf bucket0 (cons c bucket0)))
	       ((= counter 1)
		(setf bucket1 (cons c bucket1)))
	       ((= counter 2)
		(setf bucket2 (cons c bucket2)))))
    (reverse-join bucket0 bucket1 bucket2)))

(defun reverse-join (&rest lists)
  (apply 'concatenate 'string
	 (map
	  'list
	  (lambda (l) (concatenate 'string (reverse l)))
	  lists)))
