(defun two-sum (nums target)
  (let ((hash (make-hash-table))
	(iter-hash-item nil))
    (loop for x in nums and idx from 0
	  do (setf (gethash x hash) (cons idx (gethash x hash))))
    (loop for k being each hash-key of hash
	  do (when (setf iter-hash-item (gethash (- target k) hash))
	       (unless (and (= target (* k 2)) (not (cdr iter-hash-item)))
		 (return-from two-sum (car iter-hash-item)))))))
