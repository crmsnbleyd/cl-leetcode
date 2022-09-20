(defun two-sum (nums target)
  (let ((hash (make-hash-table)))
     (loop for x in nums and idx from 0
	   do (setf (gethash x hash) (cons idx (gethash x hash))))
     (loop for k being each hash-key of hash
	   do (when (gethash (- target k) hash)
		(unless (and (= target (* k 2)) (not (cdr (gethash k hash))))
		(return-from two-sum (car (gethash (- target k) hash))))))))
