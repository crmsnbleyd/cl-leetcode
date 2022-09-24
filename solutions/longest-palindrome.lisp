(defun longest-palindrome (s)
  (let* ((n (length s))
	 (dp (make-array (list n n) :initial-element nil))
	 (start 0)
	 (retval 1))
    (when (zerop n) (return-from longest-palindrome ""))
    (loop for i from 0 to (1- n) do
      (setf (aref dp i i) t)
      (unless (= i (1- n))
	(setf (aref dp i (1+ i))
	      (when
		  (char= (char s i) (char s (1+ i)))
		(setf start i)
		(setf retval 2)
		t))))
    (loop with k = 3
	  while (<= k n)
	  do
	     (loop with i = 0
		   while (< i (1+ (- n k)))
		   for j = (1- (+ i k))
		   do
		      (when
			  (and (aref dp (1+ i) (1- j))
			       (char= (char s i) (char s j)))
			(setf (aref dp i j) t)
			(when (> k retval)
			  (setf start i)
			  (setf retval k)))
		      (incf i))
	     (incf k))
    (subseq s start (+ start retval))))
