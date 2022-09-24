(defun longest-palindrome (s)
(let* ((n (length s))
       (dp (make-array (list n n) :initial-element nil))
       (retval 0))
  (loop for i from 0 to (1- n) do
    (setf (aref dp i i) t)
    (unless (= i (1- n))
      (setf (aref dp i (1+ i))
	  (if
	   (char= (char s i) (char s (1+ i)))
	   t nil))))
  (loop for i from 0 to (1- n) do
    (loop for j from i to (1- n) do
	  (when (or (aref dp i j)
		 (and
		  (aref dp (1+ i) (1- j))
		  (char= (char s i) (char s j))))
	       (setf (aref dp i j) t)
	       (setf retval (max retval (1+ (- j i)))))))
retval))
