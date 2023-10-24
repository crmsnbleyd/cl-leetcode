(defun longest-substring (s)
  (loop :with start = 0
	:with res = ""
	:with seen = (make-hash-table)
	:for i :below (length s)
	:if (gethash (char s i) seen)
	  :do
	     (loop
	       :for curr :from start :below i
	       :while (char/= (char s curr) (char s i))
	       :do (remhash (char s curr) seen)
	       :finally (setf start (1+ curr)))
	:else
	  :do (setf (gethash (char s i) seen) t)
	:when (> (1+ (- i start)) (length res))
	   :do (setf res (subseq s start (1+ i)))
	:finally (return res)))
