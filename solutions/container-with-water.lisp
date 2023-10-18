(defun container-with-water (bars)
  "find the maximum amount of water that can be
fit between any two bars in the vector BARS."
  (loop :with start = 0
	:with end = (1- (length bars))
	:with curmax = 0
	:while (<= start end)
	:do
	   (setf curmax
		 (max curmax
		      (* (- end start)
			 (min (aref bars start)
			      (aref bars end)))))
	:if (< (aref bars start)
	       (aref bars end))
	  :do (setf start (1+ start))
	:else
	  :do (setf end (1- end))
	:finally
	   (return curmax)))
