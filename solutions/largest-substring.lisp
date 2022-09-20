(defun longest-substr (s)
  (let ((longest 0)
	(curr 0)
	(start 0)
	(seen (make-hash-table))
	(len (length s)))
    (loop for idx from 0 to (- len 1) do
      (if (gethash (char s idx) seen)
	  (progn
	   (setf longest (max longest curr))
	   (loop while (char/= (char s start) (char s idx)) do
	     (remhash (char s start) seen)
	     (incf start))
	    (incf start)
	    (setf curr (+ 1 (- idx start))))
	  (progn
	   (setf (gethash (char s idx) seen) t)
	   (incf curr))))
    (setf longest (max longest curr))
    longest))
