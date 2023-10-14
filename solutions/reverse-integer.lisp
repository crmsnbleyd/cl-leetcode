(defun reverse-integer (input)
 (let ((tentative-result (tentative-solve input)))
    (if (validate-solution tentative-result)
	tentative-result
	"-1")))

(defvar outer-limit
    (let ((maxhalf (expt 2 31)))
      (+ maxhalf (1- maxhalf))))

(defun validate-solution (strinput)
  (if (eq strinput (actually-solve outer-limit))
      nil
      (let ((inp (reverse (string-left-trim "-" strinput))))
	(cond
	  ((> (length inp) 10) nil)
	  ((< (length inp) 10) t)
          (t
	   (loop :for ichar :across inp
		 :for l :across (princ-to-string outer-limit)
		 :do
		    (cond
		      ((char> ichar l)
		       (return-from validate-solution nil))
		      ((char< ichar l)
		       (return-from validate-solution t)))
		 :finally
		    (return t)))))))

(defun tentative-solve (numinput)
  (if (> 0 numinput)
      (concatenate 'string "-"
	      (actually-solve (* -1 numinput)))
      (actually-solve numinput)))

(defun actually-solve (numinput)
  (reverse (princ-to-string numinput)))
