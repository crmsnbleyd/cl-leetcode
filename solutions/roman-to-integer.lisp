(defun string-prefix? (prefix str)
  (if (< (length str) (length prefix))
      nil
      (loop :for pchar :across prefix
	    :for schar :across str
	    :when (char/= pchar schar)
	      :return nil
	    :finally (return t))))

(defun roman-to-integer (roman-string)
  (loop :with sum = 0
	:with str = roman-string
	:do
	   (loop :for (prefix . value)
		   :in
		 '(("IV" . 4)
		   ("IX" . 9)
		   ("XL" . 40)
		   ("XC" . 90)
		   ("CD" . 400)
		   ("CM" . 900)
		   ("I"  . 1)
		   ("V"  . 5)
		   ("X"  . 10)
		   ("L"  . 50)
		   ("C"  . 100)
		   ("D"  . 500)
		   ("M"  . 1000))
		 :when (string-prefix? prefix str)
		   :do (setf sum (+ sum value))
		       (setf str (subseq str (length prefix)))
		       (return))
	:when (string= str "")
	  :return sum))
