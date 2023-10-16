(defconstant min-integer-string "-2147483648")
(defconstant max-integer-string "+2147483647")

(defun atoi (strinput)
  (read-from-string (clamp (sign-and-digits strinput))))

(defun remove-leading-whitespace (strinput)
  (string-left-trim '(#\Space #\TAB #\Newline) strinput))

(defun sign-and-digits (strinput)
  (let*
      ((trimmed (remove-leading-whitespace strinput))
       (sign-maybe (aref trimmed 0))
       (sign (cond
	       ((char= sign-maybe #\+) #\+)
	       ((char= sign-maybe #\-) #\-)
	       (t #\+)))
       (rest (string-left-trim "+-0" trimmed)))
    (cons sign
    (loop :for ch :across rest
	  :while (digit-char-p ch)
	  :collect ch))))

(defun clamp (normalised-list)
  (let* ((sign (car normalised-list))
	 (digits (cdr normalised-list))
	 (comparing-to
	   (string-left-trim
	    "+-"
	    (if
	     (char= sign #\+)
	     max-integer-string
	     min-integer-string)))
	 (diglen (length digits))
	 (complen (length comparing-to)))
    (cond
      ((< diglen complen)
       (concatenate 'string normalised-list))
      ((> diglen complen)
       (concatenate 'string `(,sign) comparing-to))
      (t
       (loop :for digit :in digits
	     :for c :across comparing-to
	     :when (char> digit c)
	       :return (concatenate 'string `(,sign) comparing-to)
	     :when (char< digit c)
	       :return (concatenate 'string normalised-list)
	     :finally
		(concatenate 'string `(,sign) comparing-to))))))
