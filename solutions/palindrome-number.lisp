(defun palindrome-number (intinput)
  (if (< intinput 0)
      nil
    (let (s (princ-to-string intinput))
      (eq s (reverse s)))))
