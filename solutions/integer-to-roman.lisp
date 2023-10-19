(defun integer-to-roman (intinput &optional (acc ""))
  (cond
    ((> intinput 4000)
     (error "Values larger than 4000 not supported"))
    ((>= intinput 1000)
     (integer-to-roman
      (- intinput 1000)
      (concatenate 'string acc "M")))
    ((>= intinput 900)
     (integer-to-roman
      (- intinput 900)
      (concatenate 'string acc "CM")))
    ((>= intinput 500)
     (integer-to-roman
      (- intinput 500)
      (concatenate 'string acc "D")))
    ((>= intinput 400)
     (integer-to-roman
      (- intinput 400)
      (concatenate 'string acc "CD")))
    ((>= intinput 100)
     (integer-to-roman
      (- intinput 100)
      (concatenate 'string acc "C")))
    ((>= intinput 90)
     (integer-to-roman
      (- intinput 90)
      (concatenate 'string acc "XC")))
    ((>= intinput 50)
     (integer-to-roman
      (- intinput 50)
      (concatenate 'string acc "L")))
    ((>= intinput 40)
     (integer-to-roman
      (- intinput 40)
      (concatenate 'string acc "XL")))
    ((>= intinput 10)
     (integer-to-roman
      (- intinput 10)
      (concatenate 'string acc "X")))
    ((>= intinput 9)
     (integer-to-roman
      (- intinput 9)
      (concatenate 'string acc "IX")))
    ((>= intinput 5)
     (integer-to-roman
      (- intinput 5)
      (concatenate 'string acc "V")))
    ((>= intinput 4)
     (integer-to-roman
      (- intinput 4)
      (concatenate 'string acc "IV")))
    ((>= intinput 1)
     (integer-to-roman
      (- intinput 1)
      (concatenate 'string acc "I")))
    (t acc)))
