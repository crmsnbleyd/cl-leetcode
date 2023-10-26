(defun fill-number-letter-map ()
  (let
      ((number-letter-map (make-hash-table)))
    (setf (gethash  #\2 number-letter-map) "abc")
    (setf (gethash  #\3 number-letter-map) "def")
    (setf (gethash  #\4 number-letter-map) "ghi")
    (setf (gethash  #\5 number-letter-map) "jkl")
    (setf (gethash  #\6 number-letter-map) "mno")
    (setf (gethash  #\7 number-letter-map) "pqrs")
    (setf (gethash  #\8 number-letter-map) "tuv")
    (setf (gethash  #\9 number-letter-map) "wxyz")
    number-letter-map))

(defun letter-combinations (strinput)
  (let ((number-letter-map (fill-number-letter-map)))
    (reduce
     (lambda (prev-letter-combinations numchar)
       (mapcan
	(lambda (ch)
	  (map
	   'list
	   (lambda (prev-list-item)
	     (concatenate 'string prev-list-item (string ch)))
	   prev-letter-combinations))
	(map 'list 'identity (gethash numchar number-letter-map))))
     strinput :initial-value (list (list)))))
