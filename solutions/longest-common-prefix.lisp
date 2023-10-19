(defun min-length (strings)
  (reduce
   (lambda (a b) (min a (length b)))
   strings
   :initial-value most-positive-fixnum))

(defun longest-common-prefix (strings)
  (cond
    ((eq strings nil)
     nil)
    ((= (length strings) 1)
     (car strings))
    (t
     (loop
       :with res = (min-length strings)
       :for end :from (1- res) :downto 0
       :for compchar = (char (car strings) end)
       :unless
       (loop :for str :in strings
	     :always
	     (char=
	      (char str end)
	      compchar))
       :do (setf res end)
       :finally
	  (return
	    (subseq (car strings) 0 res))))))
