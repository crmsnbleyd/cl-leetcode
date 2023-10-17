(defun small-matcher (expr s)
  (cond
    ((string= s expr)
     t)
    ((string= expr "")
     nil)
    ((and (string= s "")
	  (string= expr ".*"))
     t)
    ((string= s "")
     nil)
    ((and (= (length s) 1) (string= expr "."))
     t)
    ((= (length expr) 1)
     nil)
    (t :continue)))

(defun is-match (expr s)
  (let ((matchres (small-matcher expr s)))
    (if
     (not (eq matchres :continue))
     matchres
     (if
      (char/= #\* (aref expr 1))
      (if (or
	   (char= (aref expr 0) #\.)
	   (char= (aref expr 0) (aref s 0)))
	  (is-match
	   (subseq expr 1)
	   (subseq s 1))
	  nil)
      (if
       (or
	(char= (aref expr 0) #\.)
	(char= (aref expr 0)
	       (aref s 0)))
       (or
	(is-match expr
		  (subseq s 1))
	(is-match (subseq expr 2)
		  (subseq s 1)))
       (is-match (subseq expr 2) s))))))
