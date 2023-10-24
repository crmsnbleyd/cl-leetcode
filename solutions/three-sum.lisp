(defun unique-list (l &key (test 'eql))
  "Return all unique elements of list L."
  (loop
    :with res = (make-hash-table :test test)
    :for item :in l
    :unless (gethash item res)
      :collect item
    :do (setf (gethash item res) t)))

(defun three-sum (vec target)
  "Generate list of all unique triplets from VEC that sum
up to TARGET."
  (let ((dp (make-hash-table)))
    (loop :for i :below (1- (length vec))
	  :do
	     (loop :for j :from (1+ i)
		     :below (length vec)
		   :for idash = (aref vec i)
		   :for jdash = (aref vec j)
		   :for currsum = (+ idash jdash)
		   :do
		      (setf
		       (gethash currsum dp)
		       (if
			(eq nil (gethash currsum dp))
			(list (list i j))
			(cons (list i j)
			      (gethash currsum dp))))))
    (unique-list
     (loop :for i :below (length vec)
	   :for item = (aref vec i)
	   :for target-list = (gethash (- target item) dp)
	   :collect
	   (loop :for pair :in target-list
		 :unless (member i pair)
		   :return (sort (cons i pair) '<)))
     :test 'equal)))
