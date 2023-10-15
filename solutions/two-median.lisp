(defun median (vec1 vec2)
  "vec1 is a longer vector than vec2"
  (let ((n1 (length vec1))
	(n2 (length vec2))
	(low 0)
	(high (length vec1))
	(idx1 nil)
	(idx2 nil)
	(left1 nil)
	(left2 nil)
	(right1 nil)
	(right2 nil))
    (loop :while (<= low high)
	  :do
	     (setf idx1 (floor (+ low high) 2))
	     (setf idx2 (- (floor (+ n1 n2 1) 2) idx1))
	     (setf left1 (if (zerop idx1)
			     most-negative-fixnum
			     (aref vec1 (- idx1 1))))
	     (setf left2 (if (zerop idx2)
			     most-negative-fixnum
			     (aref vec2 (- idx2 1))))
	     (setf right1 (if (= n1 idx1)
			      most-positive-fixnum
			      (aref vec1 idx1)))
	     (setf right2 (if (= n2 idx2)
			      most-positive-fixnum
			      (aref vec2 idx2)))
	     (cond
	       ((and (<= left1 right2) (<= left2 right1))
		(return-from solve
		  (if (zerop (mod (+ n1 n2) 2))
		      (/ (+ (max left1 left2) (min right1 right2)) 2)
		      (max left1 left2)))
		(> left1 right2)
		(setf high (- idx1 1))
		t (setf low (+ idx1 1))))))
  0.0)

(defun find-median (vec1 vec2)
  (if (< (length vec1) (length vec2))
      (median vec2 vec1)
      (median vec1 vec2)))
