(defun three-sum-closest (vec target)
  (solution-reduce
   target
   (loop
     :for i :below (- (length vec) 2)
     :with ordered-vec = (sort vec '<)
     :collect
     (loop
       :with mindiff = most-positive-fixnum
       :with ret = most-positive-fixnum
       :with bot = (1+ i)
       :with top = (1- (length vec))
       :while (< bot top)
       :for
       currsum = (+ (aref ordered-vec i)
		    (aref ordered-vec bot)
		    (aref ordered-vec top))
       :for diff = (- target currsum)
       :when (= diff 0)
	 :do (return-from three-sum-closest currsum)
       :when (> mindiff (abs diff))
	 :do
	    (setf ret currsum)
	    (setf mindiff (abs diff))
       :if (> diff 0)
	 :do (incf bot)
       :else
	 :do (decf top)
       :finally (return ret)))))

(defun solution-reduce (target l)
  "Find integer with smallest difference to target."
  (reduce
   (lambda (a b)
     (if (< (abs (- target a))
	    (abs (- target b)))
	 a b))
   l :initial-value most-positive-fixnum))
