(defun add-numbers (a b)
  "a and b are linked lists representing
two numbers, where every cons cell has
a single digit, and the number is
reversed."
  (let
      ((n (min (length a) (length b)))
       (carry 0)
       (retlist nil))
    (loop for i from 0 to (1- n)
	  for x = a then (cdr x)
	  for y = b then (cdr y)
	  do
	     (setf retlist
		   (cons (mod (+ carry (car a) (car b)) 10) retlist))
	     (setf carry (max 0 (- (+ carry (car a) (car b)) 10)))))
  retlist)
