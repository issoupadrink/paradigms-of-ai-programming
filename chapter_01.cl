(defun power (x n)
    "Power raises x to the nth power."
    (cond ((= n 0) 1) 
          ((= n 1) x)
          (t (* x (power x (- n 1))))))

(defun count-atoms (x &optional (count 0))
    "Return the number of atoms in a list. The list must
     not contain nil values."
    (cond ((eq (car x) nil) count)
          (t (count-atoms (cdr x) (+ count 1)))))

(defun dot-product (l j)
    "Compute the dot product of two vectors."
    (apply #'+ (mapcar #'* l j)))
