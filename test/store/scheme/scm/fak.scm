(define fak (lambda (n)
	      (if (< n 2)
		  1
		  (* n (fak (- n 1))))))

(define tfak
  (lambda (n a)
    (if (< n 2)
	a
	(tail tfak (- n 1) (* n a)))))
