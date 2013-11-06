(setgc 1);
(define iter
  (lambda (n p)
    (if (= n 0)
	()
	(begin
	  (p)
	  (tail iter (- n 1) p)))));
(define fib
  (lambda (n)
    (if (= n 0)
	0
	(if (= n 1)
	    1
	    (+ (fib (- n 1)) (fib (- n 2)))))));
(define b_fib
  (lambda ()
    (time
     (lambda ()
       (show (fib 31))))));
(iter 10 b_fib);
(exit);
