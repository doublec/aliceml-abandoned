(setgc 1);
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
