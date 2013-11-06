(setgc 1);
(define iter
  (lambda (n p)
    (if (= n 0)
	()
	(begin
	  (p)
	  (tail iter (- n 1) p)))));
(define make_list_iter
  (lambda (n xs)
    (if (= n 0)
	xs
	(tail make_list_iter (- n 1) (cons n xs)))));
(define make_list
  (lambda (n)
    (tail make_list_iter n ())));
(define b_list
  (lambda ()
    (time
     (lambda ()
       (begin
	 (make_list 1000000)
	 (killtop))))));
(iter 10 b_list);
(exit);
