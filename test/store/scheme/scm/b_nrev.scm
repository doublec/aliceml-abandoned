(setgc 1);
(define make_list_iter
  (lambda (n xs)
    (if (= n 0)
	xs
	(tail make_list_iter (- n 1) (cons n xs)))));
(define make_list
  (lambda (n)
    (tail make_list_iter n ())));
(define app
  (lambda (xs ys)
    (if (eq? xs ())
	ys
	(cons (car xs) (app (cdr xs) ys)))));
(define nrev
  (lambda (xs)
    (if (eq? xs ())
	()
	(app (nrev (cdr xs)) (cons (car xs) ())))));
(define b_nrev
  (lambda ()
    (time
     (lambda ()
       (begin
	 (nrev (make_list 3000))
	 (killtop))))));
(iter 10 b_nrev);
(exit);
