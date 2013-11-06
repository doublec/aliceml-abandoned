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
(define imap
  (lambda (f xs)
    (if (eq? xs ())
	()
	(begin
	  (set_car! xs (f (car xs)))
	  (tail imap f (cdr xs))))));
(define b_imap
  (lambda ()
    (time
     (lambda ()
       (begin
	 (imap (lambda (n) (cons  n ())) (make_list 5000))
	 (killtop))))));
(iter 10 b_imap);
(exit);
