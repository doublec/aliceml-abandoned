(define nat
  (lambda (n) (cons n (lambda () (nat (+ n 1))))))

(define nats (nat 1))

(define snth
  (lambda (s n)
    (if (= n 1)
	(car s)
	(tail snth ((cdr s)) (- n 1)))))

(define smap
  (lambda (f s)
    (cons (f (car s))
	  (lambda () (smap f ((cdr s)))))))

(define ts (lambda () (show (snth nats 256))))
(define test0 (lambda () (show (snth nats 8192))))
(define test1 (lambda () (show (snth nats 16384))))

(define for
  (lambda (i n s f)
    (if (< i n)
	(begin
	  (f)
	  (for (+ i s) n s f))
	(skip))))
