(define nat
  (lambda (n) (cons n (lambda () (nat (+ n 1))))))

(define nats (nat 1))

(define snth
  (lambda (s n)
    (if (= n 1)
	(car s)
	(snth ((cdr s)) (- n 1)))))

(define smap
  (lambda (f s)
    (cons (f (car s))
	  (lambda () (smap f ((cdr s)))))))
