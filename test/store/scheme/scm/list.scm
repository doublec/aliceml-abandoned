(define length
  (lambda (xs)
    (if (eq? xs ())
	0
	(+ 1 (length (cdr xs))))))

(define nth
  (lambda (n xs)
    (if (= n 1)
	(car xs)
	(nth (- n 1) (cdr xs)))))

(define map
  (lambda (f xs)
    (if (eq? xs ())
	()
	(cons (f (car xs)) (map f (cdr xs))))))

(define foldl
  (lambda (f b xs)
    (if (eq? xs ())
	b
	(foldl f (f (car xs) b) (cdr xs)))))

(define foldr
  (lambda (f b xs)
    (if (eq? xs ())
	b
	(f (car xs) (foldr f b (cdr xs))))))

(define app
  (lambda (xs ys)
    (if (eq? xs ())
	ys
	(cons (car xs) (app (cdr xs) ys)))))

(define rev_iter
  (lambda (xs p)
    (if (eq? xs ())
	(cons p ())
	(let
	    ((cp (rev_iter (cdr xs) (car xs))))
	  (if (eq? p ())
	      cp
	      (cons p cp))))))

(define rev
  (lambda (xs)
    (if (eq? xs ())
	()
	(app (rev (cdr xs)) (cons (car xs) ())))))

(define make_list_iter
  (lambda (n)
    (if (= n 0)
	()
	(cons n (make_list_iter (- n 1))))))

(define make_list
  (lambda (n)
    (rev (make_list_iter n))))

(define xs (cons 1 (cons 2 (cons 3 ()))))

(define ys (cons 4 (cons 5 (cons 6 ()))))
