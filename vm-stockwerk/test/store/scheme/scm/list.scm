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

(define rev
  (lambda (xs)
    (if (eq? xs ())
	()
	(append (rev (cdr xs)) (cons (car xs) ())))))

(define xs (cons 1 (cons 2 (cons 3 ()))))

(define ys (cons 4 (cons 5 (cons 6 ()))))
