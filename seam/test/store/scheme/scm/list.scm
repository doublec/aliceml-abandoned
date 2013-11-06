(define length
  (lambda (xs)
    (if (eq? xs ())
	0
	(+ 1 (length (cdr xs))))))

(define nth
  (lambda (n xs)
    (if (= n 1)
	(car xs)
	(tail nth (- n 1) (cdr xs)))))

(define map
  (lambda (f xs)
    (if (eq? xs ())
	()
	(cons (f (car xs)) (map f (cdr xs))))))

(define imap
  (lambda (f xs)
    (if (eq? xs ())
	()
	(begin
	  (set_car! xs (f (car xs)))
	  (tail imap f (cdr xs))))))

(define foldl
  (lambda (f b xs)
    (if (eq? xs ())
	b
	(tail foldl f (f (car xs) b) (cdr xs)))))

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
  (lambda (xs ys)
    (if (eq? xs ())
	ys
	(tail rev_iter (cdr xs) (cons (car xs) ys)))))

(define rev
  (lambda (xs)
    (tail rev_iter xs ())))

(define make_list_iter
  (lambda (n xs)
    (if (= n 0)
	xs
	(tail make_list_iter (- n 1) (cons n xs))))) 

(define make_list
  (lambda (n)
    (tail make_list_iter n ())))

(define xs (cons 1 (cons 2 (cons 3 ()))))

(define ys (cons 4 (cons 5 (cons 6 ()))))

(define iter
  (lambda (n p)
    (if (= n 0)
	()
	(begin
	  (p)
	  (tail iter (- n 1) p)))))
