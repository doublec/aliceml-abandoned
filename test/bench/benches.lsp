;; maximal speed:
(declaim (optimize (speed 3) (safety 1) (space 0) (debug 0)))

(defun genlistaux(n m acc)
 (if (zerop n) 
     acc
   (genlistaux (1- n) (1+ m) (cons m acc))))

(defun genlist(n)
  (genlistaux n 1 nil))


(defun myrandom(n)
  (+ (mod (+ (* n 25) 1345) 10000)
     (mod (+ (* n 713)  1345) 100000)))


(defun randlistaux(n old l)
    (if (= n 0)
	l
      (let ((aux (myrandom old)))
	(randlistaux (- n 1) aux (cons aux l)))))


(defun randlist(n)
  (randlistaux n 0 nil))


(defun app (l ys)
  (if (null l)
      ys
    (cons (car l) (app (cdr l) ys))))



(defun mydotimes (n p)
  (if (zerop n)
      0
    (progn 
      (funcall p)
      (mydotimes (1- n) p))))


(defun take-time(proc) 
  (let ((t1 (get-internal-run-time)))
    (funcall proc)
    (truncate (/ (* 1000 (- (get-internal-run-time) t1))
		 internal-time-units-per-second))))


(defun dobench(proc)
  (take-time proc))


(defun dobenchn(n proc)
    (if (zerop n)
	nil
      (cons (dobench proc)
	    (dobenchn (1- n) proc))))

(defun sum(l res)
  (if (null l) 
      res
    (sum (cdr l) (+ res (car l)))))



(defun avrg(l len) 
  (truncate (/ (sum l 0) len)))

(defun dobenchavrg(n proc)
    (let ((aux (dobenchn n proc)))
      (list (avrg aux n) aux)))

(defun dobenchavrg2(name n size proc1 proc2)
  (let ((aux1 (dobenchavrg n proc1))
	(aux2 (dobenchavrg n proc2)))
    (list name 
	  size
	  (- (car aux2) (car aux1))
	  (cdr aux1)
	  (cdr aux2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fib(n)
  (if (< 2 n)
      (+ (fib (- n 2))
	 (fib (- n 1)))
    1))

(defun fibf(n)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
;  (declare (double-float n))
  (if (< 2.0 n)
      (+ (fibf (- n 2.0))
	 (fibf (- n 1.0)))
    1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tak(x y z) 
   (if (< y x)
       (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))
     z))

(defun cpstakaux(x y z k)
  (if (< y x)
      (cpstakaux (- x 1)
	   y
	   z
	   #'(lambda (v1)
	     (cpstakaux (- y 1)
		  z
		  x
		  #'(lambda (v2)
		    (cpstakaux (- z 1)
			 x
			 y
			 #'(lambda (v3)
			   (cpstakaux v1 v2 v3 k)))))))
    (funcall k z)))



(defun cpstak(x y z)
  (cpstakaux x y z #'(lambda (a) a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nrev(l)
  (if (null l)
      nil
    (app (nrev (cdr l)) (list (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quickaux(l cont)
  (if (null l)
      cont
    (partition (cdr l) (car l) nil nil cont)))

(defun partition (l a left right cont)
  (if (null l)
      (quickaux left (cons a (quickaux right cont)))
    (let ((x (car l)))
      (if (< x a)
	  (partition (cdr l) a (cons x left) right cont)
	(partition (cdr l) a left (cons x right) cont)))))

(defun quick(l)
  (quickaux l nil))



(defun quickauxho(l cont cmp)
  (if (null l)
      cont
    (partitionho (cdr l) (car l) nil nil cont cmp)))

(defun partitionho(l a left right cont cmp)
  (if (null l)
      (quickauxho left (cons a (quickauxho right cont cmp)) cmp)
    (let ((x (car l)))
      (if (funcall cmp x a)
	  (partitionho (cdr l) a (cons x left) right cont cmp)
	(partitionho (cdr l) a left (cons x right) cont cmp)))))

(defun less(x y) (< x y))

(defun quickho(l cmp)
  (quickauxho l nil cmp))




(defun partitionarray1(ar pivot pindex from to)
  (if (<= from to)
      (let ((old (aref ar from)))
	(if (> pivot old)
	    (progn
	      (setf (aref ar from) (aref ar pindex))
	      (setf (aref ar pindex) old)
	      (partitionarray1 ar pivot (+ pindex 1) (+ from 1) to))
	  (partitionarray1 ar pivot pindex (+ from 1) to)))
    (- pindex 1)))

(defun partitionarray (ar low high)
    (let ((pivot (aref ar low)))
      (let ((mid (partitionarray1 ar pivot (+ low 1) (+ low 1) high)))
	(setf (aref ar low) (aref ar mid))
	(setf (aref ar mid) pivot)
	mid)))
	
(defun quickarray1(ar low high)
   (if (< low high)
       (let ((mid (partitionarray ar low high)))
	 (quickarray1 ar low (- mid 1))
	 (quickarray1 ar (+ mid 1) high))
     ar))

(defun quickarray(ar)
  (quickarray1 ar 0 (- (length ar) 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dofoldl (in z to c)
  (if (null in)
      z
    (dofoldl (cdr in) (lft z c (car in) 1 to) to c)))


(defun loopforthread (c to in)
   (if (<= c to)
       (loopforthread (1+ c) to (dofoldl in nil to c))
     in))

(defun queens (n)
   (loopforthread 1 n '(())))


(defun noattak (xs c y)
   (noattak1 xs c y 1))

(defun noattak1 (xs c y i)
   (if (null xs)
      t
   (let ((x  (car xs))
	 (xr (cdr xs)))
      (and (/= x y)
	   (/= (abs (- x y)) (- c i))
	   (noattak1 xr c y (1+ i))))))


(defun lft (ss c xs y n)
  (if (> y n)
      ss
    (if (noattak xs c y)
	(lft (cons (app xs (list y)) ss) c xs (1+ y) n)
      (lft ss c xs (1+ y) n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant x_base  -2.0)
(defconstant y_base  1.25)
(defconstant side    2.5)

(defconstant sz 800)
(defconstant maxCount 1024)
    
(defconstant delta (/ side (double-float sz)))

(defun mandelloop3(count z_re z_im c_re c_im)
  (declare (double-float z_re z_im c_re c_im) (fixnum count))
  (if (< count maxCount)
      (let ((z_re_sq (* z_re z_re))
	    (z_im_sq (* z_im z_im)))
	(if (> (+ z_re_sq z_im_sq) 4.0)
	    count
	  (let ((z_re_im (* z_re z_im)))
	    (mandelloop3 (+ 1 count)
			 (+ (- z_re_sq z_im_sq) c_re)
			 (+ z_re_im z_re_im c_im)
			 c_re c_im))))
    count))


(defun mandelloop2(j c_im iter)
  (if (>= j sz) 
      iter
    (let* ((c_re (* x_base (+ delta  (double-float j))))
	   (count (mandelloop3 0 c_re c_im c_re c_im)))
      (mandelloop2 (+ j 1) c_im (+ iter count)))))

(defun mandelloop1(i iter) 
  (if (>= i sz) 
      iter
    (let ((c_im (- y_base (* delta (double-float i)))))
      (mandelloop1 (+ i 1) (mandelloop2 0 c_im iter)))))


(defun mandelloop()
  (mandelloop1 0 0))

(defun mandeliter()
  (declare (double-float delta x_base y_base side))
  (declare (optimize (speed 3) (safety 1)))
  (let ((sum_iterations 0)
	k)
    (declare (fixnum k sum_iterations))
    (dotimes (i sz)
      (declare (fixnum i))
      (let ((c_im (- y_base (* delta (double-float i)))))
	(declare (double-float c_im))
	(dotimes (j sz)
		 (declare (fixnum j))
		 (let ((c_re (* x_base (+ delta (double-float j))))
		       (z_re)
		       (z_im c_im))
		   (declare (double-float z_re z_im c_re))
		   (setq z_re c_re)
		   (setq k maxCount)
		   (dotimes (count maxCount)
			    (declare (fixnum count))
			    (let ((z_re_sq (* z_re z_re))
				  (z_im_sq (* z_im z_im)))
			      (declare (double-float z_re_sq z_im_sq))
			      (if (> (+ z_re_sq z_im_sq) 4.0)
				  (progn (setq k count)
					 (setq count maxCount))
				(let ((z_re_im (* z_re z_im)))
				  (setq z_re (+ (- z_re_sq z_im_sq) c_re))
				  (setq z_im (+ z_re_im (+ z_re_im c_im)))))))
		   (incf sum_iterations k)))))
    sum_iterations))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun deriv(exp x)
   (case (car exp)
     ((var) (if (eq (cadr exp) x) '(const 1) '(const 0)))
     ((const) '(const 0))
     ((plus)  (list 'plus (deriv (cadr exp) x) (deriv (caddr exp) x)))
     ((minus) (list 'minus (deriv (cadr exp) x) (deriv (caddr exp) x)))
     ((times) (let ((u (cadr exp))
		    (v (caddr exp)))
		(list 'plus 
		      (list 'times (deriv u x) v) 
		      (list 'times u (deriv v x)))))
     ((div) (let ((u (cadr exp))
		  (v (caddr exp)))
	      (list 'div 
		    (list 'minus 
			  (list 'times (deriv u x) v)
			  (list 'times u (deriv v x)))
		    (list 'exp v 2))))
     ((exp) (let ((u (cadr exp))
		  (n (caddr exp)))
	      (list 'times 
		    (list 'times (deriv u x) (list 'const n))
		    (list 'exp u (- n 1)))))
     ((uminus) (list 'uminus (deriv (car exp) x)))
     ((log) (let ((u (car exp))) (list 'div (deriv u x) u)))))

(defun nthderiv(exp x n)
  (if (= n 0)
      exp
      (nthderiv (deriv exp x) x (- n 1))))

(defun goderiv(n)
   (mydotimes n #'(lambda() (nthderiv '(exp (div (const 1) (var x)) 3) 'x 6))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getit(bench n)
  (case bench
	((fib)  #'(lambda () (fib n)))
	((fibf) #'(lambda () (fibf (double-float n))))
	((tak)  #'(lambda () (tak (* 3 n) (* 2 n) n)))
	((cpstak)  #'(lambda () (cpstak (* 3 n) (* 2 n) n)))
	((nrev)
	 (let ((l (genlist n)))
	   #'(lambda () (nrev l))))
	((quick) 
	 (let ((l (randlist 5000)))
	   #'(lambda () (mydotimes n #'(lambda() (quick l))))))
	((quickho) 
	 (let ((l (randlist 5000)))
	   #'(lambda () (mydotimes n #'(lambda() (quickho l #'less))))))
	((quickarray) 
	 (let ((l (randlist 5000)))
	   #'(lambda () 
	       (mydotimes 
		n 
		#'(lambda() (quickarray (make-array 5000 :initial-contents l)))))))
	((queens) #'(lambda () (queens n)))
	((mandel) #'(lambda () (mandelloop)))
	((deriv) #'(lambda () (goderiv n)))
	(else (print "unknown"))))

(defun printnl(x)
  (print x)
  x)

(defun doit(bench iter len)
  (printnl (append (list bench len) 
		   (time (dobenchavrg iter (getit bench len))))))

(defun doitall(iter)
  (list (doit 'fib iter 31)
;	(doit 'fibf iter 31)
	(doit 'tak iter 8)
     ;  (doit 'cpstak iter 8) ;; ACL crashes
	(doit 'nrev iter 3000)
	(doit 'quick iter 30)
;	(doit 'quickho iter 30)
;	(doit 'quickarray iter 30)
;	(doit 'queens iter 10)
	(doit 'deriv iter 30)
	))
;	(doit 'mandel iter 4711)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; compile and load files:
; (compile-file "../tools/tools.lsp" :load t)
; (load "../tools/tools.fasl")
; (compile-file "benches.lsp" :load t)  (load "benches.fasl")

; example usage:
;
;     (doit 'fib 5 31)
;
; runs 5 times fib(31) 
