structure b =
struct

    token INT of int | PLUS | MINUS | LPAR | RPAR
    

    fun test 0 = [INT 0]
      | test n = (INT 0)::PLUS::(test (n-1))

    rule exp : int = 
        INT
      | LPAR, exp, RPAR             => (exp)
      | n1 as exp, oper, n2 as exp  => (oper(n1,n2))  prec PLUS
    and oper =
        PLUS  => (op+)
      | MINUS => (op-)

    parser eval = exp
    
    fun odd n = n mod 2 = 1

    val mklexer = fn () =>
	let val listSize = 400000
	    val p = ref 0;
	in fn () =>
	    if !p <= listSize then 
		(p := !p+1; (SOME (if odd (!p) then INT 0 else PLUS),!p,!p))  
	    else (NONE,!p,!p)
	end
    
    (* benchmark *)
    val run = fn () => let val t = Timer.startRealTimer();
			   val t1 = Timer.checkRealTimer t
			   val _ = eval (mklexer ())
			   val t2 = Timer.checkRealTimer t
			   val _ = eval (mklexer ())
			   val t3 = Timer.checkRealTimer t
			   val _ = eval (mklexer ())
			   val t4 = Timer.checkRealTimer t
			   val _ = eval (mklexer ())
			   val t5 = Timer.checkRealTimer t
			   val _ = eval (mklexer ())
			   val t6 = Timer.checkRealTimer t
			   val m = Time.-
			   val p = Time.+
		       in
			   (List.map Time.toString [t1,t2,t3,t4,t5,t6])
		       end
end
