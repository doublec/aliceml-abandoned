structure b =
struct

    token INT of int | PLUS | MINUS | LPAR | RPAR
    

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
	let val listSize = 100000
	    val p = ref 0;
	in fn () =>
	    if !p <= listSize then 
		(p := !p+1; (SOME (if odd (!p) then INT 0 else PLUS),!p,!p))  
	    else (NONE,!p,!p)
	end
    
    (* benchmark *)
    val run = fn () => let val t = Timer.startRealTimer();
			   val _ = eval (mklexer ())
			   val _ = eval (mklexer ())
			   val _ = eval (mklexer ())
			   val _ = eval (mklexer ())
			   val _ = eval (mklexer ())
		       in
			   Time.toString (Timer.checkRealTimer t)
		       end
end
