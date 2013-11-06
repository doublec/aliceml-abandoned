(* very simple (toplevel) grammar definition *)


(* replaced 'lexer' with 'lexxer' because of problems with hose *)


signature MYTOKEN =
sig
    token LPAR | RPAR | PLUS | MINUS | TIMES | NUM of int
end

structure a =
struct
    token LPAR | RPAR | PLUS | MINUS | TIMES | NUM of int 

    assocl TIMES 
    assocl PLUS MINUS 

    rule exp : int = 
        NUM 
      | LPAR, exp, RPAR             => (exp)
      | n1 as exp, oper1, n2 as exp => (oper1(n1,n2)) prec PLUS
      | n1 as exp, oper2, n2 as exp => (oper2(n1,n2)) prec TIMES
    and oper1 =
        PLUS  => (op+)
      | MINUS => (op-)
    and oper2 =
	TIMES  => (op*)

    parser eval = exp
    
    val lexxer = 
	let val t = [NUM 5, MINUS, NUM 3, TIMES, 
		     LPAR, NUM 2, TIMES, NUM 2, PLUS, NUM 2, RPAR]
	    fun f x = (SOME x,1,1)
	    val t = ref (map f t)
	in fn () =>
	    let val h = hd (!t) in (t:=tl(!t); h)  end handle _ => (NONE,1,1)
	end
end
