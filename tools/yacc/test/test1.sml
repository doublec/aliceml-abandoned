(* replaced 'lexer' with 'lexxer' because of problems with hose *)


structure a =
struct
  token LPAR | RPAR

  datatype T = Lf of int | Br of T * int
 
  rule exp =
    A as skip               => (Lf (Aright)) 
    | LPAR, exp, RPAR  => (Br (exp,RPARleft))

  parser eval = exp

    val lexxer = 
	let val t = [LPAR,LPAR,RPAR,RPAR]
	    fun f x = (SOME x,1,1)
	    val t = ref (map f t)
	in fn () =>
	    let val h = hd (!t) in (t:=tl(!t); h)  end handle _ => (NONE,1,1)
	end
end

