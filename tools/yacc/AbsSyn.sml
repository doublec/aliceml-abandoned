(* abstract syntax for parse tree of jacke input *)

structure AbsSyn =
struct
    datatype bnfexp =
	Skip
      | Symbol of string
      | As of string * bnfexp
      | Seq of bnfexp list
      | Prec of bnfexp * string
      | Transform of bnfexp * string list
      | Alt of bnfexp list
	
    and parsetree =
	TokenDec of (string * string option) list
      | AssoclDec of idlist
      | AssocrDec of idlist
      | NonassocDec of idlist
      | RuleDec of rule list
      | ParserDec of (string * string option * string) list
      | MLCode of string list
	
    withtype 
	rule = string * string option * bnfexp
    and idlist = string list

    exception MultipleDefinition of string


    (* check that
     0. each token occurs at most once in all the nonassoc, assocr, assocl decls (* still missing *)
     1. only one token declaration in program  (too strict ?)
     2. only one rule declaration per rule name  (too strict ?) *)

    fun checkAdmissibility p = 
	let fun chk (NONE,env') (TokenDec t) = (SOME (), env')
	      | chk (SOME (),env') (TokenDec t) = raise MultipleDefinition ("token")
	      | chk (t,env') (RuleDec []) = (t,env')
	      | chk (t,env') (RuleDec ((r,_,b)::rs)) = if List.exists (fn x =>x=r) env' then 
	                                                   raise MultipleDefinition ("rule "^r)
						       else chk (t,r::env') (RuleDec rs)
	      | chk env _ = env
	in (foldr (fn (p,(t,env')) => chk (t,env') p) (NONE,[]) p; true) 
	end handle MultipleDefinition _ => false




    fun toString _ = ""   (* missing *)
	
    fun pr t =
	let fun indent 0 = ""
	      | indent n = " "^(indent (n-1))
	    fun f ind Skip = (indent ind)^"Skip\n"
	      | f ind (Symbol s) = (indent ind)^"Symbol ("^s^")\n"
	      | f ind (As (s,b)) = (indent ind)^"Symbol ("^s^") As\n"^ (f (ind+4) b)
	      | f ind (Alt []) = ""
	      | f ind (Alt (b::bs)) = (f ind b)^(f ind (Alt bs))
	      | f ind (Prec (b,s)) = (indent ind)^"Prec\n"^(f (ind+4) b)^(indent (ind+4))^"Symbol ("^s^")\n"
	      | f ind (Transform (b,s)) = (indent ind)^"Transform\n"^(f (ind+4) b)
		                          ^(indent (ind+4))^(foldr (fn (s,b)=>s^" "^b) "\n" s) 
	      | f ind (Seq []) = ""
	      | f ind (Seq (b::bs)) = (f ind b)^(f ind (Seq bs))
	    fun g ind (TokenDec l) = (indent ind)^"TokenDec\n"^(indent (ind+4))^(foldr (fn ((s,_),b)=>s^" "^b) "\n" l)
	      | g ind (AssoclDec l) = (indent ind)^"AssoclDec\n"
	      | g ind (AssocrDec l) = (indent ind)^"AssocrDec\n"
	      | g ind (NonassocDec l) = (indent ind)^"NonassocDec\n"
	      | g ind (ParserDec l) = (indent ind)^"ParserDec\n"^(indent (ind+4))^(foldr (fn ((s,_,t),b)=>s^" = "^t^"  "^b) "" l)
	      | g ind (RuleDec l) = (indent ind)^"RuleDec\n"^(foldr (fn ((s,_,r),b) => (indent (ind+4))^s^"\n"^(f (ind+8) r)^b) "\n" l)
	      | g ind (MLCode m) = (indent ind)^"MLCode\n"^(indent(ind+4))^(foldr(fn (s,b)=>s^" "^b) "\n" m)

	in foldr (fn (s,b)=>(g 2 s)^b) "\n" t
	end
end
