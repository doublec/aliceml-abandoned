(* abstract syntax for parse tree of jacke input *)

structure AbsSyn =
struct
    structure E = ErrorMsg

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


    fun checkOnlyOneTokenDec t = 
	let val l = List.filter (fn (TokenDec _) => true | _ => false) t
	    val l' = if List.length l <= 0 then []
		     else case (hd l) of 
			 TokenDec  d => List.map (fn x => #1 x) d
	    val b = List.length l<=1
 	in 
	    if b then (b, l') 
	    else (E.error 0 "Multiple Token Definitions"; (b,l'))
	end

    fun checkRulenames b rs [] = (b, rs)
      | checkRulenames b rs (t::ts) =
	let fun chk b [] rs = (b,rs) 
	      | chk b ((r,_,_)::s) rs = 
	    if List.all (fn y => y<>r) rs then chk b s (r::rs)
	    else (E.error 0 ("Multiple definition of rule "^r); chk false s rs)
	in case t of
	    RuleDec r => 
		let val (b',r') = (chk b r rs)
		in checkRulenames b' r' ts end
   	  | _ => checkRulenames b rs ts
	end

    fun checkDisjointness toks rules =
	if List.all (fn t => List.all (fn r => r<>t) rules) toks
	    then true 
	else (E.error 0 "Token and rule names not disjoint";false)

    fun checkAssocDecs rs [] = (true, rs)
      | checkAssocDecs rs (t::ts) =
	let fun chk [] rs = rs 
	      | chk (r::s) rs = 
	    if List.all (fn y => y<>r) rs then chk s (r::rs)
	    else (E.error 0 ("Multiple occurrence in assoc declaration of token "^r); chk s rs)
	in case t of
	    AssoclDec r => checkAssocDecs (chk r rs) ts
	  | AssocrDec r => checkAssocDecs (chk r rs) ts
	  | NonassocDec r => checkAssocDecs (chk r rs) ts
   	  | _ => checkAssocDecs rs ts
	end

    fun subset s1 s2 = 
	List.all (fn x => if List.exists 
		  (fn y => x=y) s2 then true else (E.error 0 ("undefined identifier "^x); false)) s1

    fun semanticalAnalysis t =
	let val (b1,tok) = checkOnlyOneTokenDec t
	    val (b2,rules) = checkRulenames true [] t
	    val b3 = checkDisjointness tok rules 
	    val (b4,tok') = checkAssocDecs [] t
	    val b5 = subset tok' tok 
	in List.all (fn x => x) [b1,b2,b3,b4,b5] end
end
