(* build internal grammar for ML-Yacc *)

structure Translate =
struct
    structure G = Grammar
    structure A = AbsSyn 

    val toNonterm = List.find 
    val toTerm = List.find

    (* ML yacc assumes 
     - nonterms numbered 0,...,|nonterms|,
     - terms numbered 0,...,|terms|
     - rules numbered 0,...,|rules|
     - start nonterm not in any rhs
     - higher prec means tighter binding 
     *)

    val eop = "EOP"
    val start = "internalNewStartSymbol"    

    fun mkTermDict l = 
	let fun toDict i [] = [((eop,NONE), i)]
	      | toDict i (to::toks) = (to,i)::(toDict (i+1) toks)
	in 
	    List.concat(List.map (fn A.TokenDec t => toDict 0 t | _ => []) l)
	end

    fun mkNontermDict l =
	let fun toDict i [] = [((start,NONE), i)]
	      | toDict i ((s,ty,_)::ss) = ((s,ty),i)::(toDict (i+1) ss)
	in 
	    List.concat(List.map (fn A.RuleDec t => toDict 0 t | _ => []) l)
	end

    fun termToString d (G.T n) = 
	let val SOME ((str,_),_) = List.find (fn (_,m) => n=m) d 
	in str end

    fun nontermToString d (G.NT n) = 
	let val SOME ((str,_),_) = List.find (fn (_,m) => n=m) d 
	in str end

    fun stringToTerm d s =
	let val SOME (_,i) = List.find (fn ((str,_),_) => s=str) d 
	in G.T i end

    fun stringToNonterm d s =
	let val SOME (_,i) = List.find (fn ((str,_),_) => s=str) d 
	in G.NT i end

    fun stringToSymbol td ntd s =
	(G.TERM (stringToTerm td s)) handle _ => 
	    G.NONTERM (stringToNonterm ntd s)
 
    fun translateRule td ntd i (x,_,A.Prec(a,s)) =
	let val precedence = SOME (stringToTerm td s)
	    val {lhs=lhs,rhs=rhs,...} = translateRule td ntd i (x,NONE,a)
	in {lhs = lhs,
	    rhs = rhs,
	    precedence = precedence,
	    rulenum = i}
	end

    fun translate x = 
	let val td = mkTermDict x
	    val ntd = mkNontermDict x
	    val stringToSymbol = stringToSymbol td ntd
	    val start = start
	    val eop = [eop]
	    val terms = List.length td
	    val nonterms = List.length ntd
	    val noshift = []
	    val termToString = termToString td
	    val nontermToString = nontermToString ntd
	in
	    x
	end
    
end
