(* print after preprocessing: ML code + embedded tokens + parser *)
structure Output =
struct
    structure A = AbsSyn

    fun cr s = s^"\n"
    fun blank i = if i<0 then "" else " "^(blank (i-1))

    fun tokenDecToString l = 
	let fun tyToString (s,NONE) = s
	      | tyToString (s,SOME t) = s^" of "^t
	    fun toString [] = ""
	      | toString [e] = tyToString e
	      | toString (e::es) = 
		(tyToString e)^"\n"^(blank 14)^"| "^toString es
	in 
	    cr ("\ndatatype token = "^toString l)
	end

    fun printSValue (terms,nonterms) =
	let fun tyarg (_, 0) = ""
	      | tyarg (str,1) = str^"1"
	      | tyarg (str,n) =
	    let fun mk m = if m=n then str^(Int.toString m)
			   else str^(Int.toString m)^","^(mk (m+1)) 
	    in "("^(mk 1)^")" end
	    fun prTermConstr terms =
		let fun tyToString (s,NONE) = s^" of unit -> unit"
		      | tyToString (s,SOME t) = s^" of unit -> ("^t^")"
		    fun toString [] = ""
		      | toString [e] = tyToString e
		      | toString (e::es) = 
			(tyToString e)^"\n"^(blank 1)^"| "^toString es 
		in toString terms end
	    fun prNontermConstr tyVar nonterms = 
		let fun toString n [] = ""
		      | toString n [nt] = nt^" of unit -> "
		           ^tyVar^(Int.toString n)
		      | toString n (nt::nts) = nt^" of unit -> "
		           ^tyVar^(Int.toString n)
		           ^"\n"^(blank 1)^"| "^(toString (n+1) nts)
		in toString 1 nonterms end
	    val tyVarName = "'ty"
	    val numNonterms =  List.length nonterms
	    val numTerms = List.length terms
	in
	    "(* datatype svalue, holding semantic values on parse stack *)\n"
	    ^"datatype "^(tyarg(tyVarName, numNonterms))
	    ^" svalue = "
	    ^"\n    VOID" (* do I need this ? *)
	    ^(if numTerms=0 then "" else "\n  | ")   
	    ^(prTermConstr terms)
	    ^(if numNonterms=0 then "" else "\n  | ")   
	    ^(prNontermConstr tyVarName nonterms)
	    ^"\n"
	end

    fun printToIntTokenFn (terms,stringToTerm) =
	let fun constrPat (t,NONE) = "("^t^",p1,p2)"
	      | constrPat (t,SOME _) = "("^t^" a,p1,p2)"
	    fun constrInt (t,NONE) = t^" (fn () => ())"
	      | constrInt (t,SOME _) = t^" (fn () => a)"
	    fun constrCase (tok as (t,_)) = 
		(constrPat tok)
		^" => Token.TOKEN(LrTable.T "
		^(Int.toString(case stringToTerm t of Grammar.T i => i))
		^", (SValue."^(constrInt tok)
		^", p1, p2))\n"
	    fun cases [] = "" (* should not happen *)
	      | cases [t] = constrCase t
	      | cases (t::ts) = (constrCase t)^"\n    | "^(cases ts)
	in
	    "\n(* type lexer = unit -> (token * pos * pos) *)"
	    ^"\nfun toInternalToken (lexer :lexer) =\n"
	    ^"val fn () =>\n    case lexer() of\n"
	    ^"      "^(cases terms)
	end

    fun printPrelimString (name,terms,nonterms,stringToTerm) =
	"\n(* declarations for parser "^name^" *)\n"
	^"local\nstructure Token = LrParser.Token\n"
	^"structure SValue =\nstruct\n"
	^(printSValue (terms,nonterms))
	^"end\n"
	^"\n"^(printToIntTokenFn (terms,stringToTerm))
	^"in\n" (* HERE: insert parser fn *)
	^"end\n"

    (* to be done: code for parser *)
    fun parserDecToString (terms,nonterms,stringToTerm) [] = ""
      | parserDecToString (terms,nonterms,stringToTerm) ((name,ty,sr)::ds) =
	let val prefix = case ty of
	    NONE => "val "^name^" "
	  | SOME ty => "val "^name^" : lexer -> "^ty^" "
		val decString = printPrelimString (name,terms,nonterms,stringToTerm)
	in
	    cr (decString^prefix^" = fn lexer => lexer\n")
	end

    fun removeRuleDecs _ [] = []
      | removeRuleDecs true ((r as (A.RuleDec _))::l) = 
	r ::(removeRuleDecs false l)
      | removeRuleDecs false ((A.RuleDec _)::l) = 
	removeRuleDecs false l
      | removeRuleDecs first (x::l) = x::(removeRuleDecs first l)

    fun absSynToString _ _ (A.TokenDec l) = tokenDecToString l
      | absSynToString _ _ (A.MLCode l) = List.foldr (fn (x,r) => x^" "^r) "" l
      | absSynToString (terms,nonterms,stringToTerm) _ (A.ParserDec l) = 
	  parserDecToString (terms,nonterms,stringToTerm) l
      | absSynToString _ lrTable (A.RuleDec l) =
	"\nlocal structure Table =\nstruct\nopen LrTable\n\n"
	^lrTable
	^"end\nin val generatedLrTable = Table.generatedLrTable end\n\n"
      | absSynToString _ _ _ = "" (* remove assocDecs *)

    fun mkPrint init =
	let val str = ref init
	    val print = fn s => str := (!str)^s
	in (str, print) end
 
    fun output filename = 
	let val (r,print) = mkPrint ""
	    val p = (NormalForm.toNormalForm (Parse.parse filename))
	    val Translate.TRANSLATE {grammar,stringToTerm,stringToNonterm,termlist} = 
		  Translate.translate p
	    val Grammar.GRAMMAR{terms,nonterms,termToString,nontermToString,...} 
		= grammar
	    val nontermlist = map nontermToString 
		(List.tabulate (nonterms, fn x => Grammar.NT x))
	    val (table,_,_,_) = MakeLrTable.mkTable (grammar,true)
	    val lrTable = (PrintStruct.makeStruct {table=table,
						   name="generatedLrTable",
						   print=print,
						   verbose=false}; !r)
	    val code = List.map 
		(absSynToString (termlist,nontermlist,stringToTerm) lrTable)
		(removeRuleDecs true p)
	in
	    List.foldr (fn (x,r) => x^" "^r) "\n" code
	end

end
