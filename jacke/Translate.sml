(* build internal grammar for ML-Yacc *)

structure Translate =
struct
    structure G = Grammar
    structure A = AbsSyn 
    structure N = NormalForm

    datatype translate = TRANSLATE of {grammar :G.grammar,
				       stringToTerm :string -> G.term,
				       stringToNonterm :string -> G.nonterm,
				       parsers :int,
				       (*termToString :int -> string,
				       nontermToString :int -> string,
				       parserSymbols : ?*)
				       termlist :(string * string option) list,
				       rules :A.rule list
				       }

    val toNonterm = List.find 
    val toTerm = List.find

    fun mkNewNamefunction s m =
	let val r = ref m 
	in 
	    fn () => (r:=(!r+1); s^"__"^Int.toString(!r-1))^"__"
	end

    val newTokenname = mkNewNamefunction "TOKEN" 675
    val newRulename = mkNewNamefunction "startrule" 675
    val eop = "EOP"

    (* ML yacc assumes 
     - nonterms numbered 0,...,|nonterms|,
     - terms numbered 0,...,|terms|
     - rules numbered 0,...,|rules|
     - start nonterm not in any rhs
     - higher prec means tighter binding 
     *)
    (*
    val eop = "EOP"
    val start = "NewStartSymbol"    
    *)

    (* tl: new tokens to distinguish between different parsers *)
    (* eop to the end *) 
    fun mkTermDict tl l = 
	let fun toDict i [] = []
	      | toDict i (to::toks) = (to,i)::(toDict (i+1) toks)
	    val t1 = toDict 0 (map (fn x => (x,NONE)) tl)
	    val parsers = List.length t1
	    val t2 = t1@[((eop,NONE),parsers)]
	    val t3 = List.filter (fn A.TokenDec _ => true | _ => false) l 
	    val t3 = (List.last
		 (List.map (fn A.TokenDec t => toDict (parsers+1) t  | _ => []) t3))
	    fun pr ((s,_),i) = print ("("^s^","^(Int.toString i)^")")
	    (* val _ = List.app pr (t2@t3) *)
	in 
	    t2@t3
	end

    fun mkNontermDict l =
	let fun toDict i [] = [((N.start,NONE), i)]
	      | toDict i ((s,ty,_)::ss) = 
	    let val d = toDict i ss 
	    in 
		if List.exists (fn ((s',_),_) => s'=s) d  
		    then d
		else ((s,ty),i)::(toDict (i+1) ss)
	    end
	in 
	    toDict 0 (List.concat(List.map (fn A.RuleDec t =>  t | _ => []) l))
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
 
    fun symbolToString td ntd (G.TERM t) = termToString td t
      | symbolToString td ntd (G.NONTERM t) = nontermToString ntd t


    (* add new dummy tokens and rules, for each parser dec *)
    fun mkParsers l =
	let val ps = List.concat (List.map (fn (A.ParserDec l) => l 
                                              | _ => []) l)
	    val enum = List.tabulate (List.length ps, fn x => x)
	    val enumParsers = ListPair.zip(ps,enum)
	    fun toRule tl [] = (tl,[])
	      | toRule tl (((pname,_,stsym),pnum)::l) =
		let val tok = newTokenname () 
		    val (tl,rules) = toRule tl l
		in (tok::tl,(N.start,NONE,A.Transform
			     (A.Seq [A.As(tok,A.Symbol(tok)),
				     A.As(stsym,A.Symbol(stsym)),
				     A.As(eop,A.Symbol(eop))],
			      ["SValue.S"^(Int.toString pnum)^" ( "^ stsym^" )"]))::rules)
		end
	in
	    toRule [] enumParsers
	end

    fun translateSeq td ntd = 
	List.map (fn (A.As (_,A.Symbol s)) => stringToSymbol td ntd s)

    fun translateRule td ntd i (x,_,A.Prec(A.Transform(A.Seq r,c),s)) =
	let val precedence = NONE (* SOME (stringToTerm td s) *)
	    val lhs = stringToNonterm ntd x
	    val rhs = translateSeq td ntd r
	    val rulenum = i
	in 
	    {lhs=lhs, rhs=rhs, rulenum=rulenum, precedence=precedence} 
	end
      | translateRule td ntd i (x,_,A.Transform(A.Seq r,c)) =
	let val precedence = NONE
	    val lhs = stringToNonterm ntd x
	    val rhs = translateSeq td ntd r
	    val rulenum = i
	in 
	    {lhs=lhs, rhs=rhs, rulenum=rulenum, precedence=precedence} 
	end
    
    fun mkRules td ntd l =
	let val rs =
	    List.concat(List.map (fn A.RuleDec t => t | _ => []) l)
	    fun toRule i [] = [] (* add start rule ? *)
	      | toRule i (r::rs) = 
		(translateRule td ntd i r)::(toRule (i+1) rs)
	in 
	    (rs,toRule 0 rs)
	end

    fun removePrec (A.Prec (bnf,_)) = removePrec bnf
      | removePrec bnf = bnf

    fun translate y = 
	let val (ts,x1) = mkParsers y
	    val parsers = List.length ts (* number of parsers in file *)
	    val x = (A.RuleDec x1)::y
	    val td = mkTermDict ts x
	    val ntd = mkNontermDict x
	    val stringToSymbol = stringToSymbol td ntd
	    val start = stringToNonterm ntd N.start
	    val eop = [stringToTerm td eop]
	    val terms = List.length td
	    val nonterms = List.length ntd
	    val noshift = []
	    val termToString = termToString td
	    val nontermToString = nontermToString ntd
	    val stringToTerm = stringToTerm td
	    val stringToNonterm = stringToNonterm ntd
	    val termlist = map (fn (x,_) => x) td
	    val (origRules,rules) = mkRules td ntd x
	    val origRules = map 
		(fn (s,t,bnf) => (s,t,removePrec bnf)) origRules
	    val precedence = fn t => NONE
	    val grammar = G.GRAMMAR {rules=rules,
				     terms=terms,
				     nonterms=nonterms,
				     start=start,
				     eop=eop,
				     noshift=noshift,
				     precedence=precedence,
				     termToString=termToString,
				     nontermToString=nontermToString
				     }
	in TRANSLATE {grammar=grammar, 
		      stringToTerm = stringToTerm,
		      stringToNonterm = stringToNonterm,
		      termlist = termlist,
		      rules = origRules,
		      parsers = parsers
		      }
	end
    
    val print = fn (g as G.GRAMMAR{termToString,nontermToString,...}) =>
	let fun symbolToString (G.TERM t) = termToString t
	      | symbolToString (G.NONTERM t) = nontermToString t
	in
	    IntGrammar.prGrammar (symbolToString,nontermToString,print) g
	end
end
