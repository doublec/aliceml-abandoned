(* build internal grammar for ML-Yacc *)

structure Translate =
struct
    structure G = Grammar
    structure A = AbsSyn 
    structure N = NormalForm
    open Array

    val DEBUG = false
(*    val error = fn s => print ("Error: "^s^"\n")   *)

    datatype translate = TRANSLATE of {grammar :G.grammar,
				       stringToTerm :string -> G.term,
				       stringToNonterm :string -> G.nonterm,
				       parsers :int,
				       termlist :(string * string option) list,
				       rules :A.rule list
				       }

    val toNonterm = List.find 
    val toTerm = List.find

    fun timeStamp () =
	let val time = Time.fmt 0 (Time.now ())
	in 
	    time
	end

    fun mkNewNamefunction s m =
	let val r = ref m 
	in 
	    fn () => (r:=(!r+1); s^"__"^Int.toString(!r-1))^"__"
	end

    val newTokenname = mkNewNamefunction "TOKEN" 675
    val newRulename = mkNewNamefunction "startrule" 675
    val eop = "EOP"^(timeStamp ())
    val start = "NewStartSymbol"^(timeStamp ())    

    (* ML yacc assumes 
     - nonterms numbered 0,...,|nonterms|,
     - terms numbered 0,...,|terms|
     - rules numbered 0,...,|rules|
     - start nonterm not in any rhs
     - higher prec means tighter binding

     additionally: DummyTok1,...DummyTokn,EOPToken,origTokens... 
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
	    val t3 = if List.null t3 then []
		     else (List.last
			   (List.map (fn A.TokenDec t => toDict (parsers+1) t
			 | _ => []) t3))
	    fun pr ((s,_),i) = print ("("^s^","^(Int.toString i)^")")
	in 
	    t2@t3
	end

    fun mkNontermDict l =
	let fun toDict i [] = [((start,NONE), i)]
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
		in (tok::tl,(start,NONE,A.Transform
			     (A.Seq [A.As(tok,A.Symbol(tok)),
				     A.As(stsym,A.Symbol(stsym)),
				     A.As(eop,A.Symbol(eop))],
			      (NONE, A.EXP [A.ATEXP ("SValue.S"
					    ^ Int.toString pnum),
					    A.ATEXP " ( ", A.ATEXP stsym,
					    A.ATEXP " )"])))::rules)
		end
	in
	    toRule [] enumParsers
	end

    fun translateSeq td ntd = 
	List.map (fn (A.As (_,A.Symbol s)) => stringToSymbol td ntd s)

    fun translateRule td ntd i (x,_,A.Prec(A.Transform(A.Seq r,c),s)) =
	let val precedence = SOME (stringToTerm td s)
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


    (* from ML-Yacc, deal with associativities *)
    val termPrec = fn (numTerms,termToString,stringToTerm,parsetree) =>
	let val precData = array(numTerms, NONE : int option)
	    val addPrec = fn termPrec => fn term as (G.T i) =>
		case sub(precData,i)
		   of SOME _ => raise ErrorMsg.Error
		      (* already dealt with in AbsSyn
		       error ("multiple precedences specified for terminal " ^
				(termToString term)) *)
		 | NONE => update(precData,i,termPrec)
	    val termPrec = 
		fn ((A.AssoclDec slist), i) => List.map (fn s => (s,i)) slist
		 | ((A.AssocrDec slist), i) => List.map (fn s => (s,i+2)) slist
		 | ((A.NonassocDec slist), i) => List.map (fn s => (s,i+1)) slist
	    val assoclist = 
		let val assocs = List.rev (List.filter (fn A.AssoclDec _ => true
	                                                 | A.AssocrDec _ => true
							 | A.NonassocDec _ => true
							 | _ => false) parsetree)
		    fun compAssocs ([],i) = []
		      | compAssocs (h::t,i) = termPrec (h,i) ::(compAssocs (t,i+3))
		in 
		    List.concat (compAssocs (assocs,0))  (* rev ? *)
		end
	    val _ = List.app (fn (s,i) => addPrec (SOME i) (stringToTerm s)) assoclist
	in fn (G.T i) =>
	    if  DEBUG andalso (i < 0 orelse i >= numTerms) then
		NONE
	    else sub(precData,i)
	end

    (* from ML-Yacc: deal with rule precedences *)
    val elimAssoc =  fn i => (i - (i mod 3) + 1)
    val rulePrec = fn termPrec =>
	let fun findRightTerm (nil,r) = r
	      | findRightTerm (G.TERM t :: tail,r) =
	    findRightTerm(tail,SOME t)
	      | findRightTerm (_ :: tail,r) = findRightTerm(tail,r)
	in fn rhs =>
	    case findRightTerm(rhs,NONE)
		of NONE => NONE
	      | SOME term => 
		    case termPrec term
		       of SOME i => SOME  (elimAssoc i)
		     | a => a
	end
    
    val grammarRules = fn termPrec => fn rules =>
	let val conv = fn {lhs,rhs,precedence,rulenum} =>
	    {lhs=lhs,rhs =rhs,precedence=
	     case precedence
		 of SOME t => (case termPrec t
				   of SOME i => SOME(elimAssoc i)
				 | a => a)
	       | _ => rulePrec termPrec rhs,
		     rulenum=rulenum}
	in map conv rules
	end

    fun translate y = 
	let val (ts,x1) = mkParsers y
	    val parsers = List.length ts (* number of parsers in file *)
	    val x = (A.RuleDec x1)::y
	    val td = mkTermDict ts x
	    val ntd = mkNontermDict x
	    val stringToSymbol = stringToSymbol td ntd
	    val start = stringToNonterm ntd start
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
	    val precedence = termPrec (terms,termToString,stringToTerm,y)
	    val grammar = G.GRAMMAR {rules=grammarRules precedence rules,
				     terms=terms,
				     nonterms=nonterms,
				     start=start,
				     eop=eop,
				     noshift=noshift,
				     precedence=precedence,
				     termToString=termToString,
				     nontermToString=nontermToString
				     }
	    fun prPrec i = if i<terms 
			   then ((termToString (G.T i))^": "
				       ^(case (precedence (G.T i)) of SOME j => Int.toString j
			     | NONE => "%")
				       ^"\n"^ (prPrec (i+1))) 
		       else ""
	in if DEBUG then print (prPrec 0) else ();
	    TRANSLATE {grammar=grammar, 
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
