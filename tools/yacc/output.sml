(* print after preprocessing: ML code + embedded tokens + parsers *)


(* replaced 'lexer' with 'lexxer' because of problems with hose *)


structure Output =
struct
    (* evaluate only after parse? *)
    val closure = false
    (* print error structure at the beginning ? *)
    val printErrorStruct = true
    (* print description file *)
    val description = true

    structure A = AbsSyn

    fun cr s = s^"\n"
    fun blank i = if i<0 then "" else " "^(blank (i-1))

    fun timeStamp () =
	let val time = Time.fmt 0 (Time.now ())
	in 
	    time
	end

    fun warning s = TextIO.print ("Warning: "^s^"\n")

    fun error s = TextIO.print ("Error: "^s^"\n")
	
    local open MakeLrTable.Errs
    in
	fun issueWarnings errs =
	    let val rr = (List.length) 
		(List.filter (fn (RR _) => true | _ => false) errs)
		val sr = (List.length) 
		(List.filter (fn (SR _) => true | _ => false) errs)
		val nr = (List.length) 
		(List.filter (fn (NOT_REDUCED _) => true | _ => false) errs)
	    in
		(if rr>0 
		     then warning 
			 ((Int.toString rr)^" reduce/reduce conflict(s)")
		 else ();
		 if sr>0 
		     then warning 
			 ((Int.toString sr)^" shift/reduce conflict(s)")
		 else ();
		 if nr>0 
		     then warning
			 ((Int.toString nr)^" rule(s) not reduced")
		 else ())
	    end
    end 

    fun structureName () =
	let val name = "JackeDeclarationsStruct__"
	    val time = timeStamp ()
	in name^time
	end

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

    fun printSValue (terms,nonterms,parsers) =
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
	    fun prSumConstr (tyVar,0) = "\n\n"
	      | prSumConstr (tyVar,1) = " S0 of "^tyVar^"1\n\n"
	      | prSumConstr (tyVar,n) = let val i=Int.toString (n-1)
					    val iplus1=Int.toString n
					in 
					    " S"^i^" of "^tyVar^iplus1
					    ^" |"^(prSumConstr (tyVar,n-1))
					end
	    val tyVarName = "'ty"
	    val numNonterms =  List.length nonterms
	    val numTerms = List.length terms
	in
	    "(* toplevel datatype for each single parser in this file *)\n"
	    ^"datatype "^(tyarg(tyVarName, parsers))
	    ^" sum = "
	    ^(if parsers<=0 then "NOPARSERS\n" else prSumConstr (tyVarName,parsers))
	    ^"(* datatype svalue, holding semantic values on parse stack *)\n"
	    ^"datatype "^(tyarg(tyVarName, numNonterms))
	    ^" svalue = "
	    ^"\n    VOID" (* do I need this ? *)
	    ^(if numTerms=0 then "" else "\n  | ")   
	    ^(prTermConstr terms)
	    ^(if numNonterms=0 then "" else "\n  | ")   
	    ^(prNontermConstr tyVarName nonterms)
	    ^"\n"
	end

    fun printToIntTokenFn (terms,stringToTerm,parsers) =
	let fun constrPat (t,NONE) = "(SOME ( "^t^" ),p1,p2)"
	      | constrPat (t,SOME _) = "(SOME ("^t^" a),p1,p2)"
	    fun constrInt (t,NONE) = t^" (fn () => ())"
	      | constrInt (t,SOME _) = t^" (fn () => a)"
	    fun constrCase (tok as (t,_)) = 
		(constrPat tok)
		^" => Token.TOKEN(LrTable.T "
		^(Int.toString(case stringToTerm t of Grammar.T i => i))
		^", (SValue."^(constrInt tok)
		^", p1, p2))\n"
	    fun cases [] = "(NONE,p1,p2) => Token.TOKEN(LrTable.T "^
		(Int.toString parsers)
		^", (SValue."^
		(Translate.eop)
		^" (fn () => ()), p1, p2))\n" 
	      | cases (t::ts) = (constrCase t)^"\n    | "^(cases ts)
	in
	    "\n(* lexxer -> (unit -> (SValue.svalue, pos) Token.token),\n "
	    ^"      where type lexxer = unit -> (token * pos * pos) *)"
	    ^"\nfun toInternalToken lexxer =\n"
	    ^"    fn () =>\n    case lexxer() of\n"
	    ^"      "^(cases terms)
	end

    fun printActions stringToNonterm rules = 
	let fun printActions n ((r as (lhs,t,A.Transform(bnf,(posInfo,code))))::rs) = 
	    let fun mkPat rulenum rhs = 
		let fun prRhs first [] = "rest671"
		      | prRhs first ((A.As(s,A.Symbol t))::rhs) =
		    "(_, (SValue."^t^"("^s^"), "
		    ^(if first then s^"left, " else s^"left, ")
		    ^(if List.length rhs = 0 then s^"right)) ::" else s^"right)) ::")
		    ^(prRhs false rhs)
		in 
		    "("^(Int.toString rulenum)^", "
		    ^(prRhs true rhs)^")\n" 
		end
		fun mkMatch [] = ""
		  | mkMatch ((A.As(s,A.Symbol t))::rhs) =
		    (blank 44)^"val "^s^" = "^s^" ()\n"
		    ^(mkMatch rhs)
		val A.Seq rhs = bnf
		val code = A.expToString code
		val rightpos = if List.null rhs then "defPos"
			       else let val A.As(name,_) = List.last rhs 
				    in name^"right" end 
		val leftpos = if List.null rhs then "defPos"
			      else let val A.As(name,_) = List.hd rhs
				   in name^"left" end
		val posInfo = case posInfo of SOME s => s^"\n" | _ => ""  
	    in
		(blank 1)^posInfo
		^(blank 1)^(mkPat n (List.rev rhs))^
(* ------------------------------------------------- *)
		(if closure then
		    ((blank 12)^"=> let val result =\n"
			^(blank 20)^"SValue."^lhs^" (fn () => let\n"
			^(mkMatch rhs)
			^(blank 40)^"in ( "^code^" ) end )\n")  
(* ------------------------------------------------- *)
		else ((blank 12)^"=> let val result =\n"
		^(blank 20)^"let\n"
		^(mkMatch rhs)
		^"in SValue."^lhs^" (fn () => ( "^code^" )) end "))
(* ------------------------------------------------- *)
		^(blank 15)^"in "
		^"(LrTable.NT "
		^(Int.toString (case stringToNonterm lhs of LrTable.NT i => i))
		^", (result, "^leftpos^", "^rightpos^"), rest671) end\n"
		^" | "^(printActions (n+1) rs) 
	    end
	      | printActions _ _ = "_ => raise (exnAction i392)\n"
	in 
	    printActions 0 rules
	end

    fun removeRuleDecs _ [] = []
      | removeRuleDecs true ((r as (A.RuleDec _))::l) = 
	r ::(removeRuleDecs false l)
      | removeRuleDecs false ((A.RuleDec _)::l) = 
	removeRuleDecs false l
      | removeRuleDecs first (x::l) = x::(removeRuleDecs first l)

    fun undoBinding [] = ""
      | undoBinding ((t,_)::ts) = "fun "^t^" _ = 1\n"^(undoBinding ts)

    fun mkPrint init =
	let val str = ref init
	    val print = fn s => str := (!str)^s
	in (str, print) end
 
    fun output filename = 
	let fun mkPrint init =
	    let val str = ref init
		val print = fn s => str := (!str)^s
	    in (str, print) end
	    val currParser = ref 0
	    val (r,print) = mkPrint ""
	    val p = (NormalForm.toNormalForm (Parse.parse filename))
	    val Translate.TRANSLATE {grammar,
				     stringToTerm,
				     stringToNonterm,
				     termlist,
				     parsers,
				     rules} = Translate.translate p
	    val Grammar.GRAMMAR{terms,
				nonterms,
				termToString,
				nontermToString,
				rules=grammarRules,
				...} = grammar

	    fun symbolToString (Grammar.NONTERM s) = nontermToString s
	      | symbolToString (Grammar.TERM s) = termToString s
	    val nontermlist = map nontermToString 
		(List.tabulate (nonterms, fn x => Grammar.NT x))
	    val (table,stateErrs,corePrint,errs) 
		= MakeLrTable.mkTable (grammar,true)
	    val entries = (PrintStruct.makeStruct {table=table,
						   name="generatedLrTable",
						   print=print,
						   verbose=false})
	    val lrTableString = !r
	    val _ = issueWarnings errs
	    val structureName = structureName ()
	    val structureName2 = "rmConstrStatus"^(timeStamp())
	    val structureName3 = "ErrorStruct"^(timeStamp())

	    fun parserDecToString  [] = ""
	      | parserDecToString  ((name,ty,sr)::ds) =
		let val prefix = case ty of
		    NONE => "val "^name^" "
		  | SOME ty => "val "^name^" : lexxer -> "^ty^" "
		    val decString = "\nlocal structure J = "^structureName^"\nin\n"
		    val n = !currParser
		    val s = decString^prefix^" = fn lexxer =>\n"
			^"let val (a as (_,p1,p2)) = lexxer()\n"
			^"    val f = fn _ => J.Token.TOKEN(J.LrTable.T "
			^(Int.toString n)^", (J.SValue."
			^((fn (a,_) => a)(List.nth (termlist,n)))^" (fn _ => ()),p1,p2))\n"
			^"    val lexxer1 = J.Misc.mkGet (fn _ => a) lexxer\n"
			^"    val lexxer2 = J.Misc.mkGet f (J.toInternalToken lexxer1)\n"
			^"    val arg = ()\n"
			^"    val table = J.generatedLrTable\n"
			^"    val saction = J.SAction.actions\n"
			^"    val void = J.SValue.VOID\n"
			^"    val error = "
			^(if printErrorStruct then structureName3^".error\n" else "parseError\n")
			^"    fun extract (J.SValue."^(Translate.start)^" a) =\n"
			^"        case a () of (J.SValue.S"^(Int.toString n)^" b) => b\n"
			^"in\n    extract (J.LrParser.parse {arg=arg,\n" 
			^"                            lexxer=lexxer2,\n"
			^"                            saction=saction,\n"
			^"                            table=table,\n"
			^"                            void=void,\n"
			^"                            error=error})\nend\nend\n"
		in
		    (currParser := !currParser + 1; cr s)
		end

	    val hack = 
		"(* ---hack: remove constructor bindings in scope *)\n"
		^"structure "^structureName2^" = \nstruct\n"
		^(undoBinding termlist)^"end\n(* --- *)\n\n"
	    val errorDef =
		if printErrorStruct then
		    "(* ---error function for syntax errors: adapt to needs *)\n"
		    ^"structure "^structureName3^" = \nstruct\n"
		    ^"  exception ParseError\n\n"
		    ^"  (* val error : position type -> parse result type *)\n" 
		    ^"  val error = fn pos => raise ParseError\n"
		    ^"end\n(* --- *)\n\n"
		else ""

	    val p = (A.MLCode (A.EXP [A.ATEXP errorDef, A.ATEXP hack])) ::p
 
	    fun absSynToString _ (A.TokenDec l) = tokenDecToString l
	      | absSynToString _ (A.MLCode l) = A.expToString l
	      | absSynToString _ (A.ParserDec l) = parserDecToString  l
	      | absSynToString lrTable (A.RuleDec l) =
		"\nstructure "^structureName^" =\nstruct\n" 
		^"structure LrParser = LrParserEng\n"
		^"structure Token = LrParser.Token\n"
		^"structure LrTable = LrParser.LrTable\n\n"
		^"\n(* LR table for all parsers in this file *)\n"
		^"local structure Table =\nstruct\nopen LrTable\n\n"
		^lrTable
		^"end\nin val generatedLrTable = Table.generatedLrTable end\n\n"
		^"structure SValue =\nstruct\n"
		^(printSValue (termlist,nontermlist,parsers))
		^"end\n\n"
		(* use only "real" tokens, not the ones for distinguishing between parsers *)
		^(printToIntTokenFn 
		  (List.drop(termlist,parsers+1),stringToTerm,parsers))
		^"\n(* semantic actions *)\n"
		^"structure SAction =\nstruct\n"
		^"open "^structureName2^"\n"
		^"exception exnAction of int\n"
		^"val actions =\n    fn (i392,defPos,stack,()) =>\n"
		^"        case (i392,stack) of\n"
		^(printActions stringToNonterm rules)
		^"end\n"
		^"\n(* additional useful fns, common to all parsers in the file *)\n"
		^"structure Misc =\nstruct\n"
		^"fun mkGet f g =\n    let val t = ref true\n" 
		^"in\n    fn () => if !t then (t:=false; f ()) else g ()\nend"
		^"\nend\nend (* end of structure "^structureName^" *)\n\n"
	      | absSynToString _ _ = "" (* do not output assocDecs *)

	    val code = List.foldr (fn (x,r) => x^" "^r) "\n" 
		         (List.map 
			  (absSynToString  lrTableString)
			  (removeRuleDecs true p))
	    val outfile = TextIO.openOut (filename^".out")
	    val _ = TextIO.output(outfile,code)
	    val _ = TextIO.flushOut outfile
	    val _ = TextIO.closeOut outfile

	in
	    if description then
			 let val f = TextIO.openOut (filename ^ ".desc")
	     val say = fn s=> TextIO.output(f,s)
	     val printRule =
	        let val rules = Array.fromList grammarRules
	        in fn say => 
		   let val prRule = fn {lhs,rhs,precedence,rulenum} =>
		     ((say o nontermToString) lhs; say " : ";
		      app (fn s => (say (symbolToString s); say " ")) rhs)
	           in fn i => prRule (Array.sub(rules,i))
	           end
	        end
	 in Verbose.printVerbose
	    {termToString=termToString,nontermToString=nontermToString,
	     table=table, stateErrs=stateErrs,errs = errs,entries = entries,
	     print=say, printCores=corePrint,printRule=printRule};
	    TextIO.closeOut f;
	    TextIO.print ("Written output to "^filename^".out\n"^
			  "Written description file "^filename^".desc\n")
	 end
	    else
		TextIO.print ("Written output to "^filename^".out\n")
	end 
 (*   handle _ => 
	TextIO.print ("Some error(s) occurred while processing "^filename^"\n")
       *)
end



