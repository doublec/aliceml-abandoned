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

    (* to be done: code for parser *)
    fun parserDecToString [] = ""
      | parserDecToString ((name,ty,sr)::ds) =
	let val prefix = case ty of
	    NONE => "val "^name^" "
	  | SOME ty => "val "^name^" : lexer -> "^ty^" "
	in
	    cr (prefix^" = fn lexer => lexer\n")
	end

    fun removeRuleDecs _ [] = []
      | removeRuleDecs true ((r as (A.RuleDec _))::l) = 
	r ::(removeRuleDecs false l)
      | removeRuleDecs false ((A.RuleDec _)::l) = 
	removeRuleDecs false l
      | removeRuleDecs first (x::l) = x::(removeRuleDecs first l)

    fun absSynToString _ (A.TokenDec l) = tokenDecToString l
      | absSynToString _ (A.MLCode l) = List.foldr (fn (x,r) => x^" "^r) "" l
      | absSynToString _ (A.ParserDec l) = parserDecToString l
      | absSynToString lrTable (A.RuleDec l) =
	"\nlocal structure Table =\nstruct\nopen LrTable\n\n"
	^lrTable
	^"end\nin val generatedLrTable = Table.generatedLrTable end\n\n"
      | absSynToString _ _ = "" (* remove assocDecs *)

    fun mkPrint init =
	let val str = ref init
	    val print = fn s => str := (!str)^s
	in (str, print) end
 
    fun output filename = 
	let val (r,print) = mkPrint ""
	    val p = Parse.parse filename
	    val yaccGrammar = Translate.translate (NormalForm.toNormalForm p)
	    val (table,_,_,_) = MakeLrTable.mkTable (yaccGrammar,true)
	    val lrTable = (PrintStruct.makeStruct {table=table,
						   name="generatedLrTable",
						   print=print,
						   verbose=false}; !r)
	    val code = List.map (absSynToString lrTable)(removeRuleDecs true p)
	in
	    List.foldr (fn (x,r) => x^" "^r) "\n" code
	end

end
