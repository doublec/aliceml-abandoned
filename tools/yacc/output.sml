(* print after preprocessing: ML code + embedded tokens + parser *)
structure Output =
struct
    structure A = AbsSyn
    structure P = PrintStruct

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

    (* to be done: code for parser with start symbol*)
    fun parserDecToString [] = ""
      | parserDecToString ((name,ty,sr)::ds) =
	let val prefix = case ty of
	    NONE => "val "^name^" "
	  | SOME ty => "val "^name^" : lexer -> "^ty^" "
	in
	    cr (prefix^" = fn lexer => lexer\n")
	end

    fun absSynToString (A.TokenDec l) = tokenDecToString l
      | absSynToString (A.MLCode l) = List.foldr (fn (x,r) => x^" "^r) "" l
      | absSynToString (A.ParserDec l) = parserDecToString l
      | absSynToString _ = "" (* remove assocDecs, ruleDecs *)

    fun toString p = 
	List.foldr (fn (x,r) => x^" "^r) "\n" (List.map absSynToString p)

end
