structure ParsingError :> PARSING_ERROR =
  struct

  (* Pretty printer *)

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

  (* Types *)

    type VId = VId.t

    datatype error =
	(* Lexer *)
	  UnclosedComment
	| InvalidChar		of char
	| InvalidString
	| IntTooLarge
	| WordTooLarge
	| RealTooLarge
	| CharLengthInvalid	of string
	| EscapeCharTooLarge	of bool
	(* Parser *)
	| SyntaxError		of string
	(* Derived forms *)
	| WithtypeInvalid
	| WithtypeArityMismatch
	(* Infix *)
	| InfixMisplaced	of VId
	| AssocConflict		of VId * VId


    type warning = unit		(* yet empty *)


  (* Pretty printing *)

    fun ppQuoted s	= "`" ^ s ^ "'"
    fun ppVId vid	= ppQuoted(VId.toString vid)

    fun ppError(UnclosedComment) =
	  textpar["unclosed","comment"]
      | ppError(InvalidChar c) =
	  textpar["invalid","character",ppQuoted(Char.toCString c)]
      | ppError(InvalidString) =
	  textpar["invalid","string","constant"]
      | ppError(IntTooLarge) =
	  textpar["integer","constant","too","large"]
      | ppError(WordTooLarge) =
	  textpar["word","constant","too","large"]
      | ppError(RealTooLarge) =
	  textpar["real","constant","too","large"]
      | ppError(CharLengthInvalid "") =
	  textpar["empty","character","constant"]
      | ppError(CharLengthInvalid s) =
	  textpar["multiple","characters","in","character","constant"]
      | ppError(EscapeCharTooLarge uc) =
	  textpar[if uc then "unicode" else "ASCII",
		  "escape","character","too","large"]
      (* Parser *)
      | ppError(SyntaxError s) =
	  textpar(String.tokens (fn c => c = #" ") s)
      (* Derived forms *)
      | ppError(WithtypeInvalid) =
	  textpar["invalid","type","binding","inside","withtype"]
      | ppError(WithtypeArityMismatch) =
	  textpar["type","has","wrong","arity"]
      (* Infix *)
      | ppError(InfixMisplaced vid) =
	  textpar["misplaced","infix","identifier",ppVId vid]
      | ppError(AssocConflict(vid1,vid2)) =
	  textpar["conflicting","infix","associativity","between","operators",
		  ppVId vid1,"and",ppVId vid2]


    fun ppWarning w = empty


  (* Export *)

    fun errorToString e   = PrettyPrint.toString(ppError e, 75)
    fun warningToString w = PrettyPrint.toString(ppWarning w, 75)

    fun error(region, e)  = Error.error(region, errorToString e)
    fun warn(region, w)   = Error.warn(region, warningToString w)

  end
