structure Backend=
    struct
	open Common

	type stamp=IntermediateGrammar.stamp

	(* Hashtabelle f�r Stamps. *)
	structure StampHash = MakeHashImpMap(type t=stamp val hash=Stamp.hash)

	(* Scoped Sets f�r Stamps. Wird zur Berechnung der freien
	 Variablen benutzt. *)
	structure ScopedStampSet = MakeHashScopedImpSet(type t=stamp
							val hash=Stamp.hash)

	(* Hashtabelle f�r Listen von Strings. Wird ben�tigt bei der
	 statischen Berechnung der Recordarit�ten. *)
	structure StringListHash = MakeHashImpMap(StringListHashKey)

	(* Hashtabelle f�r Integers. Wird ben�tigt zum statischen
	 Generieren von Integerkonstanten. *)
	structure LitHash = MakeHashImpMap (LitHashKey)

	structure StampSet = MakeHashImpSet(type t=stamp val hash=Stamp.hash)

	val toplevel = Stamp.new()
	val illegalStamp = Stamp.new()
	val illegalId = Id ((0,0), illegalStamp, InId)
	val DEBUG = ref 0
	val ECHO = ref 0

	structure Lambda = MakeLambda(structure StampSet=StampSet
				      structure StampHash=StampHash
				      val toplevel=toplevel)

	(* Labelz�hler, aNewLabel liefert einen neuen String "label?", ? ist Zahl.
	 Den ersten Stack brauchen wir, damit f�r jede Klasse wieder bei label1
	 begonnen wird. *)
	structure Label =
	    struct
		val labelcount = ref 0
		val stack : (int list) ref= ref nil
		val handleStack : (string list) ref = ref nil

		fun new () =
		    (labelcount := !labelcount + 1;
		     "label"^Int.toString(!labelcount))
		fun push () =
		    (stack := (!labelcount :: (!stack));
		     labelcount := 0)
		fun pop () = (labelcount:=hd(!stack);
				    stack:=tl(!stack))
		fun newNumber () =
		    (labelcount := !labelcount + 1;
		     !labelcount)
		fun fromNumber i = "label"^Int.toString i

		fun pushANewHandle () =
		    let
			val label'=new()
		    in
			handleStack := label'::(!handleStack);
			label'
		    end
		fun popHandle () =
		    hd (!handleStack) before handleStack := tl (!handleStack)
	    end

	(* Verwaltung der lokalen JVM-Register *)
	structure Local =
	    struct
		local
		    val localscount = ref 1
		    val stack:int list ref = ref nil
		    val register: int StampHash.t    = StampHash.new ()
		    val lambda  : int StampHash.t  = StampHash.new ()
		    val fields  : string StampHash.t = StampHash.new ()
		in
		    (* Nummer des n�chsten freien lokalen Registers der aktuellen Methode. *)
		    fun nextFree () = (localscount := !localscount + 1;
				       !localscount)

		    (* Betreten bzw. Verlassen einer (Unter-) Funktion . *)
		    fun push () = (stack := (!localscount)::(!stack);
					 localscount := 1)
		    fun pop () =
			case !stack of
			    ((lc)::rest) => (stack := rest; localscount := lc)
			  | nil => raise Error("empty locals stack")
		    fun max () = !localscount

		    (* Zuordnung von Ids zu JVM-Registern *)
		    fun assign (Id(_,stamp',InId), wohin) =
			(StampHash.insert (register, stamp', wohin);
			 wohin)
		      | assign (Id(_,stamp',ExId name), wohin) =
			(StampHash.insert (register, stamp', wohin);
			 StampHash.insert (fields, stamp', name);
			 wohin)

		    fun get stamp' =
			case StampHash.lookup (register, stamp') of
			    NONE => 1 (* nichtgebundene Stamps sind formale Parameter. *)
			  | SOME register => register

		    (* Zuordnung von Ids zu JVM-Registern mit definierender Funktion *)
		    (* xxx inzwischen �berfl�ssig ? *)
		    fun assignLambda (Id(_,stamp',_), wohin) =
			(StampHash.insert(lambda,stamp',wohin);
			 wohin)
		    fun getLambda stamp' = (* xxx Unterschied zu Lambda.getLambda? *)
			case StampHash.lookup(lambda,stamp') of
			    NONE => ~1
			  | SOME lambda => lambda

		    fun generateVariableTable rest =
			StampHash.foldi
			(fn (stamp',register',rest') =>
			 Comment ("var "^(Int.toString register')^": ("^
			      (case StampHash.lookup(fields, stamp') of
				  NONE => "anonymous, "^(Stamp.toString stamp')
				| SOME x => x^", "^(Stamp.toString stamp'))^")")::
			 rest')
			rest
			register
		end
	    end

	(* Zuordnung von Funktionen-Ids auf Freie Variablen
	 und von beliebigen Ids auf den formalen Parameter der umgebenden Funktion.
	 Der formale Parameter einer Funktion ist immer eindeutig, w�hrend eine Funktion
	 mehrere Bezeichner zugeordnet haben kann. *)

	structure FreeVars =
	    struct
		val free:stamp list StampHash.t=StampHash.new ()
		val defFun:stamp StampHash.t=StampHash.new ()

		(* Freie Variablen einer Id setzen oder auslesen *)
		fun setVars (stamp', freeVarList) =
		    StampHash.insert (free, stamp', freeVarList)
		fun getVars (Id(_,stamp',_)) =
		    (* Falls undef, exception Option *)
		    valOf (StampHash.lookup (free, stamp'))

		(* Umgebende Funktion einer Id setzen oder auslesen *)
		fun setFun (Id(_,stamp',_), stamp'') =
		    if (isSome (StampHash.lookup(defFun, stamp')))
			then print "setFun twice!"
		    else
			 StampHash.insert(defFun, stamp', stamp'')
		fun getFun stamp' =
		    case StampHash.lookup (defFun, stamp') of
			NONE => toplevel
		      | SOME stamp'' => stamp''
		fun printFun () =
		    StampHash.appi (fn (stamp', stamp'') =>
				    print ("("^Stamp.toString stamp'^","^
					   Stamp.toString stamp''^")"))
		    defFun
	    end


	(* Die innerste Catch-Klausel mu� in der Exceptiontable ganz oben stehen.
	 Die Liste mu� also umgedreht werden *)
	structure Catch =
	    struct
		val stack=ref (nil:INSTRUCTION list list)
		val liste=ref (nil:INSTRUCTION list)

		fun add x = liste := x::(!liste)
		fun push () = (stack := (!liste)::(!stack); liste:=nil)
		fun pop () = (liste:=hd(!stack); stack:=(tl (!stack)))
		fun top () = !liste
	    end


	fun atCodeInt (i:Int32.int) =
	    if LargeInt.>= (i, Int.toLarge ~1) andalso LargeInt.<= (i, Int.toLarge 5)
		then Iconst (Int.fromLarge i) else
		    if LargeInt.>= (i, Int.toLarge ~128)
			andalso LargeInt.<= (i, Int.toLarge 127)
			then Bipush (Int.fromLarge i) else
			    if LargeInt.>= (i, Int.toLarge ~32768)
				andalso LargeInt.<= (i, Int.toLarge 32767)
				then Sipush (Int.fromLarge i)
			    else Ldc (JVMInt i)

	fun atCodeWord (i:Word32.word) =
	    if LargeWord.>= (i, Word.toLargeWord (Word.fromInt ~1)) andalso
		LargeWord.<= (i, Word.toLargeWord (Word.fromInt 5))
		then Iconst (Int.fromLarge (LargeWord.toLargeInt i)) else
		    if LargeWord.>= (i, Word.toLargeWord(Word.fromInt ~128))
			andalso LargeWord.<= (i, Word.toLargeWord(Word.fromInt 127))
			then Bipush (Int.fromLarge (LargeWord.toLargeInt i)) else
			    if LargeWord.>= (i, Word.toLargeWord(Word.fromInt ~32768))
				andalso LargeWord.<= (i, Word.toLargeWord (Word.fromInt 32767))
				then Sipush (Int.fromLarge (LargeWord.toLargeInt i))
			    else Ldc (JVMWord i)

	fun atCode (CharLit c) = atCodeInt(Int.toLarge (ord c))
	  | atCode (IntLit i)  = atCodeInt i
	  | atCode (RealLit r) =
	    let
		val r = valOf(Real.fromString r)
	    in
		if (Real.sign (r-0.0)=0)
		    orelse (Real.sign(r-1.0)=0)
		    orelse (Real.sign (r-2.0)=0) then Fconst (trunc r)
		else Ldc (JVMFloat r)
	    end
	  | atCode (StringLit s)= Ldc (JVMString s)
	  | atCode (WordLit w)  = atCodeInt (LargeWord.toLargeInt w)

	(* Die Arit�ten von Records koennen statisch gebaut werden. Zur
	 Laufzeit gen�gt es, ein (statisches) Feld der Hauptklasse
	 auszulesen. *)
	structure RecordLabel =
	    struct
		(* Die Arit�ten werden in einer Hashtabelle verwaltet,
		 um doppeltes Generieren zu vermeiden. *)
		val arity: int StringListHash.t = StringListHash.new ()
		val number = ref 0

		fun fieldname number = "arity"^Int.toString number

		fun staticfield number =
		    (Class.getLiteralName()^"/"^(fieldname number), [Arraysig, Classsig CLabel])

		(* Hinzuf�gen einer Recordarity *)
		fun insert (strings as (s::rest)) =
		     case StringListHash.lookup (arity, strings) of
			 NONE => (
				  number := ((!number)+1);
				  StringListHash.insert (arity, strings, !number);
				  staticfield (!number))
		       | SOME number' => staticfield number'

		(* Generieren aller Recordarities zur �bersetzungszeit *)
		fun generate () =
		    let
			fun codeall (strs, aritynumber, acc) =
			    let
				val size=List.length strs
				fun codeone (str, (acc, n)) =
				    let
					val c =
					    (Dup::
					     (atCodeInt n)::
					     (New CLabel)::
					     Dup::
					     (Ldc (JVMString str))::
					     (Invokespecial (CLabel, "<init>",
							     ([Classsig CString], [Voidsig])))::
					     Aastore::
					     acc)
					val d = LargeInt.+ (n, Int.toLarge 1)
				    in
					if d=(Int.toLarge size)
					    then
						(atCodeInt (Int.toLarge size)::
						 (Anewarray CLabel)::
						 c,d)
					else (c,d)
				    end

				val (r,_) = List.foldr
				    codeone
				    (Putstatic (staticfield aritynumber)::
				     acc,
				     0)
				    strs
			    in
				r
			    end
		    in
			StringListHash.foldi
			codeall
			[Return]
			arity
		    end

		(* Erzeugen der .field Eintr�ge *)
		fun makefields () =
		    StringListHash.fold
		    (fn (number, fields) =>
		     Field ([FPublic, FStatic, FFinal],
			    fieldname number,
			    [Arraysig, Classsig CLabel])::fields)
		    nil
		    arity
	    end

	(* Literale zu Konstruieren ist aufgrund unserer
	 Wrapper-Klassen recht teuer. Wir bauen sie daher zur
	 Compilezeit und schreiben sie in statische Felder. *)
	structure Literals =
	    struct
		(* Die Konstanten werden in einer Hashtabelle
		 verwaltet, um doppeltes Generieren zu vermeiden. *)
		val lithash: int LitHash.t = LitHash.new ()
		val number = ref 0

		fun litClass (CharLit _) = CChar
		  | litClass (IntLit _)    = CInt
		  | litClass (RealLit _)   = CReal
		  | litClass (StringLit _) = CStr
		  | litClass (WordLit _)   = CWord

		fun fieldname number = "lit"^Int.toString number

		fun staticfield number =
		    Class.getLiteralName()^"/"^(fieldname number)

		(* Hinzuf�gen einer Konstanten *)
		fun insert lit' =
		    case LitHash.lookup (lithash, lit') of
			NONE => (number := ((!number)+1);
				 LitHash.insert (lithash, lit', !number);
				 staticfield (!number))
		      | SOME number' => staticfield number'

		(* Erzeugen aller Literale zur
		 �bersetzungszeit *)
		fun generate startwert =
		     let
			 fun codelits (lit', constnumber, acc) =
			     let
				 val jType = case lit' of
				     CharLit _   => ([Charsig],[Voidsig])
				   | IntLit _    => ([Intsig],[Voidsig])
				   | RealLit _   => ([Floatsig],[Voidsig])
				   | StringLit _ => ([Classsig CString], [Voidsig])
				   | WordLit _   => ([Intsig],[Voidsig])
				 and scon = litClass lit'
			     in
				 (New scon ::
				  Dup ::
				  atCode lit' ::
				  (Invokespecial (scon,"<init>",jType)) ::
				  (Putstatic ((staticfield constnumber),[Classsig scon])) ::
				  acc)

			     end
		     in
			 LitHash.foldi codelits startwert lithash
		     end

		(* Erzeugen der .field Eintr�ge *)
		fun makefields startwert =
		    LitHash.foldi
		    (fn (lit', number, fields) =>
		     Field ([FPublic, FStatic, FFinal],
			    fieldname number,
			    [Classsig (litClass lit')])::fields)
		    startwert
		    lithash
	    end

	fun psl (x::nil) = Stamp.toString x
	  | psl (x::xs)  = Stamp.toString x^", "^(psl xs)
	  | psl nil = ""

	fun printStampList xs = print ("Free: ("^(psl xs)^")\n")

    end
