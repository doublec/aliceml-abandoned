(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Backend=
    struct
	open Common

	type stamp=IntermediateGrammar.stamp

	(* Hashtabelle für Stamps. *)
	structure StampHash = MakeHashImpMap(type t=stamp val hash=Stamp.hash)

	(* Mengen von Stamps. Wird u.a. zur Berechnung der freien
	 Variablen benutzt. *)
	structure StampSet = MakeHashScopedImpSet(type t=stamp
						  val hash=Stamp.hash)

	(* Hashtabelle für Listen von Strings. Wird benötigt bei der
	 statischen Berechnung der Recordaritäten. *)
	structure StringListHash = MakeHashImpMap(StringListHashKey)

	(* Hashtabelle für Strings. Wird zum Verschmelzen von Labels benutzt. *)
	structure StringHash = MakeHashImpMap(StringHashKey)

	(* Set of Strings. Used for Labelfusion. *)
	structure StringSet = MakeHashImpSet(StringHashKey)

	(* Hashtabelle für Integers. Wird benötigt zum statischen
	 Generieren von Integerkonstanten. *)
	structure LitHash = MakeHashImpMap (LitHashKey)

	structure StampSet = MakeHashImpSet(type t=stamp val hash=Stamp.hash)

	val toplevel = Stamp.new()
	val illegalStamp = Stamp.new()
	val illegalId = Id ((0,0), illegalStamp, InId)
	val DEBUG = ref 0
	val ECHO = ref 0
	val OPTIMIZE = ref 0

	structure Lambda = MakeLambda(structure StampSet=StampSet
				      structure StampHash=StampHash
				      val toplevel=toplevel)

	(* Labelzähler, aNewLabel liefert einen neuen String "label?", ? ist Zahl.
	 Den ersten Stack brauchen wir, damit für jede Klasse wieder bei label1
	 begonnen wird. *)
	structure Label =
	    struct
		val labelcount = ref 0
		val stack : (int list) ref= ref nil
		val handleStack : (string list) ref = ref nil

		fun newNumber () =
		    (labelcount := !labelcount + 1;
		     !labelcount)

		fun new () =
		    "label"^Int.toString (newNumber ())

		fun newSwitch () =
		    "switch"^Int.toString (newNumber ())

		fun push () =
		    (stack := (!labelcount :: (!stack));
		     labelcount := 0)

		fun pop () = (labelcount:=hd(!stack);
				    stack:=tl(!stack))

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
	structure Register =
	    struct
		local
		    val localscount = ref 1
		    val stack:(int * int StampHash.t) list ref = ref nil
		    val register: int StampHash.t ref   = ref (StampHash.new ())
		    val lambda  : int StampHash.t  = StampHash.new ()
		    val fields  : string StampHash.t = StampHash.new ()
		in
		    (* Nummer des nächsten freien lokalen Registers der aktuellen Methode. *)
		    fun nextFree () = (localscount := !localscount + 1;
				       !localscount)

		    (* Betreten bzw. Verlassen einer (Unter-) Funktion . *)
		    fun push () = (stack := (!localscount, !register)::(!stack);
				   localscount := 1;
				   register := StampHash.new())
		    fun pop () =
			case !stack of
			    ((lc,regs)::rest) => (stack := rest;
						  localscount := lc;
						  register := regs)
			  | nil => raise Error("empty locals stack")
		    fun max () = !localscount

		    (* Zuordnung von Ids zu JVM-Registern *)
		    fun assign (Id(_,stamp',InId), wohin) =
			(StampHash.insert (!register, stamp', wohin);
			 wohin)
		      | assign (Id(_,stamp',ExId name), wohin) =
			(StampHash.insert (!register, stamp', wohin);
			 StampHash.insert (fields, stamp', name);
			 wohin)

		    fun get stamp' =
			case StampHash.lookup (!register, stamp') of
			    NONE => 1 (* nichtgebundene Stamps sind formale Parameter. *)
			  | SOME reg => reg

		    (* Zuordnung von Ids zu JVM-Registern mit definierender Funktion *)
		    (* xxx inzwischen überflüssig ? *)
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
			(!register)
		end
	    end

	(* Zuordnung von Funktionen-Ids auf Freie Variablen
	 und von beliebigen Ids auf den formalen Parameter der umgebenden Funktion.
	 Der formale Parameter einer Funktion ist immer eindeutig, während eine Funktion
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
(*		    if (isSome (StampHash.lookup(defFun, stamp')))
			then print "setFun twice!"
		    else*)
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


	(* Die innerste Catch-Klausel muß in der Exceptiontable ganz oben stehen.
	 Die Liste muß also umgedreht werden *)
	structure Catch =
	    struct
		val stack=ref (nil:INSTRUCTION list list)
		val liste=ref (nil:INSTRUCTION list)

		fun add x = liste := x::(!liste)
		fun push () = (stack := (!liste)::(!stack); liste:=nil)
		fun pop () = (liste:=hd(!stack); stack:=(tl (!stack)))
		fun top () = !liste
	    end


	fun atCodeInt (i:LargeInt.int) =
	    if LargeInt.>= (i, Int.toLarge ~1) andalso LargeInt.>= (Int.toLarge 5, i)
		then Iconst (Int.fromLarge i) else
		    if LargeInt.>= (i, Int.toLarge ~128)
			andalso LargeInt.>= (Int.toLarge 127, i)
			then Bipush (Int.fromLarge i) else
			    if LargeInt.>= (i, Int.toLarge ~32768)
				andalso LargeInt.>= (Int.toLarge 32767, i)
				then Sipush (Int.fromLarge i)
			    else Ldc (JVMInt i)

	fun atCodeWord (i:LargeWord.word) =
	    if LargeWord.>= (i, Word.toLargeWord (Word.fromInt ~1)) andalso
		LargeWord.>= (Word.toLargeWord (Word.fromInt 5), i)
		then Iconst (Int.fromLarge (LargeWord.toLargeInt i)) else
		    if LargeWord.>= (i, Word.toLargeWord(Word.fromInt ~128))
			andalso LargeWord.>= (Word.toLargeWord(Word.fromInt 127), i)
			then Bipush (Int.fromLarge (LargeWord.toLargeInt i)) else
			    if LargeWord.>= (i, Word.toLargeWord(Word.fromInt ~32768))
				andalso LargeWord.>= (Word.toLargeWord (Word.fromInt 32767), i)
				then Sipush (Int.fromLarge (LargeWord.toLargeInt i))
			    else Ldc (JVMWord i)

	fun atCode (CharLit c) = atCodeInt(Int.toLarge (ord c))
	  | atCode (IntLit i)  = atCodeInt i
	  | atCode (RealLit r) =
	    let
		val r = valOf(Real.fromString r)
	    in
		if (Real.compare (r,0.0)=EQUAL)
		    orelse (Real.compare(r,1.0)=EQUAL)
		    orelse (Real.compare(r,2.0)=EQUAL) then Fconst (Real.trunc r)
		else Ldc (JVMFloat r)
	    end
	  | atCode (StringLit s)= Ldc (JVMString s)
	  | atCode (WordLit w)  = atCodeInt (LargeWord.toLargeInt w)

	(* Die Aritäten von Records koennen statisch gebaut werden. Zur
	 Laufzeit genügt es, ein (statisches) Feld der Hauptklasse
	 auszulesen. *)
	structure RecordLabel =
	    struct
		(* Die Aritäten werden in einer Hashtabelle verwaltet,
		 um doppeltes Generieren zu vermeiden. *)
		val arity: int StringListHash.t = StringListHash.new ()
		val number = ref 0

		fun fieldname number = "arity"^Int.toString number

		fun staticfield number =
		    (Class.getLiteralName()^"/"^(fieldname number), [Arraysig, Classsig CLabel])

		(* Hinzufügen einer Recordarity *)
		fun insert (strings as (s::rest)) =
		     case StringListHash.lookup (arity, strings) of
			 NONE => (
				  number := ((!number)+1);
				  StringListHash.insert (arity, strings, !number);
				  staticfield (!number))
		       | SOME number' => staticfield number'

		(* Generieren aller Recordarities zur Übersetzungszeit *)
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

		(* Erzeugen der .field Einträge *)
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

		(* Hinzufügen einer Konstanten *)
		fun insert lit' =
		    case LitHash.lookup (lithash, lit') of
			NONE => (number := ((!number)+1);
				 LitHash.insert (lithash, lit', !number);
				 staticfield (!number))
		      | SOME number' => staticfield number'

		(* Erzeugen aller Literale zur
		 Übersetzungszeit *)
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

		(* Erzeugen der .field Einträge *)
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
