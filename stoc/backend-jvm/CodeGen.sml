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

structure CodeGen =
    struct
	(* Intermediate Representation: *)
	open ImperativeGrammar
	open Prebound
	open Prebound'
	open Main

	(* Backend *)
	open JVMInst
	open ToJasmin
	open Abbrev
	open Backend

	val _ = Compiler.Control.Print.printLength := 10000;
	val _ = Compiler.Control.Print.printDepth := 10000;
	val _ = SMLofNJ.Internals.GC.messages false

	(* falls was böses passiert, wird eine Error-exception mit sinnvollem Inhalt 'geraist' *)
	exception Error of string

	(* xxx For Debugging: *)
	datatype deb=B of bool
	  | Is of id list
	  | Ias of id list array
	  | IasSS of id list array * int * int
	  | II of id * id
	  | Okay
	  | Dec of stm
	  | Test of test
	  | Exp of exp
	exception Debug of deb

	val d = ref nil:ImperativeGrammar.program ref
	(* Labelzähler, aNewLabel liefert einen neuen String "label?", ? ist Zahl.
	 Den ersten Stack brauchen wir, damit für jede Klasse wieder bei label1
	 begonnen wird. *)
	structure Label =
	    struct
		val labelcount = ref 0
		val stack : (int list) ref= ref nil

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
	    end

	(* Den Feldnamen zu einer Id bestimmen. Die Id kann eine beliebige Variable sein. *)
	fun fieldNameFromStamp stamp' = "field"^(Stamp.toString stamp')
	fun fieldNameFromId (Id(_,stamp',_)) = fieldNameFromStamp stamp'

	fun nameFromId (Id (_,stamp',InId)) = "unnamed"^(Stamp.toString stamp')
	  | nameFromId (Id (_,stamp',ExId name')) = name'^(Stamp.toString stamp')

	(* Den Stamp aus einer Id extrahieren. *)
	fun stampFromId (Id (_, stamp', _)) = stamp'

	(* Dieser Label steht am Ende der Registerinitialisierung von Methoden. *)
	val afterInit = "labelAfterInit"

	(* alpha steht am Begin einer Methode *)
	val alpha = "labelAlpha"

	(* Omega kommt vor dem abschliessenden Areturn *)
	val omega = "labelOmega"

	(* Lokales JVM-Register, in dem das Übersetzungsergebnis festgehalten wird. *)
	val mainpickle = ref ~1 (* JVM-Register, in dem Struktur steht *)

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
		    (* Nummer des nächsten freien lokalen Registers der aktuellen Methode. *)
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
			register
		end
	    end

	(* Name der aktuellen Klasse. Der Stack wird für verschachtelte Funktionen benötigt.
	 (für jede Funktion wird eine eigene Klasse erzeugt.) *)
	structure Class =
	    struct
		val stack = ref [""]
		val initial = ref ""

		fun getCurrent () = case !stack of (x::xs) => x | _ => raise Error("Class.getCurrent")
		fun push name = stack := name::(!stack)
		fun pop () =  case !stack of (x::xs) => stack := xs | _ => raise Error("Class.pop")
		fun setInitial name = ((stack := [name]); initial := name)
		fun getInitial () = (!initial)
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
		    (Class.getInitial ()^"/"^(fieldname number), [Arraysig, Classsig CLabel])

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
		     Field ([FPublic, FStatic],
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
		    Class.getInitial ()^"/"^(fieldname number)

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
		     Field ([FPublic, FStatic],
			    fieldname number,
			    [Classsig (litClass lit')])::fields)
		    startwert
		    lithash
	    end

	fun psl (x::nil) = Stamp.toString x
	  | psl (x::xs)  = Stamp.toString x^", "^(psl xs)
	  | psl nil = ""

	fun printStampList xs = print ("Free: ("^(psl xs)^")\n")

	(* Berechnung der freien Variablen *)
	local
	    structure fV =
		struct
		    val free:ScopedStampSet.t= ScopedStampSet.new ()

		    fun insert (Id (_,stamp',_)) =
			(print ("Top "^(Stamp.toString (Lambda.top()))^
				". insert free: "^(Stamp.toString stamp')^"\n");
			 if stamp'=stamp_builtin orelse
			     Lambda.isSelfCall stamp'
			     then ()
			 else
			     ScopedStampSet.insert (free, stamp'))
		    fun delete (Id (_,stamp',_)) =
			(print ("Top "^(Stamp.toString (Lambda.top()))^
				" delete free: "^(Stamp.toString stamp')^"\n");
			 ScopedStampSet.delete(free, stamp'))

		    fun get () =
			let
			    val x = ScopedStampSet.foldScope (fn (x,xs) => x::xs) nil free
			in
			    print ("Top "^(Stamp.toString (Lambda.top())));
			    printStampList x;
			    x
			end

		    fun isFree stamp' =
			ScopedStampSet.member (free, stamp')

		    (* Betreten einer neuen Subfunktion. *)
		    fun enter () =
			 ScopedStampSet.insertScope free

		    (* Verlassen der Subfunktion. *)
		    fun exit () =
			 ScopedStampSet.mergeScope free
		end
	in
	    fun freeVarsExp (LitExp _) = ()
	      | freeVarsExp (VarExp (_, id')) = fV.insert id'
	      | freeVarsExp (ConAppExp (_, id', id'')) = (fV.insert id'; fV.insert id'')
	      | freeVarsExp (TupExp (_,ids)) = app fV.insert ids
	      | freeVarsExp (RecExp (_,labids)) = app (fn (lab, id') => fV.insert id') labids
	      | freeVarsExp (SelExp _) = ()
	      | freeVarsExp (FunExp(_,_, idbodys)) =
		let
		    fun freeVarsFun ((OneArg (id' as Id (_,stamp',name)),body')::idbodys') =
			(fV.enter();
			 Lambda.push id';
			 freeVarsDecs body';
			 fV.delete id';
			 FreeVars.setVars (stamp',fV.get ());
			 FreeVars.setFun (id', Lambda.top ());
			 freeVarsFun idbodys';
			 Lambda.setId();
			 Lambda.pop ();
			 case name of
			     ExId name' => Lambda.assignName
				 (stamp',name')
			   | InId => ();
			 fV.exit())
		      | freeVarsFun _ = () (* xxx noch bearbeiten! xxx *)
		in
		    freeVarsFun idbodys
		end
	      | freeVarsExp (AdjExp(_,id',id'')) =
		(fV.insert id';
		 fV.insert id'')
	      | freeVarsExp (AppExp(_,id', idargs')) =
		(fV.insert id';
		 case idargs' of
		     OneArg id'' => fV.insert id''
		   | TupArgs ids => app fV.insert ids
		   | RecArgs stringids => app
			 (fn (_,id') => fV.insert id') stringids
			 )
	      | freeVarsExp (ConExp(_, id', _)) = fV.insert id'
	      | freeVarsExp (SelAppExp(_,lab',id')) = fV.insert id'
	      | freeVarsExp e = raise Debug (Exp e)

	    and freeVarsDec (RaiseStm(_,id')) = fV.insert id'
	      | freeVarsDec (HandleStm(_,body',id',body'')) =
		(freeVarsDecs body'';
		 freeVarsDecs body';
		 FreeVars.setFun (id', Lambda.top());
		 fV.delete id')
	      | freeVarsDec (EndHandleStm(_,body')) = freeVarsDecs body'
	      | freeVarsDec (TestStm(_,id',test',body',body'')) =
		(freeVarsDecs  body'';
		 freeVarsDecs  body';
		 freeVarsTest test';
		 fV.insert id')
	      | freeVarsDec (SharedStm(_,body',raf as ref 0)) = (raf := ~1; freeVarsDecs body')
	      | freeVarsDec (SharedStm _) = ()
	      | freeVarsDec (ValDec(_,id',exp', _)) =
		(Lambda.pushFun id';
		 freeVarsExp exp';
		 FreeVars.setFun (id', Lambda.top());
		 fV.delete id';
		 Lambda.popFun ())
	      | freeVarsDec (RecDec(_,idsexps, _)) =
		let
		    fun freeVarsRecDec ((id',exp')::rest) =
			(Lambda.pushFun id';
			 freeVarsExp exp';
			 FreeVars.setFun (id',Lambda.top());
			 freeVarsRecDec rest;
			 fV.delete id';
			 Lambda.popFun ())
		      | freeVarsRecDec nil = ()
		in
		    freeVarsRecDec idsexps
		end
	      | freeVarsDec (ConDec(_,id',_, _)) = (fV.delete id';
						 FreeVars.setFun (id', Lambda.top()))
	      | freeVarsDec (EvalStm(_, exp')) = freeVarsExp exp'
	      | freeVarsDec (ReturnStm(_,exp')) = freeVarsExp exp'
	      | freeVarsDec (ExportStm _) = ()
	    and
		freeVarsTest (LitTest _) = ()
	      | freeVarsTest (ConTest(id',NONE)) = fV.insert id'
	      | freeVarsTest (ConTest(id',SOME id'')) = (fV.insert id'; fV.delete id'')
	      | freeVarsTest (RecTest stringlablist) =
		app (fn (_,id') => fV.delete id') stringlablist
	      | freeVarsTest (TupTest lablist) = app fV.delete lablist
	      | freeVarsTest (LabTest(_,id')) = fV.delete id'

	    and freeVarsDecs (decs) =
		(app freeVarsDec (List.rev decs)
		 (*;app (fn
		      (ValDec (_,id',_,_)) => fV.delete id'
		    | RecDec (_,idexps,_) => app (fn (id',_) => fV.delete id') idexps
		    | ConDec (_,id',_,_) => fV.delete id'
		    | _ => ())
		 decs*))
	end

(* Das macht jetzt alles Leif. *)
	  (*	fun annotateTailExp (AppExp (_,_,_,tailPos)) = tailPos:=true
	  | annotateTailExp (FunExp (foo, bar, (ids, exp')::idsexps)) =
	    (annotateTailExp exp';
	     annotateTailExp (FunExp(foo, bar, idsexps)))
	  | annotateTailExp (AdjExp _) = raise Error "seltsame Operation AdjExp"
(*	  | annotateTailExp (SeqExp (_, exp'::nil)) = annotateTailExp exp'
	  | annotateTailExp (SeqExp (foo, exp::exps)) = annotateTailExp (SeqExp(foo,exps)) *)
	(* Der Ausdruck, in dem die Exception gehandlet wird, steht eigentlich auch in Tailposition.
	 Allerdings bilden wir bei der Codeerzeugung Handles direkt auf den Java Handle-Mechanismus ab,
	 der nur innerhalb einer Methode arbeitet. Bei unserer Behandlung von Tailrekursion wird aber
	 die Methode verlassen, so daß wir den Handle-Ausdruck duplizieren müßten, also lassen wir es
	 ganz. *)
	  | annotateTailExp (HandleStm (_, _, _, exp')) = annotateTailExp exp'
(*	  | annotateTailExp (LetExp (_, decs, exp')) = (app annotateTailDec decs; annotateTailExp exp') *)
	  | annotateTailExp _ = ()
	and annotateTailDec (ValDec (_,_,exp')) = annotateTailExp exp'
(*	  | annotateTailDec (Waldeck (_,_,exp')) = annotateTailExp exp' *)
	  | annotateTailDec (RecDec (foo,(_,exp')::idsexps)) =
	    (annotateTailExp exp'; annotateTailDec (RecDec (foo, idsexps)))
	  | annotateTailDec _ = () *)

	(* Den Klassennamen einer Id bestimmen, die üblicherweise die Id eines formalen
	 Funktionsparameters ist. *)
	fun classNameFromStamp stamp' = Class.getInitial()^"class"^(Stamp.toString stamp')
	fun classNameFromId (Id (_,stamp',_)) = classNameFromStamp stamp'

	(* Einstiegspunkt *)
	fun genProgramCode (name, program) =
	    (Class.setInitial name;
	     let
		 (* freie Variablen berechnen. *)
		 val _ = app freeVarsDec program
		 val _ = FreeVars.printFun ()
		 val _ = Lambda.createIdsLambdaTable()
		 (* val _ = app annotateTailDec program*)
		 (* Alle Deklarationen übersetzen *)
		 val insts = decListCode program

		 (* JVM-Register initialisieren. *)
		 fun initializeLocals 0 = [Label afterInit]
		   | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		 val iL = initializeLocals (Local.max())

		 val _ = if (!mainpickle= ~1) then
		     raise Error "Derzeit sind nur Strukturen übersetzbar." else ()

		 (* Verschachtelt endrekursive Funktionsaufrufe werden uebersetzt, indem im
		  aktuellen Thread-Objekt die naechste Methode gespeichert wird. Da solche
		  Aufrufe auch waehrend der Initialisierung stattfinden koennen, muss auch
		  diese in einem DML-Thread ablaufen. main erzeugt also lediglich eine neue
		  Instanz der eigenen Klasse und startet einen neuen Thread. Die eigentliche
		  Initialisierungsarbeit geschieht in run. *)
		 val main = Method([MStatic,MPublic],"main",([Arraysig, Classsig CString],[Voidsig]),
				   Locals 1,
				   Local.generateVariableTable
				   [New name,
				    Dup,
				    Invokespecial (name, "<init>", ([], [Voidsig])),
				    Invokevirtual (CThread, "start", ([], [Voidsig])),
				    Return], nil, false)
		 (* Standardinitialisierung. Die Superklasse wird aufgerufen. *)

		 val clinit = Method([MPublic],"<clinit>",([],[Voidsig]),
				   Locals 6,
				     Literals.generate(
						      RecordLabel.generate()),
				   nil, false)

		 val init = Method([MPublic],"<init>",([],[Voidsig]),
				   Locals 1,
				   [Aload 0,
				    Invokespecial (CDMLThread, "<init>", ([], [Voidsig])),
				    Return], nil, false)
		 (* In run wird die Toplevel-Umgebung aufgebaut. Alle Objekte werden erzeugt
		  und initialisiert, die Funktionsabschluesse werden gebildet. Anschliessend
		  wird das Ergebnis zusammen mit den Classfiles in ein Pickle geschrieben.
		  Dies ist der letzte Schritt des Compilierungsvorganges. *)
		 val run = Method([MPublic], "run", ([], [Voidsig]),
				  Locals (Local.max()+1),
				   iL @
				   insts @
				   [Getstatic ("java/lang/System/out",[Classsig "java/io/PrintStream"]),
				    Aload (!mainpickle),
				    Invokevirtual ("java/io/PrintStream","print",
						   ([Classsig "java/lang/Object"],
						   [Voidsig])),
				    Getstatic CPickle,
				    New CTuple,
				    Dup,
				    Iconst 2,
				    Anewarray CVal,
				    Dup,
				    Iconst 0,
				    New CStr,
				    Dup,
				    Ldc (JVMString (name^".pickle")),
				    Invokespecial (CStr, "<init>",
						   ([Classsig CString], [Voidsig])),
				    Aastore,
				    Dup,
				    Iconst 1,
				    Aload (!mainpickle),
				    Aastore,
				    Invokespecial (CTuple, "<init>",
						   ([Arraysig, Classsig CVal],
						    [Voidsig])),
				    Invokeinterface (CVal, "apply",
						     ([Classsig CVal],
						      [Classsig CVal])),
				    Pop,
				    Return],
				  Catch.top(), false)
		 (* die Hauptklasse *)
		 val class = Class([CPublic],
				   name,
				   CDMLThread,
				   Literals.makefields
				   (RecordLabel.makefields ()),
				   [main, clinit, init, run])
		 val ziel = schreibsAuf (name^".j");
	     in
		 print "Erzeuge Hauptklasse...";
		 classToJasmin (class,ziel);
		 schreibsZu ziel;
		 print "ferdich\n"
	     end
	 )

	and getBuiltin builtin' =
	    [Getstatic (Literals.insert (StringLit builtin'), [Classsig CStr]),
	     Invokestatic (CBuiltin, "getBuiltin",
			   ([Classsig CStr], [Classsig CVal]))]

	and builtinStamp stamp' =
	    if stamp'=stamp_plus then ([Getstatic CPlus],true) else
		if stamp'=stamp_Match then ([Getstatic CMatch],true) else
		    if stamp'=stamp_false then ([Getstatic CFalse],true) else
			if stamp'=stamp_true then ([Getstatic CTrue],true) else
			    if stamp'=stamp_nil then ([Getstatic CNil],true) else
				if stamp'=stamp_cons then ([Getstatic CCons],true) else
				    if stamp'=stamp_ref then ([Getstatic CRef],true) else
					if stamp'=stamp_Bind then ([Getstatic CBind],true) else
					    if stamp'=stamp_eq then ([Getstatic CEquals],true) else
						if stamp'=stamp_assign then ([Getstatic CAssign],true) else
						    (nil,false)


	and decListCode decs = List.concat (map decCode decs)

	(* Codegenerierung für Deklarationen *)
	and decCode (ValDec(_, id', exp',_)) =
	    let
		val loc = Local.assign(id', Local.nextFree())
	    in
		(Lambda.pushFun id';
		 (expCode exp' @
		  [Comment "Store 2", Astore loc])
		 before Lambda.popFun())
	    end
	  | decCode (RecDec (_, nil, _)) = nil
	  | decCode (RecDec (_,idexps as ((recid,_)::_),_)) =
	    (* RecDec of coord * (id * exp) list * isTopLevel *)
	    (* 1. Für alle ids ein passendes (nichtabgeschlossenes) Objekt bauen *)
	    (*    und jeweils in ein frisches Register stopfen. *)
	    (* 2. exp auswerten und Ergebnis wegwerfen*)
	    (* !! die Abschlüsse der Objekte passieren beim Auswerten von exp *)
	    let
		(* 1. Schritt *)
		local
		    fun init ((id',exp')::ids'') =
			let
			    val loc = Local.assign(id',Local.nextFree())
			    fun funList ((OneArg id'',_)::rest) =
				let
				    val className = classNameFromId id''
				    (* Der formale Parameter einer Funktion wird immer in
				     Register 1 übergeben. *)
				    val _ = Local.assign(id'',1)
				    val _ = Local.assignLambda(id'', loc)
				in
				    [New className,
				     Dup,
				     Invokespecial (className, "<init>",
						    ([],[Voidsig])),
				     Astore loc] @
				    (funList rest)
				end
			      | funList nil = nil
			    val one = case exp' of
				(* Benutzerdefinierte Funktion *)
				FunExp (_,_,idexplist) =>
				    funList idexplist
			      (* Konstruktorapplikation. *)
			      | ConAppExp (_,id',id'') =>
				    ([New CConVal,
				      Dup] @
				     (idCode id') @
				     [Invokespecial (CConVal, "<init>",
						     ([Classsig CConstructor],
						      [Voidsig])),
				      Dup,
				      Astore loc] @
				     (idCode id'') @
				     [Invokeinterface (CConVal, "setContent",
						       ([Classsig CVal],
							[Voidsig]))])
			      | VarExp (_, id') => (idCode id'@
						    [Astore loc])
			      (* ConVal *)
			      (* ConExp _ => [New CConVal,
				  Dup,
				  Invokespecial (CConVal,"<init>", ([],Voidsig)),
				  Astore loc] xxx Varexp *)
			      (* Record mit Arity *)
			      | RecExp _ =>
				    [New CRecord,
				     Dup,
				     Invokespecial (CRecord,"<init>",
						    ([],[Voidsig])),
				     Astore loc]
			      (* Tuple *)
			      | TupExp _ =>
				    [New CTuple,
				     Dup,
				     Invokespecial (CTuple,"<init>",
						    ([],[Voidsig])),
				     Astore loc]
			in
			    one @ init ids''
			end
		      | init nil = nil
		in
		    val initcode = init idexps
		end
		    (* 2. Schritt *)
		local
		    fun evalexp ((id',exp')::idexps') =
			(Lambda.pushFun id';
			 (expCode exp')
			 before Lambda.popFun())
			@
			(Pop::
			 evalexp idexps')
		      | evalexp nil = nil
		in
		    val expcode = evalexp idexps
		end
	    in
		initcode @ expcode
	    end

	  | decCode (ConDec (_, id' as Id(_, stamp', name'), hasArgs,_)) =
	    (* ConDec of coord * id * bool *)
	    let
		val loc = Local.assign(id',Local.nextFree())
	    in
		if hasArgs then
		    let
			val constructorName = "constructor"^(Stamp.toString stamp')
		    in
			[New CConstructor,
			 Dup,
			 Ldc (JVMString constructorName),
			 Invokespecial (CConstructor, "<init>",
					([Classsig CString],[Voidsig])),
			 Astore loc]
		    end
		else
		    let
			val nameName = "name"^(Stamp.toString stamp')
		    in
			[New CName,
			 Dup,
			 Ldc (JVMString nameName),
			 Invokespecial (CName, "<init>",
					([Classsig CString],[Voidsig])),
			 Astore loc]
		    end
	    end
	  | decCode (RaiseStm(_,id')) =
		 [New CExWrap,
		  Dup] @
		 (idCode id') @
		 [Invokespecial(CExWrap,"<init>",
				([Classsig CVal],[Voidsig])),
		  Athrow]
	  | decCode (TestStm(_,id' as Id (_,stamp',_),test',body',body'')) =
	    (* teste id' mit test'. body' ist then-Fall, body'' else-Fall *)
	    let
		val danach = Label.new ()
		val elselabel = Label.new ()

		fun testCode (LitTest lit') =
		    let
			val eq = case lit' of
			    WordLit w' =>
				stampCode stamp' @
				[Instanceof CWord,
				 Ifeq elselabel] @
				(stampCode stamp') @
				[Checkcast CWord,
				 Getfield (CWord^"/value", [Wordsig]),
				 atCodeWord w',
				 Ificmpne elselabel]
			  | IntLit i' =>
				stampCode stamp' @
				[Instanceof CInt,
				 Ifeq elselabel] @
				(stampCode stamp') @
				[Checkcast CInt,
				 Getfield (CInt^"/value", [Intsig]),
				 atCodeInt i',
				 Ificmpne elselabel]
			  | CharLit c' =>
				stampCode stamp' @
				[Instanceof CChar,
				 Ifeq elselabel] @
				(stampCode stamp') @
				[Checkcast CChar,
				 Getfield (CChar^"/value", [Charsig]),
				 atCodeInt (Int.toLarge (Char.ord c')),
				 Ificmpne elselabel]
			  | StringLit s' =>
				stampCode stamp' @
				[Instanceof CStr,
				 Ifeq elselabel] @
				(stampCode stamp') @
				[Checkcast CStr,
				 Getfield (CStr^"/value", [Classsig CString]),
				 Ldc (JVMString s'),
				 Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])),
				 Ifeq elselabel]
			  | r as (RealLit r') =>
				stampCode stamp' @
				[Instanceof CReal,
				 Ifeq elselabel] @
				(stampCode stamp') @
				[Checkcast CReal,
				 Getfield (CReal^"/value", [Floatsig]),
				 atCode r,
				 Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])),
				 Ifeq elselabel]
		    in
			eq
		    end
		  | testCode (ConTest (id'',NONE)) =
		    Comment "Hi8" ::
		    (stampCode stamp') @
		    (idCode id'') @
		    [Ifacmpne elselabel]
		  | testCode (ConTest(id'',SOME id''')) =
		    let
			val loc = Local.assign(id''',Local.nextFree())
			val _ = FreeVars.setFun (id''', Lambda.top())
		    in
			Comment "Hi9" ::
			 (stampCode stamp') @
			 [Instanceof CConVal,
			  Ifeq elselabel] @
			 (stampCode stamp') @
			 [Checkcast CConVal,
			  Invokeinterface (CConVal, "getConstructor",
					   ([], [Classsig CConstructor])),
			  Comment "Hi10"] @
			 (idCode id'') @
			 (Ifacmpne elselabel ::
			  (idCode id')) @
			 [Checkcast CConVal,
			  Invokeinterface (CConVal, "getContent",
					   ([],[Classsig CVal])),
			  Astore loc]
		    end
		  | testCode (RecTest stringid) =
		    (* Arity laden, vergleichen, dann binden *)
		    let
			(* dreht Liste um und entfernt überflüssige ids *)
			fun stringids2strings ((l, _)::stringids',s')=
			    stringids2strings (stringids', l::s')
			  | stringids2strings (nil, s') = s'
			fun bindit ((_,id'')::nil,i) =
			    let
				val loc = Local.assign(id'',Local.nextFree())
				val _ = FreeVars.setFun (id'', Lambda.top())
			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) :: nil
			    end
			  | bindit ((_,id'')::rest,i) =
			    let
				val loc = Local.assign(id'',Local.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())
			    in
				Dup ::
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				bindit(rest,i+1)
			    end
			  | bindit (nil,_) = nil
		    in
			New CRecordArity ::
			Dup ::
			(atCodeInt (Int.toLarge (length stringid))) ::
			(Anewarray CLabel) ::
			(Getstatic (RecordLabel.insert
				    (stringids2strings (stringid, nil)))) ::
			(Invokespecial (CRecordArity,"<init>",
					([Arraysig,Classsig CLabel],
					 [Voidsig]))) ::
			(Comment "Hi11") ::
			((stampCode stamp') @
			 ((Invokevirtual (CRecord,"getArity",
					  ([],[Classsig CRecordArity]))) ::
			  (Ifacmpne elselabel) ::
			  (stampCode stamp'))) @
			[Invokevirtual (CRecord,"getValues",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(stringid,0))
		    end
		  | testCode (TupTest ids) =
		    (* Arity vergleichen (int), dann binden *)
		    let
			fun bindit (id''::nil,i) =
			    let
				val loc = Local.assign(id'',Local.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())

			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				nil
			    end
			  | bindit (id''::rest,i) =
			    let
				val loc = Local.assign(id'',Local.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())
			    in
				Dup ::
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				bindit(rest,i+1)
			    end
			  | bindit (nil,_) = nil
		    in
			stampCode stamp'@
			[Checkcast CDMLTuple,
			 Invokeinterface (CDMLTuple,"getArity",([],[Intsig])),
			 atCodeInt (Int.toLarge (length ids)),
			 Ificmpne elselabel] @
			(stampCode stamp') @
			[Checkcast CDMLTuple,
			 Invokeinterface (CDMLTuple,"getVals",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(ids,0))
		    end
		  | testCode test' = raise Debug (Test test')

	    in
		(*zuerst @ *)
		(testCode test') @
		List.concat (map decCode body') @
		[Goto danach,
		 Label elselabel] @
		List.concat (map decCode body'') @
		[Label danach]
	    end

	  | decCode (SharedStm(_,body',da as ref schonda)) =
	    if schonda <= 0
		then
		    let
			val _ = da := Label.newNumber ()
		    in
			(Label (Label.fromNumber (!da)))::
			(List.concat (map decCode body'))
		    end
	    else
		 [Goto (Label.fromNumber schonda)]

	  | decCode (ReturnStm (_,ap as AppExp(_,id' as (Id (_,stamp',_)),arg'))) =
		(* Tailcall Applikation *)
		if Lambda.isSelfCall stamp' then
		    idArgCode arg' @
		    [Astore 1,
		     Goto afterInit]
		else
		    (expCode ap) @
		    [Areturn]
    (*else
     [Invokestatic (CThread, "currentThread", ([],Classsig CThread)),
     Checkcast CDMLThread] @
     (idCode(id')) @
     ((Putfield (CDMLThread^"/tail",CVal))::
     (idCode(id'')))@
     [Areturn]
			else
			    let
				val (startlabel, endlabel) = (Label.new(), Label.new())
			    in
				[Label startlabel,
				Invokeinterface (CVal, "apply", ([Classsig CVal],Classsig CVal)),
				Invokestatic (CThread, "currentThread", ([],Classsig CThread)),
				Checkcast CDMLThread,
				Getfield (CDMLThread^"/tail",CVal),
				Dup,
				Ifnull endlabel,
				Swap,
				Goto startlabel,
				Label endlabel,
				Pop,
				Areturn]
    end*)
	  | decCode (ReturnStm (_, exp')) =
	(* sonstiges Return *)
		    (expCode exp')@
		    [Areturn]

	  | decCode (HandleStm(_,body', id',body'')) =
		    let
			val loc = Local.assign (id', Local.nextFree())
			val try   = Label.new()
			val to = Label.new()
			val using = Label.new()
			fun endHandleSearch (EndHandleStm(_,body')) =
			    Label to::
			    (List.concat(map decCode body'))
			  | endHandleSearch sonstwas = decCode sonstwas
			val b1 = List.concat (map endHandleSearch body')
			val b2 = List.concat (map decCode body'')
			val nocatch = Label.new()
		    in
			Catch.add (Catch (CExWrap, try, to, using));
			 (Label try)::
			 b1 @
			 [Goto nocatch] @
			 [Label using,
			  Invokevirtual (CExWrap,"getValue",
					 ([],[Classsig CVal])),
			  Astore loc] @
			 b2 @
			 [Label nocatch]
		    end
	  | decCode (EndHandleStm _) = raise Error "unexpected EndHandleStm"
	  | decCode (EvalStm (_, exp')) =
		    (expCode exp') @ [Pop]
	  | decCode (ExportStm (_,Id (_,stamp',_)::_)) = (mainpickle:=Local.get stamp'; nil)
	  | decCode dings = raise Debug (Dec dings)
	and
	    idCode (Id(_,stamp',_)) = stampCode stamp'
	and
	    stampCode stamp' =
	    let
		val (bstamp,isBuiltin) = builtinStamp stamp'
	    in
		if isBuiltin then bstamp
		else
		    if stamp'=(stampFromId (Lambda.getId
					    (Lambda.top())))
			then (* Zugriff auf die aktuelle
			      Funktion. Diese steht in Register 0. *)
			    (Lambda.noSapply ();
			     [Aload 0])
		    else
			if FreeVars.getFun stamp' = Lambda.top ()
			    (* Falls stamp' im aktuellen Lambda gebunden
			     wurde, kann die Variable direkt aus einem
			     JVM-Register geladen werden. *)
			    then
				if stamp' = Lambda.top() then
				    (* Stamp ist der formale Parameter der
				     aktuellen Funktion. Dieser liegt
				     immer in Register 1. *)
				    [Comment "Hi.Parm",
				     Aload 1]
				else
				    [Comment "Hi6",
				     Aload (Local.get stamp')]
			else
			    (* Es handelt sich um eine freie Variable, die
			     beim Abschluss bilden in ein Feld der
			     aktuellen Klasse kopiert wurde.
			     In diesem Fall kann apply nicht als statisch
			     deklariert werden. *)
			    (Lambda.noSapply ();
			     [Comment ("Hi. Stamp="^(Stamp.toString stamp')^
				       ". Lambda.top = "^Stamp.toString (Lambda.top())^
				       " in "^Int.toString (Local.get stamp')^
				       ". Fun = "^(Stamp.toString
						   (FreeVars.getFun
						    stamp'))^
				       "Id (Lambda.top) ="^(Stamp.toString (stampFromId
									  (Lambda.getId(Lambda.top()))))^"\n"),
			      Aload 0,
			      Getfield (Class.getCurrent()^"/"^(fieldNameFromStamp stamp'), [Classsig CVal])])
	    end
	and
	    idArgCode (OneArg id') = idCode id'

	  | idArgCode (TupArgs ids) =
	    let
		fun iac (id'::ids, accu) =
		    iac (ids, Dup::(idCode id'@(Aastore::accu)))
		  | iac (nil, accu) = accu
	    in
		[New CTuple,
		 Dup,
		 atCodeInt (Int.toLarge (List.length ids)),
		 Anewarray CVal]@
		(iac (ids, nil))@
		[Invokespecial (CTuple, "<init>",
				([Arraysig, Classsig CVal], [Voidsig]))]
	    end

	  | idArgCode (RecArgs stringids) =
	    let
		val arity = List.length stringids
		fun iac ((name',id')::ids, accu1,accu2) =
		    iac (ids,
			 (Dup::
			  (Ldc (JVMString name'))::
			  Aastore::
			  accu1),
			 (Dup::
			  (idCode id'@
			   (Aastore::
			    accu2))))
		  | iac (nil, accu1, accu2) =
		    (atCodeInt (Int.toLarge arity))::
		    (Anewarray CString)::
		    accu1@
		    ((atCodeInt (Int.toLarge arity))
		     ::(Anewarray CVal)
		     ::accu2)
	    in
		[New CTuple,
		 Dup] @
		(iac (stringids, nil, nil))@
		[Invokespecial (CTuple, "<init>",
				([Arraysig, Classsig CVal], [Voidsig]))]
	    end

	(*	  | expCode (SeqExp(_, exps)) =
	    let
		fun eiter (exp'::nil) = expCode exp'
		  | eiter (exp'::exps) = expCode exp' @ [Pop] @ eiter exps
		  | eiter _ = raise Error "eiter"
	    in
		eiter exps
	    end*)

	and
	    expCode (AppExp(_,id' as Id(_,stamp',_),ida'')) =
	    let
		val idacode = idArgCode ida''
	    in
		if stamp'=stamp_builtin then
		    idacode @
		    [Invokestatic (CBuiltin, "getBuiltin",
				   ([Classsig CStr], [Classsig CVal]))]
		else
		    [Ifstatic (stamp',
			       idacode @
			       [Invokestatic (classNameFromStamp
					      (Lambda.getLambda stamp'),
					      "sapply",
					      ([Classsig CVal],
					       [Classsig CVal]))],
			       stampCode stamp' @
			       idacode @
			       [Invokeinterface (CVal, "apply",
						 ([Classsig CVal],
						  [Classsig CVal]))])]
	    end

	  | expCode (BuiltinAppExp (_, name, ids)) =
		(case name of
		     "print" => (Getstatic COut::
				 (idCode (hd ids))@
				 [Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj],[Voidsig])),
				  Getstatic CUnit])
		   | _ => nil)

(* Tail-Call-Optimierung: *)
(* Unterscheidung: Applikation in Tail-Call-Position oder nicht *)
	    (*  - wann muß kein Tail-Call sein: *)
	    (*    ^ nicht rekursive Funktion wird appliziert *)
	    (*    ^ self-tail-call ist anders *)
	    (*let*)
		(*val simpleCall = false *)(* keine wilde Wurschtelei nach apply *)
(*val selfTailCall = Lambda.isSelfCall exp'
 andalso tailPosition *)(* spricht für sich *)
	  (*val (startlabel,endlabel) = (Label.new(), Label.new())
		val _ = print ("Toplevel ist "^Int.toString toplevel^", Lambdatop ist "^Int.toString(Lambda.top ())^"\n")
		val tailCallApp = if tailPosition andalso (Lambda.top ()<>toplevel)
				      then if Lambda.isSelfCall exp'
					       then
						   [Astore 1,
						    Goto afterInit]
					   else
					       [Swap,
						Invokestatic (CThread, "currentThread", ([],Classsig CThread)),
						Checkcast CDMLThread,
						Swap,
						Putfield (CDMLThread^"/tail",CVal)]
				  else if simpleCall orelse (Lambda.top ()=toplevel)
					   then
					       [Invokeinterface (CVal, "apply", ([Classsig CVal],Classsig CVal))]
				       else
					   [Label startlabel,
					    Invokeinterface (CVal, "apply", ([Classsig CVal],Classsig CVal)),
					    Invokestatic (CThread, "currentThread", ([],Classsig CThread)),
					    Checkcast CDMLThread,
					    Getfield (CDMLThread^"/tail",CVal),
					    Dup,
					    Ifnull endlabel,
					    Swap,
					    Goto startlabel,
					    Label endlabel,
					    Pop]

		val (inl, bef, aft) = inlineExpCode exp'
		val e2 = expCode exp''
		val e = if inl then
		     bef @ e2 @ aft
			else expCode exp' @ e2 @ tailCallApp
	    in
		[Comment "[ apply"]
		@ e @
		[Comment "end of apply ]"]
	    end*)

	  | expCode (FunExp(coord',string', (lambda as (OneArg (id' as Id (_,stamp',_)), _))::rest)) =
		     (* FunExp of coord * string * (id args * dec) list *)
		     (* id ist formaler Parameter *)
		     (* 1. Abschluß bilden:  - Objekt bauen oder aus Register nehmen *)
		     (*                      - freie Variablen aus Register mit putfields versenken *)
		     (* 2. Klasse erzeugen *)
		     let
			 val className = classNameFromStamp stamp'
			 val freeVarList = FreeVars.getVars id' (* yyy*)
			 (*			     val _ = (print "FunExpFree: ";printStampList freeVarList)*)
			 (*		val _ = annotateTailExp exp'*)
			 (* 1. *)
			 val object = let
					  val loc = Local.getLambda stamp'
				      in
					  if loc= ~1 (* dann baue Objekt *)
					      then
						  [New className,
						   Dup,
						   Invokespecial (className,
								  "<init>",
								  ([],
								   [Voidsig]))]
					  else
					      [Comment "Hi2", Aload loc]
				      end
			 (* 2. *)
			 local
			     fun loadFreeVar stamp'' =
				 let
				     val (bstamp,isBuiltin) = builtinStamp stamp''
				 in
				     if isBuiltin
					 then
					     Dup ::
					     bstamp @
					     [Putfield (className^"/"^
							(fieldNameFromStamp stamp''),
							[Classsig CVal])]
				     else
					 if FreeVars.getFun stamp'' = Lambda.top() then
					     if stamp'' <> stampFromId(Lambda.getId stamp')
						 then
						     (* if Local.get id' = ~1 then
						       raise Debug (Ias FreeVars.array)
					     else *)
						     [Dup,
						      Comment ("Hi3: stamp="^Stamp.toString stamp''),
						      Aload (Local.get stamp''),
						      Putfield(className^"/"^(fieldNameFromStamp stamp''),[Classsig CVal]),
						      Comment ("load local variable")]
					     else nil
					 else
					     if stamp''=(stampFromId (Lambda.getId
								     (Lambda.top())))
						 then (* Zugriff auf die aktuelle
						       Funktion. Diese steht in Register 0. *)
						     (Lambda.noSapply ();
						      [Dup,
						       Aload 0,
						       Putfield (className^"/"^(fieldNameFromStamp stamp''),[Classsig CVal])])
					     else
						 (Lambda.noSapply ();
						  [Dup,
						   Aload 0,
						   Comment ("Getfield 1; (FreeVars.getFun stamp'' = "^
							    Stamp.toString (FreeVars.getFun stamp'')^"; Lambda.top() = "^
							    Stamp.toString (Lambda.top())),
						   Getfield(Class.getCurrent()^"/"^(fieldNameFromStamp stamp''),[Classsig CVal]),
						   Putfield(className^"/"^(fieldNameFromStamp stamp''),[Classsig CVal])])
				 end
			 in
			     val loadVars = List.concat (map loadFreeVar freeVarList)
			 end
		     in
			 Lambda.push id';
			 Catch.push ();
			 Label.push();
			 Local.push();
			 Class.push(className);
			 expCodeClass(lambda);
			 Class.pop();
			 Local.pop();
			 Label.pop();
			 Catch.pop();
			 Lambda.pop();
			 object @ loadVars @
			 (case
			      rest of nil => nil
			    | _ => expCode (FunExp(coord', string', rest)))
		     end

	  (*	  | expCode (LetExp(_,declist,exp')) =
	   decListCode declist @ expCode exp'*)

	  | expCode (RecExp(_, nil)) =
		     [Getstatic (CConstants^"/dmlunit", [Classsig CName])]

	  | expCode (RecExp(_,labid)) =
		     (* RecExp of coord * (lab * id) list *)
		     (* 1. Label[] laden *)
		     (* 2. Value[] bauen *)
		     (* 3. Record erzeugen *)
		     (* Label[] bauen passiert statisch! *)
		     let
			 val arity = length labid
			 (* 1. *)
			 (*		local
					    fun labelcode (Lab(_,label),index) =
						[Dup,
						atCodeInt index,
						New CLabel,
						Dup,
						Ldc (JVMString label),
						Invokespecial (CLabel,"<init>",([Classsig CString], Voidsig)),
						Aastore]
fun labeliter ((l,_)::rest,i) = labelcode (l,i) @ labeliter(rest,i+1)
  | labeliter (nil,_) = nil
		     in *)

			 (* dreht Liste um und entfernt überflüssige ids *)
			 fun labids2strings ((Lab (_,l), _)::labids',s')=
			     labids2strings (labids', l::s')
			   | labids2strings (nil, s') = s'

			 (*end*)
			 (* 2. *)
			 fun load ((_,Id (_,stamp',_))::rs,j) =
			     Dup::
			     (atCodeInt j)::
			     (Comment "Hi4")::
			     (Aload (Local.get stamp'))::
			     Aastore::
			     (load (rs,j+1))
			   | load (nil,_) = nil
			 (* 3. *)
		     in
			 [Comment "[Record "] @
			 [New CRecord,
			  Dup(*,
			      atCodeInt arity,
			      Anewarray CLabel*),
			  Getstatic (RecordLabel.insert
				     (labids2strings (labid, nil))),
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal] @
			 (load (labid,0)) @
			 [Invokespecial (CRecord,"<init>",
					 ([Arraysig, Classsig CLabel,
					   Arraysig, Classsig CVal],
					  [Voidsig])),
			  Comment "Record ]"]
		     end

	  | expCode (LitExp(_,lit')) =
		     let
			(* val jValue = case lit' of
			     CharLit c => atCodeInt(ord c)
			   | IntLit i  => atCodeInt (LargeInt.toInt i)
			   | RealLit r =>
				 let
				     val SOME r = Real.fromString r
				 in
				     if (Real.sign (r-0.0)=0)
					 orelse (Real.sign(r-1.0)=0)
					 orelse (Real.sign (r-2.0)=0) then Fconst (trunc r)
				     else Ldc (JVMFloat r)
				 end
			   | StringLit s => Ldc (JVMString s)
			   | WordLit w   => atCodeInt (LargeInt.toInt (LargeWord.toLargeInt w))
			 val jType = case lit' of
			     CharLit _   => ([Intsig],[Voidsig])
			   | IntLit _    => ([Intsig],[Voidsig])
			   | RealLit _   => ([Floatsig],[Voidsig])
			   | StringLit _ => ([Classsig CString], [Voidsig])
			   | WordLit _   => ([Intsig],[Voidsig])*)
			 val scon = case lit' of
			     CharLit _   => CChar
			   | IntLit _    => CInt
			   | RealLit _   => CReal
			   | StringLit _ => CStr
			   | WordLit _   => CInt
		     in
			 [Getstatic (Literals.insert lit', [Classsig scon])]
(*				 [Comment "constant(",
				  New scon,
				  Dup,
				  jValue,
				  Invokespecial (scon,"<init>",jType),
				  Comment "end of constant)"] *)
		     end

	  | expCode (TupExp(_,longids)) =
		     let
			 val arity = length longids
			 fun ids ((id' as Id(_,stamp',_))::rest,i) =
			     Dup::
			     (atCodeInt i)::
			     (Comment "Hi5")::
			     (stampCode stamp') @
			     (Aastore::
			     ids(rest,i+1))
			   | ids (nil,_) = nil
		     in
			 [New CTuple,
			  Dup,
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal]@
			 ids (longids, 0)@
			 [Invokespecial (CTuple, "<init>",
					 ([Arraysig, Classsig CVal],
					  [Voidsig]))]
		     end

	  | expCode (VarExp(_,id')) =
		     idCode id'

	  | expCode (AdjExp _) = raise Error "seltsame operation adjexp"

	  | expCode (SelExp(_,Lab(_,lab'))) =
		     (case LargeInt.fromString lab' of
			 NONE =>
			     [New CSelString,
			      Dup,
			      Ldc (JVMString lab'),
			      Invokespecial (CSelString, "<init>",
					     ([Classsig CString],[Voidsig]))]
		       | SOME i =>
			     [New CSelInt,
			      Dup,
			      atCodeInt i,
			      Invokespecial (CSelInt, "<init>",
					     ([Intsig],[Voidsig]))])

	  | expCode (ConExp (_, id', _)) =
		     idCode id'

	  | expCode (ConAppExp (_, id', id'')) =
		     idCode id' @
		     (idCode id'') @
		     [Invokeinterface (CVal, "apply",
				       ([Classsig CVal], [Classsig CVal]))]
	  | expCode (SelAppExp (_, Lab (_,label'), id')) =
		     idCode id' @
		     ((Checkcast CDMLTuple) ::
		      (case LargeInt.fromString label' of
			  NONE =>
			      [Ldc (JVMString label'),
			       Invokeinterface (CDMLTuple, "get",
						([Classsig CString], [Classsig CVal]))]
			| SOME i =>
			      [atCodeInt i,
			       Invokeinterface (CDMLTuple, "get",
						([Intsig], [Classsig CVal]))]))

	  | expCode e = raise Debug (Exp e)
	and
	    expCodeClass (OneArg id',body') =
	    let
		val _ = print ("create Class "^(Class.getCurrent())^"\n")
		val e = (Getstatic ("java/lang/System/out", [Classsig "java/io/PrintStream"])::
			 Ldc (JVMString "Betrete: (")::
			 Invokevirtual ("java/io/PrintStream","print",([Classsig CObj],[Voidsig]))::
			 Getstatic ("java/lang/System/out", [Classsig "java/io/PrintStream"])::
			 Ldc (JVMString (nameFromId (Lambda.getFun ())))::
			 Invokevirtual ("java/io/PrintStream","println",([Classsig CObj],[Voidsig]))::
			 (List.concat (map decCode body')))
		val className = classNameFromId id'
		val freeVarList = FreeVars.getVars id'
		(* baut die Felder, d.h. die freien Variablen der Klasse *)
		local
		    fun fields (stamp'::stamps) =
			(Field ([FPublic],fieldNameFromStamp stamp', [Classsig CVal]))::
			(fields stamps)
		      | fields nil = nil
		    fun vars (0, akku) = akku
		      | vars (n, akku) =
			vars (n-1,Var (n, "woasweisi"^(Int.toString n),
			      [Classsig CVal], alpha, omega)::akku)
		in
		    val fieldscode = fields freeVarList
		    val bd = vars (Local.max(),e)
		end
		local (* Register in apply initialisieren *)
		    fun initializeLocals 0 = [Label afterInit]
		      | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		in
		    val initRegister = initializeLocals (Local.max ())
		end
		(* Wir bauen jetzt den Rumpf der Abstraktion *)
		val ap =
		    (Label alpha::
		     initRegister @
		     bd @
		     [Label omega,
		      Areturn])
		val applY = Method ([MPublic],
				    "apply",
				    ([Classsig CVal], [Classsig CVal]),
				    Locals (Local.max()+1),
				    ap,
				    Catch.top(), false)
		val sapply = Method ([MPublic, MStatic],
				     "sapply",
				     ([Classsig CVal], [Classsig CVal]),
				     Locals (Local.max()),
				     ap,
				     Catch.top(), true)
		(* die Standard-Initialisierung *)
		val init = Method ([MPublic],"<init>",([], [Voidsig]), Locals 1,
				   [Aload 0,
				    Invokespecial (CFcnClosure, "<init>",
						   ([], [Voidsig])),
				    Return],
				   nil,
				   false)

		(* die ganze Klasse *)
		val class = Class([CPublic],className,
				  CFcnClosure,
				  fieldscode,
				  (applY::init::
				   (if Lambda.sapplyPossible () then
					[sapply]
				    else nil)))
		val ziel = schreibsAuf (className^".j");
	    in
		classToJasmin (class,ziel);
		schreibsZu ziel
	    end
and
    compile prog = genProgramCode ("Emil", imperatifyString prog)
and
    compilefile f = (d:=(imperatifyFile f);
		     genProgramCode ("Emil", !d);
		     !d)
    end
