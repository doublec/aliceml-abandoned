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

	val _ = Compiler.Control.Print.printLength := 10000;
	val _ = Compiler.Control.Print.printDepth := 10000;
	val _ = SMLofNJ.Internals.GC.messages false

	(* falls was b�ses passiert, wird eine Error-exception mit sinnvollem Inhalt 'geraist' *)
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
	exception Debug of deb

	(* Hashtabelle fuer Stamps. *)
	structure StampHash=MakeHashImpMap(type t=stamp val hash=Stamp.hash)
	structure ScopedStampSet=MakeHashScopedImpSet(type t=stamp val hash=Stamp.hash)
	structure StringListHash=MakeHashImpMap(StringListHashKey)

	(* Labelz�hler, aNewLabel liefert einen neuen String "label?", ? ist Zahl.
	 Den ersten Stack brauchen wir, damit f�r jede Klasse wieder bei label1
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
	fun fieldNameFromId (Id(_,stamp',ExId name')) = "field"^name'^(Int.toString stamp')
	  | fieldNameFromId (Id(_,stamp',InId)) ="field"^(Int.toString stamp')

	(* Den Stamp aus einer Id extrahieren. *)
	fun stampFromId (Id (_, stamp', _)) = stamp'

	(* Dieser Label steht am Ende der Registerinitialisierung von Methoden. *)
	val afterInit = "labelAfterInit"

	val toplevel = Stamp.new()

	(* Lokales JVM-Register, in dem das �bersetzungsergebnis festgehalten wird. *)
	val mainpickle = ref ~1 (* JVM-Register, in dem Struktur steht *)

	(* Verwaltung der lokalen JVM-Register *)
	structure Local =
	    struct
		local
		    val localscount = ref 1
		    val stack:int list ref = ref nil
		    val register: int StampHash.t    = StampHash.new ()
		    val lambda  : stamp StampHash.t  = StampHash.new ()
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
			    NONE => ~1
			  | SOME register => register

		    fun fieldNameFromStamp stamp' =
			"field"^
			(case StampHash.lookup(fields, stamp') of
			     NONE => ""
			   | SOME name => name)
			     ^(Int.toString stamp')

		    (* Zuordnung von Ids zu JVM-Registern mit definierender Funktion *)
		    (* xxx inzwischen �berfl�ssig ? *)
		    fun assignLambda (Id(_,stamp',_), wohin) =
			(StampHash.insert(lambda,stamp',wohin);
			 wohin)
		    fun getLambda stamp' =
			case StampHash.lookup(lambda,stamp') of
			    NONE => ~1
			  | SOME lambda => lambda
		end
	    end

	(* Name der aktuellen Klasse. Der Stack wird f�r verschachtelte Funktionen ben�tigt.
	 (f�r jede Funktion wird eine eigene Klasse erzeugt.) *)
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

	(* Funktionenstack *)
	structure Lambda =
	    struct
		fun printstack (x::xs) = (print (Int.toString x^", "); printstack xs)
		  | printstack nil = print "\n"
		val stack = ref (toplevel::nil)
		fun push (Id(_,stamp',_)) = (printstack (stamp'::(!stack)); stack := (stamp'::(!stack)))
		fun pop () = (printstack (tl (!stack)); stack := tl(!stack))

		(* Lambda.top () liefert stets die aktuelle Funktion *)
		fun top () = hd(!stack)
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
		fun setFun (Id(_,stamp',_)) =
		    if (isSome (StampHash.lookup(defFun, stamp')))
			then print "setFun twice!"
		    else
			StampHash.insert(defFun, stamp', Lambda.top ())
		fun getFun stamp' =
		    (* Falls undef, exception Option *)
		    (print  ("getFun Okay: "^Int.toString stamp');
		     case StampHash.lookup (defFun, stamp') of
			 NONE => toplevel
		       | SOME stamp'' => stamp'')
	    end

	(* Zuordnung von formalen Parametern auf zugehoerige Funktions-Stamps *)
	structure LambdaIds =
	    struct
		val lambdas:id StampHash.t=StampHash.new ()

		val stack = ref [Id ((0,0),toplevel,InId)]:id list ref

		fun pushFun ids = stack:=(ids::(!stack))
		fun popFun () = stack:=tl(!stack)

		fun setId () = StampHash.insert(lambdas,Lambda.top(),hd(!stack))

		(* raises Option if undef *)
		fun getId id' = valOf (StampHash.lookup(lambdas, id'))

		(* feststellen, ob eine Applikation selbstrekursiv ist. xxx *)
		fun isSelfCall stamp' =
		    case StampHash.lookup(lambdas, Lambda.top()) of
			NONE => false
		      | SOME id'' => stamp'=stampFromId(id'')
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

	fun atCodeInt i =
	    if i >= ~1 andalso i<=5 then Iconst i else
		if i >= ~128 andalso i <= 127 then Bipush i else
		    if i >= ~32768 andalso i <= 32767 then Sipush i
		    else Ldc (JVMInt i)

	structure RecordLabel =
	    struct
		val arity: int StringListHash.t = StringListHash.new ()
		val number = ref 0

		fun fieldname number = "arity"^Int.toString number

		fun staticfield number =
		    (Class.getInitial ()^"/"^(fieldname number), CLabel, 1)

		fun insert strings =
		    (print "insert Recordlabel\n";
		     case StringListHash.lookup (arity, strings) of
			 NONE => (number := ((!number)+1);
				  StringListHash.insert (arity, strings, !number);
				  staticfield (!number))
		       | SOME number' => staticfield number')

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
					val d = n+1
				    in
					if d=size
					    then
						(atCodeInt size::
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
		fun makefields () =
		    StringListHash.fold
		    (fn (number, fields) =>
		     Field ([FPublic, FStatic],
			    fieldname number,
			    Classtype (CLabel, 1))::fields)
		    nil
		    arity
	    end

	(* Berechnung der freien Variablen *)
	local
	    structure fV =
		struct
		    val free:ScopedStampSet.t= ScopedStampSet.new ()

		    fun insert (Id (_,stamp',_)) =
			if LambdaIds.isSelfCall stamp'
			    then ()
			else
			    (ScopedStampSet.insert (free, stamp');
			     print ("inserted "^Int.toString stamp'^"\n"))
		    fun delete (Id (_,stamp',_)) =
			(ScopedStampSet.deleteExistent(free, stamp')
			 handle ScopedStampSet.Delete _ => print "nicht geloescht:\n";
			 print ("deleted "^Int.toString stamp'^"\n");
			 if ScopedStampSet.member(free, stamp')
			     then print "isdrin\n"
			 else ();
			     ScopedStampSet.foldScope (fn (x,_) => if x=stamp' then
						       print "isdochdrin" else ()) () free
				 )

		    fun get () = ScopedStampSet.foldScope (fn (x,xs) => x::xs) nil free

		    (* Betreten einer neuen Subfunktion. *)
		    fun enter () =
			(print "ENTER scope\n";
			 ScopedStampSet.insertScope free)

		    (* Verlassen der Subfunktion. *)
		    fun exit () =
			(print "LEAVE scope\n";
			 ScopedStampSet.mergeScope free)
		end
	in
	    fun freeVarsExp (LitExp _) = ()
	      | freeVarsExp (VarExp (_, id')) = fV.insert id'
	      | freeVarsExp (ConAppExp (_, id', id'')) = (fV.insert id'; fV.insert id'')
	      | freeVarsExp (TupExp ((i,_),ids)) = (print ("TupExp "^(Int.toString i));app fV.insert ids)
	      | freeVarsExp (RecExp (_,labids)) = app (fn (lab, id') => fV.insert id') labids
	      | freeVarsExp (SelExp _) = ()
	      | freeVarsExp (FunExp(_,_, idbodys)) =
		let
		    fun freeVarsFun ((OneArg (id' as Id (_,stamp',_)),body')::idbodys') =
			(fV.enter();
			 Lambda.push id';
			 freeVarsDecs body';
			 fV.delete id';
			 FreeVars.setVars (stamp',fV.get ());
			 FreeVars.setFun id';
			 print "setVars\n";
			 freeVarsFun idbodys';
			 LambdaIds.setId();
			 Lambda.pop ();
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

	    and freeVarsDec (RaiseStm(_,id')) = fV.insert id'
	      | freeVarsDec (HandleStm(_,body',id',body'')) = (print "HandleExp";
							       freeVarsDecs body'';
							       freeVarsDecs body';
							       FreeVars.setFun id';
							       fV.delete id'; print "/HandleExp\n")
	      | freeVarsDec (EndHandleStm(_,body')) = freeVarsDecs body'
	      | freeVarsDec (TestStm(_,id',test',body',body'')) =(freeVarsDecs  body'';
								  freeVarsDecs  body';
								  freeVarsTest test';
								  fV.insert id')
	      | freeVarsDec (SharedStm(_,body',raf as ref 0)) = (raf := ~1; freeVarsDecs body')
	      | freeVarsDec (SharedStm _) = ()
	      | freeVarsDec (ValDec(_,id',exp', _)) = (print "ValDec";
						  freeVarsExp exp';
						  print "/exp";
						  FreeVars.setFun id';
						  fV.delete id';
						  print "/ValDec\n")
	      | freeVarsDec (RecDec(_,idsexps, _)) =
		let
		    fun freeVarsRecDec ((id',exp')::rest) =
			(freeVarsExp exp';
			 FreeVars.setFun id';
			 freeVarsRecDec rest;
			 fV.delete id')
		      | freeVarsRecDec nil = ()
		in
		    freeVarsRecDec idsexps
		end
	      | freeVarsDec (ConDec(_,id',_, _)) = (fV.delete id';
						 FreeVars.setFun id')
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
	 die Methode verlassen, so da� wir den Handle-Ausdruck duplizieren m��ten, also lassen wir es
	 ganz. *)
	  | annotateTailExp (HandleStm (_, _, _, exp')) = annotateTailExp exp'
(*	  | annotateTailExp (LetExp (_, decs, exp')) = (app annotateTailDec decs; annotateTailExp exp') *)
	  | annotateTailExp _ = ()
	and annotateTailDec (ValDec (_,_,exp')) = annotateTailExp exp'
(*	  | annotateTailDec (Waldeck (_,_,exp')) = annotateTailExp exp' *)
	  | annotateTailDec (RecDec (foo,(_,exp')::idsexps)) =
	    (annotateTailExp exp'; annotateTailDec (RecDec (foo, idsexps)))
	  | annotateTailDec _ = () *)

	(* Den Klassennamen einer Id bestimmen, die �blicherweise die Id eines formalen
	 Funktionsparameters ist. *)
	fun classNameFromId (Id(_,stamp',ExId name')) = Class.getInitial()^"$class"^name'^(Int.toString stamp')
	  | classNameFromId (Id (_,stamp',InId)) = Class.getInitial()^"$class"^(Int.toString stamp')

	(* Einstiegspunkt *)
	fun genProgramCode (name, program) =
	    (Class.setInitial name;
	     let
		 (* freie Variablen berechnen. *)
		 val _ = app freeVarsDec program
		 (* val _ = app annotateTailDec program*)
		 (* Alle Deklarationen �bersetzen *)
		 val insts = decListCode program

		 (* JVM-Register initialisieren. *)
		 fun initializeLocals 0 = [Label afterInit]
		   | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		 val iL = initializeLocals (Local.max())

		 val _ = if (!mainpickle= ~1) then
		     raise Error "Derzeit sind nur Strukturen �bersetzbar." else ()

		 (* Verschachtelt endrekursive Funktionsaufrufe werden uebersetzt, indem im
		  aktuellen Thread-Objekt die naechste Methode gespeichert wird. Da solche
		  Aufrufe auch waehrend der Initialisierung stattfinden koennen, muss auch
		  diese in einem DML-Thread ablaufen. main erzeugt also lediglich eine neue
		  Instanz der eigenen Klasse und startet einen neuen Thread. Die eigentliche
		  Initialisierungsarbeit geschieht in run. *)
		 val main = Method([MStatic,MPublic],"main",([Arraysig, Classsig CString],[Voidsig]),
				   Locals 1,
				   [New name,
				    Dup,
				    Invokespecial (name, "<init>", ([], [Voidsig])),
				    Invokevirtual (CThread, "start", ([], [Voidsig])),
				    Return], nil)
		 (* Standardinitialisierung. Die Superklasse wird aufgerufen. *)

		 val clinit = Method([MPublic],"<clinit>",([],[Voidsig]),
				   Locals 6,
				   RecordLabel.generate(),
				   nil)

		 val init = Method([MPublic],"<init>",([],[Voidsig]),
				   Locals 1,
				   [Aload 0,
				    Invokespecial (CDMLThread, "<init>", ([], [Voidsig])),
				    Return], nil)
		 (* In run wird die Toplevel-Umgebung aufgebaut. Alle Objekte werden erzeugt
		  und initialisiert, die Funktionsabschluesse werden gebildet. Anschliessend
		  wird das Ergebnis zusammen mit den Classfiles in ein Pickle geschrieben.
		  Dies ist der letzte Schritt des Compilierungsvorganges. *)
		 val run = Method([MPublic], "run", ([], [Voidsig]),
				  Locals (Local.max()+1),
				   iL @
				   insts @
				   [Getstatic CPickle,
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
				    Comment "Generate" ] @
				   [Return],
				  Catch.top())
		 (* die Hauptklasse *)
		 val class = Class([CPublic],
				   name,
				   CDMLThread,
				   RecordLabel.makefields (),
				   [main, clinit, init, run])
	     in
		 schreibs(name^".j",classToJasmin class)
	     end
	     )

	and builtinStamp stamp' =
	    if stamp'=stamp_plus then CPlus else
		if stamp'=stamp_Match then CMatch else
		    if stamp'=stamp_false then CFalse else
			if stamp'=stamp_true then CTrue else
			    if stamp'=stamp_nil then CNil else
				if stamp'=stamp_cons then CCons else
				    if stamp'=stamp_ref then CRef else
					if stamp'=stamp_Bind then CBind else
					    if stamp'=stamp_eq then CEquals else
						if stamp'=stamp_assign then CAssign else
						    ("","",0)


	and decListCode decs = List.concat (map decCode decs)

	(* Codegenerierung f�r Deklarationen *)
	and decCode (ValDec(_, id', exp',_)) =
	    let
		val loc = Local.assign(id', Local.nextFree())
	    in
		LambdaIds.pushFun id';
		(expCode exp' @
		 [Comment "Store 2", Astore loc])
		before LambdaIds.popFun()
	    end
	  | decCode (RecDec (_, nil, _)) = nil
	  | decCode (RecDec (_,idexps as ((recid,_)::_),_)) =
	    (* RecDec of coord * (id * exp) list * isTopLevel *)
	    (* 1. F�r alle ids ein passendes (nichtabgeschlossenes) Objekt bauen *)
	    (*    und jeweils in ein frisches Register stopfen. *)
	    (* 2. exp auswerten und Ergebnis wegwerfen*)
	    (* !! die Abschl�sse der Objekte passieren beim Auswerten von exp *)
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
				     Register 1 �bergeben. *)
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
			(LambdaIds.pushFun id';
			 (expCode exp')
			 before LambdaIds.popFun())
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
			val constructorName = "constructor"^(Int.toString stamp')
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
			val nameName = "name"^(Int.toString stamp')
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
		val zuerst = [] (* (case id' of Id (_,stomp, _) => print ("Stomp"^(Int.toString stomp));[Comment "Hi7", Aload (Local.get id')]) *)

		fun testCode (LitTest lit') =
		    let
			val litcode = expCode (LitExp((~1,~1),lit'))
		    in
			litcode @
			(idCode id') @
			[Invokevirtual (CObj, "equals",
					([Classsig CObj],[Boolsig])),
			 Ifeq elselabel]
		    end
		  | testCode (ConTest (id'',NONE)) =
		    Comment "Hi8" ::
		    (idCode id') @
		    (idCode id'') @
		    [Ifacmpne elselabel]
		  | testCode (ConTest(id'',SOME id''')) =
		    let
			val loc = Local.assign(id''',Local.nextFree())
			val _ = FreeVars.setFun id'''
		    in
			Comment "Hi9" ::
			 (idCode id') @
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
			(* dreht Liste um und entfernt �berfl�ssige ids *)
			fun stringids2strings ((l, _)::stringids',s')=
			    stringids2strings (stringids', l::s')
			  | stringids2strings (nil, s') = s'
			fun bindit ((_,id')::nil,i) =
			    let
				val loc = Local.assign(id',Local.nextFree())
			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) :: nil
			    end
			  | bindit ((_,id')::rest,i) =
			    let
				val loc = Local.assign(id',Local.nextFree())
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
			(atCodeInt (length stringid)) ::
			(Anewarray CLabel) ::
			(Getstatic (RecordLabel.insert
				    (stringids2strings (stringid, nil)))) ::
			(Invokespecial (CRecordArity,"<init>",
					([Arraysig,Classsig CLabel],
					 [Voidsig]))) ::
			(Comment "Hi11") ::
			(Aload (Local.get stamp')) ::
			(Invokevirtual (CRecord,"getArity",
					([],[Classsig CRecordArity]))) ::
			(Ifacmpne elselabel) ::
			(idCode id') @
			[Invokevirtual (CRecord,"getValues",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(stringid,0))
		    end
		  | testCode (TupTest ids) =
		    (* Arity vergleichen (int), dann binden *)
		    let
			fun bindit (id'::nil,i) =
			    let
				val loc = Local.assign(id',Local.nextFree())
			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				nil
			    end
			  | bindit (id'::rest,i) =
			    let
				val loc = Local.assign(id',Local.nextFree())
			    in
				Dup ::
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				bindit(rest,i+1)
			    end
			  | bindit (nil,_) = nil
		    in
			idCode id'@
			[Checkcast CDMLTuple,
			 Invokevirtual (CDMLTuple,"getArity",([],[Intsig])),
			 Iconst (length ids),
			 Ificmpne elselabel] @
			(idCode id') @
			[Checkcast CDMLTuple,
			 Invokevirtual (CDMLTuple,"getValues",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(ids,0))
		    end
		  | testCode test' = raise Debug (Test test')

	    in
		(*zuerst @ *)
		(testCode test') @
		(List.concat (map decCode body')) @
		[Goto danach,
		 Label elselabel] @
		(List.concat (map decCode body'')) @
		[Label danach]
	    end

	  | decCode (SharedStm(_,body',da as ref schonda)) =
	    if schonda <= 0
		then
		    let
			val _ = da := Label.newNumber ()
			val e = List.concat (map decCode body')
		    in
			(Label (Label.fromNumber (!da)))::
			(List.concat (map decCode body'))
		    end
	    else
		[Goto (Label.fromNumber schonda)]

	  | decCode (ReturnStm (_,ap as AppExp(_,id' as (Id (_,stamp',_)),_))) =
		(* Tailcall Applikation *)
		(* if Lambda.top ()<>toplevel
			    then *)
		if LambdaIds.isSelfCall stamp' then
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
	    idCode (id' as Id(_,stamp',_)) =
	    let
		val bstamp = builtinStamp stamp'
	    in
		if bstamp<>("","",0) then [Getstatic bstamp]
		else
		    let
			val _ = print("("^(Int.toString stamp')^" Stamp in "^
				      (Int.toString (Local.get stamp'))^")\n")
			val isFree = not ((FreeVars.getFun stamp') = Lambda.top ())
		    in
			if isFree then (* bei Rekursion reicht eigentlich [Aload 0] *)
			    if stamp'=(stampFromId (LambdaIds.getId (Lambda.top()))) then
				[Comment "Hi 42",
				 Aload 0]
			    else
				[Comment ("Hi. Stamp="^(Int.toString stamp')^
					  ". Lambda = "^Int.toString (Lambda.top())^
					  " in "^Int.toString (Local.get stamp')^
					  ". Fun = "^(Int.toString
						      (FreeVars.getFun stamp'))^"\n"),
				 Aload 0,
				 Getfield (Class.getCurrent()^"/"^(fieldNameFromId id'), CVal,0)]
			else
			    if stamp' = Lambda.top() then
				[Comment "Hi.Parm",
				 Aload 1]
			    else
				[Comment "Hi6",
				 Aload (Local.get stamp')]
		    end
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
		 Iconst (List.length ids),
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
		    (Iconst arity)::
		    (Anewarray CString)::
		    accu1@
		    ((Iconst arity)
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
	    if stamp'=stamp_builtin then
		idArgCode ida'' @
		[Invokestatic (CBuiltin, "getBuiltin",
			       ([Classsig CStr], [Classsig CVal]))]
	    else
		idCode id' @
		(idArgCode ida'') @
		[Invokeinterface (CVal, "apply",
				  ([Classsig CVal], [Classsig CVal]))]
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
	    (*  - wann mu� kein Tail-Call sein: *)
	    (*    ^ nicht rekursive Funktion wird appliziert *)
	    (*    ^ self-tail-call ist anders *)
	    (*let*)
		(*val simpleCall = false *)(* keine wilde Wurschtelei nach apply *)
(*val selfTailCall = LambdaIds.isSelfCall exp'
 andalso tailPosition *)(* spricht f�r sich *)
	  (*val (startlabel,endlabel) = (Label.new(), Label.new())
		val _ = print ("Toplevel ist "^Int.toString toplevel^", Lambdatop ist "^Int.toString(Lambda.top ())^"\n")
		val tailCallApp = if tailPosition andalso (Lambda.top ()<>toplevel)
				      then if LambdaIds.isSelfCall exp'
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
		     (* 1. Abschlu� bilden:  - Objekt bauen oder aus Register nehmen *)
		     (*                      - freie Variablen aus Register mit putfields versenken *)
		     (* 2. Klasse erzeugen *)
		     let
			 val className = classNameFromId id'
			 val freeVarList = FreeVars.getVars id'
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
				     val bstamp = builtinStamp stamp''
				 in
				     if bstamp<>("","",0)
					 then
					     [Dup,
					      Getstatic bstamp,
					      Putfield (className^"/"^
							(Local.fieldNameFromStamp stamp''),
							CVal, 0)]
				     else
					 if FreeVars.getFun stamp'' = Lambda.top() then
					     if stamp'' <> stampFromId(LambdaIds.getId stamp')
						 then
						     (print ("mache "^Int.toString stamp'^" in "^Int.toString(Lambda.top())^"\n");
						      (* if Local.get id' = ~1 then
						       raise Debug (Ias FreeVars.array)
					     else *)
						      [Dup,
						       Comment ("Hi3: id="^Int.toString (Local.get stamp'')),
						       Aload (Local.get stamp''),
						       Putfield(className^"/"^(Local.fieldNameFromStamp stamp''),CVal, 0),
						       Comment ("load local variable")])
					     else nil
					 else
					     (print ("bearbeite "^Int.toString stamp'^" in "^Int.toString(Lambda.top())^"\n");
					      [Dup,
					       Aload 0,
					       Comment ("Getfield 1; (FreeVars.getFun stamp'' = "^
							Int.toString (FreeVars.getFun stamp'')^"; Lambda.top() = "^
							Int.toString (Lambda.top())),
					       Getfield(Class.getCurrent()^"/"^(Local.fieldNameFromStamp stamp''),CVal, 0),
					       Putfield(className^"/"^(Local.fieldNameFromStamp stamp''),CVal, 0)])
				 end
			 in
			     val _ = print "Jetzt kommen die Loadvars\n"
			     val loadVars = List.concat (map loadFreeVar freeVarList)
			 end
		     in
			 print "funexp";
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
		     [Getstatic (CConstants^"/dmlunit", CName, 0)]

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

			 (* dreht Liste um und entfernt �berfl�ssige ids *)
			 fun labids2strings ((Lab (_,l), _)::labids',s')=
			     labids2strings (labids', l::s')
			   | labids2strings (nil, s') = s'

			 val labelinsts = (*labeliter(lablongid,0)*)
			     [Getstatic (RecordLabel.insert (labids2strings (labid, nil)))]
			 (*end*)
			 (* 2. *)
			 local
			     fun load ((_,Id (_,stamp',_))::rs,j) =
				 Dup::
				 (atCodeInt j)::
				 (Comment "Hi4")::
				 (Aload (Local.get stamp'))::
				 Aastore::
				 (load (rs,j+1))
			       | load (nil,_) = nil
			 in
			     val loadids = load (labid,0)
			 end
			 (* 3. *)
			 val result =
			     [Comment "[Record "] @
			     [New CRecord,
			      Dup(*,
				  atCodeInt arity,
				  Anewarray CLabel*)] @
			     labelinsts@
			     [atCodeInt arity,
			      Anewarray CVal] @
			     loadids @
			     [Invokespecial (CRecord,"<init>",
					     ([Arraysig, Classsig CLabel,
					       Arraysig, Classsig CVal],
					      [Voidsig])),
			      Comment "Record ]"]
		     in
			 result
		     end

	  | expCode (LitExp(_,lit')) =
		     let
			 val jValue = case lit' of
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
			   | WordLit _   => ([Intsig],[Voidsig])
			 and scon = case lit' of
			     CharLit _   => CInt
			   | IntLit _    => CInt
			   | RealLit _   => CReal
			   | StringLit _ => CStr
			   | WordLit _   => CInt
		     in
			 [Comment "constant(",
			  New scon,
			  Dup,
			  jValue,
			  Invokespecial (scon,"<init>",jType),
			  Comment "end of constant)"
			  ]
		     end

	  | expCode (TupExp(_,longids)) =
		     let
			 val arity = length longids
			 fun ids (Id(_,stamp',_)::rest,i) =
			     Dup::
			     (atCodeInt i)::
			     (Comment "Hi5")::
			     (Aload (Local.get stamp'))::
			     Aastore::
			     ids(rest,i+1)
			   | ids (nil,_) = nil
		     in
			 [New CTuple,
			  Dup,
			  Iconst arity,
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
		     [New CSel,
		      Dup,
		      Ldc (JVMString lab'),
		      Invokespecial (CSel, "<init>",
				     ([Classsig CString],[Voidsig]))]

	  | expCode (ConExp (_, id', _)) =
		     idCode id'

	  | expCode (ConAppExp (_, id', id'')) =
		     idCode id' @
		     (idCode id'') @
		     [Invokeinterface (CVal, "apply",
				       ([Classsig CVal], [Classsig CVal]))]

	  | expCode _ = raise Error "boom"
	and
	    expCodeClass (OneArg id',body') =
	    let
		val e = List.concat (map decCode body')
		val className = classNameFromId id'
		val freeVarList = FreeVars.getVars id'
		(* baut die Felder, d.h. die freien Variablen der Klasse *)
		local
		    fun fields (stamp'::stamps) =
			(Field ([FPublic],Local.fieldNameFromStamp stamp', Classtype (CVal, 0)))::(fields stamps)
		      | fields nil = nil
		in
		    val fieldscode = fields freeVarList
		end
		local (* Register in apply initialisieren *)
		    fun initializeLocals 0 = [Label afterInit]
		      | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		in
		    val initRegister = initializeLocals (Local.max ())
		end
		(* Wir bauen jetzt den Rumpf der Abstraktion *)
		val applY =Method ([MPublic],"apply",([Classsig CVal], [Classsig CVal]),
				   Locals (Local.max()+1),
				   (* (Aload 1) :: *) initRegister @ [Comment "Hi"] @ e @ [Comment "Ho", Areturn],
				   Catch.top())
		(* die Standard-Initialisierung *)
		val init = Method ([MPublic],"<init>",([], [Voidsig]), Locals 1,
				   [Aload 0,
				    Invokespecial (CFcnClosure, "<init>",
						   ([], [Voidsig])),
				    Return],
				   nil)

		(* die ganze Klasse *)
		val class = Class([CPublic],className,
				  CFcnClosure,fieldscode,
				  [applY, init])
	    in
		schreibs(className^".j",classToJasmin class)
	    end
and
    compile prog = genProgramCode ("Emil", imperatifyString prog)
and
    compilefile f = genProgramCode ("Emil", imperatifyFile f)
    end
