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

	(* Backend *)
	open ToJasmin
	open Backend


	(* Berechnung der freien Variablen *)
	local
	    structure fV =
		struct
		    val free:ScopedStampSet.t= ScopedStampSet.new ()

		    fun insert (Id (_,stamp',_)) =
			(if !ECHO>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    ". insert free: "^(Stamp.toString stamp')^"\n")
			 else ();

			 if stamp'=stamp_builtin orelse
			     Lambda.isSelfCall stamp'
			     then ()
			 else
			     ScopedStampSet.insert (free, stamp'))
		    fun delete (Id (_,stamp',_)) =
			(if !ECHO>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    " delete free: "^(Stamp.toString stamp')^"\n")
			 else ();
			 ScopedStampSet.delete(free, stamp'))

		    fun get () =
			let
			    val x = ScopedStampSet.foldScope (fn (x,xs) => x::xs) nil free
			in
			    if !ECHO>=2 then
				(print ("Top "^(Stamp.toString (Lambda.top())));
				 printStampList x)
			    else ();
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
			 Lambda.pushFun illegalId;
			 freeVarsDecs body';
			 fV.delete id';
			 FreeVars.setVars (stamp',fV.get ());
			 FreeVars.setFun (id', Lambda.top ());
			 freeVarsFun idbodys';
			 Lambda.popFun ();
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
	      | freeVarsDec (IndirectStm (_, ref (SOME body'))) = freeVarsDecs body'
	      | freeVarsDec (IndirectStm (_, ref NONE)) = ()
	    and
		freeVarsTest (LitTest _) = ()
	      | freeVarsTest (ConTest(id',NONE)) = fV.insert id'
	      | freeVarsTest (ConTest(id',SOME id'')) = (fV.insert id'; fV.delete id'')
	      | freeVarsTest (RecTest stringlablist) =
		app (fn (_,id') => fV.delete id') stringlablist
	      | freeVarsTest (TupTest lablist) = app fV.delete lablist
	      | freeVarsTest (LabTest(_,id')) = fV.delete id'

	    and freeVarsDecs (decs) =
		app freeVarsDec (List.rev decs)
	end

	(* Einstiegspunkt *)
	fun genProgramCode (debug, echo, name, program) =
	    (DEBUG := debug;
	     ECHO := echo;
	     Class.setInitial name;
	     let
		 (* freie Variablen berechnen. *)
		 val _ = app freeVarsDec program
		 val _ = if !ECHO>=2 then FreeVars.printFun () else ()
		 val _ = Lambda.createIdsLambdaTable()
		 (* val _ = app annotateTailDec program*)
		 (* Alle Deklarationen übersetzen *)
		 val insts = decListCode program

		 (* JVM-Register initialisieren. *)
		 fun initializeLocals 0 = [Label afterInit]
		   | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		 val iL = initializeLocals (Register.max())

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
				   Register.generateVariableTable
				   [New name,
				    Dup,
				    Invokespecial (name, "<init>", ([], [Voidsig])),
				    Invokevirtual (CThread, "start", ([], [Voidsig])),
				    Return], nil, false)
		 (* Standardinitialisierung. Die Superklasse wird aufgerufen. *)
		 val init = Method([MPublic],"<init>",([],[Voidsig]),
				   Locals 1,
				   [Aload 0,
				    Invokespecial (CDMLThread, "<init>", ([], [Voidsig])),
				    Return], nil, false)
		 (* In run wird die Toplevel-Umgebung aufgebaut. Alle Objekte werden erzeugt
		  und initialisiert, die Funktionsabschluesse werden gebildet. Anschliessend
		  wird das Ergebnis zusammen mit den Classfiles in ein Pickle geschrieben.
		  Dies ist der letzte Schritt des Compilierungsvorganges. *)
		 val literalName = Class.getLiteralName()
		 val run = Method([MPublic], "run", ([], [Voidsig]),
				  Locals (Register.max()+1),
				   iL @
				   insts @
				   (if !ECHO >= 1 then
					[Getstatic ("java/lang/System/out",[Classsig "java/io/PrintStream"]),
					 Aload (!mainpickle),
					 Invokevirtual ("java/io/PrintStream","print",
							([Classsig "java/lang/Object"],
							 [Voidsig]))]
				    else nil) @
				    [Ldc (JVMString (name^".pickle")),
				     New literalName,
				     Dup,
				     Invokespecial (literalName,
						    "<init>",
						    ([], [Voidsig])),
				     Aload (!mainpickle),
				     Invokestatic MPickle,
				     Pop,
				     Return],
				    Catch.top(), false)
		 (* die Hauptklasse *)
		 val class = Class([CPublic],
				   name,
				   CDMLThread,
				   nil,
				   nil,
				   [main, init, run])

		 (* literals can not be stored in the main class because
		  DMLThreads must not be pickled *)
		 val clinit = Method([MPublic],
				     "<clinit>",
				     ([],[Voidsig]),
				     Locals 6,
				     Literals.generate
				     (RecordLabel.generate()),
				     nil,
				     false)

		 (* Standardinitialisierung. Die Superklasse wird aufgerufen. *)
		 val litinit = Method([MPublic],"<init>",([],[Voidsig]),
				      Locals 1,
				      Aload 0::
				      Invokespecial (CFcnClosure, "<init>", ([], [Voidsig]))::
				      (Lambda.generatePickleFn
				       [Return]),
				      nil,
				      false)

		 val literale = Class([CPublic],
				      Class.getLiteralName(),
				      CFcnClosure,
				      [ISerializable],
				      Lambda.makePickleFields
				      (Literals.makefields
				       (RecordLabel.makefields ())),
				      [clinit, litinit])
	     in
		 if !ECHO >=2 then print "Erzeuge Haupt- und Literalklasse..." else ();
		 classToJasmin (class);
		 classToJasmin (literale);
		 if !ECHO >=2 then print "Okay.\n" else ()
	     end
	 )

	and getBuiltin builtin' =
	    [Getstatic (Literals.insert (StringLit builtin'), [Classsig CStr]),
	     Invokestatic (CBuiltin, "getBuiltin",
			   ([Classsig CStr], [Classsig CVal]))]

	and builtinStamp stamp' =
	    if stamp'=stamp_Match then ([Getstatic CMatch],true) else
		if stamp'=stamp_false then ([Getstatic CFalse],true) else
		    if stamp'=stamp_true then ([Getstatic CTrue],true) else
			if stamp'=stamp_nil then ([Getstatic CNil],true) else
			    if stamp'=stamp_cons then ([Getstatic CCons],true) else
				if stamp'=stamp_ref then ([Getstatic CRef],true) else
				    if stamp'=stamp_Bind then ([Getstatic CBind],true) else
					if stamp'=stamp_eq then ([Getstatic CEquals],true) else
					    if stamp'=stamp_assign then ([Getstatic CAssign],true) else
						if stamp'=stamp_builtin then ([Getstatic CBuilt],true) else
						    (nil,false)

	and decListCode decs = List.concat (map decCode decs)

	(* Codegenerierung für Deklarationen *)
	and decCode (ValDec(_, id' as Id (_,stamp',_), exp',_)) =
	    let
		val l=Register.get stamp'
		val loc =
		    if l = 1
			then Register.assign(id', Register.nextFree())
		    else l
	    in
		(Lambda.pushFun id';
		 (expCode exp' @
		  [Comment ("Store 2. Stamp = "^Stamp.toString stamp'^(if l = 1 then " NEU " else " ALT ")), Astore loc])
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
			    val loc = Register.assign(id',Register.nextFree())
			    fun funList ((OneArg id'',_)::rest) =
				let
				    val className = classNameFromId id''
				    (* Der formale Parameter einer Funktion wird immer in
				     Register 1 übergeben. *)
				    val _ = Register.assign(id'',1)
				    val _ = Register.assignLambda(id'', loc)
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
		val loc = Register.assign(id',Register.nextFree())
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
		val stampcode' = stampCode stamp'

		fun testCode (LitTest lit') =
		    let
			val eq = case lit' of
			    WordLit w' =>
				stampcode' @
				[Instanceof CWord,
				 Ifeq elselabel] @
				stampcode' @
				[Checkcast CWord,
				 Getfield (CWord^"/value", [Intsig]),
				 atCodeWord w',
				 Ificmpne elselabel]
			  | IntLit i' =>
				stampcode' @
				[Instanceof CInt,
				 Ifeq elselabel] @
				stampcode' @
				[Checkcast CInt,
				 Getfield (CInt^"/value", [Intsig]),
				 atCodeInt i',
				 Ificmpne elselabel]
			  | CharLit c' =>
				stampcode' @
				[Instanceof CChar,
				 Ifeq elselabel] @
				stampcode' @
				[Checkcast CChar,
				 Getfield (CChar^"/value", [Charsig]),
				 atCodeInt (Int.toLarge (Char.ord c')),
				 Ificmpne elselabel]
			  | StringLit s' =>
				stampcode' @
				[Instanceof CStr,
				 Ifeq elselabel] @
				stampcode' @
				[Checkcast CStr,
				 Getfield (CStr^"/value", [Classsig CString]),
				 Ldc (JVMString s'),
				 Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])),
				 Ifeq elselabel]
			  | r as (RealLit r') =>
				stampcode' @
				[Instanceof CReal,
				 Ifeq elselabel] @
				stampcode' @
				[Checkcast CReal,
				 Getfield (CReal^"/value", [Floatsig]),
				 atCode r,
				 Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])),
				 Ifeq elselabel]
		    in
			Comment "Hi73"::eq
		    end
		  | testCode (ConTest (id'',NONE)) =
		    Comment "Hi8" ::
		    stampcode' @
		    (idCode id'') @
		    [Ifacmpne elselabel]
		  | testCode (ConTest(id'',SOME id''')) =
		    let
			val loc = Register.assign(id''',Register.nextFree())
			val _ = FreeVars.setFun (id''', Lambda.top())
		    in
			Comment "Hi9" ::
			 stampcode' @
			 [Instanceof CConVal,
			  Ifeq elselabel] @
			 stampcode' @
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
				val loc = Register.assign(id'',Register.nextFree())
				val _ = FreeVars.setFun (id'', Lambda.top())
			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) :: nil
			    end
			  | bindit ((_,id'')::rest,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
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
			Comment "Hi11" ::
			stampcode' @
			(Instanceof CRecord::
			 Ifeq elselabel::
			 New CRecordArity ::
			 Dup ::
			 Getstatic (RecordLabel.insert
				    (stringids2strings (stringid, nil))) ::
			 Invokespecial (CRecordArity,"<init>",
					([Arraysig,Classsig CLabel],
					 [Voidsig])) ::
			 stampcode') @
			(Checkcast CRecord::
			 Invokevirtual (CRecord,"getArity",
					 ([],[Classsig CRecordArity])) ::
			 Ifacmpne elselabel ::
			 stampcode') @
			[Checkcast CRecord,
			 Invokevirtual (CRecord,"getValues",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(stringid,0))
		    end
		  | testCode (TupTest ids) =
		    (* Arity vergleichen (int), dann binden *)
		    let
			fun bindit (id''::nil,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())

			    in
				(atCodeInt i) ::
				Aaload ::
				(Astore loc) ::
				nil
			    end
			  | bindit (id''::rest,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
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
			Comment "Hi 99"::
			stampcode' @
			[Instanceof CDMLTuple,
			 Ifeq elselabel] @
			stampcode' @
			[Checkcast CDMLTuple,
			 Invokeinterface (CDMLTuple,"getArity",([],[Intsig])),
			 atCodeInt (Int.toLarge (length ids)),
			 Ificmpne elselabel] @
			stampcode' @
			[Checkcast CDMLTuple,
			 Invokeinterface (CDMLTuple,"getVals",
					([],[Arraysig, Classsig CVal]))] @
			(bindit(ids,0))
		    end
		  | testCode (LabTest (s', id' as Id (_,stamp'',_))) =
		    let
			val r = Register.get stamp''
			val n = if r = 1
				    then Register.assign(id', r)
				else r
		    in
			Comment "Hi 69"::
			stampcode' @
			[Instanceof CDMLTuple,
			 Ifeq elselabel] @
			stampcode' @
			[Checkcast CDMLTuple,
			 Ldc (JVMString s'),
			 Invokeinterface (CDMLTuple,"get",([],[Classsig CVal])),
			 Astore n]
		    end
		      | testCode test' = raise Debug (Test test')

	    in
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
	  | decCode (ReturnStm (_, exp')) =
	(* sonstiges Return *)
		    (expCode exp')@
		    [Areturn]

	  | decCode (HandleStm(_,body', id',body'')) =
		    let
			val loc = Register.assign (id', Register.nextFree())
			val try   = Label.new()
			val to = Label.pushANewHandle ()
			val using = Label.new()
			val b1 = List.concat (map decCode body')
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
	  | decCode (EndHandleStm (_, body')) =
		    let
			val lab' = Label.popHandle()
		    in
			Label lab'::
			List.concat (map decCode body')
		    end
	  | decCode (EvalStm (_, exp')) =
		    (expCode exp') @ [Pop]
	  | decCode (ExportStm (_,ids)) =
		    (* ExportStm of coord * id list *)
		    (* 1. Label[] laden *)
		    (* 2. Value[] bauen *)
		    (* 3. Record erzeugen *)
		    (* Label[] bauen passiert statisch! *)
		    let
			val arity = length ids
			(* 1. *)
			(* dreht Liste um und erzeugt Labelstrings *)
			fun ids2strings (Id (_,_,ExId l)::ids',s')=
			    ids2strings (ids', l::s')
			  | ids2strings (Id (_,stamp',InId)::ids',s') =
			    ids2strings (ids', Stamp.toString stamp'::s')
			  | ids2strings (nil, s') = s'

			(* 2. *)
			 fun load (Id (_,stamp',_)::rs,j) =
			     Dup::
			     (atCodeInt j)::
			     (Comment "Hi4'")::
			     (Aload (Register.get stamp'))::
			     Aastore::
			     (load (rs,j+1))
			   | load (nil,_) = nil
		    (* 3. *)
			 val mp = Register.nextFree()
		    in
			(mainpickle:=mp;
			 [Comment "[Mainpickle "] @
			 [New CRecord,
			  Dup,
			  Getstatic (RecordLabel.insert
				     (ids2strings (ids, nil))),
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal] @
			 (load (ids,0)) @
			 [Invokespecial (CRecord,"<init>",
					 ([Arraysig, Classsig CLabel,
					   Arraysig, Classsig CVal],
					  [Voidsig])),
			  Astore mp,
			  Comment "Mainpickle ]"])
		     end

	  | decCode (IndirectStm (_, ref (SOME body'))) = List.concat (map decCode body')
	  | decCode (IndirectStm (_, ref NONE)) = nil
	and
	    idCode (Id(_,stamp',_)) = Comment "Hi87"::stampCode stamp'
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
				     Aload (Register.get stamp')]
			else
			    (* Es handelt sich um eine freie Variable, die
			     beim Abschluss bilden in ein Feld der
			     aktuellen Klasse kopiert wurde.
			     In diesem Fall kann apply nicht als statisch
			     deklariert werden. *)
			    (Lambda.noSapply ();
			     [Comment ("Hi. Stamp="^(Stamp.toString stamp')^
				       ". Lambda.top = "^Stamp.toString (Lambda.top())^
				       " in "^Int.toString (Register.get stamp')^
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
			       Comment "Hi20"::
			       stampCode stamp' @
			       idacode @
			       [Invokeinterface (CVal, "apply",
						 ([Classsig CVal],
						  [Classsig CVal]))])]
	    end

	  | expCode (PrimAppExp (_, name, ids)) =
		(case name of
		     "print" => (Getstatic COut::
				 (idCode (hd ids))@
				 [Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj],[Voidsig])),
				  Getstatic CUnit])
		   | _ => nil)

	  | expCode (FunExp(coord',string', (lambda as (OneArg (id' as Id (_,stamp',_)), _))::rest)) =
		     (* FunExp of coord * string * (id args * dec) list *)
		     (* id ist formaler Parameter *)
		     (* 1. Abschluß bilden:  - Objekt bauen oder aus Register nehmen *)
		     (*                      - freie Variablen aus Register mit putfields versenken *)
		     (* 2. Klasse erzeugen *)
		     let
			 val className = classNameFromStamp stamp'
			 val freeVarList = FreeVars.getVars id'
			 (* 1. *)
			 val object = let
					  val loc = Register.getLambda stamp'
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
				 Dup ::
				 Comment "Hi0408"::
				 (stampCode stamp'') @
				 [Putfield (className^"/"^
					    (fieldNameFromStamp stamp''),
					    [Classsig CVal])]
			 in
			     val loadVars = List.concat (map loadFreeVar freeVarList)
			 end
		     in
			 Lambda.push id';
			 Catch.push ();
			 Lambda.pushFun illegalId;
			 Label.push();
			 Register.push();
			 Class.push(className);
			 expCodeClass(lambda);
			 Class.pop();
			 Register.pop();
			 Label.pop();
			 Lambda.popFun ();
			 Catch.pop();
			 Lambda.pop();
			 object @ loadVars @
			 (case
			      rest of nil => nil
			    | _ => expCode (FunExp(coord', string', rest)))
		     end

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
			 (* dreht Liste um und entfernt überflüssige ids *)
			 fun labids2strings ((Lab (_,l), _)::labids',s')=
			     labids2strings (labids', l::s')
			   | labids2strings (nil, s') = s'

			 (* 2. *)
			 fun load ((_,Id (_,stamp',_))::rs,j) =
			     Dup::
			     (atCodeInt j)::
			     (Comment "Hi4")::
			     (Aload (Register.get stamp'))::
			     Aastore::
			     (load (rs,j+1))
			   | load (nil,_) = nil
			 (* 3. *)
		     in
			 [Comment "[Record "] @
			 [New CRecord,
			  Dup,
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
			 val scon = case lit' of
			     CharLit _   => CChar
			   | IntLit _    => CInt
			   | RealLit _   => CReal
			   | StringLit _ => CStr
			   | WordLit _   => CInt
		     in
			 [Getstatic (Literals.insert lit', [Classsig scon])]
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
		val _ = if !ECHO >=1 then print ("create Class "^(Class.getCurrent())^"\n") else ()
		val e = if !DEBUG >=2 then
		    let
			val nodebug=Label.new()
		    in
			(Getstatic (Class.getCurrent()^"/DEBUG", [Boolsig])::
			 Ifeq nodebug::
			 Getstatic ("java/lang/System/out", [Classsig "java/io/PrintStream"])::
			 Ldc (JVMString "Betrete: (")::
			 Invokevirtual ("java/io/PrintStream","print",([Classsig CObj],[Voidsig]))::
			 Getstatic ("java/lang/System/out", [Classsig "java/io/PrintStream"])::
			 Ldc (JVMString (nameFromId (Lambda.getOuterFun ())))::
			 Invokevirtual ("java/io/PrintStream","println",([Classsig CObj],[Voidsig]))::
			 Label nodebug::
			 (List.concat (map decCode body')))
		    end
			else (List.concat (map decCode body'))

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
		    val bd = vars (Register.max(),e)
		end
		local (* Register in apply initialisieren *)
		    fun initializeLocals 0 = [Label afterInit]
		      | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		in
		    val initRegister = initializeLocals (Register.max ())
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
				    Locals (Register.max()+1),
				    ap,
				    Catch.top(), false)
		val sapply = Method ([MPublic, MStatic],
				     "sapply",
				     ([Classsig CVal], [Classsig CVal]),
				     Locals (Register.max()),
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
		val class = Class([CPublic],
				  className,
				  CFcnClosure,
				  nil,
				  fieldscode,
				  (applY::init::
				   (if Lambda.sapplyPossible () then
					[sapply]
				    else nil)))
	    in
		classToJasmin (class)
	    end
and
    compile prog = genProgramCode (0,0,"Emil", imperatifyString prog)
and
    compilefile f = genProgramCode (0,0,"Emil", imperatifyFile f)
    end
