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
		    val free:StampSet.t ref = ref (StampSet.new ())
		    val freeStack:StampSet.t list ref = ref nil

		    fun insert (Id (_,stamp',_)) =
			(if !ECHO>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    ". insert free: "^(Stamp.toString stamp')^"\n")
			 else ();

			 if stamp'=stamp_builtin orelse
			     Lambda.isSelfCall stamp'
			     then ()
			 else
			     StampSet.insert (!free, stamp'))
		    fun delete (Id (_,stamp',_)) =
			(if !ECHO>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    " delete free: "^(Stamp.toString stamp')^"\n")
			 else ();
			 StampSet.delete(!free, stamp'))

		    fun get () =
			let
			    val x = StampSet.fold (fn (x,xs) => x::xs) nil (!free)
			in
			    if !ECHO>=2 then
				(print ("Top "^(Stamp.toString (Lambda.top())));
				 printStampList x)
			    else ();
			    x
			end

		    fun isFree stamp' =
			StampSet.member (!free, stamp')

		    (* Betreten einer neuen Subfunktion. *)
		    fun enter () =
			(freeStack := !free::(!freeStack);
			 free := StampSet.new())

		    (* Verlassen der Subfunktion. *)
		    fun exit () =
			(StampSet.app
			 (fn x => StampSet.insert (!free,x))
			 (hd (!freeStack));
			 freeStack := tl (!freeStack))
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
	fun genProgramCode (debug, echo, optimize, name, program) =
	    (DEBUG := debug;
	     ECHO := echo;
	     OPTIMIZE := optimize;
	     Class.setInitial name;
	     let
		 (* freie Variablen berechnen. *)
		 val _ = app freeVarsDec program
		 val _ = if !ECHO>=2 then FreeVars.printFun () else ()
		 val _ = Lambda.createIdsLambdaTable()
		 (* val _ = app annotateTailDec program*)
		 (* Alle Deklarationen übersetzen *)

		 (* JVM-Register initialisieren. *)
		 fun initializeLocals 0 = [Label afterInit]
		   | initializeLocals x = [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))

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
		 (* Toplevel environment is built in the run method.
		  All Objects are instantiated and initialized, function closures are built.
		  The result as well as the generated class files is stored into one single
		  pickle file. This is the last step of the compilation process. *)
		 val literalName = Class.getLiteralName()

		 val decs = decListCode program
		 val run = Method([MPublic], "run", ([], [Voidsig]),
				  Locals (Register.max()+1),
				  Multi decs ::
				  (if !ECHO >= 1 then
				       Multi [Getstatic ("java/lang/System/out",
							 [Classsig "java/io/PrintStream"]),
					      Aload (!mainpickle),
					      Invokevirtual ("java/io/PrintStream","print",
							     ([Classsig "java/lang/Object"],
							      [Voidsig]))]
				   else Nop) ::
				       Ldc (JVMString (name^".pickle")) ::
				       New literalName ::
				       Dup ::
				       Invokespecial (literalName,
						      "<init>",
						      ([], [Voidsig])) ::
				       Aload (!mainpickle) ::
				       Invokestatic MPickle ::
				       Pop ::
				       Return ::
				       nil,
				  Catch.top(),
				  false)

		 (* The main class *)
		 val class = Class([CPublic],
				   name,
				   CDMLThread,
				   nil,
				   nil,
				   [main, init, run])

		 (* Literals can not be stored in the main class because
		  DMLThreads must not be pickled *)
		 val clinit = Method([MPublic],
				     "<clinit>",
				     ([],[Voidsig]),
				     Locals 6,
				     Literals.generate
				     (RecordLabel.generate()),
				     nil,
				     false)

		 (* Default initialization: Invocation of the super class. *)
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
		 if !ECHO >=2 then print "Generating main and literal class..." else ();
		 classToJasmin (class);
		 classToJasmin (literale);
		 if !ECHO >=2 then print "Okay.\n" else ()
	     end
	 )

	and getBuiltin builtin' =
	    Multi [Getstatic (Literals.insert (StringLit builtin'), [Classsig CStr]),
		   Invokestatic (CBuiltin, "getBuiltin",
				 ([Classsig CStr], [Classsig CVal]))]

	and builtinStamp stamp' =
	    if stamp'=stamp_Match then (Getstatic CMatch,true) else
		if stamp'=stamp_false then (Getstatic CFalse,true) else
		    if stamp'=stamp_true then (Getstatic CTrue,true) else
			if stamp'=stamp_nil then (Getstatic CNil,true) else
			    if stamp'=stamp_cons then (Getstatic CCons,true) else
				if stamp'=stamp_ref then (Getstatic CRef,true) else
				    if stamp'=stamp_Bind then (Getstatic CBind,true) else
					if stamp'=stamp_eq then (Getstatic CEquals,true) else
					    if stamp'=stamp_assign then (Getstatic CAssign,true) else
						if stamp'=stamp_builtin then (Getstatic CBuilt,true) else
						    (Nop,false)

	and dcl (d, akku) = Multi (decCode d) :: akku

	and decListCode (dec::rest) =
	    Multi (decCode dec) ::
	    decListCode rest
	  | decListCode nil = nil

	and normalReturn e =
	    if !DEBUG>=2 then
		let
		    val nodebug2=Label.new()
		in
		    Getstatic (Class.getCurrent()^"/DEBUG", [Boolsig]) ::
		    Ifeq nodebug2 ::
		    Getstatic ("java/lang/System/out", [Classsig "java/io/PrintStream"]) ::
		    Ldc (JVMString ")\n") ::
		    Invokevirtual ("java/io/PrintStream","print",([Classsig CObj],[Voidsig])) ::
		    Label nodebug2 ::
		    e
		end
	    else
		e

	(* Code generation for declarations *)
	and decCode (ValDec(_, id' as Id (_,stamp',_), exp',_)) =
	    let
		val l=Register.get stamp'
		val loc =
		    if l = 1
			then Register.assign(id', Register.nextFree())
		    else l
	    in
		(Lambda.pushFun id';
		 Multi (expCode exp') ::
		 Astore loc ::
		 nil)
		before Lambda.popFun()
	    end

	  | decCode (RecDec (_, nil, _)) = nil
	  | decCode (RecDec (_,idexps as ((recid,_)::_),_)) =
	    (* RecDec of coord * (id * exp) list * isTopLevel *)
	    (* 1. create a new object for each id and store it into a new register. *)
	    (* 2. evaluate the expression and pop the result *)
	    (* !! The closure is made on evaluation of the expressions *)
	    let
		(* 1st step *)
		local
		    fun init ((id',exp'),akku) =
			let
			    val loc = Register.assign(id',Register.nextFree())
			    fun funList ((OneArg id'',_)::rest) =
				let
				    val className = classNameFromId id''
				    (* the formal parameter of a function can always be found
				in register 1. *)
				    val _ = Register.assign(id'',1)
				    val _ = Register.assignLambda(id'', loc)
				in
				    New className ::
				    Dup ::
				    Invokespecial (className, "<init>",
						   ([],[Voidsig])) ::
				    Astore loc ::
				    funList rest
				end
			      | funList nil = nil

			    val one = case exp' of
				(* user defined function *)
				FunExp (_,_,idexplist) =>
				    funList idexplist

			      (* constructor application *)
			      | ConAppExp (_,id',id'') =>
				    New CConVal ::
				    Dup ::
				    idCode id' ::
				    Invokespecial (CConVal, "<init>",
						   ([Classsig CConstructor],
						    [Voidsig])) ::
				    Dup ::
				    Astore loc ::
				    idCode id'' ::
				    Invokeinterface (CConVal, "setContent",
						     ([Classsig CVal],
						      [Voidsig])) ::
				    nil

			      | VarExp (_, id') =>
				    idCode id' ::
				    Astore loc ::
				    nil

			      (* record *)
			      | RecExp _ =>
				    New CRecord ::
				    Dup ::
				    Invokespecial (CRecord,"<init>",
						   ([],[Voidsig])) ::
				    Astore loc ::
				    nil

			      (* tuple *)
			      | TupExp _ =>
				    New CTuple ::
				    Dup ::
				    Invokespecial (CTuple,"<init>",
						   ([],[Voidsig])) ::
				    Astore loc ::
				    nil
			in
			    Multi one ::
			    akku
			end
		in
		    val initcode = List.foldr init nil idexps
		end

		(* 2nd step *)
		local
		    fun evalexp ((id',exp')::idexps') =
			(Lambda.pushFun id';
			 Multi (expCode exp')
			 before Lambda.popFun()) ::
			Pop::
			evalexp idexps'
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
		  Dup,
		  idCode id',
		  Invokespecial(CExWrap,"<init>",
				([Classsig CVal],[Voidsig])),
		  Athrow]

	  | decCode (TestStm(_,id' as Id (_,stamp',_),test',body',body'')) =
		 (* test whether id' matches with test'.
		  If so, eval body', if not, eval body'' *)
	    let
		val danach = Label.new ()
		val elselabel = Label.new ()
		val stampcode' = stampCode stamp'

		fun switchNumber (LitTest (WordLit a)) =
		    LargeWord.toLargeInt a
		  | switchNumber (LitTest (IntLit a)) =
		    a
		  | switchNumber (LitTest (CharLit a)) =
		    Int.toLarge (ord a)
		  | switchNumber _ = 0

		fun checkForSwitch (test', body'', whichSwitch) =
		    (case body'' of
			 (sh as SharedStm (_,b'', r as ref shared))::_ =>
			     if shared <=0 then
				 let
				     val ret as (isSwitch,_,_) = checkForSwitch (test', b'', !r)
				 in
				     (if whichSwitch>0 andalso isSwitch then
					  r := whichSwitch else ();
					  ret)
				 end
			     else if shared = whichSwitch orelse whichSwitch=0 then
				 (false, sh, switchNumber test')
				  else (print ("Let him who has understanding reckon the number of the Leif, for it is a human's number: Its number is "^Int.toString shared^" and not "^Int.toString whichSwitch^".\n");
					(false, sh, switchNumber test'))
		       | IndirectStm (_,ref (SOME b''))::_ =>
				      checkForSwitch (test', b'', whichSwitch)
		       | (t as TestStm (_, Id (_, stamp'', _),
					test'', body''', body''''))::_ =>
			     (if stamp' = stamp'' then
			      (case
				   (test', test'') of
				   (LitTest (WordLit a), LitTest (WordLit _)) =>
				       (true, t, LargeWord.toLargeInt a)
				 | (LitTest (IntLit a), LitTest (IntLit _)) =>
				       (true, t, a)
				 | (LitTest (CharLit a), LitTest (CharLit _)) =>
				       (true, t, Int.toLarge (ord a))
				 | _ => (false, t, switchNumber test'))
			  else (false, t, switchNumber test'))
		       | _ => (false, hd body'', switchNumber test'))

		fun generateSwitch begin =
		    let
			fun generateBody (akku, switchlist, labelList, body',
					  test', body'') =
			    let
				val lab = Label.newSwitch ()
				val (switch, inst, number) =
				    checkForSwitch (test', body'', begin)
			    in
				if switch
				    then
					case inst of
					    TestStm (_, _, t'',b',b'')
					    => generateBody
					    (Label lab ::
					     Multi (decListCode body') ::
					     akku,
					     number::switchlist,
					     lab :: labelList,
					     b', t'', b'')
					  | _ => raise Match
				else
				    let
					val behind = Label.new ()
				    in
					(Label lab ::
					 Multi (decListCode body') ::
					 Multi akku ::
					 Label behind ::
					 decListCode body'',
					 number::switchlist,
					 lab :: labelList,
					 behind)
				    end
			    end

			fun isTableSwitch (i::(rest as (j::_))) =
			    if LargeInt.- (i,Int.toLarge 1) = j then
				isTableSwitch rest
			    else (false, 0)
			  | isTableSwitch (i::nil) = (true, i)
			  | isTableSwitch nil = (false, 0)

			fun makeSwitch (bod, switchlist, labellist, behind) =
			    (case isTableSwitch switchlist of
				 (true, i) => Tableswitch (i, labellist, behind)
			       | _ => Lookupswitch (switchlist, labellist, behind)) ::
			     bod

			val gb as (_, _, _, behind) = generateBody (nil, nil, nil, body', test', body'')
			val sw = makeSwitch gb
		    in
			Label (Label.fromNumber begin) ::
			stampcode' ::
			(case test' of
			     LitTest (WordLit startwert) =>
				 Instanceof CWord ::
				 Ifeq behind ::
				 stampcode' ::
				 Checkcast CWord ::
				 Getfield (CWord^"/value", [Intsig]) ::
				 sw
			   | LitTest (IntLit startwert) =>
				 Instanceof CInt ::
				 Ifeq behind ::
				 stampcode' ::
				 Checkcast CInt ::
				 Getfield (CInt^"/value", [Intsig]) ::
				 sw
			   | LitTest (CharLit startwert) =>
				 Instanceof CChar ::
				 Ifeq behind ::
				 stampcode' ::
				 Checkcast CChar ::
				 Getfield (CChar^"/value", [Charsig]) ::
				 sw)
		    end

		fun testCode (LitTest lit') =
		    (case lit' of
			 WordLit w' =>
			     stampcode' ::
			     Instanceof CWord ::
			     Ifeq elselabel ::
			     stampcode' ::
			     Checkcast CWord ::
			     Getfield (CWord^"/value", [Intsig]) ::
			     atCodeWord w' ::
			     Ificmpne elselabel ::
			     nil
		       | IntLit i' =>
			     stampcode' ::
			     Instanceof CInt ::
			     Ifeq elselabel ::
			     stampcode' ::
			     Checkcast CInt ::
			     Getfield (CInt^"/value", [Intsig]) ::
			     atCodeInt i' ::
			     Ificmpne elselabel ::
			     nil
		       | CharLit c' =>
			     stampcode' ::
			     Instanceof CChar ::
			     Ifeq elselabel ::
			     stampcode' ::
			     Checkcast CChar ::
			     Getfield (CChar^"/value", [Charsig]) ::
			     atCodeInt (Int.toLarge (Char.ord c')) ::
			     Ificmpne elselabel ::
			     nil
		       | StringLit s' =>
			     stampcode' ::
			     Instanceof CStr ::
			     Ifeq elselabel ::
			     stampcode' ::
			     Checkcast CStr ::
			     Getfield (CStr^"/value", [Classsig CString]) ::
			     Ldc (JVMString s') ::
			     Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])) ::
			     Ifeq elselabel ::
			     nil
		       | r as (RealLit r') =>
			     stampcode' ::
			     Instanceof CReal ::
			     Ifeq elselabel ::
			     stampcode' ::
			     Checkcast CReal ::
			     Getfield (CReal^"/value", [Floatsig]) ::
			     atCode r ::
			     Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])) ::
			     Ifeq elselabel ::
			     nil)

		  | testCode (ConTest (id'',NONE)) =
			 stampcode' ::
			 idCode id'' ::
			 Ifacmpne elselabel ::
			 nil

		  | testCode (ConTest (id'',SOME id''')) =
		    let
			val loc = Register.assign(id''',Register.nextFree())
			val _ = FreeVars.setFun (id''', Lambda.top())
		    in
			stampcode' ::
			Instanceof CConVal ::
			Ifeq elselabel ::
			stampcode' ::
			Checkcast CConVal ::
			Invokeinterface (CConVal, "getConstructor",
					 ([], [Classsig CConstructor])) ::
			idCode id'' ::
			Ifacmpne elselabel ::
			idCode id' ::
			Checkcast CConVal ::
			Invokeinterface (CConVal, "getContent",
					 ([],[Classsig CVal])) ::
			Astore loc ::
			nil
		    end

		  | testCode (RecTest stringid) =
		    (* Load arity, compare, then bind *)
		    let
			(* reverse the list and remove ids *)
			fun stringids2strings ((l, _)::stringids',s')=
			    stringids2strings (stringids', l::s')
			  | stringids2strings (nil, s') = s'
			fun bindit ((_,id'')::nil,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
				val _ = FreeVars.setFun (id'', Lambda.top())
			    in
				atCodeInt i ::
				Aaload ::
				Astore loc ::
				nil
			    end
			  | bindit ((_,id'')::rest,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())
			    in
				Dup ::
				atCodeInt i ::
				Aaload ::
				Astore loc ::
				bindit(rest,i+1)
			    end
			  | bindit (nil,_) = nil
		    in
			stampcode' ::
			Instanceof CRecord::
			Ifeq elselabel::
			New CRecordArity ::
			Dup ::
			Getstatic (RecordLabel.insert
				   (stringids2strings (stringid, nil))) ::
			Invokespecial (CRecordArity,"<init>",
				       ([Arraysig,Classsig CLabel],
					[Voidsig])) ::
			stampcode' ::
			Checkcast CRecord::
			Invokevirtual (CRecord,"getArity",
				       ([],[Classsig CRecordArity])) ::
			Ifacmpne elselabel ::
			stampcode' ::
			Checkcast CRecord ::
			Invokevirtual (CRecord,"getValues",
				       ([],[Arraysig, Classsig CVal])) ::
			bindit (stringid,0)
		    end

		  | testCode (TupTest ids) =
		    (* compare the arity (int), then bind *)
		    let
			fun bindit (id''::rest,i) =
			    let
				val loc = Register.assign(id'',Register.nextFree())
				val _ = FreeVars.setFun (id'',Lambda.top())
				val b' = atCodeInt i ::
				    Aaload ::
				    Astore loc ::
				    bindit(rest,i+1)
			    in
				case rest of
				    nil => b'
				  | _ => Dup :: b'
			    end
			  | bindit (nil,_) = nil

			val lgt = length ids
		    in
			if lgt = 0
			    then
				stampcode' ::
				Getstatic CUnit ::
				Ifacmpne elselabel ::
				nil
			else
			    stampcode' ::
			    Instanceof CDMLTuple ::
			    Ifeq elselabel ::
			    stampcode' ::
			    Checkcast CDMLTuple ::
			    Invokeinterface
			    (CDMLTuple, "getArity",
			     ([], [Intsig])) ::
			    atCodeInt (Int.toLarge lgt) ::
			    Ificmpne elselabel ::
			    stampcode' ::
			    Checkcast CDMLTuple ::
			    Invokeinterface
			    (CDMLTuple,"getVals",
			     ([],[Arraysig,
				  Classsig CVal])) ::
			    bindit(ids,0)
		    end

		  | testCode (LabTest (s', id' as Id (_,stamp'',_))) =
		    let
			val r = Register.get stamp''
			val n = if r = 1
				    then Register.assign(id', r)
				else r
		    in
			stampcode' ::
			Instanceof CDMLTuple ::
			Ifeq elselabel ::
			stampcode' ::
			Checkcast CDMLTuple ::
			Ldc (JVMString s') ::
			Invokeinterface (CDMLTuple,"get",([Classsig CString],[Classsig CVal])) ::
			Astore n ::
			nil
		    end

		  | testCode (test') = raise Debug (Test test')

		val begin = Label.newNumber ()

		fun normalTest () =
		    Multi (testCode test') ::
		    Multi
		    (decListCode body') ::
		    Goto danach ::
		    Label elselabel ::
		    Multi
		    (decListCode body'') ::
		    [Label danach]
	    in
		if !OPTIMIZE >=3 then
		    case checkForSwitch (test', body'', 0) of
			(true, _, _) => generateSwitch begin
		      | _ => normalTest ()
		else normalTest ()
	    end

	  | decCode (SharedStm(_,body',da as ref schonda)) =
	    if schonda <= 0
		then
		    let
			val _ = da := Label.newNumber ()
		    in
			Comment "SharedStm" ::
			Label (Label.fromNumber (!da))::
			decListCode body'
		    end
	    else
		 [Goto (Label.fromNumber schonda)]

	  | decCode (ReturnStm (_,ap as AppExp(_,id' as (Id (_,stamp',_)),arg'))) =
		(* tailcall applikation *)
		if Lambda.isSelfCall stamp' then
		    idArgCode
		    (arg',
		     [Astore 1,
		      Goto alpha])
		else
		    normalReturn
		    [Multi (expCode ap),
		     Areturn]
	  | decCode (ReturnStm (_, exp')) =
		    (* ordinary Return *)
		    normalReturn
		    [Multi (expCode exp'),
		     Areturn]

	  | decCode (HandleStm(_,body', id',body'')) =
		    let
			val loc = Register.assign (id', Register.nextFree())
			val try   = Label.new()
			val to = Label.pushANewHandle ()
			val using = Label.new()
			val nocatch = Label.new()
		    in
			Catch.add (Catch (CExWrap, try, to, using));
			Label try::
			Multi (decListCode body') ::
			Goto nocatch ::
			Label using ::
			Invokevirtual (CExWrap,"getValue",
				       ([],[Classsig CVal])) ::
			Astore loc ::
			Multi (decListCode body'') ::
			[Label nocatch]
		    end

	  | decCode (EndHandleStm (_, body')) =
		    let
			val lab' = Label.popHandle()
		    in
			Comment ("EndHandleStm: Label "^lab') ::
			Label lab'::
			decListCode body'
		    end

	  | decCode (EvalStm (_, exp')) =
		    Multi (expCode exp') ::
		    [Pop]

	  | decCode (ExportStm (_,ids)) =
		    (* ExportStm of coord * id list *)
		    (* 1. load Label[] *)
		    (* 2. build Value[] *)
		    (* 3. create Record *)
		    (* Label[] are built statically! *)
		    let
			val arity = length ids
			(* 1st *)
			(* revert the list and create Labelstrings *)
			fun ids2strings (Id (_,_,ExId l)::ids',s')=
			    ids2strings (ids', l::s')
			  | ids2strings (Id (_,stamp',InId)::ids',s') =
			    ids2strings (ids', Stamp.toString stamp'::s')
			  | ids2strings (nil, s') = s'

			(* 2nd *)
			 fun load (Id (_,stamp',_)::rs,j) =
			     Dup::
			     (atCodeInt j)::
			     (Aload (Register.get stamp'))::
			     Aastore::
			     (load (rs,j+1))
			   | load (nil,_) = nil
			 (* 3rd *)
			 val mp = Register.nextFree()
		    in
			(mainpickle:=mp;
			 [Comment "[Mainpickle ",
			  New CRecord,
			  Dup,
			  Getstatic (RecordLabel.insert
				     (ids2strings (ids, nil))),
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal,
			  Multi (load (ids,0)),
			  Invokespecial (CRecord,"<init>",
					 ([Arraysig, Classsig CLabel,
					   Arraysig, Classsig CVal],
					  [Voidsig])),
			  Astore mp,
			  Comment "Mainpickle ]"])
		     end

	  | decCode (IndirectStm (_, ref (SOME body'))) =
		     decListCode body'

	  | decCode (IndirectStm (_, ref NONE)) = nil

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
			     Aload 0)
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
				    Aload 1
				else
				    Aload (Register.get stamp')
			else
			    (* Es handelt sich um eine freie Variable, die
			     beim Abschluss bilden in ein Feld der
			     aktuellen Klasse kopiert wurde.
			     In diesem Fall kann apply nicht als statisch
			     deklariert werden. *)
			    (Lambda.noSapply ();
			      Get [Aload 0,
				   Getfield (Class.getCurrent()^"/"^
					     (fieldNameFromStamp stamp'),
					     [Classsig CVal])])
	    end
	and
	    idArgCode (OneArg id', init) = idCode id' :: init

	  | idArgCode (TupArgs ids, init) =
	    let
		fun iac (id'::ids, accu) =
		    iac (ids, Dup::idCode id'::Aastore::accu)
		  | iac (nil, accu) = accu
	    in
		New CTuple ::
		Dup ::
		atCodeInt (Int.toLarge (List.length ids)) ::
		Anewarray CVal ::
		iac
		(ids,
		 Invokespecial
		 (CTuple, "<init>",
		  ([Arraysig, Classsig CVal], [Voidsig])) ::
		 init)
	    end

	  | idArgCode (RecArgs stringids, init) =
	    let
		val arity = List.length stringids
		fun iac ((name',id')::ids, accu1,accu2) =
		    iac (ids,
			 Dup ::
			 Ldc (JVMString name') ::
			 Aastore ::
			 accu1,
			 Dup ::
			 idCode id' ::
			 Aastore ::
			 accu2)
		  | iac (nil, accu1, accu2) =
		    atCodeInt (Int.toLarge arity) ::
		    Anewarray CString ::
		    Multi accu1 ::
		    atCodeInt (Int.toLarge arity) ::
		    Anewarray CVal ::
		    accu2
	    in
		New CTuple ::
		Dup ::
		iac
		(stringids,
		 nil,
		 Invokespecial (CTuple, "<init>",
				([Arraysig, Classsig CVal],
				 [Voidsig])) ::
		 init)
	    end

	and
	    expCode (AppExp(_,id' as Id(_,stamp',_),ida'')) =
	    if stamp'=stamp_builtin then
		idArgCode
		(ida'',
		 Invokestatic (CBuiltin, "getBuiltin",
			       ([Classsig CStr],
				[Classsig CVal])) ::
		 nil)
		else
		    [Ifstatic
		     (stamp',
		      idArgCode
		      (ida'',
		       [Invokestatic (classNameFromStamp
				     (Lambda.getLambda stamp'),
				     "sapply",
				     ([Classsig CVal],
				      [Classsig CVal]))]),
		      stampCode stamp' ::
		      idArgCode
		      (ida'',
		       [Invokeinterface
			(CVal, "apply",
			 ([Classsig CVal],
			  [Classsig CVal]))]))]

	  | expCode (PrimAppExp (_, name, ids)) =
		(case name of
		     "print" => [Getstatic COut,
				 idCode (hd ids),
				 Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj],[Voidsig])),
				 Getstatic CUnit]
		   | _ => nil)

	  | expCode (FunExp(coord',string', (lambda as (OneArg (id' as Id (_,stamp',_)), _))::rest)) =
		     (* FunExp of coord * string * (id args * dec) list *)
		     (* id is formal parameter *)
		     (* 1st build closure: - instantiate or load object *)
		     (*                    - set free variables via putfields *)
		     (* 2nd generate corresponding class file *)
		     let
			 val className = classNameFromStamp stamp'
			 val freeVarList = FreeVars.getVars id'
			 (* 1st *)
			 val object = let
					  val loc = Register.getLambda stamp'
				      in
					  if loc= ~1 (* then instantiate object *)
					      then
						  Multi
						  [New className,
						   Dup,
						   Invokespecial (className,
								  "<init>",
								  ([],
								   [Voidsig]))]
					  else
					      Aload loc
				      end
			 (* 2. *)
			 local
			     fun loadFreeVar (stamp'', akku) =
				 Dup ::
				 stampCode stamp'' ::
				 Putfield (className^"/"^
					   (fieldNameFromStamp stamp''),
					   [Classsig CVal]) ::
				 akku
			 in
			     val loadVars =
				 List.foldr
				 loadFreeVar
				 (case
				      rest of nil => nil
				    | _ => expCode (FunExp(coord', string', rest)))
				 freeVarList
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
			 object :: loadVars
		     end

	  | expCode (RecExp(_, nil)) =
		     [Getstatic (CConstants^"/dmlunit", [Classsig CName])]

	  | expCode (RecExp(_,labid)) =
		     (* RecExp of coord * (lab * id) list *)
		     (* 1st load Label[] *)
		     (* 2nd build Value[] *)
		     (* 3rd create Record *)
		     (* the Label[] is built statically! *)
		     let
			 val arity = length labid
			 (* 1st *)
			 (* reverse list and remove ids *)
			 fun labids2strings ((Lab (_,l), _)::labids',s')=
			     labids2strings (labids', l::s')
			   | labids2strings (nil, s') = s'

			 (* 2nd *)
			 fun load ((_,Id (_,stamp',_))::rs,j) =
			     Dup ::
			     atCodeInt j ::
			     Aload (Register.get stamp') ::
			     Aastore ::
			     (load (rs,j+1))
			   | load (nil,_) = nil
			 (* 3. *)
		     in
			 [Comment "[Record ",
			  New CRecord,
			  Dup,
			  Getstatic (RecordLabel.insert
				     (labids2strings (labid, nil))),
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal,
			  Multi (load (labid,0)),
			  Invokespecial (CRecord,"<init>",
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
			     Dup ::
			     atCodeInt i ::
			     stampCode stamp' ::
			     Aastore ::
			     ids(rest,i+1)
			   | ids (nil,_) = nil
		     in
			 [New CTuple,
			  Dup,
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal,
			  Multi (ids (longids, 0)),
			  Invokespecial (CTuple, "<init>",
					 ([Arraysig, Classsig CVal],
					  [Voidsig]))]
		     end

	       | expCode (VarExp(_,id')) =
		     [idCode id']

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
		     [idCode id']

		   | expCode (ConAppExp (_, id', id'')) =
		     [idCode id',
		      idCode id'',
		      Invokeinterface (CVal, "apply",
				       ([Classsig CVal], [Classsig CVal]))]

		   | expCode (SelAppExp (_, Lab (_,label'), id')) =
		     let
			 val afterthrow = Label.new ()
		     in
			 idCode id' ::
			 Dup ::
			 Instanceof CDMLTuple ::
			 Ifne afterthrow ::
			 Pop ::
			 New CExWrap ::
			 Dup ::
			 Getstatic (Literals.insert
				    (StringLit "Typfehler"),
				    [Classsig CStr]) ::
			 Invokespecial(CExWrap,"<init>",
				       ([Classsig CVal],[Voidsig])) ::
			 Athrow ::
			 Label afterthrow ::
			 Checkcast CDMLTuple ::
			 (case LargeInt.fromString label' of
			     NONE =>
				 [Ldc (JVMString label'),
				  Invokeinterface (CDMLTuple, "get",
						   ([Classsig CString], [Classsig CVal]))]
			   | SOME i =>
				 [atCodeInt i,
				  Invokeinterface (CDMLTuple, "get",
						   ([Intsig], [Classsig CVal]))])
		     end
	  | expCode e = raise Debug (Exp e)
	and
    expCodeClass (OneArg id',body') =
	    let
		val _ = if !ECHO >=1 then print ("create Class "^(Class.getCurrent())^"\n") else ()
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
			vars (n-1,Var (n, "woaswoisi"^(Int.toString n),
			      [Classsig CVal], alpha, omega)::akku)
		in
		    val fieldscode = fields freeVarList
		    val bd = (*vars
			(Register.max(), *)
			 let
			     val decs = decListCode body'
			 in
			     if !DEBUG >=2 then
				 let
				     val nodebug=Label.new()
				 in
				     Getstatic
				     (Class.getCurrent()^"/DEBUG",
				      [Boolsig])::
				     Ifeq nodebug::
				     Getstatic
				     ("java/lang/System/out",
				      [Classsig "java/io/PrintStream"])::
				     Ldc (JVMString "Betrete: (")::
				     Invokevirtual
				     ("java/io/PrintStream",
				      "print",([Classsig CObj],
					       [Voidsig]))::
				     Getstatic
				     ("java/lang/System/out",
				      [Classsig "java/io/PrintStream"])::
				     Ldc (JVMString
					  (nameFromId
					   (Lambda.getOuterFun ())))::
				     Invokevirtual
				     ("java/io/PrintStream",
				      "println",([Classsig CObj],
						 [Voidsig]))::
				     Comment "expCodeClass: Label nodebug" ::
				     Label nodebug::
				     decs
				 end
			     else decs
			 end (*)*)
		end
		(* Wir bauen jetzt den Rumpf der Abstraktion *)
		val ap =
		    (Label alpha::
		     Multi bd ::
		     normalReturn
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
    compile prog = genProgramCode (0,0,2,"Emil", imperatifyString prog)
and
    compileFile (f, optimize) = genProgramCode (0,0,optimize,"Emil", imperatifyFile f)
    end
