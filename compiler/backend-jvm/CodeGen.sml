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
			(if !VERBOSE>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    ". insert free: "^(Stamp.toString stamp')^"\n")
			 else ();

			 if (*stamp'=stamp_builtin orelse*)
			     Lambda.isSelfCall stamp'
			     then ()
			 else
			     StampSet.insert (!free, stamp'))
		    fun delete (Id (_,stamp',_)) =
			(if !VERBOSE>=2 then
			     print ("Top "^(Stamp.toString (Lambda.top()))^
				    " delete free: "^(Stamp.toString stamp')^"\n")
			 else ();
			 StampSet.delete(!free, stamp'))

		    fun get () =
			let
			    val x = StampSet.fold (fn (x,xs) => x::xs) nil (!free)
			in
			    if !VERBOSE>=2 then
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

	    fun argApp (f, OneArg id') =
		f id'
	      | argApp (f, TupArgs ids) =
		app f ids
	      | argApp (f, RecArgs labids) =
		app (fn (_, id') => f id') labids
	in
	    fun freeVarsExp (LitExp _) = ()
	      | freeVarsExp (VarExp (_, id')) = fV.insert id'
	      | freeVarsExp (ConAppExp (_, id', idargs)) =
		(fV.insert id';
		 argApp (fV.insert,idargs))
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
		      | freeVarsFun ((args,body')::idbodys') =
			(fV.enter();
			 freeVarsDecs body';
			 argApp (fV.delete,args);
			 argApp
			 (fn (Id (_, stamp', _)) =>
			  FreeVars.setVars (stamp', fV.get ()),
			  args);
			 fV.exit();
			 freeVarsFun idbodys')
		      | freeVarsFun nil = ()
		in
		    freeVarsFun idbodys
		end
	      | freeVarsExp (AdjExp(_,id',id'')) =
		(fV.insert id';
		 fV.insert id'')
	      | freeVarsExp (AppExp(_,id', idargs')) =
		(fV.insert id';
		 argApp (fV.insert, idargs'))
	      | freeVarsExp (ConExp(_, id', _)) = fV.insert id'
	      | freeVarsExp (SelAppExp(_,_,id')) = fV.insert id'
	      | freeVarsExp (PrimExp (_, name)) = ()
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
	      | freeVarsTest (RecTest labids) =
		app (fn (_,id') => fV.delete id') labids
	      | freeVarsTest (TupTest labs) = app fV.delete labs
	      | freeVarsTest (LabTest(_,id')) = fV.delete id'
	      | freeVarsTest (VecTest _) = raise Error "Was'n VecTest?" (* xxx *)

	    and freeVarsDecs (decs) =
		app freeVarsDec (List.rev decs)
	end

	(* Einstiegspunkt *)
	fun genProgramCode (debug, verbose, optimize, name, program) =
	    (DEBUG := debug;
	     VERBOSE := verbose;
	     OPTIMIZE := optimize;
	     Class.setInitial name;
	     let
		 (* freie Variablen berechnen. *)
		 val _ = app freeVarsDec program
		 val _ = if !VERBOSE>=2 then FreeVars.printFun () else ()
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
				   Locals 2,
				   Register.generateVariableTable
				   [New name,
				    Dup,
				    Invokespecial (name, "<init>", ([], [Voidsig])),
				    Invokevirtual (CThread, "start", ([], [Voidsig])),
				    Return], nil)
		 (* Standardinitialisierung. Die Superklasse wird aufgerufen. *)
		 val init = Method([MPublic],"<init>",([],[Voidsig]),
				   Locals 1,
				   [Aload 0,
				    Invokespecial (CDMLThread, "<init>", ([], [Voidsig])),
				    Return], nil)
		 (* Toplevel environment is built in the run method.
		  All Objects are instantiated and initialized, function closures are built.
		  The result as well as the generated class files is stored into one single
		  pickle file. This is the last step of the compilation process. *)
		 val literalName = Class.getLiteralName()

		 val decs = decListCode program
		 val run = Method([MPublic], "run", ([], [Voidsig]),
				  Locals (Register.max()+1),
				  Multi decs ::
				  (if !VERBOSE >= 1 then
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
				  Catch.top())

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
				     nil)

		 (* Default initialization: Invocation of the super class. *)
		 val litinit = Method([MPublic],"<init>",([],[Voidsig]),
				      Locals 1,
				      Aload 0::
				      Invokespecial (CFcnClosure, "<init>", ([], [Voidsig]))::
				      (Lambda.generatePickleFn
				       [Return]),
				      nil)

		 val literale = Class([CPublic],
				      Class.getLiteralName(),
				      CFcnClosure,
				      [ISerializable],
				      Lambda.makePickleFields
				      (Literals.makefields
				       (RecordLabel.makefields ())),
				      [clinit, litinit])
	     in
		 if !VERBOSE >=2 then print "Generating main class..." else ();
		 classToJasmin (class);
		 if !VERBOSE >=2 then print "Generating literal class..." else ();
		 classToJasmin (literale);
		 if !VERBOSE >=2 then print "Okay.\n" else ()
	     end
	 )

	and valList 0 = nil
		  | valList n = Classsig CVal :: valList (n-1)

	and builtinStamp stamp' =
	    if stamp'=stamp_Match then (Getstatic CMatch,true) else
		if stamp'=stamp_false then (Getstatic CFalse,true) else
		    if stamp'=stamp_true then (Getstatic CTrue,true) else
			if stamp'=stamp_nil then (Getstatic CNil,true) else
			    if stamp'=stamp_cons then (Getstatic CCons,true) else
				if stamp'=stamp_ref then (Getstatic CRef,true) else
				    if stamp'=stamp_Bind then (Getstatic CBind,true) else
					if stamp'=parm1Stamp then (Aload 1, true) else
					    if stamp'=parm2Stamp then (Aload 2, true) else
						if stamp'=parm3Stamp then (Aload 3, true) else
						    if stamp'=parm4Stamp then (Aload 4, true) else
							if stamp'=thisStamp then (Aload 0, true) else
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
			then Register.assign(id', Register.new())
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
	    (* !! The closure is built on evaluation of the expressions *)
	    let
		(* 1st step *)
		local
		    fun init ((id',exp'),akku) =
			let
			    val loc = Register.assign(id',Register.new())
			    (* The first entry is always a OneArg.
			     We use the stamp of this OneArg for identification
			     of the function. Therefore, we ignore the other
				 args here. *)
			    fun funList ((OneArg id'',_)::_) =
				let
				    val className = classNameFromId id''
				    (* the formal parameter of a function
				     can always be found in register 1. *)
				    val _ = Register.assign(id'',1)
				    val _ = Register.assignLambda(id'', loc)
				in
				    New className ::
				    Dup ::
				    Invokespecial (className, "<init>",
						   ([],[Voidsig])) ::
				    Astore loc ::
				    nil
				end
			      | funList _ = raise
				Error "Frontend Error: OneArg expected."

			    fun specTups (id' :: rest) =
				idCode id' ::
				specTups rest
			      | specTups nil = nil

			    fun normalConAppExp (_,id',idargs) =
				New CConVal ::
				Dup ::
				idCode id' ::
				Invokespecial (CConVal, "<init>",
					       ([Classsig CConstructor],
						[Voidsig])) ::
				Dup ::
				Astore loc ::
				idArgCode
				(idargs,
				 Invokeinterface (CConVal, "setContent",
						  ([Classsig CVal],
						   [Voidsig])) ::
				 nil)

			    val one = case exp' of
				(* user defined function *)
				FunExp (_,_,idexplist) =>
				    funList idexplist

			      (* constructor application *)
			      | ConAppExp (parms as
					   (_,id', (TupArgs ids))) =>
				    (case length ids of
					 0 => [Getstatic CUnit]
				       | 2 => New CConVal2 ::
					     Dup ::
					     Multi (specTups ids) ::
					     Invokespecial
					     (CConVal2, "<init>",
					      ([Classsig CVal,
						Classsig CVal],
					       [Voidsig])) ::
					     nil
				       | 3 => New CConVal3 ::
					     Dup ::
					     Multi (specTups ids) ::
					     Invokespecial
					     (CConVal3, "<init>",
					      ([Classsig CVal,
						Classsig CVal,
						Classsig CVal],
					       [Voidsig])) ::
					     nil
				       | 4 => New CConVal4 ::
					     Dup ::
					     Multi (specTups ids) ::
					     Invokespecial
					     (CConVal4, "<init>",
					      ([Classsig CVal,
						Classsig CVal,
						Classsig CVal,
						Classsig CVal],
					       [Voidsig])) ::
					     nil
				       | _ => normalConAppExp parms)

			      | ConAppExp parms => normalConAppExp parms

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
			      | TupExp (_, ids) =>
				    (case length ids of
					 2 => New CTuple2 ::
					     Dup ::
					     Invokespecial (CTuple2,"<init>",
							    ([],[Voidsig])) ::
					     Astore loc ::
					     nil
				       | 3 => New CTuple3 ::
					     Dup ::
					     Invokespecial (CTuple3,"<init>",
							    ([],[Voidsig])) ::
					     Astore loc ::
					     nil
				       | 4 => New CTuple4 ::
					     Dup ::
					     Invokespecial (CTuple4,"<init>",
							    ([],[Voidsig])) ::
					     Astore loc ::
					     nil
				       | _ => New CTuple ::
					     Dup ::
					     Invokespecial (CTuple,"<init>",
							    ([],[Voidsig])) ::
					     Astore loc ::
					     nil)
			      | _ => raise Error
					 "Intermediate Error: illegal expression in RecDec"
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
		val loc = Register.assign(id',Register.new())
	    in
		if hasArgs then
		    [New CConstructor,
		     Dup,
		     Invokespecial (CConstructor, "<init>",
				    ([],[Voidsig])),
		     Astore loc]
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
		val ilselabel = Label.new ()
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
			     else (false, sh, switchNumber test')
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
					  | _ => raise Mitch
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
			    if LargeInt.+ (j,Int.toLarge 1) = i then
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
				 sw
			   | _ => raise Mitch)
		    end

		fun testCode (LitTest lit') =
		    (case lit' of
			 WordLit w' =>
			     stampcode' ::
			     Dup ::
			     Instanceof CWord ::
			     Ifeq ilselabel ::
			     Checkcast CWord ::
			     Getfield (CWord^"/value", [Intsig]) ::
			     atCodeWord w' ::
			     Ificmpne elselabel ::
			     nil
		       | IntLit i' =>
			     stampcode' ::
			     Dup ::
			     Instanceof CInt ::
			     Ifeq ilselabel ::
			     Checkcast CInt ::
			     Getfield (CInt^"/value", [Intsig]) ::
			     atCodeInt i' ::
			     Ificmpne elselabel ::
			     nil
		       | CharLit c' =>
			     stampcode' ::
			     Dup ::
			     Instanceof CChar ::
			     Ifeq ilselabel ::
			     Checkcast CChar ::
			     Getfield (CChar^"/value", [Charsig]) ::
			     atCodeInt (Int.toLarge (Char.ord c')) ::
			     Ificmpne elselabel ::
			     nil
		       | StringLit s' =>
			     stampcode' ::
			     Dup ::
			     Instanceof CStr ::
			     Ifeq ilselabel ::
			     Checkcast CStr ::
			     Getfield (CStr^"/value", [Classsig CString]) ::
			     Ldc (JVMString s') ::
			     Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])) ::
			     Ifeq elselabel ::
			     nil
		       | r as (RealLit r') =>
			     stampcode' ::
			     Dup ::
			     Instanceof CReal ::
			     Ifeq ilselabel ::
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
			val loc = Register.assign(id''',Register.new())
			val _ = FreeVars.setFun (id''', Lambda.top())
		    in
			stampcode' ::
			Dup ::
			Instanceof CConVal ::
			Ifeq ilselabel ::
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
				val loc = Register.assign(id'',Register.new())
				val _ = FreeVars.setFun (id'', Lambda.top())
			    in
				atCodeInt i ::
				Aaload ::
				Astore loc ::
				nil
			    end
			  | bindit ((_,id'')::rest,i) =
			    let
				val loc = Register.assign(id'',Register.new())
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
			Dup ::
			Instanceof CRecord ::
			Ifeq ilselabel ::
			Checkcast CRecord ::
			Getstatic (RecordLabel.insert
				   (stringids2strings (stringid, nil))) ::
			Invokevirtual (CRecord, "checkArity",
				       ([Arraysig, Classsig CLabel],
					[Arraysig, Classsig CVal])) ::
			Dup ::
			Ifnull ilselabel ::
			bindit (stringid,0)
		    end

		  | testCode (TupTest ids) =
		    (* compare the arity (int), then bind *)
		    let
			fun bindit (id''::rest,i) =
			    let
				val loc = Register.assign(id'',Register.new())
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
			    (if lgt >=2 andalso lgt <=4 then
				 let
				     val i0 = hd ids
				     val r0 = Register.assign
					 (i0, Register.new())
				     val _ = FreeVars.setFun (i0, Lambda.top())
				     val i1 = List.nth (ids, 1)
				     val r1 = Register.assign
					 (i1, Register.new())
				     val _ = FreeVars.setFun (i1, Lambda.top())
				 in
				     (case lgt of
					  2 => Dup ::
					      Instanceof CTuple2 ::
					      Ifeq ilselabel ::
					      Checkcast CTuple2 ::
					      Dup ::
					      Invokevirtual
					      (CTuple2, "get0",
					       ([], [Classsig CVal])) ::
					      Astore r0 ::
					      Invokevirtual
					      (CTuple2, "get1",
					       ([], [Classsig CVal])) ::
					      Astore r1 ::
					      nil
					| 3 => let
						   val i2 = List.nth (ids, 2)
						   val r2 = Register.assign
						       (i2, Register.new())
						   val _ = FreeVars.setFun (i2, Lambda.top())
					       in
						   Dup ::
						   Instanceof CTuple3 ::
						   Ifeq ilselabel ::
						   Checkcast CTuple3 ::
						   Dup ::
						   Invokevirtual
						   (CTuple3, "get0",
						    ([], [Classsig CVal])) ::
						   Astore r0 ::
						   Dup ::
						   Invokevirtual
						   (CTuple3, "get1",
						    ([], [Classsig CVal])) ::
						   Astore r1 ::
						   Invokevirtual
						   (CTuple3, "get2",
						    ([], [Classsig CVal])) ::
						   Astore r2 ::
						   nil
					       end
				    | 4 => let
						   val i2 = List.nth (ids, 2)
						   val r2 = Register.assign
						       (i2, Register.new())
						   val _ = FreeVars.setFun (i2, Lambda.top())
						   val i3 = List.nth (ids, 3)
						   val r3 = Register.assign
						       (i3, Register.new())
						   val _ = FreeVars.setFun (i3, Lambda.top())
					   in
					       Dup ::
					       Instanceof CTuple4 ::
					       Ifeq ilselabel ::
					       Checkcast CTuple4 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get0",
						([], [Classsig CVal])) ::
					       Astore r0 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get1",
						([], [Classsig CVal])) ::
					       Astore r1 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get2",
						([], [Classsig CVal])) ::
					       Astore r2 ::
					       Invokevirtual
					       (CTuple4, "get3",
						([], [Classsig CVal])) ::
					       Astore r3 ::
					       nil
					   end
				    | _ => raise Mitch)
				 end
			     else
				 Dup ::
				 Instanceof CDMLTuple ::
				 Ifeq ilselabel ::
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
				 bindit(ids,0))
		    end

		  | testCode (LabTest (s', id' as Id (_,stamp'',_))) =
		    let
			val r = Register.get stamp''
			val n = if r = 1
				    then Register.assign(id', r)
				else r
		    in
			stampcode' ::
			Dup ::
			Instanceof CDMLTuple ::
			Ifeq ilselabel ::
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
		    Label ilselabel ::
		    Pop ::
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
			val loc = Register.assign (id', Register.new())
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
			 val mp = Register.new()
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
			     Get [Comment ("FreeVars.getFun stamp' = "^Stamp.toString (FreeVars.getFun stamp')^"; Lambda.top = "^Stamp.toString (Lambda.top())),
				  Aload 0,
				  Getfield (Class.getCurrent()^"/"^
					    (fieldNameFromStamp stamp'),
					    [Classsig CVal])])
	    end
	and
	    createTuple (ids, init) =
	    let
		val arity = length ids

		fun f ((id' as Id(_,stamp',_))::rest, i, akku) =
		    f (rest,
			 i+1,
			 Dup ::
			 atCodeInt i ::
			 stampCode stamp' ::
			 Aastore ::
			 akku)
		  | f (nil, _, akku) = akku

		fun specTups (id' :: rest) =
		    idCode id' ::
		    specTups rest
		  | specTups nil = nil
	    in
		if arity <= 4 andalso arity >= 2 then
		    (case arity of
			 2 => New CTuple2 ::
			     Dup ::
			     Multi (specTups ids) ::
			     Invokespecial
			     (CTuple2, "<init>",
			      ([Classsig CVal, Classsig CVal],
			       [Voidsig])) ::
			     init
		       | 3 => New CTuple3 ::
			     Dup ::
			     Multi (specTups ids) ::
			     Invokespecial
			     (CTuple3, "<init>",
			      ([Classsig CVal,
				Classsig CVal,
				Classsig CVal],
			       [Voidsig])) ::
			     init
		       | 4 => New CTuple4 ::
			     Dup ::
			     Multi (specTups ids) ::
			     Invokespecial
			     (CTuple4, "<init>",
			      ([Classsig CVal,
				Classsig CVal,
				Classsig CVal,
				Classsig CVal],
			       [Voidsig])) ::
			     init
		       | _ => raise Mitch)
		else
		    if arity = 0 then
			Getstatic CUnit::init
		    else
			New CTuple ::
			Dup ::
			atCodeInt (Int.toLarge arity) ::
			Anewarray CVal ::
			f (ids,
			   0,
			   Invokespecial (CTuple, "<init>",
					  ([Arraysig, Classsig CVal],
					   [Voidsig])) ::
			   init)
	    end
	and
	    createRecord (labid,init) =
	    (* labids: (lab * id) list *)
	    (* 1st load Label[] *)
	    (* 2nd build Value[] *)
	    (* 3rd create Record *)
	    (* the Label[] is built statically! *)
	    let
		val arity = length labid
		(* 1st *)
		(* reverse list and remove ids *)
		fun labids2strings ((l, _)::labids',s')=
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
		Comment "[Record " ::
		New CRecord ::
		Dup ::
		Getstatic (RecordLabel.insert
			   (labids2strings (labid, nil))) ::
		atCodeInt (Int.toLarge arity) ::
		Anewarray CVal ::
		Multi (load (labid,0)) ::
		Invokespecial (CRecord,"<init>",
			       ([Arraysig, Classsig CLabel,
				 Arraysig, Classsig CVal],
				[Voidsig])) ::
		Comment "Record ]" ::
		init
	    end

	and
	    idArgCode (OneArg id', init) = idCode id' :: init

	  | idArgCode (TupArgs ids, init) = createTuple (ids, init)

	  | idArgCode (RecArgs stringids, init) =
	    createRecord (stringids, init)

	and
	    invoke (isstatic, stamp', count) =
	    let
		val name = "apply"^(if count=1 then "" else Int.toString count)
	    in
		if isstatic then
		    Invokestatic (classNameFromStamp (Lambda.getLambda stamp'),
				  "s"^name, (valList count, [Classsig CVal]))
		else
		    Invokeinterface (CVal,
				     name,
				     (valList count, [Classsig CVal]))
	    end

	and
	    loadIds nil = nil
	  | loadIds (id'::rest) =
	    idCode id' :: loadIds rest

	and
	    normalAppExp (AppExp(_,id' as Id(_,stamp',_), ida'')) =
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

	and
	    expCode (a as AppExp(_,id' as Id(_,stamp',_), TupArgs ids)) =
	    let
		val l = length ids
	    in
		if l <> 1 andalso l <=4 then
		    [Ifstatic
		     (stamp',
		      [Multi (loadIds ids),
		       invoke (true, stamp', l)],
		      [stampCode stamp',
		       Multi (loadIds ids),
		       invoke (false, stamp', l)])]
		    else normalAppExp a
	    end
	  | expCode (a as AppExp _) =
	    normalAppExp a

	  | expCode (PrimAppExp (_, name, ids)) =
		(case name of
		     "print" => [Getstatic COut,
				 idCode (hd ids),
				 Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj],[Voidsig])),
				 Getstatic CUnit]
		   | _ => (print ("PrimAppExp: "^name); nil))

	  | expCode (PrimExp (_, name)) =
		     [Getstatic (Literals.insert (StringLit name), [Classsig CStr]),
		      Invokestatic (CBuiltin, "getBuiltin",
				    ([Classsig CStr], [Classsig CVal]))]

	  | expCode (FunExp(_,_, lambda as (OneArg (id' as Id (_,stamp',_)), _):: _)) =
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
				 List.foldr loadFreeVar nil freeVarList
			 end
		     in
			 Lambda.push id';
			 Catch.push ();
			 Lambda.pushFun illegalId;
			 Label.push();
			 Register.push();
			 Class.push(className);
			 expCodeClass lambda;
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

	  | expCode (RecExp(_,labid)) = createRecord (labid, nil)

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

	  | expCode (TupExp(_,longids)) = createTuple (longids, nil)

	  | expCode (VarExp(_,id')) =
		     [idCode id']

	  | expCode (AdjExp _) = raise Error "seltsame operation adjexp"

	  | expCode (SelExp(_,lab')) =
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

	  | expCode (ConAppExp (_, id', idargs)) =
			  idCode id' ::
			  idArgCode
			  (idargs,
			   [Invokeinterface (CVal, "apply",
					    ([Classsig CVal],
					     [Classsig CVal]))])

	  | expCode (SelAppExp (_, label', id')) =
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
					 (StringLit "type error"),
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

	    expCodeClass ((OneArg id',body')::specialApplies) =
	    let
		val _ = if !VERBOSE >=1 then
		    print ("create Class "^(Class.getCurrent())^"\n")
			else ()
		val className = classNameFromId id'
		val freeVarList = FreeVars.getVars id'
		(* generate fields for the free variables of the function *)

		fun fields (stamp'::stamps) =
		    (Field ([FPublic],fieldNameFromStamp stamp', [Classsig CVal]))::
		    (fields stamps)
		  | fields nil = nil

		val fieldscode = fields freeVarList

		fun createApplies ((t as TupArgs ids, body'')::rest,
				   parm, a0, a2, a3, a4, ra) =
		    let
			val l = length ids
		    in
			case l of
			    0 => (Catch.push ();
				  createApplies
				  (rest, parm,
				   (decListCode body'', Catch.pop ()),
				   a2, a3, a4, ra))
			  | 2 => (Catch.push ();
				  Register.assignParms (ids,1);
				  createApplies
				  (rest, parm, a0,
				   (decListCode body'', Catch.pop ()),
				   a3, a4, ra))
			  | 3 => (Catch.push ();
				  Register.assignParms (ids,1);
				  createApplies
				  (rest, parm, a0, a2,
				   (decListCode body'', Catch.pop ()),
				   a4, ra))
			  | 4 => (Catch.push ();
				  Register.assignParms (ids,1);
				  createApplies
				  (rest, parm, a0, a2, a3,
				   (decListCode body'', Catch.pop ()), ra))
			  | _ => createApplies
				(rest, parm, a0, a2, a3, a4,
				 [TestStm (dummyCoord,
					   Id (dummyPos, parm, InId),
					   TupTest ids, body'', ra)])
		    end
		  | createApplies ((t as RecArgs labids, body'') :: rest,
				   parm, a0, a2, a3, a4, ra) =
		    createApplies (rest, parm, a0, a2, a3, a4,
				   [TestStm (dummyCoord, Id (dummyPos, parm, InId),
					     RecTest labids,
					     body'', ra)])
		  | createApplies (nil, parm, a0, a2, a3, a4, ra) =
		    (Catch.push ();
		     (a0, a2, a3, a4, (decListCode ra, Catch.pop ())))
		  | createApplies (_, _, _, _, _, _, _) = raise Mitch

		val ((ap0,ex0), (ap2,ex2), (ap3,ex3), (ap4,ex4), (ad, exd)) =
		    createApplies (List.rev specialApplies, stampFromId id',
				   (nil,nil), (nil,nil), (nil,nil), (nil,nil), body')

		fun buildSpecialApply (count, spec, id'') =
		    case spec of
			nil => Nop
		      | _ => let
				 val elselabel = Label.new ()
				 val tupNo = CTuple^(Int.toString count)
				 val r = Register.new ()
				 fun gets (act, to') =
				     Invokevirtual
				     (tupNo, "get"^Int.toString act,
				      ([], [Classsig CVal])) ::
				     (if act = to' then
					  nil
				      else
					  Aload r ::
					  gets (act+1, to'))
			     in
				 Multi
				 (idCode id'' ::
				  Instanceof tupNo ::
				  Ifeq elselabel ::
				  Ifstatic (stampFromId id'',
					    [Nop],
					    [Aload 0]) ::
				  idCode id'' ::
				  Checkcast tupNo ::
				  Dup ::
				  Astore r ::
				  Multi (gets (0, count-1)) ::
				  Ifstatic
				  (stampFromId id'',
				   [Invokestatic
				    (classNameFromId id'',
				     "sapply"^Int.toString count,
				     ([Classsig CVal, Classsig CVal],
				      [Classsig CVal]))],
				   [Invokevirtual
				    (classNameFromId id'',
				     "apply"^Int.toString count,
				     ([Classsig CVal, Classsig CVal],
				      [Classsig CVal]))]) ::
				  normalReturn
				  [Areturn,
				   Label elselabel])
			  end

		val defaultApply =
		    (case ap0 of
			 nil => Nop
		       | _ => let
				  val elselabel=Label.new()
			      in
				  Multi (idCode id' ::
					 Getstatic CUnit ::
					 Ifacmpne elselabel ::
					 Multi ap0 ::
					 normalReturn
					 [Areturn,
					  Label elselabel])
			      end) ::
			 buildSpecialApply(2, ap2, id') ::
			 buildSpecialApply(3, ap3, id') ::
			 buildSpecialApply(4, ap4, id') ::
			 ad

		fun addDebugInfo d =
		    if !DEBUG >=2 then
			let
			    val nodebug=Label.new()
			in
			    Getstatic
			    (Class.getCurrent()^"/DEBUG",
			     [Boolsig]) ::
			    Ifeq nodebug ::
			    Getstatic
			    ("java/lang/System/out",
			     [Classsig "java/io/PrintStream"]) ::
			    Ldc (JVMString "Enter: (") ::
			    Invokevirtual
			    ("java/io/PrintStream",
			     "print",([Classsig CObj],
				      [Voidsig])) ::
			    Getstatic
			    ("java/lang/System/out",
			     [Classsig "java/io/PrintStream"]) ::
			    Ldc (JVMString
				 (nameFromId
				  (Lambda.getOuterFun ()))) ::
			    Invokevirtual
			    ("java/io/PrintStream",
			     "println",([Classsig CObj],
					[Voidsig])) ::
			    Comment "expCodeClass: Label nodebug" ::
			    Label nodebug::
			    d
			end
		    else d

		(* Now generate the code for the main apply function *)
		val ap =
		    (Label alpha ::
		     Multi
		     (addDebugInfo defaultApply) ::
		     normalReturn
		     [Label omega,
		      Areturn])

		fun makeApplyMethod (modifiers, name, vals, insts, handles) =
		    Method (MPublic :: modifiers,
			    name,
			    (valList vals, [Classsig CVal]),
			    Locals (Register.max() +vals),
			    (case insts of
				 nil =>
				     Multi
				     (normalAppExp
				      (AppExp (dummyPos, thisId,
					       TupArgs (Vector.sub(parmIds,vals))))) ::
				     normalReturn [Areturn]
			       | _ => insts),
				 handles)

		fun parmLoad (0, akku) = akku
		  | parmLoad (n, akku) = parmLoad (n-1, Aload n :: akku)

		fun makeDummyApply number =
		    let
			val name = "apply" ^
			    (case number of
				 1 => ""
			       | _ => Int.toString number)
		    in
			makeApplyMethod (nil,name,number,
					 parmLoad
					 (number,
					  Invokestatic (className,
							"s"^name,
							(valList number,
							 [Classsig CVal])) ::
					  normalReturn [Areturn]),
					 [])
		    end

		(* normal apply methods *)
		val (applY, apply0, apply2, apply3, apply4) =
		    if Lambda.sapplyPossible () then
			(makeDummyApply 1,
			 makeDummyApply 0,
			 makeDummyApply 2,
			 makeDummyApply 3,
			 makeDummyApply 4)
		    else
			(makeApplyMethod (nil,"apply",1,ap,Catch.top()),
			 makeApplyMethod (nil,"apply0",0,ap0,ex0),
			 makeApplyMethod (nil,"apply2",2,ap2,ex2),
			 makeApplyMethod (nil,"apply3",3,ap3,ex3),
			 makeApplyMethod (nil,"apply4",4,ap4,ex4))

		(* static apply methods. Generated only if there are
		 no free variables in the function *)
		val sapplY = makeApplyMethod ([MStatic],"sapply",1,ap,Catch.top())
		val sapply0 = makeApplyMethod ([MStatic],"sapply0",0,ap0,ex0)
		val sapply2 = makeApplyMethod ([MStatic],"sapply2",2,ap2,ex2)
		val sapply3 = makeApplyMethod ([MStatic],"sapply3",3,ap3,ex3)
		val sapply4 = makeApplyMethod ([MStatic],"sapply4",4,ap4,ex4)

		(* standard constructor *)
		val init = Method ([MPublic],"<init>",([], [Voidsig]), Locals 1,
				   [Aload 0,
				    Invokespecial (CFcnClosure, "<init>",
						   ([], [Voidsig])),
				    Return],
				   nil)

		(* the whole class *)
		val class = Class([CPublic],
				  className,
				  CFcnClosure,
				  nil,
				  fieldscode,
				  (init:: applY:: apply0:: apply2:: apply3:: apply4::
				   (if Lambda.sapplyPossible () then
					[sapplY, sapply0, sapply2, sapply3,
					 sapply4]
				    else [])))
	    in
		classToJasmin (class)
	    end
	  | expCodeClass _ =
	    raise Error "Error in intermediate: FunExp should begin with OneArg."
	and
	    compile prog = genProgramCode (0,0,2,"Emil", imperatifyString prog)
	and
	    compileFile (f, optimize) = genProgramCode (0,0,optimize,"Emil", imperatifyFile f)
    end
