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

	(* constant propagation *)
	fun constPropDec (ValDec (_, Id (_,stamp', _), exp', _)) =
	    ConstProp.add (stamp', constPropExp exp')
	  | constPropDec (RecDec (_, idexps, _)) =
	    List.app
	    (fn (Id (_, stamp', _), exp') => ConstProp.add (stamp', constPropExp exp'))
	    idexps
	  | constPropDec (EvalStm (_, exp')) = (constPropExp exp'; ())
	  | constPropDec (HandleStm (_, body', _, body'', body''', shared)) =
	    if !shared = ~1 then () else
		(shared := ~1;
		 List.app constPropDec body';
		 List.app constPropDec body'';
		 List.app constPropDec body''')
	  | constPropDec (TestStm (_, _, _, body', body'')) =
		(List.app constPropDec body';
		 List.app constPropDec body'')
	  | constPropDec (ReturnStm (_, exp')) = (constPropExp exp'; ())
	  | constPropDec (IndirectStm (_, ref (SOME body'))) = List.app constPropDec body'
	  | constPropDec (SharedStm (_,body',shared')) =
		if !shared' = ~1 then () else
		    (shared' := ~1;
		     List.app constPropDec body')
	  | constPropDec _ = ()
	and
	    constPropExp (f as FunExp (_, _, _, _, body)) =
	    (List.app constPropDec body; f)
	  | constPropExp x = x

	(* compute destination classes *)
	fun destClassExp (FunExp (_, thisFun, _, args, body)) =
	    (vprint (3, "computing destclasses for "^Stamp.toString thisFun^" ...\n");
	     Lambda.getClassStamp (thisFun, Lambda.argSize args);
	     destClassDecs body;
	     vprint (3, "destclasses for "^Stamp.toString thisFun^" done.\n"))
	  | destClassExp _ = ()
	and
	    destClassDec (HandleStm(_,body',id',body'', body''', shared)) =
	    if !shared = ~2 then () else
		(shared:= ~2;
		 destClassDecs body''';
		 destClassDecs body'';
		 destClassDecs body')
	  | destClassDec (TestStm(_,id',test',body',body'')) =
	    (destClassDecs body'';
	     destClassDecs body')
	  | destClassDec (SharedStm(_,body', shared')) =
	    if !shared' = ~2 then () else
		 (shared' := ~2;
		 destClassDecs body')
	  | destClassDec (ValDec(_, id', exp', _)) = destClassExp exp'
	  | destClassDec (RecDec(_,idsexps, _)) =
	    (Lambda.insertRec idsexps;
	     app
	     (fn (id', exp') => destClassExp exp')
	     idsexps)
	  | destClassDec (EvalStm(_, exp')) = destClassExp exp'
	  | destClassDec (ReturnStm(_,exp')) = destClassExp exp'
	  | destClassDec (IndirectStm (_, ref (SOME body'))) = destClassDecs body'
	  | destClassDec _ = ()

	and destClassDecs decs =
	    app destClassDec decs

	(* apply f to all stamps of an arg. *)
	fun argApp (f, OneArg id', free, curFun, dbg) =
	    f (free, id', curFun, dbg)
	  | argApp (f, TupArgs ids, free, curFun, dbg) =
	    app (fn id' => f (free, id', curFun, dbg)) ids
	  | argApp (f, RecArgs labids, free, curFun, dbg) =
	    app (fn (_, id') => f (free, id', curFun, dbg)) labids

	(* mark a variable as free *)
	fun markfree (free, id' as Id (_,stamp', _), curFun, dbg) =
	    (StampSet.insert (free, stamp');
	     vprint (2, "markfree in "^Stamp.toString curFun^" ("^dbg^"): "^Stamp.toString stamp'^"\n"))

	(* mark a variable as bound *)
	fun markbound (free, id' as Id (_,stamp', _), curFun, dbg) =
	    (StampSet.delete (free, stamp');
	     vprint (2, "markbound in "^Stamp.toString curFun^" ("^dbg^"): "^Stamp.toString stamp'^"\n");
	     FreeVars.setFun (id', curFun))

	(* compute free variables of expressions *)
	fun freeVarsExp (LitExp _, _, _) = ()
	  | freeVarsExp (VarExp (_, id' as Id (_, stamp', _)), free, curFun) =
	    (case ConstProp.get stamp' of
		 NONE => markfree(free, id', curFun, "VarExp")
	       | SOME exp' => freeVarsExp (exp', free, curFun))
	  | freeVarsExp (NewExp _, _, _) = ()
	  | freeVarsExp (ConAppExp (_, id', idargs, _), free, curFun) =
	    (markfree (free, id', curFun, "ConAppExp");
	     argApp (markfree, idargs, free, curFun, "ConAppExp2"))
	  | freeVarsExp (RefAppExp (_, idargs), free, curFun) =
	    argApp (markfree, idargs, free, curFun, "RefAppExp")
	  | freeVarsExp (TupExp (_,ids), free, curFun) =
	    app (fn id' => markfree (free, id', curFun, "TupExp")) ids
	  | freeVarsExp (RecExp (_,labids), free, curFun) =
	    app
	    (fn (lab, id') => markfree (free, id', curFun, "RecExp"))
	    labids
	  | freeVarsExp (SelExp _, _, _) = ()
	  | freeVarsExp (AdjExp(_,id',id''), free, curFun) =
	    (markfree (free, id', curFun, "AdjExp");
	     markfree (free, id'', curFun, "AdjExp2"))
	  | freeVarsExp (AppExp(_,Id (_, stamp',_), idargs'), free, curFun) =
	    let
		val l' = Lambda.getLambda stamp'
		val as' = Lambda.argSize idargs'
		val cs' = Lambda.getClassStamp (l', as')
		val gi' = Lambda.getId cs'
	    in
		vprint (1, "AppExp: Appliziere "^Stamp.toString stamp'^" = Lambda "^
			Stamp.toString l'^" mit "^Int.toString as'^" Argumenten aus Klasse "^
			Stamp.toString cs'^", id = "^Stamp.toString(stampFromId gi')^".\n");
		markfree (free, gi', curFun, "AppExp");
		argApp (markfree, idargs', free, curFun, "AppExp2")
	    end
	  | freeVarsExp (ConExp(_, id', _), free, curFun) =
	    markfree (free, id', curFun, "ConExp")
	  | freeVarsExp (RefExp _, _, _) = ()
	  | freeVarsExp (SelAppExp(_,_,id'), free, curFun) =
	    markfree (free, id', curFun, "SelAppExp")
	  | freeVarsExp (PrimExp (_, name), _, _) = ()
	  | freeVarsExp (FunExp (_, thisFun, _, args, body), free, curFun) =
	    let
		val newfree = StampSet.new ()
		val destClass =
		    Lambda.getClassStamp (thisFun, Lambda.argSize args)
	    in
		freeVarsDecs (body, newfree, destClass);
		argApp (markbound, args, newfree, destClass, "freeVarsFun");
		vprint (2, "Fun "^Stamp.toString thisFun^", Class "^Stamp.toString destClass^". Adding FreeVars ");
		StampSet.app (fn x => vprint (2, Stamp.toString x^" ")) newfree;
		vprint (2, ".\n");
		FreeVars.addVars (destClass, newfree);
		StampSet.app (fn x => StampSet.insert (free,x)) newfree;
		vprint (2,"FunExp: ");
		FreeVars.setFun (Id (dummyIdInfo, thisFun, Name.InId), curFun)
	    end
	  | freeVarsExp (PrimAppExp (_, _, ids), free, curFun) =
	    List.app
	    (fn id' => markfree (free, id', curFun, "PrimAppExp"))
	    ids
	  | freeVarsExp (VecExp (_, ids), free, curFun) =
	    List.app
	    (fn id' => markfree (free, id', curFun, "VecExp"))
	    ids

	and freeVarsDec (RaiseStm(_,id'), free, curFun) =
	    markfree (free, id', curFun, "RaiseStm")
	  | freeVarsDec (ReraiseStm(_,id'), free, curFun) =
	    markfree (free, id', curFun, "ReraiseStm")
	  | freeVarsDec (HandleStm(_,body',id',body'', body''', shared), free, curFun) =
	    if !shared = ~3 then () else
		(shared :=  ~3;
		 freeVarsDecs (body''', free, curFun);
		 freeVarsDecs (body'', free, curFun);
		 freeVarsDecs (body', free, curFun);
		 markbound (free, id', curFun, "HandleStm"))
	  | freeVarsDec (EndHandleStm _, _, _) = ()
	  | freeVarsDec (TestStm(_,id',test',body',body''), free, curFun) =
	    (freeVarsDecs (body'', free, curFun);
	     freeVarsDecs (body', free, curFun);
	     freeVarsTest (test', free, curFun);
	     markfree (free, id', curFun, "TestStm"))
	  | freeVarsDec (SharedStm(_,body', shared'), free, curFun) =
	    if !shared' = ~3 then () else
		(shared' := ~3;
		 freeVarsDecs (body', free, curFun))
	  | freeVarsDec (ValDec(_, id', exp', _), free, curFun) =
	    (freeVarsExp (exp', free, curFun);
	     markbound (free, id', curFun, "ValDec"))
	  | freeVarsDec (RecDec(_,idsexps, _), free, curFun) =
	    (app (fn (_, exp') => freeVarsExp (exp', free, curFun)) idsexps;
	     app
	     (fn (Id (_, stamp', _), _) =>
	      let
		  val l' = Lambda.getLambda stamp'
		  val cs' = Lambda.getClassStamp (l', 1)
		  val gi' = Lambda.getId cs'
	      in
		  vprint (1, "RecDec: definiere "^Stamp.toString stamp'^" = Lambda "^
			  Stamp.toString l'^" mit 1 Argument aus Klasse "^
			  Stamp.toString cs'^", id = "^Stamp.toString(stampFromId gi')^".\n");
		  markbound (free, gi', curFun, "RecDec")
	      end)
	    idsexps)
	  | freeVarsDec (EvalStm(_, exp'), free, curFun) =
	    freeVarsExp (exp', free, curFun)
	  | freeVarsDec (ReturnStm(_,exp'), free, curFun) =
	    freeVarsExp (exp', free, curFun)
	  | freeVarsDec (ExportStm _, _, _) = ()
	  | freeVarsDec (IndirectStm (_, ref (SOME body')), free, curFun) =
	    freeVarsDecs (body', free, curFun)
	  | freeVarsDec (IndirectStm (_, ref NONE), _, _) = ()
	and
	    freeVarsTest (LitTest _, _, _) = ()
	  | freeVarsTest (ConTest(id',NONE,_), free, curFun) =
	    markfree (free, id', curFun, "ConTest none")
	  | freeVarsTest (ConTest(id',SOME id'',_), free, curFun) =
	    (markfree (free, id', curFun, "ConTest some");
	     markbound (free, id'', curFun, "ConTest some2"))
	  | freeVarsTest (RefTest id'', free, curFun) =
	    markbound (free, id'', curFun, "RefTest")
	  | freeVarsTest (RecTest labids, free, curFun) =
	    app (fn (_,id') => markbound (free, id', curFun, "RecTest")) labids
	  | freeVarsTest (TupTest labs, free, curFun) =
	    app (fn id' => markbound (free, id', curFun, "TupTest")) labs
	  | freeVarsTest (LabTest(_,id'), free, curFun) = markbound (free, id', curFun, "LabTest")
	  | freeVarsTest (VecTest labs, free, curFun) =
	    app (fn id' => markbound (free, id', curFun, "VecTest")) labs

	and freeVarsDecs (decs, free, curFun) =
	    app (fn dec' => freeVarsDec (dec', free, curFun)) (List.rev decs)

	(* check whether a stamp belongs to a builtin function
	 * and load a static field if true
	 *)
	fun builtinStamp (stamp', curFun) =
	    if stamp'=valstamp_match then (Getstatic BMatch,true)
	    else if stamp'=valstamp_false then (Getstatic BFalse,true)
	    else if stamp'=valstamp_true then (Getstatic BTrue,true)
	    else if stamp'=valstamp_nil then (Getstatic BNil,true)
	    else if stamp'=valstamp_cons then (Getstatic BCons,true)
	    else if stamp'=valstamp_ref then (Getstatic BRef,true)
	    else if stamp'=valstamp_bind then (Getstatic BBind,true)
	    else if stamp'=thisStamp
		orelse stamp'=parm1Stamp
		orelse stamp'=parm2Stamp
		orelse stamp'=parm3Stamp
		orelse stamp'=parm4Stamp
		orelse stamp'=parm5Stamp
	    then (Aload stamp', true)
	    else
		let
		    val parm = Lambda.getParmStamp(curFun, stamp')
		in
		    if parm <> stamp'
			then (Aload parm, true)
		    else (Nop,false)
		end

	(* instantiate a class *)
	fun create (classname, init) =
	    New classname ::
	    Dup ::
	    Invokespecial (classname, "<init>",
			   ([], [Voidsig])) ::
	    init

	(* create a new instance or load it from a register *)
	fun createOrLoad (NONE, classname) =
	    Multi (create (classname, [Dup]))
	  | createOrLoad (SOME (Id (_,stamp'',_)), _) = Aload stamp''

	fun lineRegion ((line, _), _) = line

	(* entry point *)
	fun genComponentCode (debug, verbose, optimize, lmaa, lines, wait, name, (components, (program, _))) =
	    let
		fun loadComponents ((Id (_, stamp', _), _, url')::rest, akku) =
		    loadComponents
		    (rest,
		     Getstatic BImport ::
		     New CStr ::
		     Dup ::
		     Ldc (JVMString (Url.toString url')) ::
		     Invokespecial (CStr, "<init>", ([Classsig CString], [Voidsig])) ::
		     Invokeinterface MApply ::
		     Astore stamp' ::
		     akku)
		  | loadComponents (nil, akku) = akku
	    in
		DEBUG := debug;
		VERBOSE := verbose;
		OPTIMIZE := optimize;
		LINES := lines;
		LMAA := lmaa;
		WAIT := wait;
		Class.setInitial name;
		vprint (2,"thisStamp: "^Stamp.toString thisStamp^"\n");
		vprint (2, "parm1Stamp: "^Stamp.toString parm1Stamp^"\n");
		vprint (2, "parm2Stamp: "^Stamp.toString parm2Stamp^"\n");
		vprint (2, "parm3Stamp: "^Stamp.toString parm3Stamp^"\n");
		vprint (2, "parm4Stamp: "^Stamp.toString parm4Stamp^"\n");
		vprint (2, "parm5Stamp: "^Stamp.toString parm5Stamp^"\n");
		vprint (2, "toplevel: "^Stamp.toString toplevel^"\n");
		vprint (2, "illegalStamp: "^Stamp.toString illegalStamp^"\n");
		let
		    (* do constant propagation *)
		    val _ = (vprint (3, "constant propagation ... ");
			     if !OPTIMIZE >= 3 then List.app constPropDec program else ();
			     vprint (3, "done\n"))
		    (* compute free variables and destination classes. *)
		    val _ = let
				    val free = StampSet.new ()
				in
				    destClassDecs program;
				    app (fn dec' => freeVarsDec (dec', free, toplevel))
				    program;
				    app (fn (id',_,_) =>
					 markfree (free,id', toplevel, "genComponentCode"))
				    components
				end

			val main = Method([MStatic,MPublic],"main",([Arraysig, Classsig CString],[Voidsig]),
					  create (Class.getInitial (),
						  [Invokevirtual (CThread, "start", ([], [Voidsig])),
						   Return]))
			(* Default initialisation. *)
			val init = Method([MPublic],"<init>",([],[Voidsig]),
					  [Aload thisStamp,
					   Invokespecial (CThread, "<init>", ([], [Voidsig])),
					   Return])
			(* Toplevel environment is built in the run method.
			 All Objects are instantiated and initialized, function closures are built.
			 The result as well as the generated class files is stored into one single
			 pickle file. This is the last step of the compilation process. *)
			val decs = decListCode (program, toplevel, toplevel)

			val run = Method([MPublic], "run", ([], [Voidsig]),
					 (loadComponents
					  (components,
					   [Multi (Lambda.generatePickleFn
						   (Literals.generate,
						    toplevel,
						    RecordLabel.generate ())),
					    Multi decs,
					    Return])))

			val clinit = Method([MPublic],
					    "<clinit>",
					    ([],[Voidsig]),
					    [Return])

			(* The main class *)
			val class = Class([CPublic],
					  Class.getInitial (),
					  CThread,
					  nil,
					  Literals.makefields
					  (toplevel, RecordLabel.makefields
					   (toplevel, nil)),
					  [clinit, main, init, run])
		    in
			vprint (2, "Generating main class...");
			classToJasmin (class);
			vprint (2, "Okay.\n")
		    end;
		    if (!VERBOSE >= 1) then (Lambda.showRecApplies ();
					     JLabel.printStackTrace ())
		    else ()
	    end

	and decListCode (dec::rest, curFun, curCls) =
	    Multi (decCode (dec, curFun, curCls)) ::
	    decListCode (rest, curFun, curCls)
	  | decListCode (nil, _, _) = nil

	(* Code generation for declarations *)
	and decCode (ValDec(info', id',
			    exp' as FunExp (_, thisFun, _, _, _),_), curFun, curCls) =
	    (Lambda.setId (thisFun, id');
	     [Line (lineRegion (#region info')),
	      Multi (expCode (exp', curFun, curCls)),
	      Astore thisFun])
	  | decCode (ValDec(info', Id (_,stamp',_),
			    ConAppExp (_,id'',idargs,_),_), curFun, curCls) =
	    Line (lineRegion (#region info')) ::
	    createConVal (SOME stamp', id'', idargs, curFun, curCls)
	  | decCode (ValDec(info', Id (_,stamp',_), exp',_), curFun, curCls) =
	    [Line (lineRegion (#region info')),
	     Multi (expCode (exp', curFun, curCls)),
	     Astore stamp']

	  | decCode (RecDec (_, nil, _), _, _) = nil
	  | decCode (RecDec (info,idexps as ((recid,_)::rest),_), curFun, curCls) =
	    (* RecDec of stm_info * (id * exp) list * isTopLevel *)
	    (* 1. create a new object for each id and store it into a new register. *)
	    (* 2. fill the empty FunExp closures *)
	    let
		fun emptyClosure ((Id (_,stamp',_),exp'),akku) =
		    case exp' of
			(* user defined function *)
			FunExp (_,thisFun,_,_,_) =>
			    create (classNameFromStamp thisFun,
				    Astore thisFun :: akku)
		      (* constructed value *)
		      | ConAppExp (_,id'',idargs,_) =>
			    Multi (createConVal (SOME stamp', id'', idargs, curFun, curCls)) ::
			    akku
		      (* record *)
		      | RecExp (_,idargs) =>
			    newRecord(idargs,
				      Astore stamp' ::
				      akku,
				      curCls)
		      | RefAppExp _ =>
			    create (CReference,
				    Astore stamp' ::
				    akku)
		      (* tuple *)
		      | TupExp (_, ids) =>
			    create (cTuple (length ids),
				    Astore stamp' ::
				    akku)
		      (* vector *)
		      | VecExp (_, ids) =>
			    create (CVector,
				    Astore stamp' ::
				    akku)
		      | exp' =>
			    Multi (expCode (exp', curFun, curCls)) ::
			    Astore stamp' ::
			    akku

		fun fillClosure ((_,FunExp(_, thisFun, _, args, body)), akku) =
		    (*--** there may be no OneArg *)
		    Multi (createFun (thisFun, [(args, body)],
				      curFun, curCls, false)) ::
		    akku
		  | fillClosure ((id'', RecExp (_, labid)), akku) =
		    createRecord (SOME id'', labid, akku, curFun, curCls)
		  | fillClosure ((id'', TupExp (_, ids)), akku) =
		    createTuple (SOME id'', ids, akku, curFun, curCls)
		  | fillClosure ((id'', VecExp (_, ids)), akku) =
		    createVector (SOME id'', ids, akku, curFun, curCls)
		  | fillClosure ((id'', RefAppExp (_, ids)), akku) =
		    createRefAppExp (SOME id'', ids, akku, curFun, curCls)
		  | fillClosure (_,akku) = akku

	    in
		Line (lineRegion (#region info)) ::
		(* 1st step *)
		(List.foldr
		 emptyClosure
		 (* 2nd step *)
		 (List.foldr fillClosure nil idexps)
		 idexps)
	    end

	  | decCode (RaiseStm(info',id'), curFun, curCls) =
	    [Line (lineRegion (#region info')),
	     New CExWrap,
	     Dup,
	     idCode (id', curFun, curCls),
	     Invokespecial(CExWrap,"<init>",
			   ([Classsig IVal],[Voidsig])),
	     Athrow]

	  | decCode (ReraiseStm(info',id'), curFun, curCls) =
	    [Line (lineRegion (#region info')),
	     New CExWrap,
	     Dup,
	     idCode (id', curFun, curCls),
	     Invokespecial(CExWrap,"<init>",
			   ([Classsig IVal],[Voidsig])),
	     Athrow]

	  | decCode (TestStm(info',id' as Id (_,stamp',_),test',body',body''), curFun, curCls) =
		 (* test whether id' matches with test'.
		  If so, eval body', if not, eval body'' *)
	    let
		val retry = JLabel.new ()
		val danach = JLabel.new ()
		val elselabel = JLabel.new ()
		val wrongclasslabel = JLabel.new ()
		val popelselabel = JLabel.new ()
		val stampcode' = idCode (id', curFun, curCls)

		(* only test for transients after the last pattern failed. *)
		fun notfinished (SharedStm (_,s,_)::_) = notfinished s
		  | notfinished (TestStm (_, Id (_,nextStamp,_),_,_,_)::_) =
		    let
			val x = nextStamp = stamp'
		    in
			if x then vprint (3, "Transients: NOT finished at "^
					  Stamp.toString nextStamp^"\n") else
			    vprint (3, "Transients: finished at "^Stamp.toString stamp'^
				    "/"^Stamp.toString nextStamp^"\n");
			x
		    end
		  | notfinished (IndirectStm (_, ref NONE)::rest) = notfinished rest
		  | notfinished (IndirectStm (_, ref (SOME i))::_) = notfinished i
		  | notfinished _ = false

		val _ = JLabel.newRetry (retry, stamp')

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
				val lab = JLabel.new ()
				val (switch, inst, number) =
				    checkForSwitch (test', body'', begin)
			    in
				if switch
				    then
					case inst of
					    TestStm (info', _, t'',b',b'')
					    => generateBody
					    (Line (lineRegion (#region info')) ::
					     Label lab ::
					     Multi (decListCode (body', curFun, curCls)) ::
					     akku,
					     number::switchlist,
					     lab :: labelList,
					     b', t'', b'')
					  | _ => raise (Crash.Crash "CodeGen: generateBody")
				else
				    (Label lab ::
				     Multi (decListCode (body', curFun, curCls)) ::
				     Multi akku ::
				     Label wrongclasslabel ::
				     (if notfinished body'' then
					  Nop
				      else
					  Multi
					  [Instanceof ITransient,
					   Ifeq elselabel,
					   stampcode',
					   Checkcast ITransient,
					   Invokeinterface MRequest,
					   Dup,
					   storeCode (stamp', curFun, curCls),
					   Goto (JLabel.popRetry ())]) ::
				     Label popelselabel ::
				     Pop ::
				     Label elselabel ::
				     decListCode (body'', curFun, curCls),
				     number::switchlist,
				     lab :: labelList)
			    end

			fun isTableSwitch (i::(rest as (j::_))) =
			    if LargeInt.+ (j,Int.toLarge 1) = i then
				isTableSwitch rest
			    else (false, 0)
			  | isTableSwitch (i::nil) = (true, i)
			  | isTableSwitch nil = (false, 0)

			fun makeSwitch (bod, switchlist, labellist) =
			    (case isTableSwitch switchlist of
				 (true, i) => Tableswitch (i, labellist, elselabel)
			       | _ => Lookupswitch ((switchlist, labellist), elselabel)) ::
			     bod

			val gb as (_, _, _) = generateBody (nil, nil, nil, body', test', body'')

			fun litTest (cls, ret) =
			    Dup ::
			    Instanceof cls ::
			    Ifeq wrongclasslabel ::
			    Checkcast cls ::
			    Getfield (cls^"/value", ret) ::
			    makeSwitch gb
		    in
			Label begin ::
			stampcode' ::
			Label retry ::
			(case test' of
			     LitTest (WordLit _) =>
				 litTest (CWord, [Intsig])
			   | LitTest (IntLit startwert) =>
				 litTest (CInt, [Intsig])
			   | LitTest (CharLit startwert) =>
				 litTest (CChar, [Charsig])
			   | _ => raise (Crash.Crash "CodeGen: GenerateSwitch"))
		    end

		fun tcode (cls, ret, cmpcode) =
		    Dup ::
		    Instanceof cls ::
		    Ifeq wrongclasslabel ::
		    Checkcast cls ::
		    Getfield (cls^"/value", ret) ::
		    cmpcode

		fun bindit (Id (_,stamp'',_)::rest,i) =
		    let
			val b' = atCodeInt i ::
			    Aaload ::
			    Astore stamp'' ::
			    bindit(rest,i+1)
		    in
			case rest of
			    nil => b'
			  | _ => Dup :: b'
		    end
		  | bindit (nil,_) = nil

		fun testCode (LitTest lit') =
		    (case lit' of
			 WordLit w' =>
			     tcode (CWord, [Intsig],
				    [atCodeWord w',
				     Ificmpne elselabel])
		       | IntLit i' =>
			     tcode (CInt, [Intsig],
				    [atCodeInt i',
				     Ificmpne elselabel])
		       | CharLit c' =>
			     tcode (CChar, [Charsig],
				    [atCodeInt (Int.toLarge (Char.ord c')),
				     Ificmpne elselabel])
		       | StringLit s' =>
			     tcode (CStr, [Classsig CString],
				    [Ldc (JVMString s'),
				     Invokevirtual MEquals,
				     Ifeq elselabel])
		       | r as (RealLit r') =>
			     tcode (CReal, [Floatsig],
				    [atCode r,
				     Fcmpl,
				     Ifne elselabel]))

		  | testCode (ConTest (id'',NONE,_)) =
			 [idCode (id'', curFun, curCls),
			  Ifacmpne elselabel]

		  | testCode (ConTest (id'' as Id (_, stamp'', _),SOME (Id (_,stamp''',_)),_)) =
			 [Dup,
			  Multi (if stamp'' = valstamp_cons then
				     [Instanceof CCons,
				      Ifeq wrongclasslabel]
				 else
				     [Instanceof IConVal,
				      Ifeq wrongclasslabel,
				      Checkcast IConVal,
				      Invokeinterface (IConVal, "getConstructor",
						       ([], [Classsig CConstructor])),
				      idCode (id'', curFun, curCls),
				      Ifacmpne elselabel,
				      stampcode']),
			  Checkcast IConVal,
			  Invokeinterface (IConVal, "getContent",
					   ([],[Classsig IVal])),
			  Astore stamp''']

		  | testCode (RefTest (Id (_,stamp''',_))) =
			 [Dup,
			  Instanceof CReference,
			  Ifeq wrongclasslabel,
			  Checkcast CReference,
			  Invokevirtual (CReference, "getContent",
					 ([],[Classsig IVal])),
			  Astore stamp''']

		  | testCode (RecTest stringid) =
			 (* Load arity, compare, then bind *)
		    let
			(* reverse the list and remove ids *)
			fun labids2strings ((l, _)::stringids',s')=
			    labids2strings (stringids', Label.toString l::s')
			  | labids2strings (nil, s') = s'
			fun bindrec ((_,Id (_,stamp'',_))::nil,i) =
			    [atCodeInt i,
			     Aaload,
			     Astore stamp'']
			  | bindrec ((_,Id (_,stamp'',_))::rest,i) =
			    Dup ::
			    atCodeInt i ::
			    Aaload ::
			    Astore stamp'' ::
			    bindrec(rest,i+1)
			  | bindrec (nil,_) = nil
		    in
			Dup ::
			Instanceof CRecord ::
			Ifeq wrongclasslabel ::
			Checkcast CRecord ::
			Getstatic (RecordLabel.insert
				   (curCls, labids2strings (stringid, nil))) ::
			Invokevirtual (CRecord, "checkArity",
				       ([Arraysig, Classsig CString],
					[Arraysig, Classsig IVal])) ::
			Dup ::
			Ifnull popelselabel ::
			bindrec (stringid,0)
		    end

		  | testCode (TupTest ids) =
		    (* compare the arity (int), then bind *)
		    let
			val lgt = length ids
		    in
			if lgt = 0
			    then
				[Getstatic BUnit,
				 Ifacmpne elselabel]
			else
			    (if lgt >=2 andalso lgt <=4 then
				 let
				     val thisTup = cTuple lgt

				     fun specBind (~1, akku) = akku
				       | specBind (number, akku) =
					 (if (number=0) then Nop else Dup) ::
					 Invokevirtual
					 (thisTup, "get"^Int.toString number,
					  ([], [Classsig IVal])) ::
					 Astore (stampFromId (List.nth (ids, number))) ::
					 specBind (number-1, akku)
				 in
				     Dup ::
				     Instanceof thisTup ::
				     Ifeq wrongclasslabel ::
				     Checkcast thisTup ::
				     specBind (lgt-1, nil)
				 end
			     else
				 Dup ::
				 Instanceof CTuple ::
				 Ifeq wrongclasslabel ::
				 Checkcast CTuple ::
				 Getfield (CTuple^"/vals", [Arraysig, Classsig IVal]) ::
				 Dup ::
				 Arraylength ::
				 atCodeInt (Int.toLarge lgt) ::
				 Ificmpne popelselabel ::
				 bindit(ids,0))
		    end

		  | testCode (LabTest (label', Id (_,stamp'',_))) =
		    [Dup,
		     Instanceof ITuple,
		     Ifeq wrongclasslabel,
		     Checkcast ITuple,
		     Multi (case Label.toInt label' of
				NONE =>
				    [Ldc (JVMString (Label.toString label')),
				     Invokeinterface (ITuple, "get",
						      ([Classsig CString], [Classsig IVal]))]
			      | SOME i =>
				    [atCodeInt (LargeInt.fromInt (i-1)),
				     Invokeinterface (ITuple, "get",
						      ([Intsig], [Classsig IVal]))]),
		     Dup,
		     Ifnull popelselabel,
		     Astore stamp'']

		  | testCode (VecTest ids) =
		    (* compare the length, then bind *)
		    let
			val lgt = length ids
		    in
			Dup ::
			Instanceof CVector ::
			Ifeq wrongclasslabel ::
			Checkcast CVector ::
			Getfield (CVector^"/vec", [Arraysig, Classsig IVal]) ::
			Dup ::
			Arraylength ::
			atCodeInt (Int.toLarge lgt) ::
			Ificmpne elselabel ::
			stampcode' ::
			bindit(ids,0)
		    end

	    (* generate ConsTest if possible. If not, generate ordinary test *)
	    local
		fun normal () =
		    Multi (testCode test') ::
		    decListCode (body', curFun, curCls)
	    in
		fun checkForConsTest (ConTest (Id (_, cstamp, _), SOME (Id (_, contentStamp',_)) ,_)) =
		    let
			fun findTup (SharedStm (_, b', _)::_) = findTup b'
			  | findTup (IndirectStm (_, ref (SOME b'))::_) = findTup b'
			  | findTup (TestStm (_, Id (_, contentStamp'',_),
					      TupTest [Id (_,t1Stamp,_), Id (_,t2Stamp,_)], b', _)::_) =
			    if contentStamp' = contentStamp'' then
				Multi
				[Dup,
				 Instanceof CCons,
				 Ifeq wrongclasslabel,
				 Checkcast CCons,
				 Dup,
				 Getfield (CCons^"/car", [Classsig IVal]),
				 storeCode (t1Stamp, curFun, curCls),
				 Getfield (CCons^"/cdr", [Classsig IVal]),
				 storeCode (t2Stamp, curFun, curCls)] ::
				decListCode (b', curFun, curCls)
			    else normal ()
			  | findTup _ = normal ()
		    in
			if ConstProp.getStamp cstamp = valstamp_cons then findTup body' else normal ()
		    end
		  | checkForConsTest _ = normal ()
	    end

	    fun normalTest () =
		    stampcode' ::
		    Label retry ::
		    Multi (checkForConsTest test') ::
		    Comment "Test: Goto danach" ::
		    Goto danach ::
		    Label wrongclasslabel ::
		    (if notfinished body'' then Nop
		     else
			 Multi
			 [Instanceof ITransient,
			  Ifeq elselabel,
			  stampcode',
			  Checkcast ITransient,
			  Invokeinterface MRequest,
			  Dup,
			  storeCode (stamp', curFun, curCls),
			  Goto (JLabel.popRetry ())]) ::
		    Label popelselabel ::
		    Pop ::
		    Label elselabel ::
		    Multi
		    (decListCode (body'', curFun, curCls)) ::
		    [Label danach]
	    in
		Line (lineRegion (#region info')) ::
		(case checkForSwitch (test', body'', 0) of
		     (true, _, _) => generateSwitch (JLabel.new ())
		   | _ => normalTest ())
	    end

	  | decCode (SharedStm(_,body',da as ref schonda), curFun, curCls) =
	    if schonda <= 0
		then
		    let
			val _ = da := JLabel.new ()
		    in
			Comment "SharedStm" ::
			Label (!da)::
			decListCode (body', curFun, curCls)
		    end
	    else
		[Comment "Goto Shared",
		 Goto schonda]

	  | decCode (ReturnStm (info,AppExp(_,Id (_,stamp,_),arg)), curFun, curCls) =
		(* tailcall application *)
		[Line (lineRegion (#region info)),
		 Multi (invokeRecApply (stamp, arg, curFun, true, curCls, false)),
		 Areturn]

	  | decCode (ReturnStm (info, exp), curFun, curCls) =
		(* ordinary Return *)
		[Line (lineRegion (#region info)),
		 Multi (expCode (exp, curFun, curCls)),
		 Areturn]

	  | decCode (HandleStm (info, trybody, Id (_,stamp',_), catchbody, contbody, shared'), curFun, curCls) =
		    let
			val try   = JLabel.new()
			val to = JLabel.new ()
			val cont = JLabel.new()
		    in
			shared' := cont;
			Line (lineRegion (#region info)) ::
			(* Catch should be the last instruction here to
			 generate the correct order. However, we reverse
			 the order here and in ToJasmin which makes dead
			 code elemination somewhat easier. *)
			Catch (CExWrap, try, to, to) ::
			Label try::
			Multi (decListCode (trybody, curFun, curCls)) ::
			Label to ::
			Invokevirtual (CExWrap,"getValue",
				       ([],[Classsig IVal])) ::
			Astore stamp' ::
			Multi (decListCode (catchbody, curFun, curCls)) ::
			Label cont ::
			decListCode (contbody, curFun, curCls)
		    end

	  | decCode (EndHandleStm (info, ref cont), _, _) =
		    [Comment "EndHandleStm",
		     Line (lineRegion (#region info)),
		     Goto cont]

	  | decCode (EvalStm (info, exp), curFun, curCls) =
		    Comment ("EvalStm. curFun = "^Stamp.toString curFun^" curCls = "^Stamp.toString curCls) ::
		    Line (lineRegion (#region info)) ::
		    Multi (expCode (exp, curFun, curCls)) ::
		    [Pop]

	  | decCode (ExportStm (info, exp), curFun, curCls) =
		    let
			val stamp' = Stamp.new()
		    in
			Line (lineRegion (#region info)) ::
			Multi (expCode (exp, curFun, curCls)) ::
			Astore stamp' ::
			(if !VERBOSE >= 1 then
			     Multi [Getstatic FOut,
				    Aload stamp',
				    Invokevirtual MPrint]
			 else Nop) ::
			Ldc (JVMString (Class.getInitial()^".pickle")) ::
			Aload stamp' ::
			Invokestatic MPickle ::
			Pop ::
			(if !WAIT
			     then [Nop]
			 else
			     [Iconst 0,
			      Invokestatic MExit])
		    end

	  | decCode (IndirectStm (_, ref (SOME body')), curFun, curCls) =
		     decListCode (body', curFun, curCls)

	  | decCode (IndirectStm (_, ref NONE), _, _) = nil

	and
	    idCode (Id (info,stamp,_), curFun, curCls) =
	    Multi [Line (lineRegion (#region info)),
		   stampCode (stamp, curFun, curCls)]

	and
	    stampCode (stamp', curFun, curCls) =
	    let
		val (bstamp,isBuiltin) = builtinStamp (Lambda.getLambda stamp', curFun)
	    in
		if isBuiltin then bstamp
		else
		    if Lambda.getLambda stamp'=curCls
			then (* Accessing current method. This can be found in
			      register 0. *)
			    Multi [Comment ("Aload thisStamp. curCls ="^Stamp.toString curCls^". stamp' = "^
					    Stamp.toString stamp'^". Lambda.getLambda stamp' = "^
					    Stamp.toString (Lambda.getLambda stamp')),
				   Aload thisStamp]
		    else
			if FreeVars.getFun stamp' = curCls
			    (* In case stamp' is bound in current lambda,
			     the variable can be loaded directly from a
			     JVM register. *)
			    then
				Multi [Comment ("Aload register. curFun = "^Stamp.toString curFun^" Lambda.getLambda = "^Stamp.toString (Lambda.getLambda stamp')),
				Aload stamp']
			else
			    (* We access a free variable which got copied into
			     a field of the actual class. *)
			    Get [Comment ("Var "^Stamp.toString stamp'^" FreeVars.getFun stamp' = "^Stamp.toString
					  (FreeVars.getFun stamp')^"; curCls = "^
					  Stamp.toString curCls^", Lambda.getLambda stamp' = "^
					  Stamp.toString (Lambda.getLambda stamp')),
				 Aload thisStamp,
				 Getfield (classNameFromStamp curCls^"/"^
					   (fieldNameFromStamp (Lambda.getLambda stamp')),
					   [Classsig IVal])]
	    end

	and storeCode (stamp', curFun, curCls) =
	    let
		val s' = Lambda.getLambda stamp'
		val ps = Lambda.getParmStamp (curFun, s')
	    in
		if ps <> s'
		    then Astore ps
		else
		    if s'=curCls
			then (* Accessing current method. Don't overwrite. *)
			    Multi [Comment ("Astore thisStamp. curCls ="^Stamp.toString curCls^". stamp' = "^
					    Stamp.toString stamp'^". Lambda.getLambda stamp' = "^
					    Stamp.toString s'),
				   Pop]
		    else
			if FreeVars.getFun stamp' = curCls
			    (* In case stamp' is bound in current lambda,
			     store to a register. *)
			    then
				Astore stamp'
			else
			    (* We access a free variable which got copied into
			     a field of the actual class. Change it there. *)
			    Get [Comment ("Store to var "^Stamp.toString stamp'^
					  " FreeVars.getFun stamp' = "^Stamp.toString
					  (FreeVars.getFun stamp')^"; curCls = "^
					  Stamp.toString curCls^", Lambda.getLambda stamp' = "^
					  Stamp.toString s'),
				 stampCode (curFun, curFun, curCls),
				 Checkcast (classNameFromStamp curCls),
				 Swap,
				 Putfield (classNameFromStamp curCls^"/"^
					   (fieldNameFromStamp stamp'),
					   [Classsig IVal])]
	    end
	and
	    createTuple (idop, ids:id list, init, curFun, curCls) =
	    (* idop: load tuple from this id. Create a new one, if idop is NONE *)
	    (* If idop is NONE, the tuple remains on top of the stack.
	     if idop is SOME id', the tuple is omitted because createTuple was called
		 from a RecDec *)
	    let
		val arity = length ids

		val ctuple = cTuple arity

		fun specTups (id' :: rest', fld::rest'', akku) =
		    specTups (rest',
			      rest'',
			      (if fld = "fst" then Nop else Dup) ::
			      idCode (id',curFun,curCls) ::
			      Putfield (ctuple^"/"^fld, [Classsig IVal]) ::
			      akku)
		  | specTups (_,_,akku) = akku

		val tuple = createOrLoad (idop, ctuple)
	    in
		Comment "create Tuple:" ::
		(if arity = 0 then
		     Getstatic BUnit::init
		 else
		     tuple ::
		     (if arity <= 4 andalso arity >= 2 then
			  specTups (ids, ["fst", "snd", "thr", "fur"], init)
		      else
			  ids2array (ids, curFun, curCls,
				     Putfield (CTuple^"/vals", [Arraysig, Classsig IVal]) ::
				     init)))
	    end
	and
	    createRecord (idop, labid,init, curFun, curCls) =
	    (* idop: load record from this id. Create a new one, if idop is NONE *)
	    (* If idop is NONE, the record remains on top of the stack.
	     if idop is SOME id', the record is omitted because createRecord was called
		 from a RecDec *)
	    (* labid: (lab * id) list *)
	    (* 1st create or load Record *)
	    (* 2nd fill in Value[] *)
	    Comment "[Record " ::
	    (case idop of
		 NONE => Multi (newRecord (labid, [Dup], curCls))
	       | SOME (Id (_,stamp'',_)) => Aload stamp'') ::
	    ids2array (foldr (fn ((_, id'), akku) => id'::akku) nil labid, curFun, curCls,
		       Putfield (CRecord^"/vals", [Arraysig, Classsig IVal]) ::
		       Comment "Record ]" ::
		       init)

	and
	    newRecord (labids, init, curCls) =
	    let
		(* reverse list and remove ids *)
		fun labids2strings ((l, _)::labids',s')=
		    labids2strings (labids', Label.toString l::s')
		  | labids2strings (nil, s') = s'
	    in
		New CRecord ::
		Dup ::
		Getstatic (RecordLabel.insert
			   (curCls, labids2strings (labids, nil))) ::
		Invokespecial (CRecord,"<init>",
			       ([Arraysig, Classsig CString], [Voidsig])) ::
		init
	    end

	and
	    createVector (idop, ids, init, curFun, curCls) =
	    createOrLoad (idop, CVector) ::
	    ids2array (ids, curFun, curCls,
		       Putfield (CVector^"/vals", [Arraysig, Classsig IVal]) ::
		       init)

	and
	    createRefAppExp (idop, idargs, init, curFun, curCls) =
	    createOrLoad (idop, CReference) ::
	    idArgCode (idargs, curFun,curCls,
		       Putfield (CReference^"/content", [Classsig IVal]) ::
		       init)

	and
	    idArgCode (OneArg id', curFun, curCls, init) =
	    Comment "OneArg" ::
	    idCode (id', curFun, curCls) ::
	    init

	  | idArgCode (TupArgs ids, curFun, curCls, init) =
	    Comment "TupArgs" ::
	    createTuple (NONE, ids, init, curFun, curCls)

	  | idArgCode (RecArgs stringids, curFun, curCls, init) =
	    Comment "RecArgs" ::
	    createRecord (NONE, stringids, init, curFun, curCls)
	and
	    ids2stampcode (Id (_, stamp', _)::ids', curFun, curCls) =
	    stampCode(stamp', curFun, curCls)::ids2stampcode (ids', curFun, curCls)
	    | ids2stampcode (nil, _, _) = nil
	and
	    invokeRecApply (stamp', args, curFun, tailCallPos, curCls, defaultApply) =
	    let
		val fnstamp = Lambda.getLambda stamp'
		val (parms, ids) =
		    case args of
			TupArgs a => (length a, a)
		      | _ => (1, nil)

		val prim = case ConstProp.get stamp' of
		    SOME (PrimExp (_, name)) =>
			PrimCode.code (name, ids, ids2stampcode (ids, curFun, curCls),
				       idArgCode (args, curFun, curCls, nil))
		  | _ => nil

		fun nullLoad (0, akku) = akku
		  | nullLoad (n, akku) = nullLoad (n-1, Aconst_null::akku)

		fun loadparms (p, ending)=
		    Comment ("p = "^Int.toString p^"; parms = "^Int.toString parms) ::
		    (if p<>parms orelse parms=1 then
			 Comment "loadparms:" ::
			 idArgCode (args, curFun, curCls, ending)
		     else
			 List.foldr
			 (fn (id', akku) =>
			  idCode (id', curFun, curCls) :: akku)
			 ending
			 ids)

		fun updateparms (p, ending)=
		    if p<>parms orelse parms=1 then
			idArgCode (args, curFun, curCls, Astore parm1Stamp :: ending)
		    else
			let
			    fun upda (from'::froms, to'::tos) =
				idCode (from', curFun, curCls) ::
				Astore (stampFromId to') ::
				upda (froms, tos)
			      | upda (_, _) = ending
			in
			    upda (ids, Vector.sub (parmIds, p))
			end

		val code' =
		    (case prim of
			 nil => (case Lambda.invokeRecApply (fnstamp, parms) of
				     InvokeRecApply (p, destStamp, pos', label') =>
					 let
					     val call' =
						 stampCode (destStamp, curFun, curCls) ::
						 Checkcast (classNameFromStamp destStamp) ::
						 loadparms
						 (p,
						  nullLoad
						  (4-p,
						   [atCodeInt (LargeInt.fromInt pos'),
						    Invokevirtual (classNameFromStamp destStamp,
								   "recApply",
								   ([Classsig IVal, Classsig IVal,
								     Classsig IVal, Classsig IVal, Intsig],
								    [Classsig IVal]))]))
					 in
					     if tailCallPos then
						 [Comment "Call",
						  Call (classNameFromStamp destStamp, "recApply",
							updateparms (p, [Goto label']),
							[Multi call',
							 Areturn])]
					     else
						 call'
					 end
				   | NormalApply p =>
					 let
					     val p' = if defaultApply then 1 else p
					 in
					     if curFun = curCls andalso curFun = fnstamp
						 andalso tailCallPos andalso not defaultApply
						 then updateparms
						     (p', [Goto JLabel.startlabel])
					     else
						 stampCode (fnstamp, curFun, curCls) ::
						 Comment "NormalApply" ::
						 loadparms (p', [Invokeinterface (mApply p')])
					 end)
		       | primcode => primcode)
	    in
		Comment ("invokeRecApply: "^Stamp.toString fnstamp^":"^Int.toString parms^" in "^Stamp.toString fnstamp) ::
		code'
	    end

	and
	    loadIds nil = nil
	  | loadIds (id'::rest) =
	    idCode id' :: loadIds rest

	and
	    createFun (thisFun, lambda, upperFun, upperCls, newClosure) =
	    (* create a new function closure for ValDec or
	     fill an empty closure for RecDec.
	     If newClosure is false, no value is returned via stack. *)
	    let
		val curCls = Lambda.getClassStamp (thisFun, 1)
		val className = classNameFromStamp curCls
		val freeVarList = FreeVars.getVars (thisFun, curCls)
		(* 1st *)
		val object =
		    if newClosure
			then
			    Multi
			    [Comment "build curCls",
			     New className,
			     Dup,
			     Invokespecial (className, "<init>",
					    ([], [Voidsig]))]
		    else
			Multi [Aload thisFun,
			       Comment "make closure"]

		(* 2. *)
		local
		    fun loadFreeVars (stamp''::rest, popWhenFinished, akku) =
			loadFreeVars
			(rest,
			 false,
			 (if popWhenFinished then Nop else Dup) ::
			 stampCode (stamp'', upperFun, upperCls) ::
			 Putfield ((if stamp'' = curCls then
					classNameFromStamp thisFun
				    else className)^"/"^
				   (fieldNameFromStamp stamp''),
				   [Classsig IVal]) ::
			     akku)
		      | loadFreeVars (nil, true, akku) = Pop::akku
		      | loadFreeVars (_,_,akku) = akku
		in
		    val loadVars = loadFreeVars (freeVarList, not newClosure, nil)
		end
	    in
		Lambda.markForPickling (thisFun, upperCls);
		expCodeClass (lambda, thisFun, curCls);
		object ::
		loadVars
	    end
	and

	    createConVal (stampop, id'', idargs, curFun, curCls) =
	    (* stampop = constructed value or NONE, if called from an expression *)
	    (* id'' = the constructor *)
	    (* idargs = the content *)
	    let
		val (getParms, arity) =
		    (case idargs of
			 TupArgs t =>
			     let
				 val n = List.length t
			     in
				 if 2<=n andalso n<=4 then
				     (List.foldr
				      (fn (id', akku) => idCode (id', curFun, curCls) :: akku)
				      nil
				      t,
				      n)
				 else
				     (idArgCode (idargs, curFun, curCls, nil), 1)
			     end
		       | _ => (idArgCode (idargs, curFun, curCls, nil),1))

		fun nullLoad 0 = nil
		  | nullLoad i = Aconst_null :: nullLoad (i-1)

		val (loadparms, fill) =
		    case stampop of
			NONE => (getParms, [])
		      | SOME stamp' => (nullLoad arity,
					[Checkcast IConVal,
					 Dup,
					 Astore stamp',
					 Multi getParms,
					 Invokeinterface (mSetContent arity)])
	    in
		idCode (id'', curFun, curCls) ::
		Multi loadparms ::
		Invokeinterface (mApply arity) ::
		fill
	    end


	and expCode (AppExp(info, Id(_,stamp',_), args), curFun, curCls) =
	    Comment "AppExp:" ::
	    Line (lineRegion (#region info)) ::
	    invokeRecApply (stamp', args, curFun, false, curCls, false)

	  | expCode (NewExp (info, _, _), _, _) =
	    (*--** genericity? *)
	    if Type.isArrow (#typ info) then
		[Line (lineRegion (#region info)),
		 New CConstructor,
		 Dup,
		 Invokespecial (CConstructor, "<init>", ([],[Voidsig]))]
	    else
		[Line (lineRegion (#region info)),
		 New CName,
		 Dup,
		 Invokespecial (CName, "<init>", ([],[Voidsig]))]

	  | expCode (PrimAppExp (info', name, ids), curFun, curCls) =
	    let
		val n = List.length ids

		fun parmcode init =
		    if n = 0 then init
		    else if n <= 4 then
			List.foldr
			(fn (id', akku) => idCode (id', curFun, curCls) :: akku)
			init
			(List.rev ids)
			 else createTuple (NONE, ids, init, curFun, curCls)
	    in
		case PrimCode.code (name, ids, ids2stampcode (ids, curFun, curCls),
				    createTuple (NONE, ids, nil, curFun, curCls)) of
		    nil =>
			Multi (expCode (PrimExp (info', name), curFun, curCls)) ::
			parmcode
			[Invokeinterface (mApply n)]
		  | primcode => primcode
	    end

	  | expCode (PrimExp (info, name), _, curCls) =
	    [Line (lineRegion (#region info)),
	     Getstatic (Literals.insert (curCls, StringLit name)),
	     Invokestatic MGetBuiltin]

	  | expCode (FunExp(info,thisFun, _, args, body), upperFun, upperCls) =
	    (*--** may not have a OneArg *)
	    (* FunExp of exp_info * fn stamp * printname * id args * body *)
	    (* 1st build closure: - instantiate class *)
	    (*                    - set free variables via putfields *)
	    (* 2nd generate corresponding class file *)
	    Line (lineRegion (#region info)) ::
	    createFun(thisFun, [(args, body)], upperFun, upperCls, true)

	  | expCode (RecExp(_, nil),_,_) =
	    raise (Crash.Crash "CodeGen.expCode: empty RecExp")

	  | expCode (RecExp(info,labid),curFun,curCls) =
	    Line (lineRegion (#region info)) ::
	    createRecord (NONE, labid, nil, curFun, curCls)

	  | expCode (LitExp(info',lit'),_,curCls) =
	    [Line (lineRegion (#region info')),
	     Getstatic (Literals.insert (curCls, lit'))]

	  | expCode (TupExp(info,longids), curFun, curCls) =
	    Line (lineRegion (#region info)) ::
	    createTuple (NONE, longids, nil, curFun, curCls)

	  | expCode (VarExp(info',id' as Id (_, stamp', _)), curFun, curCls) =
	    (case ConstProp.get stamp' of
		 NONE => [Line (lineRegion (#region info')),
			  idCode (id', curFun, curCls)]
	       | SOME exp' => expCode (exp', curFun, curCls))

	  | expCode (AdjExp (info', id', id''), curFun, curCls) =
	    [Line (lineRegion (#region info')),
	     Getstatic (Literals.insert (curCls, StringLit "General.adjoin")),
	     Invokestatic MGetBuiltin,
	     idCode (id', curFun, curCls),
	     idCode (id'', curFun, curCls),
	     Invokevirtual (mApply 2)]

	  | expCode (SelExp(info',lab'),_,_) =
	    Line (lineRegion (#region info')) ::
	    (case Label.toInt lab' of
		 NONE =>
		     [New CSelString,
		      Dup,
		      Ldc (JVMString (Label.toString lab')),
		      Invokespecial (CSelString, "<init>",
				     ([Classsig CString],[Voidsig]))]
	       | SOME i =>
		     [New CSelInt,
		      Dup,
		      atCodeInt (LargeInt.fromInt i),
		      Invokespecial (CSelInt, "<init>",
				     ([Intsig],[Voidsig]))])

	  | expCode (ConExp (info', id', _), curFun, curCls) =
	    [Line (lineRegion (#region info')),
	     Comment "ConExp",
	     idCode (id', curFun, curCls)]

	  | expCode (RefExp info,_,_) =
	    [Line (lineRegion (#region info)),
	     Getstatic BRef]

	  | expCode (ConAppExp (info', id', idargs, _), curFun, curCls) =
	    Line (lineRegion (#region info')) ::
	    createConVal (NONE, id', idargs, curFun, curCls)

	  | expCode (RefAppExp (info, idargs), curFun, curCls) =
	    Line (lineRegion (#region info)) ::
	    createRefAppExp (NONE, idargs, nil, curFun, curCls)

	  | expCode (SelAppExp (info', label', id'), curFun, curCls) =
	    let
		val afterthrow = JLabel.new ()
	    in
		Line (lineRegion (#region info')) ::
		idCode (id', curFun, curCls) ::
		Dup ::
		Instanceof ITuple ::
		Ifne afterthrow ::
		Pop ::
		New CExWrap ::
		Dup ::
		Getstatic (Literals.insert
			   (curCls, StringLit "type error")) ::
		Invokespecial(CExWrap,"<init>",
			      ([Classsig IVal],[Voidsig])) ::
		Athrow ::
		Label afterthrow ::
		Checkcast ITuple ::
		(case Label.toInt label' of
		     NONE =>
			 [Ldc (JVMString (Label.toString label')),
			  Invokeinterface (ITuple, "get",
					   ([Classsig CString], [Classsig IVal]))]
		   | SOME i =>
			 [atCodeInt (LargeInt.fromInt (i-1)),
			  Invokeinterface (ITuple, "get",
					   ([Intsig], [Classsig IVal]))])
	    end

	  | expCode (VecExp (_,ids), curFun, curCls) =
	    createVector (NONE, ids, nil, curFun, curCls)

	and expCodeClass ((OneArg id', body')::specialApplies, curFun, curCls) =
	    let
		val _ = vprint (1, "create Class "^classNameFromStamp curFun^"\n")
		val freeVarList = FreeVars.getVars (curFun, curCls)
		val _ = Lambda.setParmStamp (curFun, stampFromId id', parm1Stamp)
		val _ = FreeVars.setFun (id', curCls)

		(* generate fields for the free variables of the function *)
		fun fields (stamp'', akku) =
		    Field ([FPublic],fieldNameFromStamp stamp'', [Classsig IVal]) ::
		    akku

		val fieldscode = List.foldr fields nil freeVarList

		fun createApplies ((t as TupArgs ids, body'')::rest,
				   a0, a2, a3, a4, ra) =
		    let
			val l = length ids
			fun assignParmStamps (Id (_,stamp',_)::rest, name'::rest') =
			    (Lambda.setParmStamp (curFun, stampFromId name', stamp');
			     assignParmStamps (rest, rest'))
			  | assignParmStamps (_,_) = ()
			val b'' = if l = 0 orelse (l>=2 andalso l<=4)
				      then
					  (assignParmStamps (Vector.sub (parmIds, l), ids);
					   decListCode (body'', curFun, curCls))
				  else nil
		    in
			case l of
			    0 => createApplies
				(rest, b'', a2, a3, a4, ra)
			  | 2 => createApplies
				  (rest, a0, b'', a3, a4, ra)
			  | 3 => createApplies
				  (rest, a0, a2, b'', a4, ra)
			  | 4 => createApplies
				  (rest, a0, a2, a3, b'', ra)
			  | _ => createApplies
				(rest, a0, a2, a3, a4,
				 [TestStm (dummyStmInfo,
					   parm1Id,
					   TupTest ids, body'', ra)])
		    end
		  | createApplies ((t as RecArgs labids, body'') :: rest,
				   a0, a2, a3, a4, ra) =
		     createApplies (rest, a0, a2, a3, a4,
				    [TestStm (dummyStmInfo, parm1Id,
					      RecTest labids,
					      body'', ra)])
		  | createApplies (nil, a0, a2, a3, a4, ra) =
		     (vprint (2, "Lambda "^Stamp.toString curFun^" in "^Stamp.toString curCls);
		      (a0, a2, a3, a4, decListCode (ra, curFun, curCls)))
		  | createApplies (_, _, _, _, _, _) = raise (Crash.Crash "CodeGen: createApplies")

		val (ap0, ap2, ap3, ap4, ad) =
		    createApplies (List.rev specialApplies,
				   nil, nil, nil, nil, body')

		fun buildSpecialApply (count, spec) =
		    case spec of
			nil => Nop
		      | _ => let
				 val elselabel = JLabel.new ()
				 val thisTup = cTuple count
				 val s = Stamp.new ()
				 fun gets (act, to') =
					 Invokevirtual
					 (thisTup, "get"^Int.toString act,
					  ([], [Classsig IVal])) ::
					 (if act = to' then
					      nil
					  else
					      Aload s ::
					      gets (act+1, to'))
			     in
				 Multi
				 [Aload parm1Stamp,
				  Instanceof thisTup,
				  Ifeq elselabel,
				  Aload thisStamp,
				  Aload parm1Stamp,
				  Checkcast thisTup,
				  Dup,
				  Astore s,
				  Multi (gets (0, count-1)),
				  Invokeinterface (mApply count),
				  Areturn,
				  Label elselabel]
			     end

		val defaultApply =
		    [(case ap0 of
			  nil => Nop
			| _ => let
				   val elselabel=JLabel.new()
			       in
				   Multi [stampCode (parm1Stamp, curFun, curCls),
					  Getstatic BUnit,
					  Ifacmpne elselabel,
					  stampCode (curFun, curFun, curCls),
					  Invokeinterface (mApply 0),
					  Areturn,
					  Label elselabel]
			       end),
		     buildSpecialApply(2, ap2),
		     buildSpecialApply(3, ap3),
		     buildSpecialApply(4, ap4),
		     Multi ad,
		     Areturn]

		fun parmLoad nil = nil
		  | parmLoad (Id (_,s',_)::rest) =
		    Aload s' ::
		    parmLoad rest

		val addmatchlabel =
		    [Comment "JLabel.matchlabel",
		     Label JLabel.matchlabel,
		     Pop,
		     New CExWrap,
		     Dup,
		     Getstatic BMatch,
		     Invokespecial(CExWrap,"<init>",
				   ([Classsig IVal],[Voidsig])),
		     Athrow]

		fun makeApplyMethod (parms, insts) =
		    let
			val ta = if parms = 1
				     then OneArg parm1Id
				 else TupArgs (Vector.sub (parmIds, parms))

			val body' =
			    if Lambda.isInRecApply (curFun, parms) then
				(Lambda.addToRecApply
				 (insts, curFun, parms);
				 [Comment "makeApplyMethod:",
				  Multi (invokeRecApply (curFun, ta, curFun, true, curFun, true))])
			    else
				(case insts of
				     nil =>
					 [Comment "makeApplyMethod2:",
					  Multi (invokeRecApply (curFun, ta, curFun, true, curFun, true)),
					  Areturn]
				   | _ => insts)
		    in
			Method ([MPublic],
				applyName parms,
				(valList parms, [Classsig IVal]),
				Label JLabel.startlabel ::
				Multi body' ::
				addmatchlabel)
		    end

		(* normal apply methods *)
		val (applY, apply0, apply2, apply3, apply4, recApply) =
		    (makeApplyMethod (1, defaultApply),
		     makeApplyMethod (0, ap0),
		     makeApplyMethod (2, ap2),
		     makeApplyMethod (3, ap3),
		     makeApplyMethod (4, ap4),
		     Lambda.buildRecApply (curFun, addmatchlabel))

		(* default constructor *)
		val init = Method ([MPublic],"<init>",([], [Voidsig]),
				   [Aload thisStamp,
				    Invokespecial (CFcnClosure, "<init>",
						   ([], [Voidsig])),
				    Return])

		(* class initialization. *)
		val clinit = Method([MPublic],
				    "<clinit>",
				    ([],[Voidsig]),
				    [Return])

		(* the whole class *)
		val class = Class([CPublic],
				  classNameFromStamp curFun,
				  CFcnClosure,
				  nil,
				  Lambda.makePickleFields
				  (curCls, Literals.makefields
				   (curCls, RecordLabel.makefields
				    (curCls, fieldscode))),
				  [clinit, init, applY, apply0, apply2, apply3, apply4, recApply])
	    in
		classToJasmin (class)
	    end
	  | expCodeClass _ = raise (Crash.Crash "CodeGen.expCodeClass")

	(* make array of value list *)
	and
	    ids2array (ids, curFun, curCls, init) =
	    let
		fun f (id'::rest, i, akku) =
		    f (rest,
		       i+1,
		       Dup ::
		       atCodeInt i ::
		       idCode (id', curFun, curCls) ::
		       Aastore ::
		       akku)
		  | f (nil, _, akku) = akku
	    in
		atCodeInt (Int.toLarge (List.length ids)) ::
		Anewarray IVal ::
		f (ids, 0, init)
	    end

    end
