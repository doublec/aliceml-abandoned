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

	local
	    (* apply f to all stamps of an arg. *)
	    fun argApp (f, OneArg id', free, curFun, dbg) =
		f (free, id', curFun, dbg)
	      | argApp (f, TupArgs ids, free, curFun, dbg) =
		app (fn id' => f (free, id', curFun, dbg)) ids
	      | argApp (f, RecArgs labids, free, curFun, dbg) =
		app (fn (_, id') => f (free, id', curFun, dbg)) labids

	    (* mark a variable as free *)
	    fun markfree (free, id' as Id (_,stamp', _), curFun, dbg) =
		StampSet.insert (free, stamp')

	    (* mark a variable as bound *)
	    fun markbound (free, id' as Id (_,stamp', _), curFun, dbg) =
		(StampSet.delete (free, stamp');
		 print ("markbound: "^dbg);
		 FreeVars.setFun (id', curFun))
	in
	    (* compute free variables of expressions *)
	    fun freeVarsExp (LitExp _, _, _) = ()
	      | freeVarsExp (VarExp (_, id'), free, curFun) =
		markfree(free, id', curFun, "VarExp")
	      | freeVarsExp (NewExp _, _, _) = ()
	      | freeVarsExp (ConAppExp (_, id', idargs), free, curFun) =
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
	      | freeVarsExp (AppExp(_,id', idargs'), free, curFun) =
		(markfree (free, id', curFun, "AppExp");
		 argApp (markfree, idargs', free, curFun, "AppExp2"))
	      | freeVarsExp (ConExp(_, id', _), free, curFun) =
		markfree (free, id', curFun, "ConExp")
	      | freeVarsExp (RefExp _, _, _) = ()
	      | freeVarsExp (SelAppExp(_,_,id'), free, curFun) =
		markfree (free, id', curFun, "SelAppExp")
	      | freeVarsExp (PrimExp (_, name), _, _) = ()
	      | freeVarsExp (FunExp (_, thisFun, _, idbodies), free, curFun) =
		let
		    fun freeVarsFun (args,body') =
			let
			    val newfree = StampSet.new ()
			    val destClass = Lambda.getClassStamp
				(thisFun, Lambda.argSize args)
			in
			    freeVarsDecs (body', newfree, destClass);
			    argApp (markbound, args, newfree, destClass, "freeVarsFun");
			    print ("Fun "^Stamp.toString thisFun^", Class "^Stamp.toString
				   destClass^". Adding FreeVars ");
			    StampSet.app (fn x => print (Stamp.toString x^" ")) newfree;
			    print ".\n";
			    FreeVars.addVars (destClass, newfree);
			    StampSet.app
			    (fn x => StampSet.insert (free,x))
			    newfree
			end
		in
		    app freeVarsFun idbodies;
		    print "FunExp: ";
		    FreeVars.setFun (Id (dummyCoord, thisFun, InId), curFun)
		end

	    and freeVarsDec (RaiseStm(_,id'), free, curFun) =
		markfree (free, id', curFun, "RaiseStm")
	      | freeVarsDec (HandleStm(_,body',id',body'', body''', _), free, curFun) =
		(freeVarsDecs (body''', free, curFun);
		 freeVarsDecs (body'', free, curFun);
		 freeVarsDecs (body', free, curFun);
		 markbound (free, id', curFun, "HandleStm"))
	      | freeVarsDec (EndHandleStm _, _, _) = ()
	      | freeVarsDec (TestStm(_,id',test',body',body''), free, curFun) =
		(freeVarsDecs (body'', free, curFun);
		 freeVarsDecs (body', free, curFun);
		 freeVarsTest (test', free, curFun);
		 markfree (free, id', curFun, "TestStm"))
	      | freeVarsDec (SharedStm(_,body',raf as ref 0), free, curFun) =
		 (raf := ~1;
		  freeVarsDecs (body', free, curFun))
	      | freeVarsDec (SharedStm _, _, _) = ()
	      | freeVarsDec (ValDec(_, id', exp', _), free, curFun) =
		 (freeVarsExp (exp', free, curFun);
		  markbound (free, id', curFun, "ValDec"))
	      | freeVarsDec (RecDec(_,idsexps, _), free, curFun) =
		 (Lambda.insertRec idsexps;
		  app
		  (fn (id', exp') => (freeVarsExp (exp', free, curFun);
				      markbound (free, id', curFun, "RecDec")))
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
	      | freeVarsTest (ConTest(id',NONE), free, curFun) =
		markfree (free, id', curFun, "ConTest none")
	      | freeVarsTest (ConTest(id',SOME id''), free, curFun) =
		(markfree (free, id', curFun, "ConTest some");
		 markbound (free, id'', curFun, "ConTest some2"))
	      | freeVarsTest (RefTest id'', free, curFun) =
		markbound (free, id'', curFun, "RefTest")
	      | freeVarsTest (RecTest labids, free, curFun) =
		app (fn (_,id') => markbound (free, id', curFun, "RecTest")) labids
	      | freeVarsTest (TupTest labs, free, curFun) =
		app (fn id' => markbound (free, id', curFun, "TupTest")) labs
	      | freeVarsTest (LabTest(_,id'), free, curFun) = markbound (free, id', curFun, "LabTest")
	      | freeVarsTest (VecTest _, _, _) = Crash.crash "Was'n VecTest?" (* xxx *)

	    and freeVarsDecs (decs, free, curFun) =
		app (fn dec' => freeVarsDec (dec', free, curFun)) (List.rev decs)
	end

	(* entry point *)
	fun genComponentCode (debug, verbose, optimize, name, (nil, _, program)) =
	    (DEBUG := debug;
	     VERBOSE := verbose;
	     OPTIMIZE := optimize;
	     Class.setInitial name;
	     if !VERBOSE >=2 then
		 (print ("thisStamp: "^Stamp.toString thisStamp^"\n");
		  print ("parm1Stamp: "^Stamp.toString parm1Stamp^"\n");
		  print ("parm2Stamp: "^Stamp.toString parm2Stamp^"\n");
		  print ("parm3Stamp: "^Stamp.toString parm3Stamp^"\n");
		  print ("parm4Stamp: "^Stamp.toString parm4Stamp^"\n"))
		 else ();
	     let
		 (* compute free variables. *)
		 val _ = let
			     val free = StampSet.new ()
			 in
			     app (fn dec' => freeVarsDec (dec', free, toplevel)) program
			 end

		 val main = Method([MStatic,MPublic],"main",([Arraysig, Classsig CString],[Voidsig]),
				   Register.generateVariableTable
				   [New name,
				    Dup,
				    Invokespecial (name, "<init>", ([], [Voidsig])),
				    Invokevirtual (CThread, "start", ([], [Voidsig])),
				    Return])
		 (* Default initialisation. *)
		 val init = Method([MPublic],"<init>",([],[Voidsig]),
				   [Aload thisStamp,
				    Invokespecial (CDMLThread, "<init>", ([], [Voidsig])),
				    Return])
		 (* Toplevel environment is built in the run method.
		  All Objects are instantiated and initialized, function closures are built.
		  The result as well as the generated class files is stored into one single
		  pickle file. This is the last step of the compilation process. *)
		 val literalName = Class.getLiteralName()

		 val decs = decListCode (program, toplevel, toplevel)

		 val run = Method([MPublic], "run", ([], [Voidsig]),
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
				       nil)

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
				     Literals.generate
				     (RecordLabel.generate()))

		 (* Default initialization: Invocation of the super class. *)
		 val litinit = Method([MPublic],"<init>",([],[Voidsig]),
				      Aload thisStamp::
				      Invokespecial (CFcnClosure, "<init>", ([], [Voidsig]))::
				      (Lambda.generatePickleFn
				       [Return]))

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
	     end;
	     if (!VERBOSE >= 1) then Lambda.showRecApplies ()
	     else ())
	  | genComponentCode _ = Crash.crash "cannot translate Components"

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
					if stamp'=parm1Stamp orelse
					    stamp'=parm2Stamp orelse
					    stamp'=parm3Stamp orelse
					    stamp'=parm4Stamp orelse
					    stamp'=thisStamp then (Aload stamp', true) else
					    (Nop,false)

	and decListCode (dec::rest, curFun, curCls) =
	    Multi (decCode (dec, curFun, curCls)) ::
	    decListCode (rest, curFun, curCls)
	  | decListCode (nil, _, _) = nil

	and normalReturn (e, curCls) =
	    if !DEBUG>=2 then
		let
		    val nodebug2=Label.new()
		in
		    Getstatic (classNameFromStamp curCls^"/DEBUG", [Boolsig]) ::
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
	and decCode (ValDec(_, id' as Id (_,stamp',_), exp' as FunExp (_, thisFun, _, _),_), curFun, curCls) =
	    (print "ValDec: ";
	     Lambda.setId (thisFun, id');
	     Multi (expCode (exp', curFun, curCls)) ::
	     Astore thisFun ::
	     nil)
	  | decCode (ValDec(_, Id (_,stamp',_), exp',_), curFun, curCls) =
	    (Multi (expCode (exp', curFun, curCls)) ::
	     Astore stamp' ::
	     nil)

	  | decCode (RecDec (_, nil, _), _, _) = nil
	  | decCode (RecDec (_,idexps as ((recid,_)::rest),_), curFun, curCls) =
	    (* RecDec of coord * (id * exp) list * isTopLevel *)
	    (* 1. create a new object for each id and store it into a new register. *)
	    (* 2. evaluate the expression and pop the result *)
	    (* !! The closure is built on evaluation of the expressions *)
	    let
		(* 1st step *)
		local
		    fun init ((Id (_,stamp',_),exp'),akku) =
			let
			    fun specTups (id'' :: rest) =
				idCode (id'', curCls) ::
				specTups rest
			      | specTups nil = nil

			    fun normalConAppExp (_,id'',idargs) =
				New CConVal ::
				Dup ::
				idCode (id'', curCls) ::
				Invokespecial (CConVal, "<init>",
					       ([Classsig CConstructor],
						[Voidsig])) ::
				Dup ::
				Astore stamp' ::
				idArgCode
				(idargs,
				 curCls,
				 Invokeinterface (CConVal, "setContent",
						  ([Classsig CVal],
						   [Voidsig])) ::
				 nil)

			    val one = case exp' of
				(* user defined function *)
				FunExp (_,thisFun,_,_) =>
				    let
					val className = classNameFromStamp thisFun
					val _ = Register.instantiate thisFun
				    in
					New className ::
					Dup ::
					Invokespecial (className, "<init>",
						       ([],[Voidsig])) ::
					Astore thisFun ::
					nil
				    end
			      | n as NewExp _ =>
				    Multi (expCode (n, illegalStamp, illegalStamp)) ::
				    Astore stamp' ::
				    nil

			      (* constructor application *)
			      (* xxx falsch! Erst leere Dummies bauen *)
			      | ConAppExp (parms as
					   (_,_, (TupArgs ids))) =>
				    let
					val n = length ids
					val cls=CConVal^(Int.toString n)
				    in
					if n=0 then [Getstatic CUnit] else
					    if n>=2 andalso n<=4 then
						[New cls,
						 Dup,
						 Multi (specTups ids),
						 Invokespecial
						 (cls, "<init>",
						  (valList n, [Voidsig]))]
					    else
						normalConAppExp parms
				    end

			      | ConAppExp parms => normalConAppExp parms

			      | VarExp (_, id'') =>
				    idCode (id'', curCls) ::
				    Astore stamp' ::
				    nil

			      (* record *)
			      | RecExp _ =>
				    New CRecord ::
				    Dup ::
				    Invokespecial (CRecord,"<init>",
						   ([],[Voidsig])) ::
				    Astore stamp' ::
				    nil

			      (* tuple *)
			      | TupExp (_, ids) =>
				    let
					val n = length ids
					val cls = CTuple^(if n=1 orelse n >=4 then "" else Int.toString n)
				    in
					New cls ::
					Dup ::
					Invokespecial (cls,"<init>",
						       ([],[Voidsig])) ::
					Astore stamp' ::
					nil
				    end
			      | _ => raise Crash.crash "illegal expression in RecDec"
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
			(print ("RecDec: "^Stamp.toString (stampFromId id')^ " (curFun = "^Stamp.toString curFun^")\n");
			 Multi (expCode (exp', curFun, curCls)) ::
			 Pop::
			 evalexp idexps')
		      | evalexp nil = nil
		in
		    val expcode = evalexp (List.rev idexps)
		end
	    in
		initcode @ (Comment "expCode: " :: expcode)
	    end

	  | decCode (RaiseStm(_,id'), _, curCls) =
		 [New CExWrap,
		  Dup,
		  idCode (id', curCls),
		  Invokespecial(CExWrap,"<init>",
				([Classsig CVal],[Voidsig])),
		  Athrow]

	  | decCode (TestStm(_,id' as Id (_,stamp',_),test',body',body''), curFun, curCls) =
		 (* test whether id' matches with test'.
		  If so, eval body', if not, eval body'' *)
	    let
		val danach = Label.new ()
		val elselabel = Label.new ()
		val ilselabel = Label.new ()
		val stampcode' = stampCode (stamp', curCls)

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
				val lab = Label.new ()
				val (switch, inst, number) =
				    checkForSwitch (test', body'', begin)
			    in
				if switch
				    then
					case inst of
					    TestStm (_, _, t'',b',b'')
					    => generateBody
					    (Label lab ::
					     Multi (decListCode (body', curFun, curCls)) ::
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
					 Multi (decListCode (body', curFun, curCls)) ::
					 Multi akku ::
					 Label behind ::
					 decListCode (body'', curFun, curCls),
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
			       | _ => Lookupswitch ((switchlist, labellist), behind)) ::
			     bod

			val gb as (_, _, _, behind) = generateBody (nil, nil, nil, body', test', body'')

			val sw = makeSwitch gb

			fun litTest (cls, ret) =
			    Instanceof cls ::
			    Ifeq behind ::
			    stampcode' ::
			    Checkcast cls ::
			    Getfield (cls^"/value", ret) ::
			    sw
		    in
			Label begin ::
			stampcode' ::
			(case test' of
			     LitTest (WordLit _) =>
				 litTest (CWord, [Intsig])
			   | LitTest (IntLit startwert) =>
				 litTest (CInt, [Intsig])
			   | LitTest (CharLit startwert) =>
				 litTest (CChar, [Charsig])
			   | _ => raise Mitch)
		    end

		fun tcode (cls, ret, cmpcode) =
		    stampcode' ::
		    Dup ::
		    Instanceof cls ::
		    Ifeq ilselabel ::
		    Checkcast cls ::
		    Getfield (cls^"/value", ret) ::
		    cmpcode

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
			     tcode (CStr, [Intsig],
				    [Ldc (JVMString s'),
				     Invokevirtual (CString,"equals",
						    ([Classsig CObj],[Boolsig])),
				     Ifeq elselabel])
		       | r as (RealLit r') =>
			     tcode (CReal, [Floatsig],
				    [atCode r,
				     Invokevirtual (CString,"equals",([Classsig CObj],[Boolsig])),
				     Ifeq elselabel]))

		  | testCode (ConTest (id'',NONE)) =
			 stampcode' ::
			 idCode (id'', curCls) ::
			 Ifacmpne elselabel ::
			 nil

		  | testCode (ConTest (id'',SOME (Id (_,stamp''',_)))) =
			 stampcode' ::
			 Dup ::
			 Instanceof CConVal ::
			 Ifeq ilselabel ::
			 Checkcast CConVal ::
			 Invokeinterface (CConVal, "getConstructor",
					  ([], [Classsig CConstructor])) ::
			 idCode (id'', curCls) ::
			 Ifacmpne elselabel ::
			 stampcode' ::
			 Checkcast CConVal ::
			 Invokeinterface (CConVal, "getContent",
					  ([],[Classsig CVal])) ::
			 Astore stamp''' ::
			 nil

		  | testCode (RefTest (Id (_,stamp''',_))) =
			 stampcode' ::
			 Dup ::
			 Instanceof CReference ::
			 Ifeq ilselabel ::
			 Checkcast CReference ::
			 Invokevirtual (CReference, "getContent",
					([],[Classsig CVal])) ::
			 Astore stamp''' ::
			 nil

		  | testCode (RecTest stringid) =
		    (* Load arity, compare, then bind *)
		    let
			(* reverse the list and remove ids *)
			fun stringids2strings ((l, _)::stringids',s')=
			    stringids2strings (stringids', l::s')
			  | stringids2strings (nil, s') = s'
			fun bindit ((_,Id (_,stamp'',_))::nil,i) =
			    atCodeInt i ::
			    Aaload ::
			    Astore stamp'' ::
			    nil
			  | bindit ((_,Id (_,stamp'',_))::rest,i) =
			    Dup ::
			    atCodeInt i ::
			    Aaload ::
			    Astore stamp'' ::
			    bindit(rest,i+1)
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
				       ([Arraysig, Classsig CString],
					[Arraysig, Classsig CVal])) ::
			Dup ::
			Ifnull ilselabel ::
			bindit (stringid,0)
		    end

		  | testCode (TupTest ids) =
		    (* compare the arity (int), then bind *)
		    let
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
				     val s0 = stampFromId (hd ids)
				     val s1 = stampFromId (List.nth (ids, 1))
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
					      Astore s0 ::
					      Invokevirtual
					      (CTuple2, "get1",
					       ([], [Classsig CVal])) ::
					      Astore s1 ::
					      nil
					| 3 => let
						   val s2 = stampFromId (List.nth (ids, 2))
					       in
						   Dup ::
						   Instanceof CTuple3 ::
						   Ifeq ilselabel ::
						   Checkcast CTuple3 ::
						   Dup ::
						   Invokevirtual
						   (CTuple3, "get0",
						    ([], [Classsig CVal])) ::
						   Astore s0 ::
						   Dup ::
						   Invokevirtual
						   (CTuple3, "get1",
						    ([], [Classsig CVal])) ::
						   Astore s1 ::
						   Invokevirtual
						   (CTuple3, "get2",
						    ([], [Classsig CVal])) ::
						   Astore s2 ::
						   nil
					       end
				    | 4 => let
						   val s2 = stampFromId (List.nth (ids, 2))
						   val s3 = stampFromId (List.nth (ids, 3))
					   in
					       Dup ::
					       Instanceof CTuple4 ::
					       Ifeq ilselabel ::
					       Checkcast CTuple4 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get0",
						([], [Classsig CVal])) ::
					       Astore s0 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get1",
						([], [Classsig CVal])) ::
					       Astore s1 ::
					       Dup ::
					       Invokevirtual
					       (CTuple4, "get2",
						([], [Classsig CVal])) ::
					       Astore s2 ::
					       Invokevirtual
					       (CTuple4, "get3",
						([], [Classsig CVal])) ::
					       Astore s3 ::
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

		  | testCode (LabTest (s', Id (_,stamp'',_))) =
		    stampcode' ::
		    Dup ::
		    Instanceof CDMLTuple ::
		    Ifeq ilselabel ::
		    Checkcast CDMLTuple ::
		    Ldc (JVMString s') ::
		    Invokeinterface (CDMLTuple,"get",([Classsig CString],[Classsig CVal])) ::
		    Astore stamp'' ::
		    nil

		val begin = Label.new ()

		fun normalTest () =
		    Multi (testCode test') ::
		    Multi
		    (decListCode (body', curFun, curCls)) ::
		    Comment "Test: Goto danach" ::
		    Goto danach ::
		    Label ilselabel ::
		    Pop ::
		    Label elselabel ::
		    Multi
		    (decListCode (body'', curFun, curCls)) ::
		    [Label danach]
	    in
		if !OPTIMIZE >=3 then
		    case checkForSwitch (test', body'', 0) of
			(true, _, _) => generateSwitch begin
		      | _ => normalTest ()
		else normalTest ()
	    end

	  | decCode (SharedStm(_,body',da as ref schonda), curFun, curCls) =
	    if schonda <= 0
		then
		    let
			val _ = da := Label.new ()
		    in
			Comment "SharedStm" ::
			Label (!da)::
			decListCode (body', curFun, curCls)
		    end
	    else
		[Comment "Goto Shared",
		 Goto schonda]

	  | decCode (ReturnStm (_,AppExp(_,Id (_,stamp',_),arg')), curFun, curCls) =
		(* tailcall applikation *)
		normalReturn
		([Multi (invokeRecApply (stamp', arg', curFun, true, curCls)),
		  Areturn],
		 curCls)

	  | decCode (ReturnStm (_, exp'), curFun, curCls) =
		    (* ordinary Return *)
		    normalReturn
		    ([Multi (expCode (exp', curFun, curCls)),
		      Areturn],
		     curCls)

	  | decCode (HandleStm(_,trybody, Id (_,stamp',_), catchbody, contbody, shared'), curFun, curCls) =
		    let
			val try   = Label.new()
			val to = Label.new ()
			val cont = Label.new()
		    in
			shared' := cont;
			Catch (CExWrap, try, to, to) ::
			Label try::
			Multi (decListCode (trybody, curFun, curCls)) ::
			Label to ::
			Invokevirtual (CExWrap,"getValue",
				       ([],[Classsig CVal])) ::
			Astore stamp' ::
			Multi (decListCode (catchbody, curFun, curCls)) ::
			Label cont ::
			decListCode (contbody, curFun, curCls)
		    end

	  | decCode (EndHandleStm (_, ref cont), _, _) =
		    Comment "EndHandleStm" ::
		    Goto cont ::
		    nil

	  | decCode (EvalStm (_, exp'), curFun, curCls) =
		    Multi (expCode (exp', curFun, curCls)) ::
		    [Pop]

	  | decCode (ExportStm (_,ids), curFun, curCls) =
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
			     Dup ::
			     atCodeInt j ::
			     Comment "aload for record" ::
			     Aload stamp' ::
			     Aastore ::
			     (load (rs,j+1))
			   | load (nil,_) = nil
			 (* 3rd *)
		    in
			(mainpickle := Stamp.new();
			 [Comment "[Mainpickle ",
			  New CRecord,
			  Dup,
			  Getstatic (RecordLabel.insert
				     (ids2strings (ids, nil))),
			  atCodeInt (Int.toLarge arity),
			  Anewarray CVal,
			  Multi (load (ids,0)),
			  Invokespecial (CRecord,"<init>",
					 ([Arraysig, Classsig CString,
					   Arraysig, Classsig CVal],
					  [Voidsig])),
			  Astore (!mainpickle),
			  Comment "Mainpickle ]"])
		     end

	  | decCode (IndirectStm (_, ref (SOME body')), curFun, curCls) =
		     decListCode (body', curFun, curCls)

	  | decCode (IndirectStm (_, ref NONE), _, _) = nil

	and
	    idCode (Id(_,stamp',_), curCls) = stampCode (stamp', curCls)

	and
	    stampCode (stamp', curCls) =
	    let
		val (bstamp,isBuiltin) = builtinStamp stamp'
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
				Aload stamp'
			else
			    (* We access a free variable which got copied into
			     a field of the actual class. *)
			    Get [Comment ("Var "^Stamp.toString stamp'^" FreeVars.getFun stamp' = "^Stamp.toString
					  (FreeVars.getFun stamp')^"; curCls = "^
					  Stamp.toString curCls^", Lambda.getLambda stamp' = "^
					  Stamp.toString (Lambda.getLambda stamp')),
				 Aload thisStamp,
				 Getfield (classNameFromStamp curCls^"/"^
					   (fieldNameFromStamp stamp'),
					   [Classsig CVal])]
	    end
	and
	    createTuple (ids:id list, init, curCls) =
	    let
		val arity = length ids

		fun f ((id' as Id(_,stamp',_))::rest, i, akku) =
		    f (rest,
			 i+1,
			 Dup ::
			 atCodeInt i ::
			 stampCode (stamp', curCls) ::
			 Aastore ::
			 akku)
		  | f (nil, _, akku) = akku

		fun specTups (id' :: rest) =
		    idCode (id',curCls) ::
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
		    Aload stamp' ::
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
			       ([Arraysig, Classsig CString,
				 Arraysig, Classsig CVal],
				[Voidsig])) ::
		Comment "Record ]" ::
		init
	    end

	and
	    idArgCode (OneArg id', curCls, init) = idCode (id', curCls) :: init

	  | idArgCode (TupArgs ids, curCls, init) = createTuple (ids, init, curCls)

	  | idArgCode (RecArgs stringids, _, init) =
	    createRecord (stringids, init)
	and
	    invokeRecApply (stamp', args, curFun, tailCallPos, curCls) =
	    let
		val fnstamp = Lambda.getLambda stamp'
		val (parms, ids) =
		    case args of
			TupArgs a => (length a, a)
		      | _ => (1, nil)

		fun nullLoad (0, akku) = akku
		  | nullLoad (n, akku) = nullLoad (n-1, Aconst_null::akku)

		fun loadparms (p, ending)=
		    if p<>parms orelse parms=1 then
			idArgCode (args, curCls, ending)
		    else
			List.foldr
			(fn (id', akku) =>
			 idCode (id', curCls) :: akku)
			ending
			ids

		fun updateparms (p, ending)=
		    if p<>parms orelse parms=1 then
			idArgCode (args, curCls, Astore parm1Stamp :: ending)
		    else
			let
			    fun upda (from'::froms, to'::tos) =
				idCode (from', curCls) ::
				Astore (stampFromId to') ::
				upda (froms, tos)
			      | upda (_, _) = ending
			in
			    upda (ids, Vector.sub (parmIds, p))
			end

		val code' =
		    (case Lambda.invokeRecApply (fnstamp, parms) of
			 InvokeRecApply (p, destStamp, pos', label') =>
			     let
				 val call' =
				     stampCode (destStamp, curCls) ::
				     Checkcast (classNameFromStamp destStamp) ::
				     loadparms
				     (p,
				      nullLoad
				      (4-p,
				       [atCodeInt (LargeInt.fromInt pos'),
					Invokevirtual (classNameFromStamp destStamp,
						       "recApply",
						      ([Classsig CVal, Classsig CVal,
							Classsig CVal, Classsig CVal, Intsig],
						       [Classsig CVal]))]))
			     in
				 if tailCallPos then
				     [Comment "Call",
				      Call (classNameFromStamp destStamp, "recApply",
					    updateparms (p, [Goto label']),
					    Multi call' ::
					    normalReturn ([Areturn], curCls))]
				 else
				     call'
			     end
		       | NormalApply p =>
			     stampCode (fnstamp, curCls) ::
			     Comment "NormalApply" ::
			     loadparms (p,
					[Invokeinterface
					 (CVal,
					  applyName p,
					  (valList p, [Classsig CVal]))]))
	    in
		Comment ("invokeRecApply: "^Stamp.toString fnstamp^":"^Int.toString parms^" in "^Stamp.toString fnstamp) ::
		code'
	    end


	and
	    loadIds nil = nil
	  | loadIds (id'::rest) =
	    idCode id' :: loadIds rest

	and
	    expCode (AppExp(_,Id(_,stamp',_), args), curFun, curCls) =
	    Comment "AppExp:" ::
	    invokeRecApply (stamp', args, curFun, false, curCls)
	  | expCode (NewExp (_, hasArgs), _, _) =
	    if hasArgs then
		[New CConstructor,
		 Dup,
		 Invokespecial (CConstructor, "<init>",
				([],[Voidsig]))]
	    else
		[New CName,
		 Dup,
		 Invokespecial (CName, "<init>",
				([],[Voidsig]))]
	  | expCode (PrimAppExp (_, name, ids), _, curCls) =
		(case name of
		     "print" => [Getstatic COut,
				 idCode (hd ids, curCls),
				 Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj],[Voidsig])),
				 Getstatic CUnit]
		   | _ => (print ("PrimAppExp: "^name); nil))

	  | expCode (PrimExp (_, name), _, _) =
		     [Getstatic (Literals.insert (StringLit name), [Classsig CStr]),
		      Invokestatic (CBuiltin, "getBuiltin",
				    ([Classsig CStr], [Classsig CVal]))]

	  | expCode (FunExp(_,thisFun, _, lambda), upperFun, upperCls) =
		     (* FunExp of coord * string * (id args * dec) list *)
		     (* id is formal parameter *)
		     (* 1st build closure: - instantiate or load object *)
		     (*                    - set free variables via putfields *)
		     (* 2nd generate corresponding class file *)
		     let
			 val curCls = Lambda.getClassStamp (thisFun, 1)
			 val className = classNameFromStamp curCls
			 val freeVarList = FreeVars.getVars thisFun
			 (* 1st *)
			 val object =
			     if Register.isInstantiated curCls
				 then
				     Multi
				     [Comment "Aload curCls",
				      Aload curCls]
			     else (* instantiate object *)
				 Multi
				 [Comment "build curCls",
				  New className,
				  Dup,
				  Invokespecial (className,
						 "<init>",
						 ([],
						  [Voidsig]))]
			 (* 2. *)
			 local
			     fun loadFreeVar (stamp'', akku) =
				 let
				     val s'' = Lambda.getLambda stamp''
				 in
				     if s'' = curCls then
					 if s'' <> thisFun then
					     Aload thisFun ::
					     stampCode (s'', upperCls) ::
					     Putfield (classNameFromStamp thisFun^"/"^
						       (fieldNameFromStamp s''),
						       [Classsig CVal]) ::
					     akku
					 else akku
				     else
					 Dup ::
					 stampCode (s'', upperCls) ::
					 Putfield (className^"/"^
						   (fieldNameFromStamp s''),
						   [Classsig CVal]) ::
					 akku
				 end
			 in
			     val loadVars =
				 StampSet.fold loadFreeVar nil freeVarList
			 end
		     in
			 Lambda.markForPickling (thisFun, upperCls);
			 expCodeClass (lambda, thisFun, curCls);
			 object :: loadVars
		     end

	  | expCode (RecExp(_, nil),_,_) =
		     [Getstatic (CConstants^"/dmlunit", [Classsig CName])]

	  | expCode (RecExp(_,labid),_,_) = createRecord (labid, nil)

	  | expCode (LitExp(_,lit'),_,_) =
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

	  | expCode (TupExp(_,longids), _, curCls) = createTuple (longids, nil, curCls)

	  | expCode (VarExp(_,id'), _, curCls) =
		     [idCode (id', curCls)]

	  | expCode (AdjExp _, _, _) = Crash.crash "seltsame operation adjexp"

	  | expCode (SelExp(_,lab'),_,_) =
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

	  | expCode (ConExp (_, id', _), _, curCls) =
			  [Comment "ConExp",
			   idCode (id', curCls)]

	  | expCode (RefExp _,_,_) =
			  [Getstatic CRef]

	  | expCode (ConAppExp (_, Id (_,stamp', _), idargs), curFun, curCls) =
			  Comment "ConAppExp:" ::
			  invokeRecApply (stamp', idargs, curFun, false, curCls)

	  | expCode (RefAppExp (_, idargs), curFun, curCls) =
			  New CReference ::
			  Dup ::
			  idArgCode
			  (idargs,
			   curCls,
			   [Invokespecial (CReference, "<init>",
					   ([Classsig CVal], [Voidsig]))])

	  | expCode (SelAppExp (_, label', id'), _, curCls) =
			  let
			      val afterthrow = Label.new ()
			  in
			      idCode (id', curCls) ::
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
	and

	    expCodeClass ((OneArg id', body')::specialApplies, curFun, curCls) =
	    let
		val _ = if !VERBOSE >=1 then
		    print ("create Class "^classNameFromStamp curFun^"\n")
			else ()
		val freeVarList = FreeVars.getVars curFun
		val _ = Lambda.setId (parm1Stamp, id')
		val _ = FreeVars.setFun (id', curCls)

		(* generate fields for the free variables of the function *)
		fun fields (stamp'', akku) =
		    let
			val s'' = Lambda.getLambda stamp''
		    in
			if s'' = curFun
			    then akku
			else
			    Field ([FPublic],fieldNameFromStamp s'', [Classsig CVal]) ::
			    akku
		    end

		val fieldscode = StampSet.fold fields nil freeVarList

		fun createApplies ((t as TupArgs ids, body'')::rest,
				   a0, a2, a3, a4, ra) =
		    let
			val l = length ids
			fun setId (Id (_,stamp',_)::rest, name'::rest') =
			    (Lambda.setId (stamp', name');
			     setId (rest, rest'))
			  | setId (_,_) = ()
			val b'' = if l = 0 orelse (l>=2 andalso l<=4)
				      then
					  (setId (Vector.sub (parmIds, l), ids);
					   decListCode (body'', curFun, curFun))
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
				 [TestStm (dummyInfo,
					   parm1Id,
					   TupTest ids, body'', ra)])
		    end
		  | createApplies ((t as RecArgs labids, body'') :: rest,
				   a0, a2, a3, a4, ra) =
		     createApplies (rest, a0, a2, a3, a4,
				    [TestStm (dummyInfo, parm1Id,
					      RecTest labids,
					      body'', ra)])
		  | createApplies (nil, a0, a2, a3, a4, ra) =
		     (print ("Lambda "^Stamp.toString curFun^" in "^Stamp.toString curCls);
		      (a0, a2, a3, a4, decListCode (ra, curFun, curCls)))
		  | createApplies (_, _, _, _, _, _) = raise Mitch

		val (ap0, ap2, ap3, ap4, ad) =
		    createApplies (List.rev specialApplies,
				   nil, nil, nil, nil, body')

		fun buildSpecialApply (count, spec) =
		    case spec of
			nil => Nop
		      | _ => let
				 val elselabel = Label.new ()
				 val tupNo = CTuple^(Int.toString count)
				 val s = Stamp.new ()
				 fun gets (act, to') =
					 Invokevirtual
					 (tupNo, "get"^Int.toString act,
					  ([], [Classsig CVal])) ::
					 (if act = to' then
					      nil
					  else
					      Aload s ::
					      gets (act+1, to'))
			     in
				 Multi
				 (Aload parm1Stamp ::
				  Instanceof tupNo ::
				  Ifeq elselabel ::
				  Aload thisStamp ::
				  Aload parm1Stamp ::
				  Checkcast tupNo ::
				  Dup ::
				  Astore s ::
				  Multi (gets (0, count-1)) ::
				  Invokeinterface
				  (CVal,
				   applyName count,
				   (valList count,
				    [Classsig CVal])) ::
				  normalReturn
				  ([Areturn,
				    Label elselabel],
				   curFun))
			     end

		val defaultApply =
		    (case ap0 of
			 nil => Nop
		       | _ => let
				  val elselabel=Label.new()
			      in
				  Multi (stampCode (curFun, curCls) ::
					 Getstatic CUnit ::
					 Ifacmpne elselabel ::
					 Multi ap0 ::
					 normalReturn
					 ([Areturn,
					   Label elselabel],
					  curCls))
			      end) ::
			 buildSpecialApply(2, ap2) ::
			 buildSpecialApply(3, ap3) ::
			 buildSpecialApply(4, ap4) ::
			 ad

		fun addDebugInfo d =
		    if !DEBUG >=2 then
			let
			    val nodebug=Label.new()
			in
			    case d of
				nil => d
			      | _ => Getstatic
				    (classNameFromStamp curCls^"/DEBUG",
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
					 (classNameFromStamp curFun)) ::
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
		    Multi
		    (addDebugInfo defaultApply) ::
		    normalReturn
		    ([Areturn], curCls)

		fun parmLoad nil = nil
		  | parmLoad (Id (_,s',_)::rest) =
		    Aload s' ::
		    parmLoad rest

		fun makeApplyMethod (parms, insts) =
		    let
			val ta = TupArgs (Vector.sub (parmIds, parms))

			val body' =
			    if Lambda.isInRecApply (curFun, parms) then
				(Lambda.addToRecApply
				 (insts, curFun, parms);
				 normalReturn
				 (Comment "makeApplyMethod:" ::
				  invokeRecApply (curFun, ta, curFun, true, curFun),
				  curCls))
			    else
				(case insts of
				     nil =>
					 Multi (Comment "makeApplyMethod2:" ::
						invokeRecApply (curFun, ta, curFun, true, curFun)) ::
					 normalReturn ([Areturn], curCls)
				   | _ => insts)
		    in
			Method ([MPublic],
				applyName parms,
				(valList parms, [Classsig CVal]),
				body')
		    end

		(* normal apply methods *)
		val (applY, apply0, apply2, apply3, apply4, recApply) =
		    (makeApplyMethod (1,ap),
		     makeApplyMethod (0,addDebugInfo ap0),
		     makeApplyMethod (2,addDebugInfo ap2),
		     makeApplyMethod (3,addDebugInfo ap3),
		     makeApplyMethod (4,addDebugInfo ap4),
		     Lambda.buildRecApply curFun)

		(* default constructor *)
		val init = Method ([MPublic],"<init>",([], [Voidsig]),
				   [Aload thisStamp,
				    Invokespecial (CFcnClosure, "<init>",
						   ([], [Voidsig])),
				    Return])

		(* the whole class *)
		val class = Class([CPublic],
				  classNameFromStamp curFun,
				  CFcnClosure,
				  nil,
				  fieldscode,
				  [init, applY, apply0, apply2, apply3, apply4, recApply])
	    in
		classToJasmin (class)
	    end

	and
	    compile prog = genComponentCode (0,0,2,"Emil", imperatifyString prog)
	and
	    compileFile (f, optimize) = genComponentCode (0,0,optimize,"Emil", imperatifyFile f)
    end
