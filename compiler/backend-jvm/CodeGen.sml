structure CodeGen : CodeGen =
    struct

	open JVMInst
	open AST
	open ToJasmin
	open StackNeed
	open Abbrev

	(* falls was böses passiert, wird eine Error-exception mit sinnvollem Inhalt 'geraist' *)
	exception Error of string

	(* Labelzähler, aNewLabel liefert einen neuen String "label?", ? ist Zahl *)
	local
	    val labelcount = ref 0
	    val stack : (int list) ref= ref nil
	in
	    val aNewLabel = fn () =>
		(labelcount := !labelcount + 1;
		 "label"^Int.toString(!labelcount))
	    val pushLabel = fn () =>
		(stack := (!labelcount :: (!stack));
		 labelcount := 0)
	    val popLabel = fn () =>
		case (!stack) of
		    x::xs => (stack := xs;
			      labelcount := x)
		  | nil => raise Error "pop on empty labelstack"
	end

	structure Reuse =
	    struct
		(* Verwaltung der lokalen wiederverwertbaren Variablen.
		 Wir merken uns den maximalen Wert, welche als nächste frei ist, usw.
		 Auf einem Stack werden verschachtelte Lambdas verwurschtelt *)
		local
		    val localscount = ref 0
		    val maxlocals   = ref 0
		    val stack : ((int * int) list) ref = ref nil
		in
		    val rec
			nextFreeLocal = fn () => (localscount := !localscount - 1;
						  if !localscount < !maxlocals then maxlocals := !localscount else ();
						      !localscount)
		    and
			dropLocals = fn (x) => (localscount := !localscount + x;
						if !localscount > 0 then raise Error("localscount > 0") else ())
		    and
			pushLocals = fn () => ( stack := (!localscount,!maxlocals)::(!stack);
					       localscount := 0;
					       maxlocals   := 0)
		    and
			popLocals = fn () =>
			case !stack of
			    ((lc,ml)::rest) => (stack := rest; localscount := lc; maxlocals := ml)
			  | nil => raise Error("empty locals stack")
		    and
			maxLocals = fn () => ~(! maxlocals)
		end
	    end

	structure Persistent =
	    struct
		(* Die lokalen Variablen, die man nicht wiederverwenden kann. *)
		local
		    val localscount = ref 1
		    val stack : (int list) ref = ref nil
		in
		    val rec
			nextFreeLocal = fn () => (localscount := !localscount + 1;
						  !localscount)
		    and
			pushLocals = fn () => ( stack := (!localscount)::(!stack);
					       localscount := 1)
		    and
			popLocals = fn () =>
			case !stack of
			    ((lc)::rest) => (stack := rest; localscount := lc)
			  | nil => raise Error("empty locals stack")
		    and
			maxLocals = fn () => ! localscount
		end
	    end

	(* Wie lautet die aktuelle Klasse, in der wir sind? Stack weil verschachtelte Lambdas. *)
	local
	    val stack = ref [""]
	    val initial = ref ""
	in
	    val rec
		getCurrentClass = fn () => case !stack of (x::xs) => x | _ => raise Error("getCurrentClass kapuutt")
	    and
		pushClass = fn name => stack := name::(!stack)
	    and
		popClass  = fn () =>  case !stack of (x::xs) => stack := xs | _ => raise Error("popClass kapuutt")
	    and
		setInitialClass = fn name => ((stack := [name]); initial := name)
	    and
		getInitialClass = fn () => (!initial)
	end

	(* Just another mad function *)
	val rec flatten = fn x::xs => x@flatten(xs) | nil => nil

	(* Einstiegspunkt *)
	val rec genProgramCode = fn (name,Dec dec) =>
	    (setInitialClass name;
	     let
		 val insts = decCode dec
		 val stack = stackneeddec dec
		 val mapply = Method([MPublic],"mapply",([Classsig CVal],Classsig CVal),
				     Limits(Persistent.maxLocals()+1,Reuse.maxLocals(),stack),
				     insts @
				     [Aload 0,
				      Aload 1,
				      Invokevirtual (getInitialClass(), "apply",([Classsig CVal],Classsig CVal)),
				      Pop,
				      Return])
	     in
		schreibsDran(getInitialClass(),methodToJasmin mapply )
	     end
	     )
	and
	    decListCode = fn decs : DEC list => flatten (map decCode decs)

	and
	    decCode =
	    fn Local (localdecs, decs) =>
	    decListCode(localdecs)@decListCode(decs)

	     | Valbind patexplist =>
	    let
		val faillabel  = aNewLabel()
		val endlabel   = aNewLabel()
		val patExpCode =
		    fn (pat : PAT, exp : EXP) =>
		    let
			val expcode = expCode(exp);
			val patcode = patCode(pat)
		    in
			expcode@patcode@((Ifeq faillabel)::nil)
		    end
		val part1 = flatten (map patExpCode patexplist)
		val part2 = [Goto endlabel,
			     Label faillabel,
			     New CException0,
			     Dup,
			     Getstatic CBind,
			     Invokespecial (CException0, "<init>",([Classsig "ExName"], Voidsig)),
			     Athrow,
			     Label endlabel]
	    in
		part1 @ part2
	    end
	     | Andrec patexplist =>
	    let
		val initClosure = fn
		    (Patvid(Shortvid(_, Defining loc)), Fn(JVMString name,_,_)) =>
			[New name,
			 Dup,
			 Invokespecial (name,"<init>",([],Voidsig)),
			 Astore (loc:=Persistent.nextFreeLocal(); !loc)]
		  | _ => raise Error "Andrec initClosure"
		val iC = flatten (map initClosure patexplist)
	    in
		iC @ decCode (Valbind patexplist)
	    end
	and
	    expCode =
	    fn (Andalso(exp1,exp2)) =>
	    let
		val truelabel  = aNewLabel()
		val falselabel = aNewLabel()
		val e1 = expCode(exp1)
		val e2 = expCode(exp2)
	    in
		e1 @ [
		      Invokeinterface (CVal, "request",([],Classsig CVal)),
		      Dup,
		      Getstatic (CConstants^"/dmlfalse",CConstructor0),
		      Ifacmpeq falselabel,
		      Getstatic (CConstants^"/dmltrue",CConstructor0),
		      Ifacmpeq truelabel,
		      New CException0,
		      Dup,
		      Getstatic CMatch,
		      Invokespecial (CException0,"<init>",([Classsig CExName], Voidsig)),
		      Athrow,
		      Label truelabel]
		@ e2 @ [Label falselabel]
	    end

	     | (Appl (exp1,exp2)) =>
	    let
		val coname     = aNewLabel()
		val exname     = aNewLabel()
		val builtin    = aNewLabel()
		val errorlabel = aNewLabel()
		val endlabel   = aNewLabel()
		val exp        = expCode(exp1)
		val atexp      = expCode(exp2)
		val h          = Reuse.nextFreeLocal()
		val i          = Reuse.nextFreeLocal()
		val insts = exp @ atexp @
		    [Comment "[ apply",
		     Astore h,
		     Invokeinterface (CVal, "request", ([],Classsig CVal)),
		     Dup,
		     Invokeinterface (CVal, "whatAreYou", (nil, Intsig)),
		     Tableswitch (0,
				  [
				   coname,
				   exname,
				   builtin
				   ],
				  errorlabel),
		     Comment "error",
		     Label errorlabel,
		     New CInternalError,
		     Dup,
		     Ldc (JVMString "PANIC"),
		     Invokespecial (CInternalError, "<init>", ([Classsig CString],Voidsig)),
		     Athrow,
		     Comment "coname",
		     Label coname,
		     Checkcast CCoName,
		     Astore i,
		     New CConstructor1,
		     Dup,
		     Aload i,
		     Aload h,
		     Invokespecial (CConstructor1, "<init>", ([Classsig CCoName, Classsig CVal], Voidsig)),
		     Goto endlabel,
		     Comment "exname",
		     Label exname,
		     Checkcast CExName,
		     Astore i,
		     New CException1,
		     Dup,
		     Aload i,
		     Aload h,
		     Invokespecial (CException1, "<init>", ([Classsig CExName, Classsig CVal], Voidsig)),
		     Goto endlabel,
		     Label builtin,
		     Aload h,
		     Invokeinterface (CVal, "apply", ([Classsig CVal],Classsig CVal)),
		     Label endlabel,
		     Comment "end of apply ]"
		     ]
	    in
		Reuse.dropLocals(2); insts
	    end

	     | (Case (exp, match)) =>
	    [Comment "[ CASE EXP"] @
	    expCode(exp) @
	    [Comment "CASE EXP ] [CASE MATCH"] @
	    matchCode(match) @
	    [Comment "CASE MATCH ]"]

	     | (Explist liste) =>
	    let
		val rec
		    eiter =
		    fn exp::nil => expCode(exp)
		     | (exp::exps) => expCode(exp)@[Pop]@eiter(exps) | _ => raise Error "eiter"
	    in
		eiter liste
	    end

	     | (lambda as Fn (JVMString name,freevars,match)) =>
	    let
		val names = flatten (map Load freevars)
		val rec vals = fn (fx :: fxs) => (Classsig CVal)::(vals fxs) | nil => nil
		val i = vals(freevars)
		val result = [Comment ("{ lambda "^name),
			      New name,
			      Dup] @
		    names @
		    [Invokespecial (name, "<init>", (i, Voidsig)),
		     Comment ("end of lambda "^name^"}")]
	    in
		pushLabel();
		Reuse.pushLocals();
		Persistent.pushLocals();
		pushClass(name);
		expCodeClass(lambda);
		Reuse.popLocals();
		Persistent.popLocals();
		popClass();
		popLabel();
		result
	    end

	     | Handle(exp, match) =>
	    let
		val e     = expCode(exp)
		val m     = matchCode(match)
		val try   = aNewLabel()
		val catch = aNewLabel()
	    in
		[Catch (CException, try, catch, catch),
		 Catch (CRuntimeError, try, catch, catch),
		 Label try] @
		e @
		[Label catch] @
		m
	    end

	     | If(exp1,exp2,exp3) =>
	    let
		val e1 = expCode(exp1)
		val e2 = expCode(exp2)
		val e3 = expCode(exp3)
		val truelabel  = aNewLabel()
		val falselabel = aNewLabel()
		val endiflabel = aNewLabel()
	    in
		[Comment "[ IFBED"] @
		e1 @
		[Comment "IFBED ] [ IFTEST",
		 Invokeinterface (CVal, "request", (nil, Classsig CVal)),
		 Dup,
		 Getstatic (CConstants^"/dmltrue",CConstructor0),
		 Ifacmpeq truelabel,
		 Getstatic (CConstants^"/dmlfalse",CConstructor0),
		 Ifacmpeq falselabel,
		 New CException0,
		 Dup,
		 Getstatic CMatch,
		 Invokespecial (CException0,"<init>", ([Classsig CExName], Voidsig)),
		 Athrow,
		 Label truelabel,
		 Pop,
		 Comment "IFTEST ]",
		 Comment "[ IFCONS" ]@
		e2 @
		[Comment "IFCONS ]",
		 Goto endiflabel,
		 Label falselabel,
		 Comment "[ IFALT"]@
		e3 @
		[Comment "IFALT ]",
		 Label endiflabel]
	    end

	     | Let(declist,exp) =>
	    decListCode(declist)@expCode(exp)

	     | Orelse(exp1,exp2) =>
	    let
		val truelabel  = aNewLabel()
		val falselabel = aNewLabel()
		val e1 = expCode(exp1)
		val e2 = expCode(exp2)
	    in
		e1 @ [
		      Invokeinterface (CVal, "request", (nil, Classsig CVal)),
		      Dup,
		      Getstatic (CConstants^"/dmltrue",CConstructor0),
		      Ifacmpeq truelabel,
		      Getstatic (CConstants^"/dmlfalse",CConstructor0),
		      Ifacmpeq falselabel,
		      New CException0,
		      Dup,
		      Getstatic CMatch,
		      Invokespecial (CException0,"<init>", ([Classsig CExName], Voidsig)),
		      Athrow,
		      Label falselabel]
		@ e2 @ [Label truelabel]
	    end

	     | Raise(exp) =>
	    let
		val e = expCode(exp)
		val noexception = aNewLabel()
	    in
		e @
		[Dup,
		 Instanceof CException,
		 Ifeq noexception,
		 Checkcast CException,
		 Athrow,
		 Label noexception,
		 New CRuntimeError,
		 Dup,
		 Ldc (JVMString "cannot raise expression"),
		 Invokespecial (CRuntimeError,"<init>",([Classsig CString],Voidsig)),
		 Athrow]
	    end

	     | Record(Arity 0, _) =>
	    [Getstatic (CConstants^"/dmlunit", CConstructor0)]

	     | Record(Arity arity, reclablist) =>
	    let
		val rec
		    labelcode =
		    fn (RecStringlabel label,index) =>
		    [Dup,
		     atCodeInt(index),
		     New CLabel,
		     Dup,
		     Ldc (JVMString label),
		     Invokespecial (CLabel,"<init>",([Classsig CString], Voidsig)),
		     Aastore]
		     | (RecIntlabel label,index) =>
		    [Dup,
		     atCodeInt(index),
		     New CLabel,
		     Dup,
		     atCodeInt(label),
		     Invokespecial (CLabel,"<init>",([Intsig], Voidsig)),
		     Aastore]
		val rec
		    labeliter = fn ((l,_)::rest,i) => labelcode (l,i) @ labeliter(rest,i+1)
		  | (nil,_) => nil
		val rec
		    labexpiter = fn ((_,e)::rest,i) => [Dup, atCodeInt(i)] @ expCode(e) @ [Aastore] @ labexpiter(rest, i+1)
		  | (nil,_) => nil
	    in
		[Comment "[ Record",
		 New CRecord,
		 Dup,
		 atCodeInt(arity),
		 Anewarray CLabel] @
		labeliter(reclablist,0) @
		[atCodeInt(arity),
		 Anewarray CVal] @
		labexpiter(reclablist,0) @
		[Invokespecial (CRecord,"<init>",([Arraysig, Classsig CLabel, Arraysig, Classsig CVal],Voidsig)),
		 Comment "Record ]"]
	    end

	     | SCon(scon) =>
	    let
		val F = case scon of
		    CHARscon c   => atCodeInt(ord c)
		  | INTscon i    => atCodeInt i
		  | REALscon r   => if (Real.sign (r-0.0)=0)
			orelse (Real.sign(r-1.0)=0)
			orelse (Real.sign (r-2.0)=0) then Fconst (trunc r) else Ldc (JVMFloat r)
		  | STRINGscon s => Ldc (JVMString s)
		  | WORDscon w   => Ldc (JVMString "XXX")
		val jtype = case scon of
		    CHARscon c   => ([Intsig],Voidsig)
		  | INTscon i    => ([Intsig],Voidsig)
		  | REALscon r   => ([Floatsig],Voidsig)
		  | STRINGscon s => ([Classsig CString], Voidsig)
		  | WORDscon w   => ([Intsig],Voidsig)
		and skon = case scon of
		    CHARscon c   => CInt
		  | INTscon i    => CInt
		  | REALscon r   => CReal
		  | STRINGscon s => CStr
		  | WORDscon w   => CInt
	    in
		[Comment "constant(",
		 New skon,
		 Dup,
		 F,
		 Invokespecial (skon,"<init>",jtype),
		 Comment "end of constant)"
		 ]
	    end

	     | VId(Shortvid(vidname, Defining loc)) => (
							loc  := Persistent.nextFreeLocal();
							[Comment ("Defining Constant "^vidname^"(no code)")])

	     | VId(Shortvid(_,Bound b)) => (case !b of
						Shortvid (_,Defining wherever) =>
						    [Aload (!wherever), Comment "Bound VId"]
					      | _ => raise Error "invalid vid")
	     | VId(Shortvid(vidname,Free)) =>
	    let
		val currentClass = getCurrentClass()
	    in
		if vidname = currentClass
		    then
			[Aload 0,
			 Comment "selbstlader"]
		else
		    [Aload 0,
		     Getfield (currentClass^"/"^vidname, CVal),
		     Comment ("Free VId "^vidname)]
	    end
	     | VId(Primitive(which)) =>
	    (case which of
		 "+" => [Getstatic CPlus]
	       | "=" => [Getstatic CEquals]
	       | "not" => [Getstatic CNot]
	       | "!" => [Getstatic CDeref]
	       | ":=" => [Getstatic CAssign]
	       | _ => raise Error "unimplemented primitive")

	     | While(exp1,exp2) =>
		 let
		     val beforelabel = aNewLabel()
		     val truelabel   = aNewLabel()
		     val falselabel  = aNewLabel()
		     val i = Reuse.nextFreeLocal()
		     val h = Reuse.nextFreeLocal()
		     val e1 = expCode(exp1)
		     val e2 = expCode(exp2)
		     val code =
			 [Comment "[while bed",
			  Getstatic (CConstants^"/dmlunit",CConstructor0),
			  Astore h,
			  Label beforelabel] @
			 e1 @
			 [Comment "while bed ] [while typtest",
			  Invokeinterface (CVal,"request",(nil,Classsig CVal)),
			  Astore i,
			  Aload i,
			  Getstatic (CConstants^"/dmltrue",CConstructor0),
			  Ifacmpeq truelabel,
			  Aload i,
			  Getstatic (CConstants^"/dmlfalse",CConstructor0),
			  Ifacmpeq falselabel,
			  New CException0,
			  Dup,
			  Getstatic CMatch,
			  Invokespecial (CException0,"<init>",([Classsig CExName],Voidsig)),
			  Athrow,
			  Label truelabel,
			  Comment "while typtest] [while body"] @
			 e2 @
			 [Astore h,
			  Comment "while body]",
			  Goto beforelabel,
			  Label falselabel,
			  Aload h]
		 in
		     Reuse.dropLocals(2); code
		 end

	     | _ => raise Error "Fußschuß"

	and
	    patCode = fn
	    Patas (vid, pat) => let
				    val p = patCode(pat)
				    val store=
					(case vid of (Shortvid(_, Defining loc)) => (loc  := Persistent.nextFreeLocal();
										     [Astore (!loc)])
				      | _ => raise Error "patas undefining crash")
				in
				    Dup::store@p
				end

	  | Patcon(vid, pat) =>
				let
				    val h = Reuse.nextFreeLocal()
				    val endlabel = aNewLabel()
				    val faillabel = aNewLabel()
				    val p = patCode(pat)
				    val l= case vid of
					Shortvid(_, Bound (ref to)) => (case to of
									    Shortvid (_, Defining loc) => [Aload (!loc)]
									  | _ => raise Error "patcon")
				      | Shortvid(vidname, Free) => [Aload 0, Getfield (getCurrentClass()^"/"^vidname, CVal)]
				      | _ => raise Error "vidname not using in patcon"
				    val code =
					[Comment "[patcon",
					 Invokeinterface (CVal, "request", (nil, Classsig CVal)),
					 Astore h,
					 Aload h,
					 Instanceof CConstructor1,
					 Ifeq faillabel,
					 Aload h,
					 Checkcast CConstructor1,
					 Astore h,
					 Aload h,
					 Getfield (CConstructor^"/name", CString)] @
					l@
					[Checkcast CConstructor1,
					 Getfield (CConstructor^"/name", CString),
					 Invokevirtual (CString, "equals", ([Classsig CObj],Boolsig)),
					 Ifeq faillabel,
					 Aload h,
					 Invokevirtual (CConstructor1, "getContent", (nil, Classsig CVal))] @
					p @
					[Goto endlabel,
					 Label faillabel,
					 Iconst 0,
					 Label endlabel,
					 Comment "patcon ]"]
				in
				    Reuse.dropLocals(1); code
				end

	  | Patex(vid, pat) =>
				let
				    val h = Reuse.nextFreeLocal()
				    val endlabel = aNewLabel()
				    val faillabel = aNewLabel()
				    val p = patCode(pat)
				    val l= case vid of
					Shortvid(_, Bound (ref to)) => (case to of
									    Shortvid (_, Defining loc) => [Aload (!loc)]
									  | _ => raise Error "Patex")
				      | Shortvid(vidname, Free) => [Aload 0, Getfield (getCurrentClass()^"/"^vidname, CVal)]
				      | _ => raise Error "vidname not using in Patex"
				    val code =
					[Comment "[Patex",
					 Invokeinterface (CVal, "request", (nil, Classsig CVal)),
					 Astore h,
					 Aload h,
					 Instanceof CException1,
					 Ifeq faillabel,
					 Aload h,
					 Checkcast CException1,
					 Astore h,
					 Aload h,
					 Getfield (CException^"/name", CString)] @
					l@
					[Checkcast CException1,
					 Getfield (CException^"/name", CString),
					 Invokevirtual (CString, "equals", ([Classsig CObj],Boolsig)),
					 Ifeq faillabel,
					 Aload h,
					 Invokevirtual (CException1, "getContent", (nil, Classsig CVal))] @
					p @
					[Goto endlabel,
					 Label faillabel,
					 Iconst 0,
					 Label endlabel,
					 Comment "Patex ]"]
				in
				    Reuse.dropLocals(1); code
				end

	  | Patopenrec reclabs =>
				let
				    val faillabel = aNewLabel()
				    val endlabel = aNewLabel()
				    val loc = Reuse.nextFreeLocal()
				    val prc = patRowCode(reclabs, loc)
				    val code =
					[Comment "[ patopenrec",
					 Dup,
					 Instanceof CRecord,
					 Ifeq faillabel,
					 Checkcast CRecord,
					 Astore loc] @
					prc @
					[Goto endlabel,
					 Label faillabel,
					 Pop,
					 Iconst 0,
					 Label endlabel,
					 Comment "patopenrec ]"]
				in
				    Reuse.dropLocals(1); code
				end

	  | Patrec reclabs =>
				let
				    val loc = Reuse.nextFreeLocal()
				    val faillabel = aNewLabel()
				    val endlabel = aNewLabel()
				    val rec bauLabels =
					fn ((RecStringlabel reclab,pat)::reclabs,k) =>
					[Dup,
					 Iconst k,
					 New CLabel,
					 Dup,
					 Ldc (JVMString reclab),
					 Invokespecial (CLabel,"<init>",([Classsig CString], Voidsig)),
					 Aastore] @
					(bauLabels (reclabs,k+1))
					 | ((RecIntlabel reclab,pat)::reclabs,k) =>
					[Dup,
					 Iconst k,
					 New CLabel,
					 Dup,
					 Iconst reclab,
					 Invokespecial (CLabel,"<init>",([Intsig], Voidsig)),
					 Aastore] @
					(bauLabels (reclabs,k+1))
					 | (nil,_) => nil
				    val code =
					[Comment "[ patrec",
					 Dup,
					 Astore loc,
					 Instanceof CRecord,
					 Ifeq faillabel,
					 Aload loc,
					 Checkcast CRecord,
					 Astore loc,
					 Aload loc,
					 Invokevirtual (CRecord, "getRecordArity",([],Classsig CRecordArity)),
					 New CRecordArity,
					 Dup,
					 Iconst (length reclabs),
					 Anewarray CLabel] @
					(bauLabels (reclabs,0)) @
					[Invokespecial (CRecordArity,"<init>",([Arraysig, Classsig CLabel], Voidsig)),
					 Invokestatic (CRecord,"getRecordArity",([Classsig CRecordArity], Classsig CRecordArity)),
					 Ifacmpne faillabel] @
					patRowCode(reclabs, loc) @
					[Goto endlabel,
					 Label faillabel,
					 Iconst 0,
					 Label endlabel,
					 Comment "patrec ]"]
				in
				    (Reuse.dropLocals(1); code)
				end
    
	  | Patscon (scon) =>
				[Invokeinterface (CVal, "request", ([], Classsig CVal))] @
				expCode(SCon scon) @
				[Invokeinterface (CVal, "equals", ([Classsig CObj], Boolsig))]

	  | Patvid (Shortvid vid) =>
				(case vid of
				     (_, Defining loc) => (loc := Persistent.nextFreeLocal();
							   [Astore (!loc), Iconst 1])
				   | (_, Bound def) => (case !def of
							    Shortvid (_,Defining loc) =>
								[Aload (!loc),
								 Invokeinterface (CVal, "equals", ([Classsig CObj], Boolsig))]
							  | _ => raise Error "patvid bound def")
				   | (_, Free) => raise Error "patvid free"
					 )
	  | Patwild => [Iconst 1]
	  | _ => raise Error "patCode wasimmer"

	and
	    patRowCode = fn
	    (labpat::reclabs,i) =>
		let
		    val patRowCodeSingle = fn
			((lab,pat), i) =>
			    let
				val undef = aNewLabel()
				val endlabel = aNewLabel()
				val p = patCode(pat)
				val l = case lab of RecStringlabel s => s | RecIntlabel k => Int.toString(k)
			    in
				[Aload i,
				 Ldc (JVMString l),
				 Invokevirtual (CRecord, "getByLabel", ([Classsig CString], Classsig CVal)),
				 Dup,
				 Ifnull undef] @
				p @
				[Goto endlabel,
				 Label undef,
				 Pop,
				 Iconst 0,
				 Label endlabel]
			    end
		    val skiplabel = aNewLabel()
		    val head = patRowCodeSingle(labpat,i)
		    val prc = patRowCode(reclabs,i)
		in
		    head @
		    [Dup,
		     Ifeq skiplabel,
		     Pop] @
		    prc @
		    [Label skiplabel]
		end

	  | (nil, _) => [Iconst 1]

	and
	    matchCode =
	    fn Mrule patexplist =>
	    let
		val endlabel = aNewLabel()
		val speicherMich = Reuse.nextFreeLocal()
		val ruleCode =
		    fn (pat,exp) =>
		    let
			val eigenerendlabel = aNewLabel()
			val p = patCode(pat)
			val e = expCode(exp)
		    in
			[Comment "[ Rule",
			 Aload speicherMich] @
			p @
			[Dup,
			 Ifeq eigenerendlabel] @
			e @
			[Astore speicherMich,
			 Label eigenerendlabel,
			 Ifneq endlabel,
			 Comment "Rule ]"]
		    end
		val rc = flatten (map ruleCode patexplist)
		val code =
		    [Comment "[ MRule",
		     Astore speicherMich] @
		    rc @
		    [New CException0,
		     Dup,
		     Getstatic CMatch,
		     Invokespecial (CException0,"<init>", ([Classsig CExName], Voidsig)),
		     Athrow,
		     Label endlabel,
		     Aload speicherMich,
		     Comment "MRule ]"]
	    in
		Reuse.dropLocals(1); code
	    end

	and
	    Load = fn (JVMString name) => [Aload 0, Getfield(getCurrentClass()^"/"^name,CVal)]
	  | (JVMInt i) => [Aload i, Comment ("Load loc. Var")]
	  | _ => raise Error("cannot load scrap")

	and
	    expCodeClass =
	    fn Fn (JVMString name,freevars,match) =>
	    let
		val rec fields = fn
		    (JVMString var)::vars =>
			(Field ([FPrivate],var, Classtype CVal))::(fields(vars))
		  | nil => nil
		  | _ => raise Error "fields in expcodeclass"
		val fieldlist = fields freevars
		val rec args = fn _::vars => (Classsig CVal)::args(vars) | nil => nil
		local
		    val rec closureBody = fn
			(nil,_) => [Return]
		      | ((JVMString var)::nil,i) =>
			    [Aload i,
			     Putfield (name^"/"^var, CVal),
			     Return]
		      | ((JVMString var)::vars,i) =>
			    [Dup,
			     Aload i,
			     Putfield (name^"/"^var, CVal)]@
			    closureBody(vars,i+1)
		      | _ => raise Error "closureBody"
		    val k = length freevars
		in
		    val makeClosure = Method([MPublic],"makeClosure",(args(freevars), Voidsig),Limits (k,0, 3),
					     (if k=0
						  then [Aload 0]
					      else [(Aload 0),Dup])@
						  closureBody(freevars,1))
		end
		val mcm = matchCode match
		val rec
		    initializeLocals = fn
		    0 => nil
		  | x => [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		val iL = initializeLocals (Persistent.maxLocals())
		val stack = stackneed match + 1
		val main = if name="main"
			       then
				   let
				       val dreizehn = aNewLabel()
				       val zweiE = aNewLabel()
				       val (eins,zwei,drei) = (1,2,3)
				   in
				       [Method ([MPublic, MStatic],"main",([Arraysig,Classsig CString], Voidsig),
					       Limits (4,0,7),
					       [New (getInitialClass()),
						Dup,
						Invokespecial (getInitialClass(), "<init>",([], Voidsig)),
						Dup,
						Invokevirtual (getInitialClass(), "makeClosure",(args freevars,Voidsig)),
						Comment "[String nach DMLRecord",
						Iconst 0,
						Istore eins,
						Aload 0,
						Arraylength,
						Anewarray CLabel,
						Astore zwei,
						Aload 0,
						Arraylength,
						Anewarray CVal,
						Astore drei,
						Iconst 0,
						Istore eins,
						Goto zweiE,
						Label dreizehn,
						Aload zwei,
						Iload eins,
						New CLabel,
						Dup,
						Iload eins,
						Invokespecial (CLabel,"<init>",([Intsig], Voidsig)),
						Aastore,
						Aload drei,
						Iload eins,
						New CStr,
						Dup,
						Aload 0,
						Iload eins,
						Aaload,
						Invokespecial (CStr,"<init>",([Classsig CString], Voidsig)),
						Aastore,
						Iinc (eins,1),
						Iload eins,
						Aload 0,
						Arraylength,
						Ificmplt dreizehn,
						New CRecord,
						Dup,
						Aload zwei,
						Aload drei,
						Invokespecial (CRecord, "<init>", ([Arraysig, Classsig CLabel, Arraysig, Classsig CVal],Voidsig)),
						Invokevirtual (getInitialClass(), "mapply",([Classsig CVal],Classsig CVal)),
						Return])]
				   end
			   else
			       nil
		val applY =Method ([MPublic],"apply",([Classsig CVal], Classsig CVal),
				   Limits (Persistent.maxLocals()+1,Reuse.maxLocals(),stack),
				   (Aload 1) :: iL @ mcm @ [Areturn])
	    in
		schreibs(if name="main" then getInitialClass() else name,
			     classToJasmin(Class([CPublic],name,CFcnClosure,fieldlist,
						 [makeClosure,applY]@main)))
	    end

	     | _ => raise Error "expCodeClass"

	and
	    atCodeInt =
	    fn i =>
	    if i >= ~1 andalso i<=5 then Iconst i else
		if i >= ~128 andalso i <= 127 then Bipush i else
		    if i >= ~32768 andalso i <= 32767 then Sipush i
		    else Ldc (JVMInt i)
    end
