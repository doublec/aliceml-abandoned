structure CodeGen =
    struct

	open JVMInst
	open AST
	open ToJasmin
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

	(* Einstiegspunkt *)
	val rec genProgramCode = fn (name,Dec dec) =>
	    (setInitialClass name;
	     let
		 val insts = decCode dec
		 val stack = 100 (*stackneeddec dec*)
		 val rec
		     initializeLocals = fn
		     0 => nil
		   | x => [Aconst_null, Astore (x+1)]@(initializeLocals (x-1))
		 val iL = initializeLocals (Persistent.maxLocals())

		 val mapply = Method([MPublic],"mapply",([Classsig CVal],Classsig CVal),
				     Limits(Persistent.maxLocals()+1,Reuse.maxLocals(),stack),
				     iL @
				     insts @
				     [Aload 0,
				      Aload 1,
				      Invokevirtual (getInitialClass(), "apply",([Classsig CVal],Classsig CVal)),
				      Areturn])
	     in
		 schreibsDran(getInitialClass()^".j",methodToJasmin mapply )(*; print (Int.toString (Reuse.maxLocals()))*)
	     end
	     )
	and
	    decListCode = fn decs : DEC list => List.concat (map decCode decs)

	and
	    decCode =
	    fn Valbind patexplist =>
	    let
		val faillabel  = aNewLabel()
		val endlabel   = aNewLabel()
		val patExpCode =
		    fn (pat : PAT, exp : EXP) =>
		    let
			val expcode = expCode(exp);
			val needRequest = (case exp of
					       Fn _ => false
					     | SCon _ => false
					     | _ => true)
			val patcode = patCode(pat,needRequest)
		    in
			expcode@patcode@((Ifeq faillabel)::nil)
		    end
		val part1 = List.concat (map patExpCode patexplist)
		(* val _ = print (instructionsToJasmin part1) *)
		val part2 = [Goto endlabel,
			     Label faillabel,
			     New CCoEx0,
			     Dup,
			     Getstatic CBind,
			     Invokespecial (CCoEx0, "<init>",([Classsig CCoExName], Voidsig)),
			     Athrow,
			     Label endlabel]
	    in
		part1 @ part2
	    end

	     | Condec (Shortvid (name, Defining loc), i) =>
	    (if !loc = ~1 then loc:=Persistent.nextFreeLocal() else ();
		 let
		     val ar = (if i=0 then 0 else 1)
		 in
		     [New CCoExName,
		      Dup,
		      Ldc (JVMString name),
		      Iconst ar,
		      Invokespecial (CCoExName, "<init>", ([Classsig CString, Intsig],Voidsig)),
		      Astore (!loc)]
		 end)

	     | Declist declist => decListCode declist

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
		      Invokevirtual (CVal, "request",([],Classsig CVal)),
		      Dup,
		      Getstatic (CConstants^"/dmlfalse",CCoEx0),
		      Ifacmpeq falselabel,
		      Getstatic (CConstants^"/dmltrue",CCoEx0),
		      Ifacmpeq truelabel,
		      New CCoEx0,
		      Dup,
		      Getstatic CMatch,
		      Invokespecial (CCoEx0,"<init>",([Classsig CCoExName], Voidsig)),
		      Athrow,
		      Label truelabel]
		@ e2 @ [Label falselabel]
	    end

	     | Appl (exp1,exp2) =>
	    let
		val exp        = expCode(exp1)
		val atexp      = expCode(exp2)
	    in
		[Comment "[ apply"]
		@ exp @
		[Invokevirtual (CVal, "request", ([],Classsig CVal))]
		@ atexp @
		[Invokevirtual (CVal, "apply", ([Classsig CVal],Classsig CVal)),
		 Comment "end of apply ]"
		 ]
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

	     | (lambda as Fn (Shortvid(name, _),freevars,match)) =>
	    let
		val realname = if name="main" then getInitialClass () else name
		val Load = fn
		    Shortvid(name, Free) => (
					     (*  					     print (name^" : Free\n"); *)
					     [Aload 0, Getfield(getCurrentClass()^"/"^name,CVal)])
		  | Shortvid(name, Bound (ref (Shortvid(n, Defining loc)))) =>
			(
			  			 (*print ("Halligalli"^name^"  : Bound to : "^Int.toString(!loc)^"\n"); *)
			 [Aload (!loc), Comment ("Load loc. Var")])
		  | _ => raise Error("cannot load scrap")

		val names = List.concat (map Load freevars)
		val rec vals = fn _::fxs => (Classsig CVal)::(vals fxs) | nil => nil
		val i = vals freevars
		val result = if name="main"
				 then
				     [Comment "WILDES THIS",
				      Aload 0,
				      Dup] @
				     names @
				     [Invokevirtual (realname, "makeClosure", (i, Voidsig)),
				      Comment "THIS WILDES"]
			     else
				 [Comment ("{ lambda "^name),
				  New realname,
				  Dup,
				  Invokespecial (realname, "<init>",([], Voidsig)),
				  Dup] @
				 names @
				 [Invokevirtual (realname, "makeClosure", (i, Voidsig)),
				  Comment ("end of lambda "^name^"}")]
	    in
		pushLabel();
		Reuse.pushLocals();
		Persistent.pushLocals();
		pushClass(realname);
		expCodeClass(lambda);
		Reuse.popLocals();
		Persistent.popLocals();
		popClass();
		popLabel();
		result
	    end

	     | Handle(exp, lode) =>
	    let
		val e     = expCode(exp)
		val l     = expCode(lode)
		val try   = aNewLabel()
		val catch = aNewLabel()
		val nocatch = aNewLabel()
	    in
		[Catch (CVal, try, catch, catch),
		 Label try] @
		e @
		[Goto nocatch,
		 Label catch] @
		l @
		[Label nocatch]
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
		 Invokevirtual (CVal, "request", (nil, Classsig CVal)),
		 Dup,
		 Getstatic (CConstants^"/dmltrue",CCoEx0),
		 Ifacmpeq truelabel,
		 Getstatic (CConstants^"/dmlfalse",CCoEx0),
		 Ifacmpeq falselabel,
		 New CCoEx0,
		 Dup,
		 Getstatic CMatch,
		 Invokespecial (CCoEx0,"<init>", ([Classsig CCoExName], Voidsig)),
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
		      Invokevirtual (CVal, "request", (nil, Classsig CVal)),
		      Dup,
		      Getstatic (CConstants^"/dmltrue",CCoEx0),
		      Ifacmpeq truelabel,
		      Getstatic (CConstants^"/dmlfalse",CCoEx0),
		      Ifacmpeq falselabel,
		      New CCoEx0,
		      Dup,
		      Getstatic CMatch,
		      Invokespecial (CCoEx0,"<init>", ([Classsig CCoExName], Voidsig)),
		      Athrow,
		      Label falselabel]
		@ e2 @ [Label truelabel]
	    end

	     | Raise(exp) =>
	    let
		val e = expCode(exp)
	    in
		e @
		 [Athrow]
	    end

	     | Record(Arity 0, _) =>
	    [Getstatic (CConstants^"/dmlunit", CCoEx0)]

(*  	     | Record(Arity arity, reclablist) => *)
(*  	    let *)
(*  		val rec *)
(*  		    labelcode = *)
(*  		    fn (RecStringlabel label,index) => *)
(*  		    [Dup, *)
(*  		     atCodeInt(index), *)
(*  		     New CLabel, *)
(*  		     Dup, *)
(*  		     Ldc (JVMString label), *)
(*  		     Invokespecial (CLabel,"<init>",([Classsig CString], Voidsig)), *)
(*  		     Aastore] *)
(*  		     | (RecIntlabel label,index) => *)
(*  		    [Dup, *)
(*  		     atCodeInt(index), *)
(*  		     New CLabel, *)
(*  		     Dup, *)
(*  		     atCodeInt(label), *)
(*  		     Invokespecial (CLabel,"<init>",([Intsig], Voidsig)), *)
(*  		     Aastore] *)
(*  		val rec *)
(*  		    labeliter = fn ((l,_)::rest,i) => labelcode (l,i) @ labeliter(rest,i+1) *)
(*  		  | (nil,_) => nil *)
(*  		val rec *)
(*  		    labexpiter = fn ((_,e)::rest,i) => [Dup, atCodeInt(i)] @ expCode(e) @ [Aastore] @ labexpiter(rest, i+1) *)
(*  		  | (nil,_) => nil *)
(*  	    in *)
(*  		[Comment "[ Record", *)
(*  		 New CRecord, *)
(*  		 Dup, *)
(*  		 atCodeInt(arity), *)
(*  		 Anewarray CLabel] @ *)
(*  		labeliter(reclablist,0) @ *)
(*  		[atCodeInt(arity), *)
(*  		 Anewarray CVal] @ *)
(*  		labexpiter(reclablist,0) @ *)
(*  		[Invokespecial (CRecord,"<init>",([Arraysig, Classsig CLabel, Arraysig, Classsig CVal],Voidsig)), *)
(*  		 Comment "Record ]"] *)
(*  	    end *)

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
		val rec
		    labeliter = fn ((l,_)::rest,i) => labelcode (l,i) @ labeliter(rest,i+1)
		  | (nil,_) => nil
		val evalexp =
		    fn (l : RECLAB,e) =>
		    let
			val eCode = expCode(e)
			val store = Reuse.nextFreeLocal()
		    in
			(eCode@[Astore store],[(l,store)])
		    end
		val rec
		    evalexps =
		    fn r::rs =>
		    let
			val (insts, reclabsint) = evalexp r
			val (rinsts, rreclabsint) = evalexps rs
		    in
			(insts@rinsts, reclabsint@rreclabsint)
		    end
		     | nil => (nil,nil)
		val (insts,reclabintlist) = evalexps reclablist
		val sortedreclablist = MergeSort.sort reclabintlist
		val labelinsts = labeliter(sortedreclablist,0)
		local
		    val rec load = fn
			((_,i)::rs,j) => Dup::(Iconst j)::(Aload i)::Aastore::(load (rs,j+1))
		      | (nil,_) => nil
		in
		    val loadexps = load (sortedreclablist,0)
		end
		val alles =
		    [Comment "[Record "] @
		    insts@
		    [New CRecord,
		     Dup,
		     atCodeInt(arity),
		     Anewarray CLabel] @
		    labelinsts@
		    [atCodeInt(arity),
		     Anewarray CVal] @
		    loadexps @
		    [Invokespecial (CRecord,"<init>",([Arraysig, Classsig CLabel, Arraysig, Classsig CVal],Voidsig)),
		     Comment "Record ]"]
	    in
		Reuse.dropLocals(arity); alles
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
		  | WORDscon w   => atCodeInt (Word.toInt w)
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

	     | VId(Shortvid(vidname, Defining (loc as ref b))) => (if (!loc)= ~1
								       then
									   (loc:=Persistent.nextFreeLocal()
									    (*  									   ;print (vidname^" now bound to "^Int.toString(b)^"\n") *)
									    )
								   else
								       (*  								       print (vidname^" already bound to "^Int.toString(b)^"\n") *) () ;
								       [Comment ("Defining Constant "^vidname^"(no code)")])

	     | VId(Shortvid(n,Bound b)) =>(case !b of
					       Shortvid (_,Defining (ref wherever)) =>(										         										       (*print (n^" bound to "^Int.toString(wherever)^"\n"); *)
										       [Aload wherever, Comment "Bound VId"])
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
	       | "ref" => [Getstatic CRef]
	       | ":=" => [Getstatic CAssign]
	       | "Int.fromString" => [Getstatic CIntFromString]
	       | "Bool.fromString" => [Getstatic CBoolFromString]
	       | "Real.fromString" => [Getstatic CRealFromString]
	       | _ => raise Error "unimplemented primitive")

	     | While(exp1,exp2) =>
		 let
		     val beforelabel = aNewLabel()
		     val truelabel   = aNewLabel()
		     val falselabel  = aNewLabel()
		     val e1 = expCode(exp1)
		     val e2 = expCode(exp2)
		 in
		     [Comment "[while bed",
		      Label beforelabel] @
		     e1 @
		     [Comment "while bed ] [while typtest",
		      Invokevirtual (CVal,"request",(nil,Classsig CVal)),
		      Dup,
		      Getstatic (CConstants^"/dmltrue",CCoEx0),
		      Ifacmpeq truelabel,
		      Getstatic (CConstants^"/dmlfalse",CCoEx0),
		      Ifacmpeq falselabel,
		      New CCoEx0,
		      Dup,
		      Getstatic CMatch,
		      Invokespecial (CCoEx0,"<init>",([Classsig CCoExName],Voidsig)),
		      Athrow,
		      Iconst 0,
		      Label truelabel,
		      Pop,
		      Comment "while typtest] [while body"] @
		     e2 @
		     [Pop,
		      Comment "while body]",
		      Goto beforelabel,
		      Label falselabel,
		      Getstatic (CConstants^"/dmlunit",CCoEx0)]
		 end

	and
	    patCode = fn
	    (Patas (vid, pat), _) =>
		let
		    val p = patCode(pat, false)
		    val store=
			(case vid of (Shortvid(_, Defining loc)) => (loc  := Persistent.nextFreeLocal();
								     [Astore (!loc)])
		      | _ => raise Error "patas undefining crash")
		in
		    Dup::store@p
		end

	  | (Patcoex(vid, pat), needRequest) =>
		let
		    val endlabel = aNewLabel()
		    val faillabel = aNewLabel()
		    val p = patCode(pat, true)
		    val l= case vid of
			Shortvid(_, Bound (ref to)) => (case to of
							    Shortvid (_, Defining loc) => [Aload (!loc)]
							  | _ => raise Error "Patex")
		      | Shortvid(vidname, Free) => [Aload 0, Getfield (getCurrentClass()^"/"^vidname, CVal)]
		      | _ => raise Error "vidname not using in Patex"
		in
		    ((if needRequest
			 then
			     [Comment "[Patcoex",
			      Invokevirtual (CVal, "request", (nil, Classsig CVal))]
		     else
			 [Comment "[Patcoex"])@
			 [Dup,
			 Instanceof CCoEx1,
			 Ifeq faillabel,
			 Dup,
			 Checkcast CCoEx1,
			 Invokevirtual (CCoEx1,"getCoExName",([],Classsig CCoExName))] @
			 l@
			 [Ifacmpne faillabel,
			 Invokevirtual (CCoEx1, "getContent", (nil, Classsig CVal))] @
			 p @
			 [Goto endlabel,
			  Label faillabel,
			  Iconst 0,
			  Label endlabel,
			  Comment "Patcoex ]"])
		end

	  | (Patopenrec reclabs, needRequest) =>
		let
		    val faillabel = aNewLabel()
		    val endlabel = aNewLabel()
		    val loc = Reuse.nextFreeLocal()
		    val prc = patRowCode(reclabs, loc)
		    val code =
			(if needRequest
			     then
				 [Comment "[ patopenrec",
				  Invokevirtual (CVal, "request", ([],Classsig CVal))]
			 else
			     [Comment "[ patopenrec"])@
			     [Dup,
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

	  | (Patrec reclabs, needRequest) =>
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
			 | (nil,_) => nil
		    val sortedLabs=MergeSort.sort reclabs
		    val code =
			(if needRequest
			     then
				 [Comment "[ patrec",
				  Invokevirtual (CVal, "request", ([],Classsig CVal))]
			 else
			     [Comment "[ patrec"])@
			     [Dup,
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
			     (bauLabels (sortedLabs,0)) @
			     [Invokespecial (CRecordArity,"<init>",([Arraysig, Classsig CLabel], Voidsig)),
			      Invokestatic (CRecord,"getRecordArity",([Classsig CRecordArity], Classsig CRecordArity)),
			      Ifacmpne faillabel] @
			     patRowCode(sortedLabs, loc) @
			     [Goto endlabel,
			      Label faillabel,
			      Iconst 0,
			      Label endlabel,
			      Comment "patrec ]"]
		in
		    (Reuse.dropLocals(1); code)
		end

	  | (Patscon scon, needRequest) =>
		(if needRequest then [Comment "[patscon",Invokevirtual (CVal, "request", ([], Classsig CVal))]
		 else [Comment "[patscon"])@
		     expCode(SCon scon) @
		     [Invokevirtual (CObj, "equals", ([Classsig CObj], Boolsig))]

	  | (Patvid (Shortvid vid), _) =>
		     (case vid of
			  (n, Defining loc) => (loc := Persistent.nextFreeLocal();
						[Astore (!loc), Iconst 1])
			| (_, Bound def) => (case !def of
						 Shortvid (_,Defining loc) =>
						     [Aload (!loc),
						      Invokevirtual (CObj, "equals", ([Classsig CObj], Boolsig))]
					       | _ => raise Error "patvid bound def")
			| (_, Free) => raise Error "patvid free"
			      )
	  | (Patwild, _) => [Pop, Iconst 1]
	  | _ => raise Error "patCode wasimmer"

	and
	    patRowCode = fn
	    (labpat::reclabs,i) =>
		let
		    val patRowCodeSingle = fn
			((RecStringlabel lab,pat), i) =>
			    let
				val undef = aNewLabel()
				val endlabel = aNewLabel()
				val p = patCode(pat,true)
				val l = []
			    in
				[Aload i,
				 Ldc (JVMString lab),
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
		    fn needRequest => fn (pat,exp) =>
		    let
			val eigenerendlabel = aNewLabel()
			val p = patCode(pat,needRequest)
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
		val rc = (case patexplist of
			      nil => raise Error "Mrule empty patexplist"
			    | patex::nil => ruleCode true patex
			    | patex::rest => (ruleCode true patex)
				  @List.concat (map (ruleCode false) patexplist)
				  )
		val code =
		    [Comment "[ MRule",
		     Astore speicherMich] @
		    rc @
		    [New CCoEx0,
		     Dup,
		     Getstatic CMatch,
		     Invokespecial (CCoEx0,"<init>", ([Classsig CCoExName], Voidsig)),
		     Athrow,
		     Label endlabel,
		     Aload speicherMich,
		     Comment "MRule ]"]
	    in
		Reuse.dropLocals(1); code
	    end

	and
	    expCodeClass =
	    fn Fn (Shortvid (name, _),freevars,match) =>
	    let
		val realname = if name="main" then getInitialClass() else name
		val rec fields = fn
		    (Shortvid(var,_))::vars =>
			(Field ([FPrivate],var, Classtype CVal))::(fields(vars))
		  | nil => nil
		  | _ => raise Error "fields in expcodeclass"
		val fieldlist = fields freevars
		val rec args = fn _::vars => (Classsig CVal)::args(vars) | nil => nil
		local
		    val rec closureBody = fn
			(nil,_) => [Return]
		      | (Shortvid(var,_)::nil,i) =>
			    [Aload i,
			     Putfield (realname^"/"^var, CVal),
			     Return]
		      | (Shortvid(var,_)::vars,i) =>
			    [Dup,
			     Aload i,
			     Putfield (realname^"/"^var, CVal)]@
			    closureBody(vars,i+1)
		      | _ => raise Error "closureBody"
		    val k = length freevars
		in
		    val makeClosure = Method([MPublic],"makeClosure",(args(freevars), Voidsig),Limits (k,0, 4),
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
		val stack = 100 (*stackneed match + 1*)
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
						 Iconst 1,
						 Iadd,
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
						 Label zweiE,
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
						 Invokevirtual (CVal,"toString",([],Classsig CString)),
						 Getstatic ("java/lang/System/out", "java/io/PrintStream"),
						 Swap,
						 Invokevirtual ("java.io.PrintStream","println",([Classsig CString],Voidsig)),

						 Return])]
				   end
			   else
			       nil
		val applY =Method ([MPublic],"apply",([Classsig CVal], Classsig CVal),
				   Limits (Persistent.maxLocals()+1,Reuse.maxLocals(),stack),
				   (Aload 1) :: iL @ mcm @ [Areturn])
		val init = Method ([MPublic],"<init>",([], Voidsig), Limits (1,0,1),
				   [Aload 0, Invokespecial (CFcnClosure, "<init>",([], Voidsig)),Return])
	    in
		schreibs(realname^".j",
			 classToJasmin(Class([CPublic],realname,
					     CFcnClosure,fieldlist,
					     [makeClosure,applY, init]@main)))
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
