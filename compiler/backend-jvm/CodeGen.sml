(* Labelzähler, aNewLabel liefert einen neuen String "label?", ? ist Zahl *)
local
    val labelcount = ref 0
in
    val aNewLabel = fn () =>
	(labelcount := !labelcount + 1;
	 "label"^Int.toString(!labelcount))
end

(* falls was böses passiert, wird eine Error-exception mit sinnvollem Inhalt 'geraist' *)
exception Error of string

(* Verwaltung der lokalen Variablen. Wir merken uns den maximalen Wert, welche als nächste frei ist, usw.
 Auf einem Stack werden verschachtelte Lambdas verwurschtelt *)
local
    val localscount = ref 1 (* in 0 ist this, in 1 ist das Argument *)
    val maxlocals   = ref 1
    val stack : ((int * int) list) ref = ref nil
in
    val rec
	nextFreeLocal = fn () => (localscount := !localscount + 1;
				  if !localscount > !maxlocals then maxlocals := !localscount else ();
				     !localscount)
    and
	dropLocals = fn (x) => (localscount := !localscount - x;
				if !localscount < 1 then raise Error("localscount < 1") else ())
    and
	pushLocals = fn () => ( stack := (!localscount,!maxlocals)::(!stack);
			       localscount := 1;
			       maxlocals   := 1)
    and
	popLocals = fn () =>
	case !stack of
	    ((lc,ml)::rest) => (stack := rest; localscount := lc; maxlocals := ml)
	  | nil => raise Error("empty locals stack")
    and
	maxLocals = fn () => ! maxlocals
end

(* Wie lautet die aktuelle Klasse, in der wir sind? Stack weil verschachtelte Lambdas. *)
local
    val stack = ref [""]
in
    val rec
	getCurrentClass = fn () => case !stack of (x::xs) => x | _ => raise Error("getCurrentClass kapuutt")
    and
	pushClass = fn name => stack := name::(!stack)
    and
	popClass  = fn () =>  case !stack of (x::xs) => stack := xs | _ => raise Error("popClass kapuutt")
    and
	initialClass = fn name => stack := [name]
end

(* Just another mad function *)
val rec flatten = fn x::xs => x@flatten(xs) | nil => nil

(* Einstiegspunkt *)
val rec genProgramCode = fn (name,Dec dec) => (initialClass name; decCode (dec))

and
    decListCode = fn decs : DEC list => flatten (map decCode decs)

and
    decCode =
    fn (Local (localdecs, decs)) =>
    decListCode(localdecs)@decListCode(decs)

     | (Valbind(patexplist)) =>
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

and
    expCode=
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
	      Getstatic (CConstants^"/dmlfalse",CConstructor0),
	      Ifacmp falselabel,
	      Getstatic (CConstants^"/dmltrue",CConstructor0),
	      Ifacmp truelabel,
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
	    val h          = nextFreeLocal()
	    val i          = nextFreeLocal()
	    val insts = exp @ atexp @
		[
		 Astore h,
		 Invokevirtual (CVal, "request", ([],Classsig CVal)),
		 Dup,
		 Invokevirtual (CVal, "whatAreYou", (nil, Intsig)),
		 Tableswitch (0,
			      [
			       coname,
			       exname,
			       builtin
			       ],
			      errorlabel),
		 Label errorlabel,
		 New CInternalError,
		 Dup,
		 Ldc (JVMString "PANIC"),
		 Invokespecial (CInternalError, "<init>", ([Classsig CString],Voidsig)),
		 Athrow,
		 Label coname,
		 Checkcast CCoName,
		 Astore i,
		 New CConstructor1,
		 Dup,
		 Aload i,
		 Aload h,
		 Invokespecial (CConstructor1, "<init>", ([Classsig CCoName, Classsig CVal], Voidsig)),
		 Goto endlabel,
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
		 Invokevirtual (CVal, "apply", ([Classsig CVal],Classsig CVal)),
		 Label endlabel
		 ]
	in
	    ( dropLocals(2); insts)
	end

     | (Case (exp, match)) =>
	expCode(exp) @ matchCode(match)

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
	    val result = [New name, Dup] @ names @ [Invokespecial (name, "<init>", (i, Voidsig))]
	in
	    (
	     pushLocals();
	     pushClass(name);
	     expCodeClass(lambda);
	     popLocals();
	     popClass();
	     result)
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
	    e1@
	    [Invokevirtual (CVal, "request", (nil, Classsig CVal)),
	     Dup,
	     Getstatic (CConstants^"/dmltrue",CConstructor0),
	     Ifacmp truelabel,
	     Getstatic (CConstants^"/dmlfalse",CConstructor0),
	     Ifacmp falselabel,
	     New CException0,
	     Dup,
	     Getstatic CMatch,
	     Invokespecial (CException0,"<init>", ([Classsig CExName], Voidsig)),
	     Athrow,
	     Label truelabel,
	     Pop]@
	    e2 @
	    [Goto endiflabel,
	     Label falselabel]@
	    e3 @
	    [Label endiflabel]
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
		  Getstatic (CConstants^"/dmltrue",CConstructor0),
		  Ifacmp truelabel,
		  Getstatic (CConstants^"/dmlfalse",CConstructor0),
		  Ifacmp falselabel,
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
	    [New CRecord,
	     Dup,
	     atCodeInt(arity-1),
	     Anewarray CLabel] @
	    labeliter(reclablist,0) @
	    [atCodeInt(arity-1),
	     Anewarray CVal] @
	    labexpiter(reclablist,0) @
	    [Invokespecial (CRecord,"<init>",([Arraysig, Classsig CLabel, Arraysig, Classsig CVal],Voidsig))]
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
	    [New skon,
	     Dup,
	     F,
	     Invokespecial (skon,"<init>",jtype)]
	end

     | VId(Shortvid(vidname, Defining loc)) => (
				      loc  := nextFreeLocal();
				      nil)

     | VId(Shortvid(_,Bound b)) => (case !b of
					Shortvid (_,Defining wherever) =>
					    [Aload (!wherever)]
				      | _ => raise Error "invalid vid")
     | VId(Shortvid(vidname,Free)) =>
	[Aload 0,
	 Getfield (getCurrentClass()^vidname, CVal)]
     | VId(Primitive(which)) =>
	(case which of
	     "+" => [Getstatic CPlus]
	   | _ => raise Error "unimplemented primitive")
     | While(exp1,exp2) =>
	let
	    val beforelabel = aNewLabel()
	    val truelabel   = aNewLabel()
	    val falselabel  = aNewLabel()
	    val e1 = expCode(exp1)
	    val e2 = expCode(exp2)
	in
	    [Label beforelabel] @
	    e1 @
	    [Invokevirtual (CVal,"request",(nil,Classsig CVal)),
	     Dup,
	     Getstatic (CConstants^"/dmltrue",CConstructor0),
	     Ifacmp truelabel,
	     Getstatic (CConstants^"/dmlfalse",CConstructor0),
	     Ifacmp falselabel,
	     New CException0,
	     Dup,
	     Getstatic CMatch,
	     Invokespecial (CException0,"<init>",([Classsig CExName],Voidsig)),
	     Athrow,
	     Label truelabel,
	     Pop] @
	    e2 @
	    [Goto beforelabel,
	     Label falselabel]
	end

     | _ => raise Error "Fußschuß"

and
    patCode = fn
    Patas(Shortvid(_, Defining loc), pat) =>
	(loc := nextFreeLocal();
	 [Dup,
	  Astore (!loc)] @
	 patCode(pat))

  | Patcon(Shortvid(vidname, Bound loc), pat) =>
	let
	    val faillabel = aNewLabel()
	    val endlabel = aNewLabel()
	    val p = patCode(pat)
	in
	    [Invokevirtual (CVal, "request", (nil, Classsig CVal)),
	     Dup,
	     Instanceof CConstructor1,
	     Ifeq faillabel,
	     Checkcast CConstructor1,
	     Dup,
	     Getfield (CConstructor0^"/name",CString),
	     Ldc (JVMString vidname),
	     Invokevirtual (CString, "equals", ([Classsig CString],Intsig)),
	     Ifeq faillabel,
	     Invokevirtual (CVal, "getContent", (nil, Classsig CVal))] @
	    p @
	    [Goto endlabel,
	     Label faillabel,
	     Pop,
	     Label endlabel]
	end

  | Patex(Shortvid(vidname, Bound loc), pat) =>
	let
	    val faillabel = aNewLabel()
	    val endlabel = aNewLabel()
	    val p = patCode(pat)
	in
	    [Invokevirtual (CVal, "request", (nil, Classsig CVal)),
	     Dup,
	     Instanceof CException1,
	     Ifeq faillabel,
	     Checkcast CException1,
	     Dup,
	     Getfield (CException0^"/name",CString),
	     Ldc (JVMString vidname),
	     Invokevirtual (CString, "equals", ([Classsig CString], Intsig)),
	     Ifeq faillabel,
	     Invokevirtual (CVal, "getContent", ([], Classsig CVal))] @
	    p @
	    [Goto endlabel,
	     Label faillabel,
	     Pop,
	     Label endlabel]
	end

  | Patopenrec(reclabs) =>
	let
	    val faillabel = aNewLabel()
	    val endlabel = aNewLabel()
	    val loc = nextFreeLocal()
	    val prc = patRowCode(reclabs, loc)
	in
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
	     Label endlabel]
	end

  | Patrec _ => raise Error "not yet understood"

  | Patscon (scon) =>
	[Invokevirtual (CVal, "request", ([], Classsig CVal))] @
	expCode(SCon scon) @
	[Invokevirtual (CVal, "equals", ([Classsig CVal], Intsig))]

  | Patvid (Shortvid vid) =>
	(case vid of
	     (_, Defining loc) => (loc := nextFreeLocal();
				   [Astore (!loc), Iconst 1])
	   | (_, Bound def) => (case !def of
				    Shortvid (_,Defining loc) =>
					[Aload (!loc),
					 Invokevirtual (CVal, "equals", ([Classsig CVal], Intsig))]
				  | _ => raise Error "patvid bound def")
	   | (_, Free) => raise Error "patvid free"
		 )

  | _ => raise Error "patCode Patas"

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
	val ruleCode =
	    fn (pat,exp) =>
	    let
		val eigenerendlabel = aNewLabel()
		val p = patCode(pat)
		val e = expCode(exp)
	    in
		p @
		[Dup,
		 Ifeq eigenerendlabel] @
		e @
		[Swap,
		 Label eigenerendlabel,
		 Ifneq endlabel]
	    end
	val rc = flatten (map ruleCode patexplist)
    in
	rc @
	[New CException0,
	 Dup,
	 Getstatic CMatch,
	 Invokespecial (CException0,"<init>", ([Classsig CExName], Voidsig)),
	 Athrow,
	 Label endlabel]
    end

and
    Load = fn (JVMString name) => [Aload 0, Getfield ( getCurrentClass(), name)]
  | (JVMInt i) => [Aload i]
  | _ => raise Error("cannot load scrap")

and
    expCodeClass =
    fn Fn (JVMString name,freevars,match) =>
    let
	val access = [CPublic]
	val rec fields = fn
	    (JVMString var)::vars =>
		(Field ([FPrivate],var, Classtype CVal))::(fields(vars))
	  | nil => nil
	  | _ => raise Error "fields in expcodeclass"
	val fieldlist = fields freevars
	val rec args = fn _::vars => (Classsig CVal)::args(vars) | nil => nil
	val rec initbody = fn
	    (nil,_) => nil
	  | ((JVMString var)::nil,i) =>
		[Aload i,
		 Putfield (name^"/"^var, CVal),
		 Return]
	  | ((JVMString var)::vars,i) =>
		[Dup,
		 Aload i,
		 Putfield (name^"/"^var, CVal)]@
		initbody(vars,i+1)
	  | _ => raise Error "initbody"
	val init = Method([MPublic],"<init>",(args(freevars), Voidsig),Limits (length freevars, 3),(Aload 0)::initbody(freevars,1))
	val mcm = matchCode match
	val stack = stackneed match
	val applY = Method ([MPublic],"apply",([Classsig CVal], Classsig CVal),Limits (maxLocals(),stack),(Aload 1) :: (mcm @ [Areturn]))
    in
	schreibs(name,classToJasmin(Class(access,name,CFcnClosure,fieldlist,[init,applY])))
    end

     | _ => raise Error "expCodeClass"

and
    atCodeInt =
    fn i =>
    if i >= ~1 andalso i<=5 then Iconst i else
	if i >= ~128 andalso i <= 127 then Bipush i else
	    if i >= ~32768 andalso i <= 32767 then Sipush i
	    else Ldc (JVMInt i)
