local
    val labelcount = ref 0
in
    val aNewLabel = fn () =>
	(labelcount := !labelcount + 1;
	 "label"^Int.toString(!labelcount))
end

exception Error of string

local
    val localscount = ref 1 (* in 0 ist this, in 1 ist das Argument *)
    and maxlocals   = ref 1
    and stack : ((int * int) list) ref = ref nil
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
    and maxLocals = fn () => ! maxlocals
end

local
    val stack = ref ["Drinstehnmuesst"]
in
    val rec
	getCurrentClass = fn () => case !stack of (x::xs) => x | _ => raise Error("getCurrentClass kapuutt")
    and
	pushClass = fn (name) => stack := name::(!stack)
    and
	popClass  = fn () =>  case !stack of (x::xs) => stack := xs | _ => raise Error("popClass kapuutt")
end

val rec flatten = fn x::xs => x@flatten(xs) | nil => nil

val rec genProgramCode = fn (Dec dec) => decCode (dec)
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
		     Invokespecial (CException0, "<init>","("^CExName^")V"),
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
	      Invokevirtual (CVal, "request","()"^CVal),
	      Dup,
	      Getstatic (CConstants^"/dmlfalse",CConstructor0),
	      Ifacmp falselabel,
	      Getstatic (CConstants^"/dmltrue",CConstructor0),
	      Ifacmp truelabel,
	      New CException0,
	      Dup,
	      Getstatic CMatch,
	      Invokespecial (CException0,"<init>","("^CExName^")V"),
	      Athrow,
	      Label truelabel]
	@ e2 @ [Label falselabel]
    end
     | (Appl (exp1,exp2)) =>
	let
	    val coname     = aNewLabel()
	    and exname     = aNewLabel()
	    and builtin    = aNewLabel()
	    and errorlabel = aNewLabel()
	    and endlabel   = aNewLabel()
	    and exp        = expCode(exp1)
	    and atexp      = expCode(exp2)
	    and h          = nextFreeLocal()
	    and i          = nextFreeLocal()

	    val insts = exp @ atexp @
		[
		 Astore h,
		 Invokevirtual (CVal, "request", "()"^CVal),
		 Dup,
		 Invokevirtual (CVal, "whatAreYou", "()I"),
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
		 Invokespecial (CInternalError, "<init>", "("^CString^")V"),
		 Athrow,
		 Label coname,
		 Checkcast CCoName,
		 Astore i,
		 New CConstructor1,
		 Dup,
		 Aload i,
		 Aload h,
		 Invokespecial (CConstructor1, "<init>", "("^CCoName^CVal^")V"),
		 Goto endlabel,
		 Label exname,
		 Checkcast CExName,
		 Astore i,
		 New CException1,
		 Dup,
		 Aload i,
		 Aload h,
		 Invokespecial (CException1, "<init>", "("^CExName^CVal^")V"),
		 Goto endlabel,
		 Label builtin,
		 Aload h,
		 Invokevirtual (CVal, "apply", "("^CVal^")"^CVal),
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
		eiter = fn exp::nil => expCode(exp) | (exp::exps) => expCode(exp)@[Pop]@eiter(exps) | _ => raise Error "eiter"
	in
	    eiter liste
	end
     | (lambda as Fn (JVMString name,freevars,match)) =>
	let
	    val names = flatten (map Load freevars)
	    val rec vals = fn (fx :: fxs) => CVal^(vals(fxs)) | nil => ""
	    val istring = "("^vals(freevars)^")V"
	    val result = [New name, Dup] @ names @ [Invokespecial (name, "<init>", istring)]
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
	    and m     = matchCode(match)
	    and try   = aNewLabel()
	    and catch = aNewLabel()
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
	    and e2 = expCode(exp2)
	    and e3 = expCode(exp3)
	    and truelabel  = aNewLabel()
	    and falselabel = aNewLabel()
	    and endiflabel = aNewLabel()
	in
	    e1@
	    [Invokevirtual (CVal, "request", "()"^CVal),
	     Dup,
	     Getstatic (CConstants^"/dmltrue",CConstructor0),
	     Ifacmp truelabel,
	     Getstatic (CConstants^"/dmlfalse",CConstructor0),
	     Ifacmp falselabel,
	     New CException0,
	     Dup,
	     Getstatic CMatch,
	     Invokespecial (CException0,"<init>","("^CExName^")V"),
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
		  Invokevirtual (CVal, "request","()"^CVal),
		  Dup,
		  Getstatic (CConstants^"/dmltrue",CConstructor0),
		  Ifacmp truelabel,
		  Getstatic (CConstants^"/dmlfalse",CConstructor0),
		  Ifacmp falselabel,
		  New CException0,
		  Dup,
		  Getstatic CMatch,
		  Invokespecial (CException0,"<init>","("^CExName^")V"),
		  Athrow,
		  Label falselabel]
	    @ e2 @ [Label truelabel]
	end
    | Raise(exp) =>
	let
	    val e = expCode(exp)
	    and noexception = aNewLabel()
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
	     Invokespecial (CRuntimeError,"<init>","("^CString^")V"),
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
		 Invokespecial (CLabel,"<init>","("^CString^")V"),
		 Aastore]
		 | (RecIntlabel label,index) =>
		[Dup,
		 atCodeInt(index),
		 New CLabel,
		 Dup,
		 atCodeInt(label),
		 Invokespecial (CLabel,"<init>","(I)V"),
		 Aastore]
	    and
		labeliter = fn ((l,_)::rest,i) => labelcode (l,i) @ labeliter(rest,i+1)
	      | (nil,_) => nil
	    and
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
	    [Invokespecial (CRecord,"<init>","(["^CLabel^"["^CVal^")V")]
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
	    and jtype = case scon of
		CHARscon c   => "(I)V"
	      | INTscon i    =>  "(I)V"
	      | REALscon r   => "(F)V"
	      | STRINGscon s => "("^CString^")V"
	      | WORDscon w   => "(I)V"
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
    | While(exp1,exp2) =>
	let
	    val beforelabel = aNewLabel()
	    and truelabel   = aNewLabel()
	    and falselabel  = aNewLabel()
	    and e1 = expCode(exp1)
	    and e2 = expCode(exp2)
	in
	    [Label beforelabel] @
	    e1 @
	    [Invokevirtual (CVal,"request","()"^CVal),
	     Dup,
	     Getstatic (CConstants^"/dmltrue",CConstructor0),
	     Ifacmp truelabel,
	     Getstatic (CConstants^"/dmlfalse",CConstructor0),
	     Ifacmp falselabel,
	     New CException0,
	     Dup,
	     Getstatic CMatch,
	     Invokespecial (CException0,"<init>","("^CExName^")V"),
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
	    and endlabel = aNewLabel()
	    and p = patCode(pat)
	in
	    [Invokevirtual (CVal, "request", "()"^CVal),
	     Dup,
	     Instanceof CConstructor1,
	     Ifeq faillabel,
	     Checkcast CConstructor1,
	     Dup,
	     Getfield (CConstructor0^"/name",CString),
	     Ldc (JVMString vidname),
	     Invokevirtual (CString, "equals", "("^CString^")I"),
	     Ifeq faillabel,
	     Invokevirtual (CVal, "getContent", "()"^CVal)] @
	    p @
	    [Goto endlabel,
	     Label faillabel,
	     Pop,
	     Label endlabel]
	end
  | Patex(Shortvid(vidname, Bound loc), pat) =>
	let
	    val faillabel = aNewLabel()
	    and endlabel = aNewLabel()
	    and p = patCode(pat)
	in
	    [Invokevirtual (CVal, "request", "()"^CVal),
	     Dup,
	     Instanceof CException1,
	     Ifeq faillabel,
	     Checkcast CException1,
	     Dup,
	     Getfield (CException0^"/name",CString),
	     Ldc (JVMString vidname),
	     Invokevirtual (CString, "equals", "("^CString^")I"),
	     Ifeq faillabel,
	     Invokevirtual (CVal, "getContent", "()"^CVal)] @
	    p @
	    [Goto endlabel,
	     Label faillabel,
	     Pop,
	     Label endlabel]
	end
  | Patopenrec(reclabs) =>
	let
	    val faillabel = aNewLabel()
	    and endlabel = aNewLabel()
	    and loc = nextFreeLocal()
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
	[Invokevirtual (CVal, "request", "()"^CVal)] @
	expCode(SCon scon) @
	[Invokevirtual (CVal, "equals", "("^CVal^")I")]
  | Patvid (Shortvid vid) =>
	(case vid of
	     (_, Defining loc) => (loc := nextFreeLocal();
				   [Astore (!loc), Iconst 1])
	   | (_, Bound def) => (case !def of
				    Shortvid (_,Defining loc) =>
					[Aload (!loc),
					 Invokevirtual (CVal, "equals", "("^CVal^")I")]
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
			and endlabel = aNewLabel()
			and p = patCode(pat)
			and l = case lab of RecStringlabel s => s | RecIntlabel k => Int.toString(k)
		    in
			[Aload i,
			 Ldc (JVMString l),
			 Invokevirtual (CRecord, "getByLabel", "("^CString^")"^CVal),
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
	    and head = patRowCodeSingle(labpat,i)
	    and prc = patRowCode(reclabs,i)
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
	 Invokespecial (CException0,"<init>","("^CExName^")V"),
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
	val rec args = fn _::vars => CVal^args(vars) | nil => ""
	val rec initbody = fn
	    ((JVMString var)::nil,i) =>
		[Aload i,
		 Putfield (name^"/"^var, CVal),
		 Return]
	  | ((JVMString var)::vars,i) =>
		[Dup,
		 Aload i,
		 Putfield (name^"/"^var, CVal)]@
		initbody(vars,i+1)
	  | _ => raise Error "initbody"
	val init = Method([MPublic],"<init>","("^args(freevars)^")V",Limits (length freevars, 3),(Aload 0)::initbody(freevars,1))
	val mcm = matchCode match
	val stack = raise Error "Hallo"
	val applY = Method ([MPublic],"apply","("^CVal^")"^CVal,Limits (maxLocals(),stack),(Aload 1) :: (mcm @ [Areturn]))
    in
	Class(access,name,CFcnClosure,fieldlist,[init,applY])
    end
     | _ => raise Error "expCodeClass"
and
    atCodeInt =
    fn i =>
    if i >= ~1 andalso i<=5 then Iconst i else
	if i >= ~128 andalso i <= 127 then Bipush i else
	    if i >= ~32768 andalso i <= 32767 then Sipush i
	    else Ldc (JVMInt i)
