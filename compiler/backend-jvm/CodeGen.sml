local
    val labelcount = ref 0
in
    val aNewLabel = fn () =>
	(labelcount := !labelcount + 1;
	 "label"^Int.toString(!labelcount))
end

exception Error of string

local
    val localscount = ref 0
    and maxlocals   = ref 0
    and stack : ((int * int) list) ref = ref nil
in
    val rec
	nextFreeLocal = fn () => (localscount := !localscount + 1;
				  if !localscount > !maxlocals then maxlocals := !localscount else ();
				     !localscount)
    and
	dropLocals = fn (x) => (localscount := !localscount - x;
				if !localscount < 0 then raise Error("localscount < 0") else ())
    and
	pushLocals = fn () => ( stack := (!localscount,!maxlocals)::(!stack);
			       localscount := 0;
			       maxlocals   := 0)
    and
	popLocals = fn () =>
	case !stack of
	    ((lc,ml)::rest) => (stack := rest; localscount := lc; maxlocals := ml)
	  | nil => raise Error("empty locals stack")
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
	flatten (map expCode liste)
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
	     ExpCodeClass(lambda);
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
    | VId(vid) => []
and
    patCode= fn (_) => Swap::nil
and
    matchCode = fn (_) => Dup::nil
and
    Load = fn (JVMString name) => [Aload 0, Getfield ( getCurrentClass(), name)]
  | (JVMInt i) => [Aload i]
  | _ => raise Error("cannot load crap")
and
    ExpCodeClass = fn (_) => ()
and
    getCurrentClass = fn () => "Hier bin ich"
and
    atCodeInt = fn (_) => Pop
