local
    val labelcount = ref 0
in
    val aNewLabel = fn () =>
	(labelcount := !labelcount + 1;
	 "label"^Int.toString(!labelcount))
end
exception Error of string;

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
	val faillabel = aNewLabel()
	val endlabel = aNewLabel()
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
		     New "FQN Exception0",
		     Dup,
		     Getstatic ("FQN Bind","[oje]"),
		     Invokespecial ("FQN Exception0", "<init>","(FQN Exname)V"),
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
	      Invokevirtual ("Val", "request","()Val"),
	      Dup,
	      Getstatic ("Constants","dmlfalse"),
	      Ifacmp falselabel,
	      Getstatic ("Constants","dmltrue"),
	      Ifacmp truelabel,
	      New "Exception0",
	      Dup,
	      Getstatic ("Match","Match"),
	      Invokespecial ("Exception0","<init>","(ExName)V"),
	      Athrow,
	      Label truelabel]
	@ e2 @ [Label falselabel]
    end
     | (Appl (exp1,exp2) =>
	let
	    val coname     = aNewLabel()
	    and exname     = aNewLabel()
	    and builtin    = aNewlabel()
	    and errorlabel = aNewlabel()
		
     
and
    patCode= fn (_) => Swap::nil
