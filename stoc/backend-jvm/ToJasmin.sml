structure ToJasmin =
    struct

	open JVMInst

	val intToString = fn i => if i<0 then "-"^Int.toString(~i) else Int.toString i
	    
	local
	    val linecount = ref 0
	in
	    val line = fn () => (linecount := !linecount + 1; Int.toString(!linecount))
	end

	val rec
	    cAccessToString =
	    fn CPublic::rest => "public "^cAccessToString rest
	     | CFinal::rest  => "final "^cAccessToString rest
	     | CSuper::rest => "super "^cAccessToString rest
	     | CAbstract::rest => "abstract "^cAccessToString rest
	     | CInterface::rest => "interface "^cAccessToString rest
	     | nil => ""
	and
	    fAccessToString =
	    fn FPublic::rest    =>    "public "^fAccessToString rest
	     | FPrivate::rest   =>   "private "^fAccessToString rest
	     | FProtected::rest => "protected "^fAccessToString rest
	     | FStatic::rest    => "static "^fAccessToString rest
	     | FFinal::rest     =>     "final "^fAccessToString rest
	     | FVolatile::rest  =>  "volatile "^fAccessToString rest
	     | FTransient::rest =>  "transient "^fAccessToString rest
	     | nil => ""
	and
	    mAccessToString =
	    fn MPublic::rest        =>    "public "^mAccessToString rest
	     | MPrivate::rest       =>   "private "^mAccessToString rest
	     | MProtected::rest     => "protected "^mAccessToString rest
	     | MStatic::rest        => "static "^mAccessToString rest
	     | MFinal::rest         =>     "final "^mAccessToString rest
	     | MSynchronized::rest  =>  "synchronized "^mAccessToString rest
	     | MNative::rest        =>  "native "^mAccessToString rest
	     | MAbstract::rest      =>  "abstract "^mAccessToString rest
	     | nil => ""

	val perslocals = ref 0

	val descriptor2string = let
				    val rec desclist2string =
					fn (Classsig dl)::dls => "L"^dl^";"^(desclist2string dls)
					 | Intsig::dls => "I"^(desclist2string dls)
					 | Boolsig::dls => "Z"^(desclist2string dls)
					 | Voidsig::dls => "V"^(desclist2string dls)
					 | Floatsig::dls => "F"^(desclist2string dls)
					 | Arraysig::dls => "["^(desclist2string dls)
					 | nil => ""
				in
				    fn (arglist, arg) => "("^(desclist2string arglist)^")"^(desclist2string [arg])
				end

	local
	    val instructionToJasmin =
		fn Astore i => (let val j=if i<0 then (!perslocals-i) else i
			       in if j<4 then "astore_"^Int.toString j
				  else "astore "^Int.toString j
			       end)^" ; war wirklich: "^(Int.toString i)
		 | Aastore  => "aastore"
		 | Aaload => "aaload"
		 | Aconst_null => "aconst_null"
		 | Aload i  => (let val j=if i<0 then (!perslocals-i) else i
			       in if j<4 then "aload_"^Int.toString j
				  else "aload "^Int.toString j
			       end)^" ; war wirklich: "^(Int.toString i)
		 | Anewarray cn => "anewarray "^cn
		 | Areturn => "areturn"
		 | Arraylength => "arraylength"
		 | Athrow => "athrow"
		 | Bipush i => "bipush "^intToString i
		 | Catch(cn,from,to,use) => ".catch "^cn^" from "^from^" to "^to^" using "^use
		 | Checkcast cn => "checkcast "^cn
		 | Comment c => "\t; "^c
		 | Dup => "dup"
		 | Fconst i => if i=0 then "fconst_0" else if i=1 then "fconst_1" else "fconst_2"
		 | Getfield(fieldn, ty) => "getfield "^fieldn^" L"^ty^";"
		 | Getstatic(fieldn, ty) => "getstatic "^fieldn^" L"^ty^";"
		 | Goto l => "goto "^l
		 | Iconst i => if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
		 | Iadd => "iadd"
		 | Ifacmpeq l => "if_acmpeq "^l
		 | Ifacmpne l => "if_acmpne "^l
		 | Ifeq l => "ifeq "^l
		 | Ificmplt l => "if_icmplt "^l
		 | Ifneq l => "ifne "^l
		 | Ifnull l => "ifnull "^l
		 | Iinc (i,j) => "iinc "^(Int.toString i)^" "^(intToString j)
		 | Iload i =>let val j=if i<0 then (!perslocals-i) else i
			     in if j<4 then "iload_"^Int.toString j
				else "iload "^Int.toString j
			     end
		 | Istore i =>let val j=if i<0 then (!perslocals-i) else i
			      in if j<4 then "istore_"^Int.toString j
				 else "istore "^Int.toString j
			      end
		 | Ireturn => "ireturn"
		 | Instanceof cn => "instanceof "^cn
		 | Invokeinterface(cn,mn,ms as (arg,ret)) => "invokeinterface "^cn^"/"^mn^
			       (descriptor2string ms)^" "^Int.toString(length arg + 1)
		 | Invokespecial(cn,mn,ms) => "invokespecial "^cn^"/"^mn^(descriptor2string ms)
		 | Invokestatic(cn,mn,ms) => "invokestatic "^cn^"/"^mn^(descriptor2string ms)
		 | Invokevirtual(cn,mn,ms) => "invokevirtual "^cn^"/"^mn^(descriptor2string ms)
		 | Label l => l^": "
		 | Ldc(JVMString s) => "ldc \""^s^"\""
		 | Ldc(JVMFloat r) => "ldc "^Real.toString r
		 | Ldc(JVMInt i) => "ldc "^intToString i
		 | New cn => "new "^cn
		 | Pop => "pop"
		 | Putfield(cn,f) => "putfield "^cn^" L"^f^";"
		 | Putstatic(cn,f) => "putstatic "^cn^" L"^f^";"
		 | Return => "return"
		 | Sipush i => "sipush "^intToString i
		 | Swap => "swap"
		 | Tableswitch(low,labellist, label) =>
			       let
				   val rec
				       flatten = fn lab::labl => ("\t"^lab^"\n")^(flatten labl) | nil => ""
			       in
				   "tableswitch "^(Int.toString low)^"\n"^
				   (flatten labellist)^
				   "default: "^label
			       end
	in
	    val rec
		instructionsToJasmin =
		fn i::is => ("\t\t.line "^line()^"\n")^
		(instructionToJasmin i)^"\n"^instructionsToJasmin is
		 | nil => ""
	end

	local
	    val fieldToJasmin =
		fn Field(access,fieldname, Classtype classtype) =>
		let
		    val fcc = fAccessToString access
		in
		    ".field "^fcc^" "^fieldname^" L"^classtype^";\n"
		end
		 | Field(access,fieldname, Sonstwas i) =>
		let
		    val fcc = fAccessToString access
		in
		    ".field "^fcc^" "^fieldname^" I = 0\n"
		end
	in
	    val rec
		fieldsToJasmin =
		fn f::fs => (fieldToJasmin f)^(fieldsToJasmin fs)
		 | nil => ""
	end

	val methodToJasmin =
	    fn Method(access,methodname,methodsig,Limits (perslocs,stack), instructions) =>
	    let
		val mcc = mAccessToString access
	    in
		perslocals := perslocs;
		".method "^mcc^methodname^(descriptor2string methodsig)^"\n"^
		".limit locals "^Int.toString(perslocs+1)^"\n"^
		".limit stack "^Int.toString(stack)^"\n"^
		instructionsToJasmin(instructions)^"\n"^
		".end method\n"
	    end
	val rec
	    methodsToJasmin =
	    fn m::ms => (methodToJasmin m)^(methodsToJasmin ms)
	     | nil => ""

	val classToJasmin =
	    fn Class(access,name,super,fields,methods) =>
	    let
		val acc = cAccessToString(access)
	    in
		".class "^acc^name^"\n"^
		".super "^super^"\n"^
		fieldsToJasmin(fields)^
		methodsToJasmin(methods)
	    end

	val schreibs =
	    fn (wohin,was) =>
	    let
		val bla=TextIO.openOut wohin
	    in
		TextIO.output(bla,was);
		TextIO.closeOut bla
	    end

	val schreibsDran =
	    fn (wohin,was) =>
	    let
		val bla=TextIO.openAppend wohin
	    in
		TextIO.output(bla,was);
		TextIO.closeOut bla
	    end

(*  	val compileJasmin = *)
(*  	    fn (woher, verify:bool) => *)
(*  	    let *)
(*  		val cmd = "/bin/bash" *)
(*  		val proc = Unix.execute(cmd,["jasmin",woher]); *)
(*  	    in *)
(*  		Unix.reap proc *)
(*  	    end *)
    end

