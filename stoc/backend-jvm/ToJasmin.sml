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

val descriptor2string = let
			    val rec desclist2string =
				fn (Classsig dl)::dls => "L"^dl^";"^(desclist2string dls)
				 | Intsig::dls => "I"^(desclist2string dls)
				 | Voidsig::dls => "V"^(desclist2string dls)
				 | Floatsig::dls => "F"^(desclist2string dls)
				 | Arraysig::dls => "["^(desclist2string dls)
				 | nil => ""
			in
			    fn (arglist, arg) => "("^(desclist2string arglist)^")"^(desclist2string [arg])
			end

local
    val instructionToJasmin =
	fn Astore i => if i<4 then "astore_"^Int.toString i
		       else "astore "^Int.toString i
	 | Aastore  => "aastore"
	 | Aconst_null => "aconst_null"
	 | Aload i  => if i<4 then "aload_"^Int.toString i
		       else "aload "^Int.toString i
	 | Anewarray cn => "anewarray "^cn
	 | Areturn => "areturn"
	 | Athrow => "athrow"
	 | Bipush i => "bipush "^Int.toString i
	 | Catch(cn,from,to,use) => ".catch "^cn^" from "^from^" to "^to^" using "^use
	 | Checkcast cn => "checkcast "^cn
	 | Comment c => "\t; "^c
	 | Dup => "dup"
	 | Fconst i => if i=0 then "fconst_0" else if i=1 then "fconst_1" else "fconst_2"
	 | Getfield(cn,f) => "getfield "^cn^" L"^f^";"
	 | Getstatic(cn,f) => "getstatic "^cn^" L"^f^";"
	 | Goto l => "goto "^l
	 | Iconst i => if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	 | Ifacmp l => "ifacmp "^l
	 | Ifeq l => "ifeq "^l
	 | Ifneq l => "ifne "^l
	 | Ifnull l => "ifnull "^l
	 | Ireturn => "ireturn"
	 | Instanceof cn => "instanceof "^cn
	 | Invokeinterface(cn,mn,ms as (arg,ret)) => "invokeinterface "^cn^"/"^mn^
			   (descriptor2string ms)^" "^Int.toString(length arg + 1)
	 | Invokespecial(cn,mn,ms) => "invokespecial "^cn^"/"^mn^(descriptor2string ms)
	 | Invokevirtual(cn,mn,ms) => "invokevirtual "^cn^"/"^mn^(descriptor2string ms)
	 | Label l => l^": "
	 | Ldc(JVMString s) => "ldc \""^s^"\""
	 | Ldc(JVMFloat r) => "ldc "^Real.toString r
	 | Ldc(JVMInt i) => "ldc "^Int.toString i
	 | New cn => "new "^cn
	 | Pop => "pop"
	 | Putfield(cn,f) => "putfield "^cn^" "^f
	 | Putstatic(cn,f) => "putstatic "^cn^" "^f
	 | Return => "return"
	 | Sipush i => "sipush "^Int.toString i
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
	fn i::is => (instructionToJasmin i)^"\n"^instructionsToJasmin is
	 | nil => ""
end

local
    val fieldToJasmin =
	fn Field(access,fieldname, Classtype classtype) =>
	let
	    val fcc = fAccessToString access
	in
	    ".field "^fcc^" "^fieldname^" "^classtype^"= null\n"
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

local
    val methodToJasmin =
	fn Method(access,methodname,methodsig,Limits (locals,stack), instructions) =>
	let
	    val mcc = mAccessToString access
	in
	    ".method "^methodname^(descriptor2string methodsig)^"\n"^
	    ".limit locals "^Int.toString(locals)^"\n"^
	    ".limit stack "^Int.toString(stack)^"\n"^
	    instructionsToJasmin(instructions)^"\n"^
	    ".end method\n"
	end
in
    val rec
	methodsToJasmin =
	fn m::ms => (methodToJasmin m)^(methodsToJasmin ms)
	 | nil => ""
end

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
