structure ToJasmin =
    struct
	open JVMInst

	exception Error of string
	exception Debug of string*INSTRUCTION list

	val actclass= ref ""
	val actmeth = ref ""

	fun intToString i = if i<0 then "-"^Int.toString(~i) else Int.toString i

	local
	    val linecount = ref 0
	in
	    val line = fn () => (linecount := !linecount + 1; Int.toString(!linecount))
	end

	fun cAccessToString (CPublic::rest)    = "public "^cAccessToString rest
	  | cAccessToString (CFinal::rest)     = "final "^cAccessToString rest
	  | cAccessToString (CSuper::rest)     = "super "^cAccessToString rest
	  | cAccessToString (CAbstract::rest)  = "abstract "^cAccessToString rest
	  | cAccessToString (CInterface::rest) = "interface "^cAccessToString rest
	  | cAccessToString nil = ""

	fun fAccessToString (FPublic::rest)    = "public "^fAccessToString rest
	  | fAccessToString (FPrivate::rest)   = "private "^fAccessToString rest
	  | fAccessToString (FProtected::rest) = "protected "^fAccessToString rest
	  | fAccessToString (FStatic::rest)    = "static "^fAccessToString rest
	  | fAccessToString (FFinal::rest)     = "final "^fAccessToString rest
	  | fAccessToString (FVolatile::rest)  = "volatile "^fAccessToString rest
	  | fAccessToString (FTransient::rest) = "transient "^fAccessToString rest
	  | fAccessToString nil = ""

	fun mAccessToString (MPublic::rest)        =    "public "^mAccessToString rest
	  | mAccessToString (MPrivate::rest)       =   "private "^mAccessToString rest
	  | mAccessToString (MProtected::rest)     = "protected "^mAccessToString rest
	  | mAccessToString (MStatic::rest)        = "static "^mAccessToString rest
	  | mAccessToString (MFinal::rest)         =     "final "^mAccessToString rest
	  | mAccessToString (MSynchronized::rest)  =  "synchronized "^mAccessToString rest
	  | mAccessToString (MNative::rest)        =  "native "^mAccessToString rest
	  | mAccessToString (MAbstract::rest)      =  "abstract "^mAccessToString rest
	  | mAccessToString nil = ""

	fun descriptor2string (arglist, arg) =
	    let
		fun desclist2string ((Classsig dl)::dls) = "L"^dl^";"^(desclist2string dls)
		  | desclist2string (Intsig::dls) = "I"^(desclist2string dls)
		  | desclist2string (Boolsig::dls) = "Z"^(desclist2string dls)
		  | desclist2string (Voidsig::dls) = "V"^(desclist2string dls)
		  | desclist2string (Floatsig::dls) = "F"^(desclist2string dls)
		  | desclist2string (Arraysig::dls) = "["^(desclist2string dls)
		  | desclist2string nil = ""
	    in
		"("^(desclist2string arglist)^")"^(desclist2string [arg])
	    end

	fun siglength (Arraysig::rest) = siglength rest
	  | siglength (_::rest) = (1+siglength rest)
	  | siglength nil = 0

	fun stackNeedInstruction (Astore _) = ~1
	  | stackNeedInstruction Aastore = ~3
	  | stackNeedInstruction Aaload = ~1
	  | stackNeedInstruction Aconst_null = 1
	  | stackNeedInstruction (Aload _) = 1
	  | stackNeedInstruction (Anewarray _) = 0
	  | stackNeedInstruction Areturn = ~1
	  | stackNeedInstruction Arraylength = 0
	  | stackNeedInstruction Athrow = ~1
	  | stackNeedInstruction (Bipush _) = 1
	  | stackNeedInstruction (Catch _) = 1
	  | stackNeedInstruction (Checkcast _) = 0
	  | stackNeedInstruction (Comment _) = 0
	  | stackNeedInstruction Dup = 1
	  | stackNeedInstruction (Fconst _) = 1
	  | stackNeedInstruction (Getfield _) = 0
	  | stackNeedInstruction (Getstatic _) = 1
	  | stackNeedInstruction (Goto _) = 0
	  | stackNeedInstruction Iadd = ~1
	  | stackNeedInstruction (Iconst _) = 1
	  | stackNeedInstruction (Ifacmpeq _) = ~2
	  | stackNeedInstruction (Ifacmpne _) = ~2
	  | stackNeedInstruction (Ifeq _) = ~1
	  | stackNeedInstruction (Ificmplt _) = ~2
	  | stackNeedInstruction (Ifneq _) = ~1
	  | stackNeedInstruction (Ifnull _) = ~1
	  | stackNeedInstruction (Iload _) = 1
	  | stackNeedInstruction (Instanceof _) = 0
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist,Voidsig))) = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist, _)))      = ~(siglength arglist)
	  | stackNeedInstruction (Invokespecial (_,_,(arglist,_)))          = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,Voidsig)))    = ~(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,_)))          = 1-(siglength arglist)
	    (* Sonderfall fuer Exceptionhandling *)
	  | stackNeedInstruction (Invokevirtual (CExWrap,"getValue",([],Classsig CVal))) = 1
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,Voidsig)))    = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,_)))          = ~(siglength arglist)
	  | stackNeedInstruction Ireturn = ~1
	  | stackNeedInstruction (Istore _) = ~1
	  | stackNeedInstruction (Label _) = 0
	  | stackNeedInstruction (Ldc _) = 1 (* wir benutzen kein long und double! *)
	  | stackNeedInstruction (New _) = 1
	  | stackNeedInstruction Pop = ~1
	  | stackNeedInstruction (Putfield _) = ~2
	  | stackNeedInstruction (Putstatic _) = ~1
	  | stackNeedInstruction Return = 0
	  | stackNeedInstruction (Sipush _) = 1
	  | stackNeedInstruction Swap = 0
	  | stackNeedInstruction (Tableswitch _) = ~1

	fun stackNeed (inst::insts,need,max) =
				  let
				      val nd = stackNeedInstruction inst
				  in
				      if nd<0 then
					  stackNeed (insts, nd+need, max)
				      else stackNeed (insts, nd+need, Int.max(nd+need,max))
				  end
	  | stackNeed (nil, need, max) =
			 (* apply hat derzeit immer ein doppeltes Areturn am Ende.
			  Wird spaeter wegoptimiert. *)
				  (if need <> 0 andalso (!actmeth<>"apply") orelse
				       need <> ~1 andalso (!actmeth="apply") then
				       print ("\n\nStack Verification Error. Stack="^Int.toString need^
					      " Max="^Int.toString max^" in "^(!actclass)^"."^
					      (!actmeth)^".\n")
				   else ();
				       max)

	local
	    fun instructionToJasmin (Astore j) =
		if j<4 then
		    "astore_"^Int.toString j
		else "astore "^Int.toString j
	      | instructionToJasmin Aastore  = "aastore"
	      | instructionToJasmin Aaload = "aaload"
	      | instructionToJasmin Aconst_null = "aconst_null"
	      | instructionToJasmin (Aload j) =
		    if j<4 then
			"aload_"^Int.toString j
		    else "aload "^Int.toString j
	      | instructionToJasmin (Anewarray cn) = "anewarray "^cn
	      | instructionToJasmin Areturn = "areturn"
	      | instructionToJasmin Arraylength = "arraylength"
	      | instructionToJasmin Athrow = "athrow"
	      | instructionToJasmin (Bipush i) = "bipush "^intToString i
	      | instructionToJasmin (Catch(cn,from,to,use)) =
		".catch "^cn^" from "^from^" to "^to^" using "^use
	      | instructionToJasmin (Checkcast cn) = "checkcast "^cn
	      | instructionToJasmin (Comment c) = "\t; "^c
	      | instructionToJasmin Dup = "dup"
	      | instructionToJasmin (Fconst i) =
		if i=0 then
		    "fconst_0"
		else if i=1 then
		    "fconst_1"
		     else "fconst_2"
	      | instructionToJasmin (Getfield(fieldn, ty)) = "getfield "^fieldn^" L"^ty^";"
	      | instructionToJasmin (Getstatic(fieldn, ty)) = "getstatic "^fieldn^" L"^ty^";"
	      | instructionToJasmin (Goto l) = "goto "^l
	      | instructionToJasmin (Iconst i) =
			 if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	      | instructionToJasmin Iadd = "iadd"
	      | instructionToJasmin (Ifacmpeq l) = "if_acmpeq "^l
	      | instructionToJasmin (Ifacmpne l) = "if_acmpne "^l
	      | instructionToJasmin (Ifeq l) = "ifeq "^l
	      | instructionToJasmin (Ificmplt l) = "if_icmplt "^l
	      | instructionToJasmin (Ifneq l) = "ifne "^l
	      | instructionToJasmin (Ifnull l) = "ifnull "^l
	      | instructionToJasmin (Iload j) =
			     if j<4 then
				 "iload_"^Int.toString j
			     else "iload "^Int.toString j
	      | instructionToJasmin (Istore j) =
				 if j<4 then
				     "istore_"^Int.toString j
				 else "istore "^Int.toString j
	      | instructionToJasmin Ireturn = "ireturn"
	      | instructionToJasmin (Instanceof cn) = "instanceof "^cn
	      | instructionToJasmin (Invokeinterface(cn,mn,ms as (arg,ret))) =
			     "invokeinterface "^cn^"/"^mn^
			     (descriptor2string ms)^" "^Int.toString(length arg + 1)
	      | instructionToJasmin (Invokespecial(cn,mn,ms)) =
			     "invokespecial "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Invokestatic(cn,mn,ms)) =
			     "invokestatic "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Invokevirtual(cn,mn,ms)) =
			     "invokevirtual "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Label l) = l^": "
	      | instructionToJasmin (Ldc(JVMString s)) = "ldc \""^s^"\""
	      | instructionToJasmin (Ldc(JVMFloat r)) = "ldc "^Real.toString r
	      | instructionToJasmin (Ldc(JVMInt i)) = "ldc "^intToString i
	      | instructionToJasmin (New cn) = "new "^cn
	      | instructionToJasmin Pop = "pop"
	      | instructionToJasmin (Putfield(cn,f)) = "putfield "^cn^" L"^f^";"
	      | instructionToJasmin (Putstatic(cn,f)) = "putstatic "^cn^" L"^f^";"
	      | instructionToJasmin Return = "return"
	      | instructionToJasmin (Sipush i) = "sipush "^intToString i
	      | instructionToJasmin Swap = "swap"
	      | instructionToJasmin (Tableswitch(low,labellist, label)) =
			     let
				 fun flatten (lab::labl) = ("\t"^lab^"\n")^(flatten labl)
				   | flatten nil = ""
			     in
				   "tableswitch "^(Int.toString low)^"\n"^
				   (flatten labellist)^
				   "default: "^label
			       end
	in
	    fun instructionsToJasmin (i::is, need, max) =
		let
		    val nd = stackNeedInstruction i
		    fun noStack (Comment _) = true
		      | noStack _ = false
		in
		    (if noStack i then "" else
			 ((*"\t\t.line "^line()^*)
			  "\t; Stack: "^Int.toString need^" Max: "^Int.toString max)^"\n")^
		    (instructionToJasmin i)^"\n"^
		    (if nd<0 then
			instructionsToJasmin (is, nd+need, max)
		     else instructionsToJasmin (is, nd+need, Int.max(nd+need,max)))
		end
	      | instructionsToJasmin (nil,_,_) = ""
	end

	local
	    fun fieldToJasmin (Field(access,fieldname, Classtype classtype)) =
		let
		    val fcc = fAccessToString access
		in
		    ".field "^fcc^" "^fieldname^" L"^classtype^";\n"
		end
	      | fieldToJasmin (Field(access,fieldname, Sonstwas i)) =
		let
		    val fcc = fAccessToString access
		in
		    ".field "^fcc^" "^fieldname^" I = 0\n"
		end
	in
	    fun fieldsToJasmin (f::fs) = (fieldToJasmin f)^(fieldsToJasmin fs)
	      | fieldsToJasmin nil = ""
	end

	fun methodToJasmin (Method(access,methodname,methodsig,Locals perslocs, instructions, catches)) =
	    let
		val mcc = mAccessToString access
	    in
		actmeth:=methodname;
		".method "^mcc^methodname^(descriptor2string methodsig)^"\n"^
		".limit locals "^Int.toString(perslocs+1)^"\n"^
		".limit stack "^Int.toString(stackNeed (instructions,0,0))^"\n"^
		instructionsToJasmin(instructions,0,0)^"\n"^
		instructionsToJasmin(catches,0,0)^"\n"^
		".end method\n"
	    end
	fun methodsToJasmin (m::ms) = (methodToJasmin m)^(methodsToJasmin ms)
	  | methodsToJasmin nil = ""

	fun classToJasmin (Class(access,name,super,fields,methods)) =
	    let
		val acc = cAccessToString(access)
	    in
		actclass:=name;
		".class "^acc^name^"\n"^
		".super "^super^"\n"^
		fieldsToJasmin(fields)^
		methodsToJasmin(methods)
	    end

	fun schreibs (wohin,was) =
	    let
		val bla=TextIO.openOut wohin
	    in
		TextIO.output(bla,was);
		TextIO.closeOut bla
	    end

	fun schreibsDran (wohin,was) =
	    let
		val bla=TextIO.openAppend wohin
	    in
		TextIO.output(bla,was);
		TextIO.closeOut bla
	    end

(*  	val compileJasmin = *)
(*  	    fn (woher, verify:bool) = *)
(*  	    let *)
(*  		val cmd = "/bin/bash" *)
(*  		val proc = Unix.execute(cmd,["jasmin",woher]); *)
(*  	    in *)
(*  		Unix.reap proc *)
(*  	    end *)
    end

