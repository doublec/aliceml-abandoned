structure ToJasmin =
    struct
	open Backend
	open JVMInst

	exception Error of string
	exception Debug of string*INSTRUCTION list

	val actclass= ref ""
	val stackneed = ref 0
	val stackmax = ref 0

	fun makeArityString (0,x) = x
	  | makeArityString (n,x) = makeArityString (n-1, x^"[")

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

	fun desclist2string ((Classsig dl)::dls) = "L"^dl^";"^(desclist2string dls)
	  | desclist2string (Intsig::dls) = "I"^(desclist2string dls)
	  | desclist2string (Boolsig::dls) = "Z"^(desclist2string dls)
	  | desclist2string (Voidsig::dls) = "V"^(desclist2string dls)
	  | desclist2string (Floatsig::dls) = "F"^(desclist2string dls)
	  | desclist2string (Arraysig::dls) = "["^(desclist2string dls)
	  | desclist2string nil = ""

	fun descriptor2string (arglist, ret) =
	    "("^(desclist2string arglist)^")"^(desclist2string ret)

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
	  | stackNeedInstruction (Getself _) = 1
	  | stackNeedInstruction (Getstatic _) = 1
	  | stackNeedInstruction (Goto _) = 0
	  | stackNeedInstruction Iadd = ~1
	  | stackNeedInstruction (Iconst _) = 1
	  | stackNeedInstruction (Ifacmpeq _) = ~2
	  | stackNeedInstruction (Ifacmpne _) = ~2
	  | stackNeedInstruction (Ifeq _) = ~1
	  | stackNeedInstruction (Ificmpeq _) = ~2
	  | stackNeedInstruction (Ificmplt _) = ~2
	  | stackNeedInstruction (Ificmpne _) = ~2
	  | stackNeedInstruction (Ifneq _) = ~1
	  | stackNeedInstruction (Ifnull _) = ~1
	  | stackNeedInstruction (Ifstatic _) = 0
	  | stackNeedInstruction (Iload _) = 1
	  | stackNeedInstruction (Instanceof _) = 0
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist,[Voidsig]))) = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist, _)))      = ~(siglength arglist)
	  | stackNeedInstruction (Invokespecial (_,_,(arglist,_)))          = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,[Voidsig])))    = ~(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,_)))          = 1-(siglength arglist)
	    (* Sonderfall fuer Exceptionhandling *)
	  | stackNeedInstruction (Invokevirtual (CExWrap,"getValue",([],[Classsig CVal]))) = 1
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,[Voidsig])))    = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,_)))          = ~(siglength arglist)
	  | stackNeedInstruction Ireturn = ~1
	  | stackNeedInstruction (Istore _) = ~1
	  | stackNeedInstruction (Label _) = 0
	  | stackNeedInstruction Lcmp = ~1
	  | stackNeedInstruction (Ldc _) = 1 (* wir benutzen kein long und double! *)
	  | stackNeedInstruction (New _) = 1
	  | stackNeedInstruction Pop = ~1
	  | stackNeedInstruction (Putfield _) = ~2
	  | stackNeedInstruction (Putstatic _) = ~1
	  | stackNeedInstruction Return = 0
	  | stackNeedInstruction (Sipush _) = 1
	  | stackNeedInstruction Swap = 0
	  | stackNeedInstruction (Tableswitch _) = ~1

	(*fun stackNeed (inst::insts,need,max) =
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
				       max)*)

	local
	    fun instructionToJasmin (Astore j, s) =
		let
		    val i = if s then j-1 else j
		in
		    if i<4 then
			"astore_"^Int.toString i
		    else "astore "^Int.toString i
		end
	      | instructionToJasmin (Aastore,_)  = "aastore"
	      | instructionToJasmin (Aaload,_) = "aaload"
	      | instructionToJasmin (Aconst_null,_) = "aconst_null"
	      | instructionToJasmin (Aload j, s) =
		let
		    val i = if s then j-1 else j
		in
		    if i<4 then
			"aload_"^Int.toString i
		    else "aload "^Int.toString i
		end
	      | instructionToJasmin (Anewarray cn,_) = "anewarray "^cn
	      | instructionToJasmin (Areturn,_) = "areturn"
	      | instructionToJasmin (Arraylength,_) = "arraylength"
	      | instructionToJasmin (Athrow,_) = "athrow"
	      | instructionToJasmin (Bipush i,_) = "bipush "^intToString i
	      | instructionToJasmin (Catch(cn,from,to,use),_) =
		".catch "^cn^" from "^from^" to "^to^" using "^use
	      | instructionToJasmin (Checkcast cn,_) = "checkcast "^cn
	      | instructionToJasmin (Comment c,_) = "\t; "^c
	      | instructionToJasmin (Dup,_) = "dup"
	      | instructionToJasmin (Fconst i,_) =
		if i=0 then
		    "fconst_0"
		else if i=1 then
		    "fconst_1"
		     else "fconst_2"
	      | instructionToJasmin (Getfield(fieldn, arg),_) = "getfield "^fieldn^" "^
			 (desclist2string arg)
	      | instructionToJasmin (Getself name,s) =
			 if s then
			     "getstatic "^name
			 else
			     "aload_0"
	      | instructionToJasmin (Getstatic(fieldn, arg),_) = "getstatic "^fieldn^" "^
			     (desclist2string arg)
	      | instructionToJasmin (Goto l,_) = "goto "^l
	      | instructionToJasmin (Iconst i,_) =
			     if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	      | instructionToJasmin (Iadd,_) = "iadd"
	      | instructionToJasmin (Ifacmpeq l,_) = "if_acmpeq "^l
	      | instructionToJasmin (Ifacmpne l,_) = "if_acmpne "^l
	      | instructionToJasmin (Ifeq l,_) = "ifeq "^l
	      | instructionToJasmin (Ificmpeq l,_) = "if_icmpeq "^l
	      | instructionToJasmin (Ificmplt l,_) = "if_icmplt "^l
	      | instructionToJasmin (Ificmpne l,_) = "if_icmpne "^l
	      | instructionToJasmin (Ifneq l,_) = "ifne "^l
	      | instructionToJasmin (Ifnull l,_) = "ifnull "^l
	      | instructionToJasmin (Ifstatic _,_) = raise Error ""
	      | instructionToJasmin (Iload j,s) =
				 let
				     val i = if s then j-1 else j
				 in
				     if i<4 then
					 "iload_"^Int.toString i
				     else "iload "^Int.toString i
				 end
	      | instructionToJasmin (Istore j,s) =
				 let
				     val i = if s then j-1 else j
				 in
				     if i<4 then
				     "istore_"^Int.toString i
				     else "istore "^Int.toString i
				 end
	      | instructionToJasmin (Ireturn,_) = "ireturn"
	      | instructionToJasmin (Instanceof cn,_) = "instanceof "^cn
	      | instructionToJasmin (Invokeinterface(cn,mn,ms as (arg,ret)),_) =
			     "invokeinterface "^cn^"/"^mn^
			     (descriptor2string ms)^" "^Int.toString(length arg + 1)
	      | instructionToJasmin (Invokespecial(cn,mn,ms),_) =
			     "invokespecial "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Invokestatic(cn,mn,ms),_) =
			     "invokestatic "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Invokevirtual(cn,mn,ms),_) =
			     "invokevirtual "^cn^"/"^mn^(descriptor2string ms)
	      | instructionToJasmin (Label l,_) = l^": "
	      | instructionToJasmin (Lcmp,_) = "lcmp"
	      | instructionToJasmin (Ldc(JVMString s),_) = "ldc \""^s^"\""
	      | instructionToJasmin (Ldc(JVMFloat r),_) = "ldc "^Real.toString r
	      | instructionToJasmin (Ldc(JVMInt i),_) = "ldc "^Int32.toString i
	      | instructionToJasmin (New cn,_) = "new "^cn
	      | instructionToJasmin (Pop,_) = "pop"
	      | instructionToJasmin (Putfield(cn,arg),_) = "putfield "^cn^" "^
			     desclist2string arg
	      | instructionToJasmin (Putstatic(cn,arg),_) = "putstatic "^cn^" "^
			     desclist2string arg
	      | instructionToJasmin (Return,_) = "return"
	      | instructionToJasmin (Sipush i,_) = "sipush "^intToString i
	      | instructionToJasmin (Swap,_) = "swap"
	      | instructionToJasmin (Tableswitch(low,labellist, label),_) =
			     let
				 fun flatten (lab::labl) = ("\t"^lab^"\n")^(flatten labl)
				   | flatten nil = ""
			     in
				   "tableswitch "^(Int.toString low)^"\n"^
				   (flatten labellist)^
				   "default: "^label
			       end
	in
	    fun instructionsToJasmin (insts, need, max, staticapply) =
		let
		    fun noStack (Comment _) = true
		      | noStack _ = false
		    fun recurse (i::is, need, max) =
			let
			    val nd = stackNeedInstruction i
			in
			    case i of
				Ifstatic (stamp', then', else') =>
				    recurse
				    (if Lambda.isStatic stamp' then
					 then'
				     else else',
					 need, max)^
					 recurse (is, !stackneed,!stackmax)
			      | _ => (if noStack i
					  then ""
				      else
					  ((*"\t\t.line "^line()^*)
					   "\t; Stack: "^Int.toString need^
					   " Max: "^Int.toString max))
					      ^"\n"^
					      (instructionToJasmin (i, staticapply))^"\n"^
					      (if nd<0 then
						   recurse (is, nd+need, max)
					       else recurse (is, nd+need, Int.max(nd+need,max)))
			end
		      | recurse (nil,need,max) =
			(stackneed:= need;
			 stackmax := max;
			 "")
		in
		    recurse (insts, need, max)
		end
	end

	local
	    fun fieldToJasmin (Field(access,fieldname,arg)) =
		let
		    val fcc = fAccessToString access
		in
		    ".field "^fcc^" "^fieldname^" "^(desclist2string arg)^"\n"
		end
	in
	    fun fieldsToJasmin (f::fs) = (fieldToJasmin f)^(fieldsToJasmin fs)
	      | fieldsToJasmin nil = ""
	end

	fun methodToJasmin (Method(access,methodname,methodsig,Locals perslocs,
				   instructions, catches, staticapply)) =
	    let
		val mcc = mAccessToString access
		val catchinsts = instructionsToJasmin(catches,0,0, staticapply)
		(* Seiteneffekt: stackneed und stackmax werden gesetzt *)
		val insts = instructionsToJasmin(instructions,0,0, staticapply)
		(*val _ = if methodname="apply" then
		    raise Debug (methodname, instructions) else ()*)
	    in
		(* apply hat derzeit immer ein doppeltes Areturn am Ende.
		 Wird spaeter wegoptimiert. *)
		(if !stackneed = 0
		     orelse
		     !stackneed = ~1
		     andalso
		     ((methodname="apply")
		      orelse (methodname="sapply"))
		     then ()
		 else
		     print ("\n\nStack Verification Error. Stack="^Int.toString (!stackneed)^
			    " in "^(!actclass)^"."^methodname^".\n"));
		     ".method "^mcc^methodname^(descriptor2string methodsig)^"\n"^
		     ".limit locals "^Int.toString(perslocs+1)^"\n"^
		     ".limit stack "^Int.toString (!stackmax)^"\n"^
		     insts^"\n"^
		     catchinsts^"\n"^
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

