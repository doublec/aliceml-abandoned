(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure ToJasmin =
    struct
	open Backend
	open JVMInst

	exception Error of string
	datatype deb = SIs of string*INSTRUCTION list
	exception Debug of deb

	val actclass= ref ""
	val stackneed = ref 0
	val stackmax = ref 0

	fun makeArityString (0,x) = x
	  | makeArityString (n,x) = makeArityString (n-1, x^"[")

	fun intToString i = if i<0 then "-"^Int.toString(~i) else Int.toString i
	fun int32ToString i = if LargeInt.< (i, Int.toLarge 0) then "-"^LargeInt.toString(~i)
			      else LargeInt.toString i
	fun word32ToString i = LargeWord.toString i
	fun realToString r = if Real.<(r,0.0) then "-"^Real.toString(~r) else Real.toString r

	datatype jump = Got | Ret | IRet | ARet
	datatype branchinst = Lab of string | Jump of jump | Non
	datatype registerOps = Load of (int * INSTRUCTION) | Store

	val labelMerge: branchinst StringHash.t ref = ref (StringHash.new())

	structure JVMreg =
	    struct
		(* maps virtual registers to their first defining position *)
		val from = ref (Array.array (0,0))

		(* maps virtual registers to their last read access position *)
		val to = ref (Array.array (0,0))

		(* maps virtual registers to JVM registers *)
		val regmap = ref (Array.array (0,0))

		(* maps JVM registers to their last read access position *)
		val jvmto = ref (Array.array (0,0))

		(* maps labels to their position *)
		val labHash: int StringHash.t ref = ref (StringHash.new())

		(* maps virtual registers to virtual registers (used for
		 Aload/Astore register fusion) *)
		val fusedwith = ref (Array.array (0,0))

		(* Some (few) virtual registers are assigned more than once.
		 Therefore, they cannot be fused on Aload/Astore sequences *)
		val defines = ref (Array.array (0,0))

		fun new regs =
		    let
			val registers = if regs>=2 then regs + 1 else 3
		    in
			from := Array.array(registers, ~1);
			to := Array.array(registers, ~1);
			regmap := Array.array(registers, ~1);
			jvmto := Array.array(registers, ~1);
			labHash := StringHash.new();
			fusedwith := Array.array (registers,~1);
			defines := Array.array(registers,0)
		    end

		fun define (register, pos) =
		    let
			val old = Array.sub(!from, register)
		    in
			if old = ~1 orelse old > pos
			    then Array.update(!from, register, pos)
			else ()
		    end

		fun use (register, pos) =
		    let
			val lookup = Array.sub (!fusedwith, register)
			val genuineReg = if lookup = ~1 then register else lookup
			val old = Array.sub(!to, genuineReg)
		    in
			if old < pos
			    then Array.update(!to, genuineReg, pos)
			else ()
		    end

		fun assignAll () =
		    let
			fun assign (register) =
			    let
				val lookup = Array.sub (!fusedwith, register)
				val genuineReg = if lookup = ~1 then register else lookup
				val f' = Array.sub (!from, genuineReg)
				fun assignNextFree (act) =
				    if f' > Array.sub (!jvmto, act)
					then (Array.update (!regmap, genuineReg, act);
					      Array.update (!jvmto, act,
							    Array.sub (!to, genuineReg)))
				    else assignNextFree (act+1)
			    in
				print (Int.toString register^"/"^Int.toString (Array.length(!to)));
				if f'= ~1 then
				    print "is uninitialized!!"
				    else
					if lookup = ~1 then
					    assignNextFree 2
					else
					    ();
				if register+1<Array.length(!to)
				    then assign (register+1)
				else ()
			    end
		    in
			Array.update(!regmap, 0, 0);
			Array.update(!regmap, 1, 1);
			assign 2
		    end

		fun defineLabel (label', pos) =
		    (print ("defining "^label');
		     StringHash.insert (!labHash, label', pos))

		fun addJump (f', tolabel) =
		    let
			val t = StringHash.lookup(!labHash, tolabel)

		    (* Checks whether we jump from behind last usage into the range of the
		     virtual register or from inside the range to somewhere before first
		     declaration. If so, the range of this register is changed. *)
			fun checkReg (register) =
			    let
				val lookup = Array.sub (!fusedwith, register)
				val genuineReg = if lookup = ~1 then register else lookup
				val regfrom = Array.sub(!from, genuineReg)
				val regto = Array.sub(!to, genuineReg)
				val t' = valOf t
			    in
				if f' > regto andalso t' > regfrom andalso t' > regto
				    then
					Array.update (!to, register, t')
				else if f' > regfrom andalso f' <regto
				    andalso t' < regfrom
					 then Array.update (!from, genuineReg, f')
				     else if register<Array.length (!to)
					      then checkReg (register+1)
					  else ()
			    end
		    in
			if isSome t then checkReg 1 else ()
		    end

		fun get reg =
		    let
			val lookup = Array.sub (!fusedwith, reg)
			val genuineReg = if lookup = ~1 then reg else lookup
		    in
			if !OPTIMIZE >= 2 then Array.sub (!regmap, genuineReg) else reg
		    end

		fun max default =
		    let
			fun findMax r =
			    if r+1=Array.length (!jvmto) orelse
				Array.sub (!jvmto, r) = ~1 then
				r-1
			    else findMax (r+1)
		    in
			if !OPTIMIZE >= 2 then
			    findMax 2
			else default
		    end

		fun fuse (a,b) =
		    (if Array.sub(!defines, b)<2 then
			 (Array.update(!fusedwith, b, a);
			  if Array.sub (!from, a) > Array.sub (!from, b)
			      then Array.update(!from, a, Array.sub(!from, b)) else ();
			   if Array.sub (!to, a) > Array.sub (!to, b)
			       then Array.update(!to, a, Array.sub(!to, b)) else ())
		     else ())

		fun countDefine reg =
		    Array.update (!defines, reg,
				  Array.sub(!defines, reg)+1)
	    end

	fun directJump lab' =
	    case StringHash.lookup (!labelMerge, lab') of
		NONE => "goto "^lab'
	      | SOME (Lab lab'') => directJump lab''
	      | SOME (Jump Ret) => "return"
	      | SOME (Jump ARet) => "areturn"
	      | SOME (Jump IRet) => "ireturn"
	      | SOME _ => raise Match

	fun condJump lab' =
	    case StringHash.lookup (!labelMerge, lab') of
	      SOME (Lab lab'') => condJump lab''
	      | _ => lab'

	fun optimize (insts, registers) =
	    let
		val verylast = ref Non
		fun enter (last, Comment _::rest) =
		    enter (last, rest)
		  | enter (Lab lab', Label lab''::rest) =
		    let
			val l'' = Lab lab''
		    in
			StringHash.insert (!labelMerge, lab', l'');
			enter (l'', rest)
		    end
		  | enter (Lab lab', Goto lab''::rest) =
		    (StringHash.insert (!labelMerge, lab', Lab lab'');
		     enter (Non, rest))
		  | enter (Lab lab', Areturn::rest) =
		    (StringHash.insert (!labelMerge, lab', Jump ARet);
		     enter (Non, rest))
		  | enter (Lab lab', Return::rest) =
		    (StringHash.insert (!labelMerge, lab', Jump Ret);
		     enter (Non, rest))
		  | enter (Lab lab', Ireturn::rest) =
		    (StringHash.insert (!labelMerge, lab', Jump IRet);
		     enter (Non, rest))
		  | enter (_, Label lab'::rest) =
		    enter (Lab lab', rest)
		  | enter (_, _::rest) =
		    enter (Non, rest)
		  | enter (_, nil) = ()

		fun doubleAssigns (inst::rest) =
		    ((case inst of
			Astore reg => JVMreg.countDefine reg
		      | Istore reg => JVMreg.countDefine reg
		      | _ => ());
			  doubleAssigns rest)
		  | doubleAssigns nil = ()

		fun deadCode (last, (c as Comment _)::rest) =
		    c :: deadCode (last, rest)
		  | deadCode (last, Ifstatic (stamp', then', else')::rest) =
		    deadCode (last,
			      if Lambda.isStatic stamp'
				  then then'
			      else else')
		    @ deadCode (!verylast, rest)
		  | deadCode (Lab lab',Label l'::rest) =
		    deadCode (Lab l', rest)
		  | deadCode (Lab _, Goto lab''::rest) =
		    Goto lab'' :: deadCode (Jump Got, rest)
		  | deadCode (Lab lab', Areturn::rest) =
		    Label lab' :: Areturn :: deadCode (Jump ARet, rest)
		  | deadCode (Lab lab', Return::rest) =
		    Label lab' :: Return :: deadCode (Jump Ret, rest)
		  | deadCode (Lab lab', Ireturn::rest) =
		    Label lab' :: Ireturn :: deadCode (Jump IRet, rest)
		  | deadCode (Lab l', rest) =
		    Label l' :: deadCode (Non, rest)
		  | deadCode (i as Jump _, Label lab''::rest) =
		    deadCode
		    (if isSome (StringHash.lookup(!labelMerge,lab''))
			 then i
		     else Lab lab'',
			 rest)
		  | deadCode (i as Jump _, _::rest) =
			 deadCode (i, rest)
		  | deadCode (Non, Label lab'::rest) =
			 deadCode (Lab lab', rest)
		  | deadCode (Non, Goto lab''::rest) =
			 Goto lab'' :: deadCode (Jump Got, rest)
		  | deadCode (Non, Areturn::rest) =
			 Areturn :: deadCode (Jump ARet, rest)
		  | deadCode (Non, Return::rest) =
			 Return :: deadCode (Jump Ret, rest)
		  | deadCode (Non, Ireturn::rest) =
			 Ireturn :: deadCode (Jump IRet, rest)
		  | deadCode (Non, c::rest) =
			 c :: deadCode (Non, rest)
		  | deadCode (last, nil) = (verylast := last; nil)

		fun prepareLifeness (Astore r::insts, pos) =
		    (JVMreg.define(r, pos);
		     prepareLifeness (insts,pos+1))
		  | prepareLifeness (Istore r::insts, pos) =
		    (JVMreg.define(r, pos);
		     prepareLifeness (insts,pos+1))
		  | prepareLifeness (Aload r::insts, pos) =
		    (JVMreg.use(r, pos);
		     prepareLifeness (insts,pos+1))
		  | prepareLifeness (Iload r::insts, pos) =
		    (JVMreg.use(r, pos);
		     prepareLifeness (insts,pos+1))
		  | prepareLifeness (Label l'::insts, pos) =
		    (JVMreg.defineLabel (l', pos);
		     prepareLifeness (insts,pos+1))
		  | prepareLifeness (_::insts, pos) =
		    prepareLifeness (insts,pos+1)
		  | prepareLifeness (nil, _) = ()

		fun lifeness (Goto label'::insts, pos) =
		    (JVMreg.addJump(pos, label');
		     lifeness (insts, pos+1))
		  | lifeness (Ifacmpeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ifacmpne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ifeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ificmpeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		   | lifeness (Ificmplt label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ificmpne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ifneq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ifnull label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (_::insts, pos) =
		     lifeness (insts, pos+1)
		  | lifeness (nil, _) = ()

		fun fuse (old, Comment _::rest) =
		    fuse (old, rest)
		  | fuse (l as Load (a,_), Astore b::rest) =
		    (JVMreg.fuse (a,b);
		     fuse (Store, rest))
		  | fuse (l as Load (a,_),Istore b::rest) =
		    (JVMreg.fuse (a,b);
		     fuse (Store, rest))
		  | fuse (Load (_,ori), (l as Aload b)::rest) =
		    ori::fuse (Load (b,l), rest)
		  | fuse (Store, (l as Aload b)::rest) =
		    fuse (Load (b,l), rest)
		  | fuse (Load (_,ori), (l as Iload b)::rest) =
		    ori::fuse (Load (b,l), rest)
		  | fuse (Store, (l as Iload b)::rest) =
		    fuse (Load (b,l), rest)
		  | fuse (Store, inst::rest) =
		    inst::fuse(Store, rest)
		  | fuse (Load (_, ori), inst::rest) =
		    ori::inst::fuse(Store, rest)
		  (* The last instruction has to be return. *)
		  | fuse (_,nil) = nil

		val d' = (labelMerge := StringHash.new();
			  enter (Non, insts);
			  doubleAssigns insts;
			  fuse (Store, deadCode (Non, insts)))
	    in
		if !OPTIMIZE >=2 then
		    (prepareLifeness (d', 0);
		     lifeness (d', 0);
		     JVMreg.assignAll())
		else ();
		d'
	    end

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
	  | desclist2string (Charsig::dls) = "C"^(desclist2string dls)
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
	  | stackNeedInstruction (Var _) = 0

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
		    val i = if s then JVMreg.get j-1 else JVMreg.get j
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
		    val i = if s then JVMreg.get j-1 else JVMreg.get j
		in
		    if i<4 then
			"aload_"^Int.toString i
		    else "aload "^Int.toString i
		end
	      | instructionToJasmin (Anewarray cn,_) = "anewarray "^cn
	      | instructionToJasmin (Areturn,_) =
		if !DEBUG >=2 then
		    let
			val nodebug=Label.new()
		    in
			"getstatic "^(!actclass)^"/DEBUG Z\n"^
			"ifeq "^nodebug^"\n"^
			"getstatic java/lang/System/out Ljava/io/PrintStream;\n"^
			"ldc \")\"\n"^
			"invokevirtual java/io/PrintStream/print(Ljava/lang/Object;)V\n"^
			nodebug^":\n"^
			"areturn"
		    end
		else "areturn"
	      | instructionToJasmin (Arraylength,_) = "arraylength"
	      | instructionToJasmin (Athrow,_) = "athrow"
	      | instructionToJasmin (Bipush i,_) = "bipush "^intToString i
	      | instructionToJasmin (Catch(cn,from,to,use),_) =
		    ".catch "^cn^" from "^(condJump from)^" to "^(condJump to)^" using "^(condJump use)
	      | instructionToJasmin (Checkcast cn,_) = "checkcast "^cn
	      | instructionToJasmin (Comment c,_) =
		    if !DEBUG>=1
			then "\t; "^c
		    else ""
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
	      | instructionToJasmin (Goto l,isstatic) = directJump l
	      | instructionToJasmin (Iconst i,_) =
				     if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	      | instructionToJasmin (Iadd,_) = "iadd"
	      | instructionToJasmin (Ifacmpeq l,_) = "if_acmpeq "^(condJump l)
	      | instructionToJasmin (Ifacmpne l,_) = "if_acmpne "^(condJump l)
	      | instructionToJasmin (Ifeq l,_) = "ifeq "^(condJump l)
	      | instructionToJasmin (Ificmpeq l,_) = "if_icmpeq "^(condJump l)
	      | instructionToJasmin (Ificmplt l,_) = "if_icmplt "^(condJump l)
	      | instructionToJasmin (Ificmpne l,_) = "if_icmpne "^(condJump l)
	      | instructionToJasmin (Ifneq l,_) = "ifne "^(condJump l)
	      | instructionToJasmin (Ifnull l,_) = "ifnull "^(condJump l)
	      | instructionToJasmin (Ifstatic _,_) = raise Error ""
	      | instructionToJasmin (Iload j,s) =
					 let
					     val i = if s then JVMreg.get j-1 else JVMreg.get j
					 in
					     if i<4 then
						 "iload_"^Int.toString i
					     else "iload "^Int.toString i
					 end
	      | instructionToJasmin (Istore j,s) =
					 let
				     val i = if s then JVMreg.get j-1 else JVMreg.get j
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
	      | instructionToJasmin (Ldc(JVMString s),_) = "ldc \""^String.toCString s^"\""
	      | instructionToJasmin (Ldc(JVMFloat r),_) = "ldc "^realToString r
	      | instructionToJasmin (Ldc(JVMInt i),_) = "ldc "^int32ToString i
	      | instructionToJasmin (Ldc(JVMWord w),_) = "ldc "^word32ToString w
	      | instructionToJasmin (Ldc(JVMChar c),_) = "ldc "^Int.toString (Char.ord c)
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
				 fun flatten (lab::labl) = ("\t"^(condJump lab)^"\n")^(flatten labl)
				   | flatten nil = ""
			     in
				   "tableswitch "^(Int.toString low)^"\n"^
				   (flatten labellist)^
				   "default: "^(condJump label)
			     end
	      | instructionToJasmin (Var (number', name', descriptor', from', to'), isStatic) =
			     if (!DEBUG >= 1) then
				 ".var "^
				 (Int.toString
				  (if isStatic then number'-1 else number'))
				 ^" is "^name'^" "^(desclist2string descriptor')^
				 " from "^(condJump from')^" to "^(condJump to')
			     else ""

	in
	    fun instructionsToJasmin (insts, need, max, staticapply, ziel) =
		let
		    fun noStack (Comment _) = true
		      | noStack (Label _) = true
		      | noStack _ = false
		    fun recurse (i::is, need, max) =
			let
			    val nd = stackNeedInstruction i
			in
			    case i of
				Ifstatic (stamp', then', else') =>
				    (recurse
				     (if Lambda.isStatic stamp' then
					  then'
				      else else',
					  need, max);
				      recurse (is, !stackneed,!stackmax))
			      | Goto l' =>
				    (case is of
					Goto l''::rest =>
					    recurse (i::Comment ("Goto "^l'')::rest, need, max)
				      | _ => (TextIO.output (ziel, instructionToJasmin (i,staticapply)^"\n");
					      recurse (is, need, max)))
			      | _ =>
				    let
					val ins = instructionToJasmin (i, staticapply)
				    in
					if noStack i
					    then ()
					else
					    (if !DEBUG>=1
						then (TextIO.output (ziel,"\t\t.line "^line());
						      TextIO.output (ziel,"\t; Stack: "^Int.toString need^
								     " Max: "^Int.toString max^"\n"))
					    else ());
					    if ins <> "" then TextIO.output (ziel,ins^"\n") else ();
						if nd<=0 then
						    recurse (is, nd+need, max)
						else recurse (is, nd+need, (Int.max (nd+need,max)))
				    end
			end
		      | recurse (nil,need,max) =
			(stackneed:= need;
			 stackmax := max)
		in
		    recurse (insts, need, max)
		end
	end

	fun classToJasmin (Class(access,name,super,interfaces,fields,methods)) =
	    let
		val io = TextIO.openOut (name^".j")
		fun fieldToJasmin (Field(access,fieldname,arg)) =
		    TextIO.output(io,".field "^
				  fAccessToString access
				  ^" "^fieldname^" "^(desclist2string arg)^"\n")
		fun interfaceToJasmin (i,akku) = akku^".implements "^i^"\n"
		fun methodToJasmin (Method(access,methodname,methodsig,Locals perslocs,
					   instructions, catches, staticapply)) =
		    (* apply hat derzeit oft ein doppeltes Areturn am Ende.
		     Wird spaeter wegoptimiert. *)
		    (JVMreg.new perslocs;
		     TextIO.output(io,".method "^
				   (mAccessToString access)^
				   methodname^
				   (descriptor2string methodsig)^"\n");
		     (* Seiteneffekt: stackneed und stackmax werden gesetzt *)
		     instructionsToJasmin
		     (if !OPTIMIZE>=1 then
			  optimize (instructions, perslocs)
			  else instructions,
		      0,
		      0,
		      staticapply,
		      io);
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
		      TextIO.output(io,".limit locals "^Int.toString(JVMreg.max perslocs+1)^"\n");
		      TextIO.output(io,".limit stack "^Int.toString
				    ((if !DEBUG>=2 then 2 else 0)+(!stackmax))^"\n");
		      instructionsToJasmin(catches,0,0, staticapply, io);
		      TextIO.output(io,".end method\n"))
	    in
		actclass:=name;
		TextIO.output(io,
			      ".source "^name^".j\n");
		TextIO.output(io,
			      ".class "^(cAccessToString access)^name^"\n"^
			      ".super "^super^"\n");
		TextIO.output(io,List.foldr interfaceToJasmin "" interfaces);
		app fieldToJasmin fields;
		app methodToJasmin methods;
		TextIO.closeOut io
	    end

(*      val compileJasmin = *)
(*          fn (woher, verify:bool) = *)
(*          let *)
(*              val cmd = "/bin/bash" *)
(*              val proc = Unix.execute(cmd,["jasmin",woher]); *)
(*          in *)
(*              Unix.reap proc *)
(*          end *)
    end

