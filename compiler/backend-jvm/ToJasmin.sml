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

	val actclass = ref ""
	val actmeth = ref ""
	val lastLabel = ref ""

	fun makeArityString (0,x) = x
	  | makeArityString (n,x) = makeArityString (n-1, x^"[")

	fun intToString i = if i<0 then "-"^Int.toString(~i) else Int.toString i
	fun int32ToString i = if LargeInt.< (i, Int.toLarge 0) then "-"^LargeInt.toString(~i)
			      else LargeInt.toString i
	fun word32ToString i = LargeWord.toString i
	fun realToString r = if Real.<(r,0.0) then "-"^Real.toString(~r) else Real.toString r

	datatype jump = Got | Ret | IRet | ARet
	datatype branchinst = Lab of string * bool | Jump of jump | Non
	datatype registerOps = Load of (int * INSTRUCTION) | Store

	structure LabelMerge =
	    struct
		(* maps labels to a branch or return instruction *)
		val labelMerge: branchinst StringHash.t ref = ref (StringHash.new())

		(* set of reachable labels *)
		val reachable = ref (StringSet.new())

		(* set of used labels. Labels may be used in catch clauses although they
		 are not reachable, if they are used as the end of a try-catch block.
		 We differ reachable and used because we don't want to keep dead code after
		 a "catch", but need the label *)
		val usedlabel = ref (StringSet.new ())

		(* maps labels to the stack size on enter *)
		val stackSizeHash: int StringHash.t ref = ref (StringHash.new ())

		fun new () =
		    (labelMerge := StringHash.new();
		     reachable := StringSet.new();
		     stackSizeHash := StringHash.new();
		     usedlabel := StringSet.new())

	    (* Perform a unconditional jump to a label. If the first
	     operation there would be some kind of return, do so. *)
	    fun directJump lab' =
		case StringHash.lookup(!labelMerge, lab') of
		    NONE => "goto "^lab'
		  | SOME (Lab (lab'', _)) => directJump lab''
		  | SOME (Jump Ret) => "return"
		  | SOME (Jump ARet) => "areturn"
		  | SOME (Jump IRet) => "ireturn"
		  | SOME _ => raise Mitch

	    (* return the real label for this jump *)
	    fun condJump lab' =
		case StringHash.lookup(!labelMerge, lab') of
		    SOME (Lab (lab'', _)) => condJump lab''
		  | _ => lab'

	    (* For several branches to the same address, stack size has
	     to be equal in order to make the Java verifier happy *)
	    fun checkSizeAt (l, size) =
		case StringHash.lookup(!stackSizeHash, l) of
		    SOME s => if s <> size then
			(print ("Stack verification error: Size = "^
				Int.toString s^" or "^
				Int.toString size^
				" at "^l^" of "^(!actclass)^
				"."^(!actmeth)^".\n");
			 size)
			      else size
		  | NONE =>
				  (StringHash.insert (!stackSizeHash, l, size);
				   size)

	    (* Leave a method (via return or athrow).
	     Return the stack size after this instruction (i.e.
	     the size before the next instruction can be performed *)
	    fun leave (Comment _ :: rest, sizeAfter) = leave (rest, sizeAfter)
	      | leave (Label l :: _, sizeAfter) =
		(case StringHash.lookup(!stackSizeHash, l) of
		     SOME sz => sz
		   | NONE => sizeAfter)
	      | leave (_, sizeAfter) = sizeAfter

	    (* Leave a method (via return or athrow).
	     Check whether stack size is correct and
	     call leave to compute the stack size for the next
	     instruction. *)
	    fun leaveMethod (sizeAfter, rest) =
		(if sizeAfter <> 0
		     then
			 print ("Stack verification error: Size = "^
				Int.toString sizeAfter^" leaving "^(!actclass)^
				"."^(!actmeth)^" after "^(!lastLabel)^".\n")
		 else ();
		     leave (rest, sizeAfter))

	    (* merge two labels *)
	    fun merge (l', l'') =
		StringHash.insert (!labelMerge, l', l'')

	    (* mark a lable as reachable *)
	    fun setReachable l' =
		StringSet.insert (!reachable, l')

	    (* check whether a lable is reachable or not *)
	    fun isReachable l' =
		let val result =
		    isSome (StringHash.lookup (!labelMerge, l'))
		    orelse StringSet.member (!reachable, l')
		in
		    if !VERBOSE >= 2 then
			print ("Label "^l'^" is "^
			       (if result then "" else "not ")^
				    "reachable.\n")
		    else ();
			result
		end

	    (* check whether a lable is used or not *)
	    fun isUsed l' =
		let
		    val result =StringSet.member (!usedlabel, l')
		in
		    if !VERBOSE >= 2 then
			print ("Label "^l'^" is "^
			       (if result then "" else "not ")^
				    "used.\n")
		    else ();
			result
		end

	    (* mark a lable as used *)
	    fun setUsed l' = StringSet.insert (!usedlabel, l')
	    end

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

		(* new has to be called before each code optimization *)
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

		(* returns the VIRTUAL register associated with the one in
		 question *)
		fun getOrigin register =
		    let
			val lookup = Array.sub (!fusedwith, register)
		    in
			if lookup = ~1 orelse lookup = register
			    then register
			else getOrigin lookup
		    end

		(* returns the JVM register on which a virtual register is
		 mapped *)
		fun get reg =
		    if !OPTIMIZE >= 1 then Array.sub (!regmap, getOrigin reg) else reg

		(* returns Jasmin-code for astore/istore. May be pop for
		 unread registers *)
		fun store (reg, job) =
		    if reg = ~1 then
			"pop"
		    else job^Int.toString reg

		(* called when a register is defined. Needed for lifeness analysis *)
		fun define (register, pos) =
		    let
			val old = Array.sub(!from, register)
		    in
			if old = ~1 orelse old > pos
			    then Array.update(!from, register, pos)
			else ()
		    end

		(* we need to remember the last usage of a variable for lifeness analysis *)
		fun use (register, pos) =
		    let
			val lookup = getOrigin register
			val old = Array.sub(!to, lookup)
		    in
			if old < pos
			    then Array.update(!to, lookup, pos)
			else ()
		    end

		(* The real lifeness analysis. This function is called when we know the
		 life range of each virtual register *)
		fun assignAll parms =
		    let
			fun assign (register) =
			    let
				val notfused = (Array.sub (!fusedwith, register) = ~1)
				val genuineReg = getOrigin register
				val f' = Array.sub (!from, genuineReg)
				val t' = Array.sub (!to, genuineReg)
				fun assignNextFree (act) =
				    if f' > Array.sub (!jvmto, act)
					then (Array.update (!regmap, genuineReg, act);
					      Array.update (!jvmto, act, t'))
				    else assignNextFree (act+1)
			    in
				if f'= ~1 then
				    (* Never stored to this register. *)
				    ()
				else
				    if t' <> ~1 andalso
					(* If a register is never read,
					 we don't have to store it *)
					notfused then
					assignNextFree 1
				    else
					();
				if register+1<Array.length(!to)
				    then assign (register+1)
				else ()
			    end

			fun parmAssign 0 = ()
			  | parmAssign p =
			    (Array.update(!regmap, p, p);
			     Array.update(!jvmto, p, Array.sub (!to, p));
			     parmAssign (p-1))
		    in
			Array.update(!regmap, 0, 0);
			parmAssign parms;
			assign (parms+1)
		    end

		(* We remember the code position of labels. Needed for
		 lifeness analysis again. *)
		fun defineLabel (label', pos) =
		     StringHash.insert (!labHash, label', pos)

		(* Every jump could effect the life range of each (virtual)
		 register. We have to check this. *)
		fun addJump (f', tolabel) =
		    let
			val t = StringHash.lookup(!labHash, tolabel)

		    (* Checks whether we jump from behind last usage into the range of the
		     virtual register or from inside the range to somewhere before first
		     declaration. If so, the range of this register is changed. *)
			fun checkReg (register) =
			    let
				val genuineReg = getOrigin register
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
				     else if register+1<Array.length (!to)
					      then checkReg (register+1)
					  else ()
			    end
		    in
			if isSome t then checkReg 1 else ()
		    end

		(* returns the maximum JVM register we use *)
		fun max default =
		    let
			fun findMax r =
			    if r+1=Array.length (!jvmto) orelse
				Array.sub (!jvmto, r) = ~1 then
				r-1
			    else findMax (r+1)
		    in
			if !OPTIMIZE >= 1 then
			    findMax 2
			else default
		    end

		(* fuses two virtual registers. Called on aload/astore pairs *)
		fun fuse (x,u) =
		    let
			val (a,b) = if u = 1 then (u,x) else (x,u)
			val ori = getOrigin a
		    in
			if Array.sub(!defines, u)<2 then
			    (Array.update(!fusedwith, b, ori);
			     true)
			 else false
		    end

		(* How often is a register defined? Most registers are defined only once.
		 However, there are a few ones which are defined twice. Those must
		 not be fused on aload/atore. *)
		fun countDefine reg =
		    Array.update (!defines, reg,
				  Array.sub(!defines, reg)+1)
	    end

	fun optimize (insts, registers, parms) =
	    let
		fun deadCode (last, (c as Comment _)::rest) =
		    c :: deadCode (last, rest)

		  | deadCode (Lab (lab', dropMode),Label lab''::rest) =
		    let
			val l'' = Lab (lab'', dropMode)
			val code'=(LabelMerge.merge(lab', l'');
				   deadCode (l'', rest))
		    in
			if LabelMerge.isUsed lab' then
			    Label lab' ::
			    code'
			else code'
		    end

		  | deadCode (Lab (lab', dropMode), Goto lab''::rest) =
		    let
			val code' =
			    (LabelMerge.merge (lab', Lab (lab'', false));
			     if dropMode then
				 deadCode (Jump Got, rest)
			     else
				 Goto lab'' ::
				 deadCode (Jump Got, rest))
		    in
			if LabelMerge.isUsed lab' then
			    Label lab' ::
			    code'
			else code'
		    end

		  | deadCode (Lab (lab', dropMode), Areturn::rest) =
		    let
			val code' =
			    (LabelMerge.merge (lab', Jump ARet);
			     deadCode (Jump ARet, rest))
			val code'' =
			    if dropMode then
				code'
			    else
				Areturn ::
				code'
		    in
			if LabelMerge.isUsed lab' orelse not dropMode then
			    Label lab' ::
			    code''
			else code''
		    end

		  | deadCode (Lab (lab', dropMode), Return::rest) =
		    let
			val code' =
			    (LabelMerge.merge (lab', Jump Ret);
			     deadCode (Jump Ret, rest))
			val code'' =
			    if dropMode then
				code'
			    else
				Return ::
				code'
		    in
			if LabelMerge.isUsed lab' orelse not dropMode then
			    Label lab' ::
			    code''
			else code''
		    end

		  | deadCode (Lab (lab', dropMode), Ireturn::rest) =
		    let
			val code' =
			    (LabelMerge.merge (lab', Jump IRet);
			     deadCode (Jump IRet, rest))
			val code'' =
			    if dropMode then
				code'
			    else
				Ireturn ::
				deadCode (Jump IRet, rest)
		    in
			if LabelMerge.isUsed lab' orelse not dropMode then
			    Label lab' ::
			    code''
			else code''
		    end

		  | deadCode (Lab (l', _), rest) =
		    Label l' :: deadCode (Non, rest)

		  (* In codegeneration we ensure that backward jumps
		   only occur to labels that can be reached from before.
		   Therefore, if we both don't know a label while parsing
		   top-down and cannot reach it because it is placed after
		   an unconditional jump, we can dump it *)
		  | deadCode (i as Jump _, (l'' as Label lab'')::rest) =
		    let
			val reachable = LabelMerge.isReachable lab''
			val code' =
			    deadCode
			    (if reachable
				 then Lab (lab'', true)
			     else i,
				 rest)
		    in
			if LabelMerge.isUsed lab'' andalso not reachable then
			    l'' :: code'
			    else code'
		    end

		  | deadCode (i as Jump _, _::rest) =
			 deadCode (i, rest)

		  | deadCode (Non, Label lab'::rest) =
			 deadCode (Lab (lab', false), rest)

		  | deadCode (Non, Goto lab''::rest) =
			 Goto lab'' :: deadCode (Jump Got, rest)

		  | deadCode (Non, Areturn::rest) =
			 Areturn :: deadCode (Jump ARet, rest)

		  | deadCode (Non, Return::rest) =
			 Return :: deadCode (Jump Ret, rest)

		  | deadCode (Non, Ireturn::rest) =
			 Ireturn :: deadCode (Jump IRet, rest)

		  | deadCode (Non, Athrow::rest) =
			 Athrow :: deadCode (Jump Got, rest)

		  | deadCode (Non, c::rest) =
			 ((case c of
			       Ifacmpeq l' => LabelMerge.setReachable l'
			     | Ifacmpne l' => LabelMerge.setReachable l'
			     | Ifeq l' => LabelMerge.setReachable l'
			     | Ificmpeq l' => LabelMerge.setReachable l'
			     | Ificmplt l' => LabelMerge.setReachable l'
			     | Ificmpne l' => LabelMerge.setReachable l'
			     | Ifne l' => LabelMerge.setReachable l'
			     | Ifnull l' => LabelMerge.setReachable l'
			     | Tableswitch (_, ls, l) =>
				   List.app
				   LabelMerge.setReachable
				   (l::ls)
			     | Lookupswitch (_, ls, l) =>
				   List.app
				   LabelMerge.setReachable
				   (l::ls)
			     | _ => ());
			   c :: deadCode (Non, rest))

		  | deadCode (last, nil) = nil

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
		  | prepareLifeness (nil, pos) =
		(* Register 0 must not be overwritten. *)
		    JVMreg.use (0,pos)

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
		  | lifeness (Ifne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (Ifnull label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      lifeness (insts, pos+1))
		  | lifeness (_::insts, pos) =
		     lifeness (insts, pos+1)
		  | lifeness (nil, _) = ()

		fun fuse (old, (c as Comment _)::rest) =
		    c::fuse (old, rest)
		  | fuse (l as Load (a,ori), (stor as Astore b)::rest) =
		    (if JVMreg.fuse (a,b)
			 then fuse (Store, rest)
		     else ori::stor::fuse(Store,rest))
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
		  | fuse (old,nil) = nil

		fun flatten (inst, akku) =
		    (case inst of
			 Nop => akku
		       | Ifstatic (stamp', then', else') =>
			     List.foldr
			     flatten
			     akku
			     (if Lambda.isStatic stamp'
				  then then'
			      else else')
		       | Multi insts =>
			     List.foldr
			     flatten
			     akku
			     insts
		       | Get insts =>
			     List.foldr
			     flatten
			     akku
			     insts
		       | stor as Astore reg =>
			     (JVMreg.countDefine reg;
			      stor::akku)
		       | stor as Istore reg =>
			     (JVMreg.countDefine reg;
			      stor::akku)
		       | _ =>
			     inst ::
			     akku)

		val flattened = foldr flatten nil insts
	    in
		if !OPTIMIZE >=1 then
		    let
			val _ = if !VERBOSE >= 3 then print "fusing registers..." else ()
			val d' = fuse (Store, flattened)
			val _ = if !VERBOSE >= 3 then print "done.\n" else ()
		    in
			(* Register 0 ('this'-Pointer) and
			 register 1 (formal Parameter) are
			 defined when entering a method *)
			JVMreg.define(0, 0);
			JVMreg.define(1, 0);
			if !VERBOSE >= 3 then print "preparing lifeness... " else ();
			prepareLifeness (d', 0);
			if !VERBOSE >= 3 then print "doing lifeness... " else ();
			lifeness (d', 0);
			if !VERBOSE >= 3 then print "done.\n" else ();
			JVMreg.assignAll parms;
			if !OPTIMIZE >= 2 then
			    deadCode (Non, d')
			else d'
		    end
		else flattened
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
	  | stackNeedInstruction (Checkcast _) = 0
	  | stackNeedInstruction (Comment _) = 0
	  | stackNeedInstruction Dup = 1
	  | stackNeedInstruction (Fconst _) = 1
	  | stackNeedInstruction (Get _) = 0
	  | stackNeedInstruction (Getfield _) = 0
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
	  | stackNeedInstruction (Ifne _) = ~1
	  | stackNeedInstruction (Ifnull _) = ~1
	  | stackNeedInstruction (Ifstatic _) = 0
	  | stackNeedInstruction (Iload _) = 1
	  | stackNeedInstruction (Instanceof _) = 0
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist,[Voidsig]))) = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokeinterface (_,_, (arglist, _)))      = ~(siglength arglist)
	  | stackNeedInstruction (Invokespecial (_,_,(arglist,_)))          = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,[Voidsig])))    = ~(siglength arglist)
	  | stackNeedInstruction (Invokestatic  (_,_,(arglist,_)))          = 1-(siglength arglist)
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,[Voidsig])))    = ~1-(siglength arglist)
	  | stackNeedInstruction (Invokevirtual (_,_,(arglist,_)))          = ~(siglength arglist)
	  | stackNeedInstruction Ireturn = ~1
	  | stackNeedInstruction (Istore _) = ~1
	  | stackNeedInstruction (Label _) = 0
	  | stackNeedInstruction Lcmp = ~1
	  | stackNeedInstruction (Ldc _) = 1 (* we don't use long and double! *)
	  | stackNeedInstruction (Multi _) = 0
	  | stackNeedInstruction (New _) = 1
	  | stackNeedInstruction Nop = 0
	  | stackNeedInstruction Pop = ~1
	  | stackNeedInstruction (Putfield _) = ~2
	  | stackNeedInstruction (Putstatic _) = ~1
	  | stackNeedInstruction Return = 0
	  | stackNeedInstruction (Sipush _) = 1
	  | stackNeedInstruction Swap = 0
	  | stackNeedInstruction (Tableswitch _) = ~1
	  | stackNeedInstruction (Lookupswitch _) = ~1
	  | stackNeedInstruction (Var _) = 0

	(* generate Jasmin code for Catches *)
	fun catchesToJasmin (catches, out) =
	    let
		fun catchToJasmin (Catch(cn,from,to,use)) =
		    (LabelMerge.checkSizeAt (use, 1);
		     (* note that to is not necessarily reachable.
		      Anyhow, we have to generate the label for it. *)
		     LabelMerge.setReachable from;
		     LabelMerge.setReachable use;
		     LabelMerge.setUsed from;
		     LabelMerge.setUsed to;
		     LabelMerge.setUsed use;
		     TextIO.output(out,".catch "^cn^" from "^from^
				   " to "^to^" using "^use^"\n"))
	    in
		app catchToJasmin catches
	    end

	local
	    (* generate Jasmin code for an instruction *)
	    fun instructionToJasmin (Astore j, s) =
		let
		    val i = if s then JVMreg.get j-1 else JVMreg.get j
		in
		    if i<0 then "pop"
		    else
			if i<4 then
			    JVMreg.store(i,"astore_")
			else JVMreg.store(i,"astore ")
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
	      | instructionToJasmin (Areturn,_) = "areturn"
	      | instructionToJasmin (Arraylength,_) = "arraylength"
	      | instructionToJasmin (Athrow,_) = "athrow"
	      | instructionToJasmin (Bipush i,_) = "bipush "^intToString i
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
	      | instructionToJasmin (Get _, _) = raise Error ""
	      | instructionToJasmin (Getfield(fieldn, arg),_) = "getfield "^fieldn^" "^
				 (desclist2string arg)
	      | instructionToJasmin (Getstatic(fieldn, arg),_) = "getstatic "^fieldn^" "^
				     (desclist2string arg)
	      | instructionToJasmin (Goto l,isstatic) = LabelMerge.directJump l
	      | instructionToJasmin (Iconst i,_) =
				     if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	      | instructionToJasmin (Iadd,_) = "iadd"
	      | instructionToJasmin (Ifacmpeq l,_) = "if_acmpeq "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ifacmpne l,_) = "if_acmpne "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ifeq l,_) = "ifeq "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ificmpeq l,_) = "if_icmpeq "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ificmplt l,_) = "if_icmplt "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ificmpne l,_) = "if_icmpne "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ifne l,_) = "ifne "^(LabelMerge.condJump l)
	      | instructionToJasmin (Ifnull l,_) = "ifnull "^(LabelMerge.condJump l)
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
						 JVMreg.store(i,"istore_")
					     else JVMreg.store(i,"istore ")
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
	      | instructionToJasmin (Lookupswitch (switchlist, labellist, default), _) =
			     let
				 fun flatten (switch::switches, lab::labels) =
				     flatten (switches, labels)^
				     ("\t"^int32ToString switch^" "^
				      LabelMerge.condJump lab^"\n")
				   | flatten _ = ""
			     in
				 "lookupswitch\n"^
				 flatten (switchlist, labellist)^
				 "default: "^LabelMerge.condJump default
			     end
	      | instructionToJasmin (Multi _,_) = raise Error ""
	      | instructionToJasmin (New cn,_) = "new "^cn
	      | instructionToJasmin (Nop, _) = "nop"
	      | instructionToJasmin (Pop,_) = "pop"
	      | instructionToJasmin (Putfield(cn,arg),_) = "putfield "^cn^" "^
			     desclist2string arg
	      | instructionToJasmin (Putstatic(cn,arg),_) = "putstatic "^cn^" "^
			     desclist2string arg
	      | instructionToJasmin (Return,_) = "return"
	      | instructionToJasmin (Sipush i,_) = "sipush "^intToString i
	      | instructionToJasmin (Swap,_) = "swap"
	      | instructionToJasmin (Tableswitch(low,labellist, default),_) =
			     let
				 (* We have to reverse the labellist here, because
				  CodeGen generates it from right to left *)
				 fun flatten (lab::labl) =
				     (flatten labl)^
				     ("\t"^(LabelMerge.condJump lab)^"\n")
				   | flatten nil = ""
			     in
				 "tableswitch "^(int32ToString low)^"\n"^
				 (flatten labellist)^
				 "default: "^(LabelMerge.condJump default)
			     end
	      |  instructionToJasmin (Var (number', name', descriptor', from', to'), isStatic) =
			     if (!DEBUG >= 1) then
				 ".var "^
				 (Int.toString
				  (if isStatic then number'-1 else number'))
				 ^" is "^name'^" "^(desclist2string descriptor')^
				 " from "^(LabelMerge.condJump from')^" to "^(LabelMerge.condJump to')
			     else ""
	in
	    fun instructionsToJasmin (insts, enterstack, staticapply, ziel) =
		let
		    fun noStack (Comment _) = true
		      | noStack (Goto _) = true
		      | noStack Athrow = true
		      | noStack Return = true
		      | noStack Areturn = true
		      | noStack Ireturn = true
		      | noStack _ = false

		    fun recurse (i::is, need, max) =
			let
			    val ins = instructionToJasmin (i, staticapply)

			    val sizeAfter = need+stackNeedInstruction i

			    val nextSize = case i of
				Areturn => LabelMerge.leaveMethod (sizeAfter, is)
			      (* The Java verifier doesn't expect the stack to be
			       empty on athrow.
			       Nevertheless, JVM cleans the stack when exceptions
			       are handled, so we have to ensure an empty stack
			       when throwing exceptions. *)
			      | Athrow => LabelMerge.leaveMethod (sizeAfter, is)
			      | Goto label =>
				    (LabelMerge.checkSizeAt (label, sizeAfter);
				     LabelMerge.leave (is, sizeAfter))
			      | Ifacmpeq label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ifacmpne label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ifeq label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ificmpeq label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ificmplt label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ificmpne label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ifne label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ifnull label => LabelMerge.checkSizeAt (label, sizeAfter)
			      | Ireturn => LabelMerge.leaveMethod (sizeAfter, is)
			      | Label label =>
				    (lastLabel := label;
				     LabelMerge.checkSizeAt (label, sizeAfter))
			      | Lookupswitch (_, labs, label) =>
				    foldr
				    LabelMerge.checkSizeAt
				    sizeAfter
				    (label::labs)
			      | Return => LabelMerge.leaveMethod (sizeAfter, is)
			      | Tableswitch (_, labs, label) =>
				    foldr
				    LabelMerge.checkSizeAt
				    sizeAfter
				    (label::labs)
			      | _ => sizeAfter
			in
			    if ins <> "" then TextIO.output (ziel,ins^"\n") else ();
			    if noStack i
				then ()
			    else
				(if !DEBUG>=1
				     then (TextIO.output (ziel,"\t\t.line "^line());
					   TextIO.output (ziel,"\t; Stack: "^Int.toString nextSize^
							  " Max: "^Int.toString max^"\n"))
				 else ());
			     recurse (is, nextSize, (Int.max (nextSize,max)))
			end
		      | recurse (nil,need,max) =
			 if enterstack then
			     TextIO.output
			     (ziel, ".limit stack "^Int.toString max^"\n")
			 else ()
		in
		    recurse (insts, 0, 0)
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
		fun methodToJasmin (Method(access,methodname,
					   methodsig as (parms,_),Locals perslocs,
					   instructions, catches)) =
		    let
			val staticapply =
			    List.exists
			    (fn MStatic => true
			  | _ => false)
			    access

			val parmscount = siglength parms
		    in
			LabelMerge.new();
			JVMreg.new perslocs;
			TextIO.output(io,".method "^
				      (mAccessToString access)^
				      methodname^
				      (descriptor2string methodsig)^"\n");
			actmeth := methodname;
			catchesToJasmin (catches, io);
			instructionsToJasmin
			(optimize (instructions, perslocs, parmscount),
			 true,
			 staticapply,
			 io);
			TextIO.output(io,".limit locals "^
				      Int.toString(JVMreg.max perslocs+1+parmscount)
				      ^"\n");
			TextIO.output(io,".end method\n\n")
		    end
	    in
		actclass:= name;
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

