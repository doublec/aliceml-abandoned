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

	datatype deb = SIs of string*INSTRUCTION list
	exception Debug of deb

	val actclass = ref ""
	val actmeth = ref ""
	val lastLabel = ref ~1
	val finish = ref ""

	fun makeArityString (0,x) = x
	  | makeArityString (n,x) = makeArityString (n-1, x^"[")

	fun intToString i = if i<0 then "-"^Int.toString(~i) else Int.toString i
	fun int32ToString i = if LargeInt.< (i, Int.toLarge 0) then "-"^LargeInt.toString(~i)
			      else LargeInt.toString i
	fun word32ToString i = LargeWord.toString i
	fun realToString r = if Real.<(r,0.0) then "-"^Real.toString(~r) else Real.toString r

	datatype jump = Got | Ret | IRet | ARet
	datatype branchinst = Lab of label * bool | Jump of jump | Non
	datatype registerOps = Load of (stamp * INSTRUCTION) | Store

	fun labtoString (Lab (label', _)) = Label.toString label'
	  | labtoString (Jump Got) = "Goto"
	  | labtoString (Jump Ret) = "Return"
	  | labtoString (Jump IRet) = "Ireturn"
	  | labtoString (Jump ARet) = "Areturn"
	  | labtoString Non = "Non"

	structure LabelMerge =
	    struct
		(* maps labels to a branch or return instruction *)
		val labelMerge: branchinst IntHash.t ref = ref (IntHash.new())

		(* set of reachable labels *)
		val reachable = ref (IntSet.new())

		(* set of used labels. Labels may be used in catch clauses although they
		 are not reachable, if they are used as the end of a try-catch block.
		 We differ reachable and used because we don't want to keep dead code after
		 a "catch", but need the label *)
		val usedlabel = ref (IntSet.new ())

		(* maps labels to the stack size on enter *)
		val stackSizeHash: int IntHash.t ref = ref (IntHash.new ())

		fun new () =
		    (labelMerge := IntHash.new();
		     reachable := IntSet.new();
		     stackSizeHash := IntHash.new();
		     usedlabel := IntSet.new())

	    (* Perform an unconditional jump to a label. If the first
	     operation there would be some kind of return, do so. *)
	    fun directJump lab' =
		case IntHash.lookup(!labelMerge, lab') of
		    NONE => "goto "^Label.toString lab'
		  | SOME (Lab (lab'', _)) => directJump lab''
		  | SOME (Jump Ret) => "return"
		  | SOME (Jump ARet) => "areturn"
		  | SOME (Jump IRet) => "ireturn"
		  | SOME _ => Crash.crash "ToJasmin: directJump"

	    (* return the real label for this jump *)
	    fun condJump lab' =
		case IntHash.lookup(!labelMerge, lab') of
		    SOME (Lab (lab'', _)) => condJump lab''
		  | _ => lab'

	    fun labName lab' = "label"^Int.toString (condJump lab')

	    (* For several branches to the same address, stack size has
	     to be equal in order to make the Java verifier happy *)
	    fun checkSizeAt (l, size) =
		let
		    val l'= condJump l
		in
		    case IntHash.lookup(!stackSizeHash, l') of
			SOME s => (if s <> size then
				       print ("Stack verification error: Size = "^
					      Int.toString s^" or "^
					      Int.toString size^
					      " at "^Label.toString l'^" of "^(!actclass)^
					      "."^(!actmeth)^".\n")
				   else ();
				       size)
		      | NONE => (IntHash.insert (!stackSizeHash, l', size);
				 size)
		end

	    (* Leave a method (via return or athrow).
	     Return the stack size after this instruction (i.e.
	     the size before the next instruction can be performed *)
	    fun leave (Comment _ :: rest, sizeAfter) = leave (rest, sizeAfter)
	      | leave (Line _ :: rest, sizeAfter) = leave (rest, sizeAfter)
	      | leave (Label l :: _, sizeAfter) =
		(case IntHash.lookup(!stackSizeHash, l) of
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
				"."^(!actmeth)^" after "^Label.toString (!lastLabel)^".\n")
		 else ();
		     leave (rest, sizeAfter))

	    (* merge two labels *)
	    fun merge (l', l'') =
		(if !VERBOSE >=2 then print ("Merging "^Label.toString l'^" and "^
					     labtoString l''^".\n") else ();
		 IntHash.insert (!labelMerge, l', l''))

	    (* mark a lable as reachable *)
	    fun setReachable l' =
		IntSet.insert (!reachable, l')

	    (* check whether a lable is reachable or not *)
	    fun isReachable l' =
		let val result =
		    isSome (IntHash.lookup (!labelMerge, l'))
		    orelse IntSet.member (!reachable, l')
		in
		    if !VERBOSE >= 2 then
			print ("Label "^Label.toString l'^" is "^
			       (if result then "" else "not ")^
				    "reachable.\n")
		    else ();
			result
		end

	    (* check whether a lable is used or not *)
	    fun isUsed l' =
		let
		    val result =IntSet.member (!usedlabel, l')
		in
		    if !VERBOSE >= 2 then
			print ("Label "^Label.toString l'^" is "^
			       (if result then "" else "not ")^
				    "used.\n")
		    else ();
			result
		end

	    (* mark a lable as used *)
	    fun setUsed l' = IntSet.insert (!usedlabel, l')
	    end

	structure JVMreg =
	    struct
		(* maps stamps to their first defining position *)
		val fromPos: int StampHash.t ref = ref (StampHash.new ())

		(* maps stamps to their last read access position *)
		val toPos: int StampHash.t ref = ref (StampHash.new ())

		(* maps stamps to JVM registers *)
		val regmap =
		    let
			val r: int StampHash.t = (StampHash.new ())
		    in
			StampHash.insert (r, thisStamp, 0);
			StampHash.insert (r, parm1Stamp, 1);
			StampHash.insert (r, parm2Stamp, 2);
			StampHash.insert (r, parm3Stamp, 3);
			StampHash.insert (r, parm4Stamp, 4);
			StampHash.insert (r, parm5Stamp, 5);
			ref r
		    end

		val maxReg = ref 0;

		(* maps JVM registers to their last read access position *)
		val jvmto: int IntHash.t ref = ref (IntHash.new ())

		(* maps labels to their position *)
		val labHash: int IntHash.t ref = ref (IntHash.new())

		(* maps stamps to other stamps (used for
		 Aload/Astore register fusion) *)
		val fusedwith: stamp StampHash.t ref = ref (StampHash.new ())

		(* Some (few) stamps are assigned more than once.
		 Therefore, they cannot be fused on Aload/Astore sequences *)
		val defines: int StampHash.t ref = ref (StampHash.new ())

		(* new has to be called before each code optimization *)
		fun new regs =
		    (fromPos := StampHash.new();
		     toPos := StampHash.new();
		     jvmto := IntHash.new();
		     labHash := IntHash.new();
		     fusedwith := StampHash.new();
		     defines := StampHash.new();
		     maxReg:=regs)

		(* returns the stamp associated with the one in question *)
		fun getOrigin register =
		    case StampHash.lookup (!fusedwith, register) of
			SOME fused => if fused = register then fused else getOrigin fused
		      | NONE => register

		fun lookup stamphashreg =
		    case StampHash.lookup stamphashreg of
			NONE => ~1
		      | SOME i => i

		fun lookupInt inthashreg =
		    case IntHash.lookup inthashreg of
			NONE => ~1
		      | SOME i => i

		(* returns the JVM register on which a stamp is mapped *)
		fun get reg =
		    let
			val r = case StampHash.lookup (!regmap, getOrigin reg) of
			    NONE => Stamp.hash reg
			  | SOME v => v
		    in
			if !maxReg < r then maxReg := r else ();
			if !VERBOSE >=3 then
			    print ("Accessing stamp "^Stamp.toString reg^" in "^
				   "JVM register "^Int.toString r^"\n")
			else ();
			r
		    end

		(* returns Jasmin-code for astore/istore. May be pop for
		 unread registers *)
		fun store (reg, job) =
		    if reg = ~1 then
			"pop"
		    else job^Int.toString reg

		(* called when a stamp is defined. Needed for liveness analysis *)
		fun define (stamp', pos) =
		    let
			val old = lookup (!fromPos, stamp')
		    in
			if old = ~1 orelse old > pos
			    then StampHash.insert (!fromPos, stamp', pos)
			else ()
		    end

		(* we need to remember the last usage of a variable for liveness analysis *)
		fun use (stamp', pos) =
		    let
			val ori = getOrigin stamp'
			val old = lookup (!toPos, ori)
		    in
			if old < pos
			    then StampHash.insert(!toPos, ori, pos)
			else ()
		    end

		(* The real liveness analysis. This function is called when we know the
		 life range of each stamp *)
		fun assignAll () =
		    let
			fun assign (stamp',_) =
			    let
				val genuineReg = getOrigin stamp'
				val notmapped = not (isSome (StampHash.lookup
							     (!fusedwith, stamp')))
				    andalso
				    (not (isSome (StampHash.lookup (!regmap, genuineReg))))
				val f' = lookup (!fromPos, genuineReg)
				val t' = lookup (!toPos, genuineReg)
				fun assignNextFree act =
				    if f' >= lookupInt (!jvmto, act)
					then (if !VERBOSE >=2 then
						  print ("map "^Stamp.toString genuineReg^" to "^
							 Int.toString act^"(used until "^Int.toString (lookupInt (!jvmto, act))^"\n") else ();
					      case StampHash.lookup (!regmap, genuineReg) of
						      NONE =>
							  (StampHash.insert (!regmap, genuineReg, act);
							   IntHash.insert (!jvmto, act, t'))
						    | SOME v => print ("ERROR: trying to overwrite old value"^
								       Int.toString v^"\n"))
				    else
					assignNextFree (act+1)
			    in
				if !VERBOSE >= 2 then
				    print ("trying to assign "^Stamp.toString genuineReg^" from "^
					   Int.toString f'^" to "^Int.toString t'^"\n") else ();
				if f' <> ~1 andalso
				    (* Register is written at least once. *)
				    t' <> ~1 andalso
				    (* If a register is never read,
				     we don't have to store it *)
				    notmapped then
				    (* don't map a Stamp twice *)
				    assignNextFree 1
				else
				    ();
				    if !VERBOSE >= 2 then
					print ("assign ok.\n") else ()
			    end
		    in
			IntHash.insert (!jvmto, 0, lookup(!toPos, thisStamp));
			IntHash.insert (!jvmto, 1, lookup(!toPos, parm1Stamp));
			IntHash.insert (!jvmto, 2, lookup(!toPos, parm2Stamp));
			IntHash.insert (!jvmto, 3, lookup(!toPos, parm3Stamp));
			IntHash.insert (!jvmto, 4, lookup(!toPos, parm4Stamp));
			IntHash.insert (!jvmto, 5, lookup(!toPos, parm5Stamp));

			StampHash.appi assign (!toPos)
		    end

		(* We remember the code position of labels. Needed for
		 liveness analysis again. *)
		fun defineLabel (label', pos) =
		     IntHash.insert (!labHash, label', pos)

		(* Every jump could effect the life range of each stamp. We have to check this. *)
		fun addJump (f', tolabel) =
		    let
			val t = IntHash.lookup(!labHash, tolabel)

		    (* Checks whether we jump from behind last usage into the range of the
		     stamp or from inside the range to somewhere before first
		     declaration. If so, the range of this stamp is changed. *)
			fun checkReg (stamp', _) =
			    let
				val genuineReg = getOrigin stamp'
				val regfrom = lookup (!fromPos, genuineReg)
				val regto = lookup (!toPos, genuineReg)
				val t' = case t of
				    NONE => Crash.crash "ToJasmin: addJump"
				  | SOME v => v
			    in
				if f' > regto andalso t' > regfrom andalso t' > regto
				    then
					StampHash.insert (!toPos, stamp', t')
				else if f' > regfrom andalso f' <regto
				    andalso t' < regfrom
					 then StampHash.insert (!fromPos, genuineReg, f')
				     else ()
			    end
		    in
			if isSome t then StampHash.appi checkReg (!toPos) else ()
		    end

		(* fuses two stamps. Called on aload/astore pairs *)
		fun fuse (x,u) =
		    if u=parm1Stamp orelse u=parm2Stamp orelse u=parm3Stamp orelse
			u=parm4Stamp orelse u=parm5Stamp
			then false
		    else
			if lookup (!defines, u)<2 then
			    (StampHash.insert(!fusedwith, u, getOrigin x);
			     if !VERBOSE >= 2 then
				 print ("fuse "^Stamp.toString x^" with "^Stamp.toString u^".\n")
			     else ();
				 true)
			else false

		(* How often is a stamp written to? Most stamps are written only
		 once. However, there are a few ones which are written twice. Those must
		 not be fused on aload/atore sequences. *)
		fun countDefine reg =
		    StampHash.insert (!defines, reg,
				      case StampHash.lookup(!defines, reg) of
					  NONE => 1
					| SOME i => i+1)
	    end

	(* All catch instructions are reversed and placed at the very beginning of
	 a method. *)
	structure Catches =
	    struct
		val list = ref ([]: INSTRUCTION list)

		fun new () = list := []

		fun add c = list := c:: !list

		fun get instructions = !list @ instructions
	    end

	fun optimize insts =
	    let
		fun deadCode (last, (c as Comment _)::rest) =
		    c :: deadCode (last, rest)

		  | deadCode (last, (l as Line _):: rest) =
		    l :: deadCode (last, rest)

		  | deadCode (last, (c as Catch (_,fromC,toC,using))::rest) =
		    (Catches.add c;
		     (* note that toC is not necessarily reachable.
		      Anyhow, we have to generate the label for it. *)
		     LabelMerge.setReachable fromC;
		     LabelMerge.setReachable using;
		     LabelMerge.setUsed fromC;
		     LabelMerge.setUsed toC;
		     LabelMerge.setUsed using;
		     deadCode (last, rest))

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
		   an unconditional jump, we can dump it.
		   *)
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
			 (LabelMerge.setReachable lab'';
			  Goto lab'' :: deadCode (Jump Got, rest))

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
			     | Lookupswitch ((_, ls), l) =>
				   List.app
				   LabelMerge.setReachable
				   (l::ls)
			     | _ => ());
			   c :: deadCode (Non, rest))

		  | deadCode (last, nil) = nil

		fun prepareLiveness (Astore r::insts, pos) =
		    (JVMreg.define(r, pos);
		     prepareLiveness (insts,pos+1))
		  | prepareLiveness (Istore r::insts, pos) =
		    (JVMreg.define(r, pos);
		     prepareLiveness (insts,pos+1))
		  | prepareLiveness (Aload r::insts, pos) =
		    (JVMreg.use(r, pos);
		     prepareLiveness (insts,pos+1))
		  | prepareLiveness (Iload r::insts, pos) =
		    (JVMreg.use(r, pos);
		     prepareLiveness (insts,pos+1))
		  | prepareLiveness (Label l'::insts, pos) =
		    (JVMreg.defineLabel (l', pos);
		     prepareLiveness (insts,pos+1))
		  | prepareLiveness (_::insts, pos) =
		    prepareLiveness (insts,pos+1)
		  | prepareLiveness (nil, pos) =
		(* Register 0 must not be overwritten. *)
		    JVMreg.use (thisStamp,pos)

		fun liveness (Goto label'::insts, pos) =
		    (JVMreg.addJump(pos, label');
		     liveness (insts, pos+1))
		  | liveness (Ifacmpeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ifacmpne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ifeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ificmpeq label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		   | liveness (Ificmplt label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ificmpne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ifne label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (Ifnull label'::insts, pos) =
		     (JVMreg.addJump(pos, label');
		      liveness (insts, pos+1))
		  | liveness (_::insts, pos) =
		     liveness (insts, pos+1)
		  | liveness (nil, _) = ()

		fun fuse (old, (c as Comment _)::rest) =
		    c::fuse (old, rest)
		  | fuse (old, (l as Line _) :: rest) =
		    l :: fuse (old, rest)
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
			 Aload reg => Aload (Lambda.getLambda reg)::akku
		       | Astore reg =>
			     (JVMreg.countDefine reg;
			      Astore (Lambda.getLambda reg)::akku)
		       | Call (cls, meth, goto', apply') =>
			     List.foldr
			     flatten
			     akku
			     (if !actclass = cls andalso !actmeth = meth
				  then goto'
			      else apply')
		       | Get insts =>
			     List.foldr
			     flatten
			     akku
			     insts
		       (* sort lookupswitches. if possible, change to tableswitch *)
		       | Lookupswitch (keylabs, def) =>
			     let
				 val (key, labs) = MergeSort.sort keylabs
				 fun isTable (last, nil) = (true, last)
				   | isTable (last, next::rest) =
				     if last - 1 = next
					 then isTable (next, rest)
				     else (false, last)
			     in
				 case key of
				     nil => akku
				   | (first :: rest) =>
					 case isTable (first, rest) of
					     (true, beg) => Tableswitch (beg, labs, def) :: akku
					   | (_, _) => Lookupswitch ((key, labs), def) :: akku
			     end
		       | Iload reg => Iload (Lambda.getLambda reg)::akku
		       | Istore reg =>
			     (JVMreg.countDefine reg;
			     Istore (Lambda.getLambda reg)::akku)
		       | Multi insts =>
			     List.foldr
			     flatten
			     akku
			     insts
		       | Nop => akku
		       | _ =>
			     inst ::
			     akku)

		(* instructions must be flattened to do this.
		 With optimizations on, this is done in dead code
		 elemination. *)
		fun extractCatch ((c as Catch (_,fromC,toC,using))::rest) =
		    (Catches.add c;
		     (* note that toC is not necessarily reachable.
		      Anyhow, we have to generate the label for it. *)
		     LabelMerge.setReachable fromC;
		     LabelMerge.setReachable using;
		     LabelMerge.setUsed fromC;
		     LabelMerge.setUsed toC;
		     LabelMerge.setUsed using;
		     extractCatch rest)
		  | extractCatch (x::rest) =
		    x :: extractCatch rest
		  | extractCatch nil = nil

		val flattened = foldr flatten nil insts
	    in
		if !OPTIMIZE >=1 then
		    let
			val _ = if !VERBOSE >= 3 then print "fusing registers..." else ()
			val d' = fuse (Store, flattened)
			val _ = if !VERBOSE >= 3 then print "done.\n" else ()
		    in
			if !VERBOSE >= 3 then print "preparing liveness... " else ();
			prepareLiveness (d', 0);
			if !VERBOSE >= 3 then print "doing liveness... " else ();
			liveness (d', 0);
			if !VERBOSE >= 3 then print "done.\n" else ();
			JVMreg.assignAll ();
			if !OPTIMIZE >= 2 then
			    deadCode (Non, d')
			else d'
		    end
		else extractCatch flattened
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
	  | stackNeedInstruction (Call _) = 0
	  | stackNeedInstruction (Catch _) = 0
	  | stackNeedInstruction (Checkcast _) = 0
	  | stackNeedInstruction (Comment _) = 0
	  | stackNeedInstruction (Line _) = 0
	  | stackNeedInstruction Dup = 1
	  | stackNeedInstruction Fcmpl = ~1
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
	      | instructionToJasmin (Catch(cn,from,to,use), _) =
		".catch "^cn^" from "^LabelMerge.labName from^
		" to "^LabelMerge.labName to^" using "^
		LabelMerge.labName use
	      | instructionToJasmin (Call _,_) =
					 Crash.crash "IntructionToJasmin: unresolved Ifstatic"
	      | instructionToJasmin (Checkcast cn,_) = "checkcast "^cn
	      | instructionToJasmin (Comment c,_) =
		    if !DEBUG>=1
			then "\t; "^c
		    else ""
	      | instructionToJasmin (Line l,_) =
			if !LINES andalso l <> 0 then "\t.line "^Int.toString l else ""
	      | instructionToJasmin (Dup,_) = "dup"
	      | instructionToJasmin (Fcmpl,_) = "fcmpl"
	      | instructionToJasmin (Fconst i,_) =
			if i=0 then
			    "fconst_0"
			else if i=1 then
			    "fconst_1"
			     else "fconst_2"
	      | instructionToJasmin (Get _, _) = Crash.crash "instructionToJasmin: Unresolved Get"
	      | instructionToJasmin (Getfield(fieldn, arg),_) = "getfield "^fieldn^" "^
				 (desclist2string arg)
	      | instructionToJasmin (Getstatic(fieldn, arg),_) = "getstatic "^fieldn^" "^
				     (desclist2string arg)
	      | instructionToJasmin (Goto l,isstatic) = LabelMerge.directJump l
	      | instructionToJasmin (Iconst i,_) =
				     if i = ~1 then "iconst_m1" else "iconst_"^Int.toString i
	      | instructionToJasmin (Iadd,_) = "iadd"
	      | instructionToJasmin (Ifacmpeq l,_) = "if_acmpeq "^(LabelMerge.labName l)
	      | instructionToJasmin (Ifacmpne l,_) = "if_acmpne "^(LabelMerge.labName l)
	      | instructionToJasmin (Ifeq l,_) = "ifeq "^(LabelMerge.labName l)
	      | instructionToJasmin (Ificmpeq l,_) = "if_icmpeq "^(LabelMerge.labName l)
	      | instructionToJasmin (Ificmplt l,_) = "if_icmplt "^(LabelMerge.labName l)
	      | instructionToJasmin (Ificmpne l,_) = "if_icmpne "^(LabelMerge.labName l)
	      | instructionToJasmin (Ifne l,_) = "ifne "^(LabelMerge.labName l)
	      | instructionToJasmin (Ifnull l,_) = "ifnull "^(LabelMerge.labName l)
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
	      | instructionToJasmin (Label l,_) = Label.toString l^": "
	      | instructionToJasmin (Lcmp,_) = "lcmp"
	      | instructionToJasmin (Ldc(JVMString s),_) = "ldc \""^String.toCString s^"\""
	      | instructionToJasmin (Ldc(JVMFloat r),_) = "ldc "^realToString r
	      | instructionToJasmin (Ldc(JVMInt i),_) = "ldc "^int32ToString i
	      | instructionToJasmin (Ldc(JVMWord w),_) = "ldc "^word32ToString w
	      | instructionToJasmin (Ldc(JVMChar c),_) = "ldc "^Int.toString (Char.ord c)
	      | instructionToJasmin (Lookupswitch ((switchlist, labellist), default), _) =
			     let
				 fun flatten (switch::switches, lab::labels) =
				     flatten (switches, labels)^
				     ("\t"^int32ToString switch^": "^
				      LabelMerge.labName lab^"\n")
				   | flatten _ = ""
			     in
				 "lookupswitch\n"^
				 flatten (switchlist, labellist)^
				 "default: "^LabelMerge.labName default
			     end
	      | instructionToJasmin (Multi _,_) = Crash.crash "instructionToJasmin: unresolved Multi"
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
				     ("\t"^(LabelMerge.labName lab)^"\n")
				   | flatten nil = ""
			     in
				 "tableswitch "^(int32ToString low)^"\n"^
				 (flatten labellist)^
				 "default: "^(LabelMerge.labName default)
			     end
	      |  instructionToJasmin (Var (number', name', descriptor', from', to'), isStatic) =
			     if (!DEBUG >= 1) then
				 ".var "^
				 (Int.toString
				  (if isStatic then number'-1 else number'))
				 ^" is "^name'^" "^(desclist2string descriptor')^
				 " from "^(LabelMerge.labName from')^" to "^(LabelMerge.labName to')
			     else ""
	in
	    fun instructionsToJasmin (insts, enterstack, staticapply, ziel) =
		let
		    fun noStack (Comment _) = true
		      | noStack (Line _) = true
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
			      | Catch (_, try, to, catch) =>
				    (LabelMerge.checkSizeAt (catch, 1);
				     sizeAfter)
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
			      | Lookupswitch ((_, labs), label) =>
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
				     then (if !LINES then ()
					       else TextIO.output (ziel,"\t\t.line "^line());
					   TextIO.output (ziel,"\t; Stack: "^Int.toString nextSize^
							  " Max: "^Int.toString max^"\n"))
				 else ());
			     recurse (is, nextSize, (Int.max (nextSize,max)))
			end
		      | recurse (nil,need,max) =
			if enterstack then
			    TextIO.output
			    (ziel, !finish^".limit stack "^Int.toString max^"\n")
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
		fun lmaa (von,bis) = if von >= bis then TextIO.output(io, "goto anfg\n")
				     else (TextIO.output(io, "aconst_null\nastore "^Int.toString bis^"\n");
					   lmaa (von, bis-1))

		fun methodToJasmin (Method(access,methodname,
					   methodsig as (parms,retval), instructions)) =
		    let
			val staticapply =
			    List.exists
			    (fn MStatic => true | _ => false)
			    access

		    in
			(case instructions of
			     nil => ()
			   | _ =>
				 (LabelMerge.new();
				  Catches.new();
				  JVMreg.new (siglength parms);
				  TextIO.output(io,".method "^
						(mAccessToString access)^
						methodname^
						(descriptor2string methodsig)^"\n");
				  if !LMAA then TextIO.output(io,"goto lmaa\nanfg:\n") else ();
				  actmeth := methodname;
				  (* xxx Hack to satisfy Byte Code Verifier. *)
				  if !OPTIMIZE >= 2
				      then
					  finish := ""
				  else
				      (case retval of
					   [Voidsig] => finish := "return\n"
					 | _ => finish := "areturn\n");
				  instructionsToJasmin
				  (Catches.get (optimize instructions),
				   true,
				   staticapply,
				   io);
				  if !LMAA then
				      (TextIO.output(io,"lmaa:\n");
				       lmaa (siglength parms, !JVMreg.maxReg))
				  else ();
				  TextIO.output(io,".limit locals "^
						Int.toString(!JVMreg.maxReg + 1)
						^"\n");
				  TextIO.output(io,".end method\n\n")))
		    end
	    in
		actclass:= name;
		TextIO.output(io,
			      ".source "^Class.getInitial ()^".dml\n");
		TextIO.output(io,
			      ".class "^(cAccessToString access)^name^"\n"^
			      ".super "^super^"\n");
		TextIO.output(io,List.foldr interfaceToJasmin "" interfaces);
		app fieldToJasmin fields;
		app methodToJasmin methods;
		TextIO.closeOut io
	    end
    end

