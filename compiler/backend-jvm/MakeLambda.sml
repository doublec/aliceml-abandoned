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

(* Information of all kind about functions *)
functor MakeLambda(structure StampSet:IMP_SET
		       where type item=IntermediateGrammar.stamp
			     structure StampHash:IMP_MAP
				 where type key=IntermediateGrammar.stamp
				       structure StampIntSet:IMP_SET
					   where type item=IntermediateGrammar.stamp * int
						 structure StampIntHash:IMP_MAP
						     where type key=IntermediateGrammar.stamp * int
							   val toplevel:IntermediateGrammar.stamp) =
    struct
	open ImperativeGrammar
	open Common

	type stamp=IntermediateGrammar.stamp

	(* non top-level functions must be pickled explicitly *)
	(* xxx store in corresponding classes *)
	val pickleFn=StampSet.new()

	(* map function arguments to ids *)
	val lambdas:id StampHash.t=StampHash.new ()

	(* map names (i.e. ids) to function arguments. *)
	val ids:stamp StampHash.t=StampHash.new ()

	(* map Pairs of (crosswise recursive function) stamps and number
	 of parameters to the stamp of the class in which the function code
	     is stored and the position in its apply method *)
	val recApplies: (stamp * label) StampIntHash.t = StampIntHash.new ()

	(* map stamps to recApply function code *)
	val recApply: INSTRUCTION list StampHash.t = StampHash.new ()

	(* remember which (non-dummy) apply functions are created. *)
	val normalApplies = StampIntSet.new () (* xxx not in use yet. *)

	(* mark functions for pickling. This is necessary for non-toplevel functions only *)
	fun markForPickling (stamp', upperFun) =
	    if upperFun = toplevel
		then ()
	    else
		StampSet.insert (pickleFn, stamp')

	(* name a function. *)
	fun setId (lambda, name as Id (_, namestamp, _)) =
	    (print ("assign function "^Stamp.toString lambda^" to Stamp "^Stamp.toString namestamp^"\n");
	     StampHash.insert(lambdas,lambda,name);
	     StampHash.insert(ids, namestamp, lambda))

	(* get the name of a function *)
	fun getId stamp' =
	    case StampHash.lookup(lambdas, stamp')
		of NONE => Id (dummyCoord, stamp', InId)
	      | SOME id'' => id''

	(* return the function stamp that corresponds to a name (stamp) *)
	fun getLambda stamp' =
	    case StampHash.lookup(ids, stamp') of
		NONE => stamp'
	      | SOME stamp'' => stamp''

	(* checks whether a function call is self recursive. xxx *)
	fun isSelfCall (stamp', stamp'') =
	    getLambda stamp'= stamp''

	(* create a field name for a stamp *)
	fun fname stamp' = "f"^(Stamp.toString stamp')

	(* create a full qualified name for a stamp. *)
	fun fieldname stamp'=Class.getLiteralName()^"/"^(fname stamp')

	(* We need instances of classes to ensure that the class information
	 is stored in the pickle. Instances of inner functions are created at
	 runtime, so we have to build dummy instances of inner functions. *)
	fun generatePickleFn startwert =
	    let
		fun codePickle (stamp',acc) =
		    Aload thisStamp::
		    Ldc (JVMString (classNameFromStamp stamp'))::
		    Invokestatic MForName::
		    Putfield (fieldname stamp',
			      [Classsig CClass])::
		    acc
	    in
		StampSet.fold codePickle startwert pickleFn
	    end

	(* These are the dummy fields for generatePickleFn *)
	fun makePickleFields startwert =
	    let
		fun pickleFields (stamp', acc) =
		    Field ([FPublic, FFinal],
			   fname stamp',
			   [Classsig CClass])::acc
	    in
		StampSet.fold pickleFields startwert pickleFn
	    end

	(* Return the number of Value parameters (1-4) of an recapply of a function
	   with n parameters. *)
	fun methParms n =
	    if n > 4 then 1 else n

	(* Return the class in which apply is defined. Note that this is
	 not necessarily the class that corresponds to the stamp as crosswise
	 recursive functions are merged in one single apply *)
	fun getClassStamp (stamp', params) =
	    case StampIntHash.lookup (recApplies, (stamp', methParms params)) of
		SOME (stamp'', _) => stamp''
	      | NONE => stamp'

	fun isInRecApply (stamp', params) =
	    isSome (StampIntHash.lookup (recApplies, (stamp', methParms params)))

	local
	    val actual = ref illegalStamp
	    val dest = ref illegalStamp
	    val counter = ref 0
	    fun insertInner (OneArg _, _) =
		(StampIntHash.insert (recApplies, (!actual, 1),(!dest, !counter));
		 if !VERBOSE >= 1 then
		     print ("inserting "^Stamp.toString (!actual)^
			    ":1 for "^Stamp.toString (!dest)^" at "^
			    Int.toString (!counter)^"\n") else ();
		 counter := !counter + 1)
	      | insertInner (TupArgs ids, _) =
		let
		    val i = methParms (length ids)
		in
		    if isSome (StampIntHash.lookup (recApplies, (!actual, i)))
			then ()
		    else
			(StampIntHash.insert (recApplies, (!actual, i), (!dest, !counter));
			 if !VERBOSE >= 1 then
			     print ("inserting "^Stamp.toString (!actual)^
				    ":"^Int.toString i^" for "^Stamp.toString (!dest)^" at "^
				    Int.toString (!counter)^"\n") else ();
			 counter := !counter +1)
		end
	      | insertInner (RecArgs _, _) =
		if isSome (StampIntHash.lookup (recApplies, (!actual, 1)))
		    then ()
		else
		    (StampIntHash.insert (recApplies, (!actual, 1), (!dest, !counter));
		     if !VERBOSE >= 1 then
			 print ("inserting "^Stamp.toString (!actual)^
				":1 for "^Stamp.toString (!dest)^" at "^
				Int.toString (!counter)^"\n") else ();
		     counter := !counter + 1)

	    fun insertRec' (id' as Id (_,stamp',_), exp') =
		(case exp' of
		     FunExp (_,thisFun,_,idabodies) =>
			 (if !dest=illegalStamp then
			      dest := thisFun else ();
			  actual := thisFun;
			  print "rec': ";
			  setId (thisFun, id');
			  app insertInner idabodies)
		   | _ => ())

	    fun countUp (0, akku) = akku
	      | countUp (n, akku) = countUp (n-1, (n-1)::akku)

	    val errorlabel = Label.new ()
	in
	    fun insertRec (idexps as _::rest) =
		(dest := illegalStamp;
		 counter := 0;
		 app insertRec' idexps;
		 case rest of
		     nil => ()
		   | _ =>
			 StampHash.insert
			 (recApply,
			  !dest,
			  if !counter <= 1 then
			      []
			  else
			      [Iload 5,
			       Tableswitch (0, countUp (!counter-1, nil), errorlabel),
			       Label errorlabel,
			       New ECompiler,
			       Dup,
			       Ldc (JVMString "unbekannte Funktion."),
			       Invokespecial (ECompiler, "<init>", ([Classsig CString], [Voidsig])),
			       Athrow]))
	      | insertRec nil = ()
	end

	fun addToRecApply (insts, stamp', parms) =
	    let
		val (destStamp, label') =
		    case StampIntHash.lookup (recApplies, (stamp', parms)) of
			SOME v => v
		      | NONE => (illegalStamp, ~1)

		val oleRecApply =
		    case StampHash.lookup (recApply, destStamp) of
			NONE => nil
		      | SOME code' => code'
	    in
		print ("addToRecApply: "^Stamp.toString stamp'^":"^Int.toString parms^"\n");
		if label'= ~1 then ()
		    else StampHash.insert (recApply, destStamp,
					   Multi oleRecApply ::
					   Label label' ::
					   insts)
	    end

	fun invokeRecApply (stamp', parms, tailCallPos) =
	    let
		val p = methParms parms
	    in
		case StampIntHash.lookup (recApplies, (stamp', p)) of
		    SOME (destStamp, label') =>
			if stamp' = destStamp andalso tailCallPos then
			    GotoLabel (p, label')
			else
			    InvokeRecApply (p, destStamp, label')
		  | NONE => NormalApply p
	    end

	fun buildRecApply lambda =
	    Method ([MPublic],
		    "recApply",
		    ([Classsig CVal, Classsig CVal,
		      Classsig CVal, Classsig CVal, Intsig],
		     [Classsig CVal]),
		    (case StampHash.lookup (recApply, lambda) of
			 NONE => [Getstatic COut,
				  Ldc (JVMString "bogus method"),
				  Invokevirtual (CPrintStream, "print",
						 ([Classsig CObj], [Voidsig])),
				  Aload thisStamp,
				  Areturn]
		       | SOME v => v))
	    before (StampHash.delete (recApply, lambda))

	fun showRecApplies () =
	    (print "Merged functions:\n";
	     StampIntHash.appi
	     (fn ((fn', parms'),(dest', label')) =>
	      print ("Function "^Stamp.toString fn'^":"^Int.toString parms'^" stored in "^
		     Stamp.toString dest'^" at "^Int.toString label'^"\n"))
	     recApplies)
    end
