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
	open Abbrev

	type stamp=IntermediateGrammar.stamp

	(* non top-level functions must be pickled explicitly *)
	val pickleFn: StampSet.set StampHash.t = StampHash.new()

	(* map function arguments to ids *)
	val lambdas:id StampHash.t=StampHash.new ()

	(* map names (i.e. ids) to function arguments. *)
	val ids:stamp StampHash.t=StampHash.new ()

	(* map Pairs of (crosswise recursive function) stamps and number
	 of parameters to the stamp of the class in which the function code
	     is stored, the position in its apply method and the corresponding label *)
	val recApplies: (stamp * int * label) StampIntHash.t = StampIntHash.new ()

	(* map stamps to recApply function code *)
	val recApply: INSTRUCTION list StampHash.t = StampHash.new ()

	(* remember which (non-dummy) apply functions are created. *)
	val normalApplies = StampIntSet.new () (* xxx not in use yet. *)

	(* mark functions for pickling. This is necessary for non-toplevel functions only *)
	fun markForPickling (stamp', upperFun) =
	    if upperFun = toplevel
		then ()
	    else
		let
		    val p=
			case StampHash.lookup (pickleFn, upperFun) of
			    NONE =>
				let
				    val p' = StampSet.new ()
				in
				    StampHash.insert (pickleFn, upperFun, p');
				    p'
				end
			  | SOME p' => p'
		in
		    StampSet.insert (p, stamp')
		end

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

	(* create a field name for a stamp *)
	fun fname stamp' = "f"^(Stamp.toString stamp')

	(* create a full qualified name for a stamp. *)
	fun fieldname (curCls, stamp') = classNameFromStamp curCls^"/"^(fname stamp')

	(* We need instances of classes to ensure that the class information
	 is stored in the pickle. Instances of inner functions are created at
	 runtime, so we have to build dummy instances of inner functions. *)
	fun generatePickleFn (stamp', init) =
	    let
		fun codePickle (stamp'',acc) =
		    Aload thisStamp::
		    Ldc (JVMString (classNameFromStamp stamp''))::
		    Invokestatic MForName::
		    Putfield (fieldname (stamp',stamp''),
			      [Classsig CClass])::
		    acc
	    in
		case StampHash.lookup (pickleFn, stamp') of
		    NONE => init
		  | SOME p => StampSet.fold codePickle init p
	    end

	(* These are the dummy fields for generatePickleFn *)
	fun makePickleFields (stamp', init) =
	    let
		fun pickleFields (stamp'', acc) =
		    Field ([FPublic, FFinal],
			   fname stamp'',
			   [Classsig CClass])::acc
	    in
		case StampHash.lookup (pickleFn, stamp') of
		    NONE => init
		  | SOME p => StampSet.fold pickleFields init p
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
		SOME (stamp'', _, _) => (if !VERBOSE>=3 then print ("getClassStamp "^Stamp.toString stamp'^" = "^Stamp.toString stamp'') else ();
					     stamp'')
	      | NONE => (if !VERBOSE>=3 then print ("getClassStamp "^Stamp.toString stamp'^" fails.") else ();
			     stamp')

	fun isInRecApply (stamp', params) =
	    isSome (StampIntHash.lookup (recApplies, (stamp', methParms params)))

	fun getDestClass ((_,FunExp (_,thisFun,_,_))::_) = thisFun
	  | getDestClass (_::rest) = getDestClass rest
	  | getDestClass nil = illegalStamp

	fun argSize (OneArg _) = 1
	  | argSize (RecArgs _) = 1
	  | argSize (TupArgs i) = methParms (length i)

	fun insertRec (idexps as _::rest) =
	    let
		fun countFunExps ((_, FunExp _) :: rest, n) = countFunExps (rest, n+1)
		  | countFunExps ((_, _) :: rest, n) = countFunExps (rest, n)
		  | countFunExps (nil, n) = n

		val dest = getDestClass idexps
		val fncount = countFunExps (idexps, 0)
		val actual = ref illegalStamp
		val counter = ref 0

		fun insertInner (args, _) =
		    let
			val i = argSize args
		    in
			if isSome (StampIntHash.lookup (recApplies, (!actual, i)))
			    then ()
			else
			    (StampIntHash.insert (recApplies, (!actual, i),(dest, !counter, Label.new ()));
			     if !VERBOSE >= 1 then
				 print ("inserting "^Stamp.toString (!actual)^
					":"^Int.toString i^" for "^Stamp.toString (dest)^" at "^
					Int.toString (!counter)^"\n") else ();
				 counter := !counter + 1)
		    end

		fun insertRec' (id' as Id (_,stamp',_), exp') =
		    (case exp' of
			 FunExp (_,thisFun,_,idabodies) =>
			     (actual := thisFun;
			      print "rec': ";
			      setId (thisFun, id');
			      if fncount>1 then app insertInner idabodies
				  else ())
		       | _ => ())

		fun countUp (0, akku) = akku
		  | countUp (n, akku) = countUp (n-1, (n-1)::akku)

		val errorlabel = Label.new ()
	    in
		counter := 0;
		app insertRec' idexps;
		if fncount > 1 then
		    (StampHash.insert
		     (recApply,
		      dest,
		      if !counter <= 1 then
			  []
		      else
			  [Iload parm5Stamp,
			   Lookupswitch (StampIntHash.foldi
					 (fn (_, (stamp', pos', label'), (akku1, akku2)) =>
					  if dest = stamp' then
					      (LargeInt.fromInt pos'::akku1, label'::akku2) else (akku1, akku2))
					 (nil, nil)
					 recApplies,
					 errorlabel),
			   Label errorlabel,
			   New CCompilerException,
			   Dup,
			   Ldc (JVMString "unbekannte Funktion."),
			   Invokespecial (CCompilerException, "<init>", ([Classsig CString], [Voidsig])),
			   Athrow]))
		    else ()
	    end
	  | insertRec nil = ()

	fun addToRecApply (insts, stamp', parms) =
	    let
		val (destStamp, _, label') =
		    case StampIntHash.lookup (recApplies, (stamp', parms)) of
			SOME v => v
		      | NONE => (illegalStamp, ~1, ~1)

		val oleRecApply =
		    case StampHash.lookup (recApply, destStamp) of
			NONE => nil
		      | SOME code' => code'
	    in
		print ("addToRecApply: "^Stamp.toString stamp'^":"^Int.toString parms^"\n");
		if label' = ~1 then ()
		    else StampHash.insert (recApply, destStamp,
					   Multi oleRecApply ::
					   Label label' ::
					   insts)
	    end

	fun invokeRecApply (stamp', parms) =
	    let
		val p = methParms parms
	    in
		case StampIntHash.lookup (recApplies, (stamp', p)) of
		    SOME (destStamp, pos', label') =>
			InvokeRecApply (p, destStamp, pos', label')
		  | NONE => NormalApply p
	    end

	fun buildRecApply lambda =
	    Method ([MPublic],
		    "recApply",
		    ([Classsig IVal, Classsig IVal,
		      Classsig IVal, Classsig IVal, Intsig],
		     [Classsig IVal]),
		    (case StampHash.lookup (recApply, lambda) of
			 NONE => [Getstatic FOut,
				  Ldc (JVMString "bogus method"),
				  Invokevirtual (CPrintStream, "print",
						 ([Classsig CObject], [Voidsig])),
				  Aload thisStamp,
				  Areturn]
		       | SOME v => v))
	    before (StampHash.delete (recApply, lambda))

	fun showRecApplies () =
	    (print "Merged functions:\n";
	     StampIntHash.appi
	     (fn ((fn', parms'),(dest', pos', label')) =>
	      print ("Function "^Stamp.toString fn'^":"^Int.toString parms'^" stored in "^
		     Stamp.toString dest'^" at "^Int.toString pos'^"(label"^Int.toString label'^")\n"))
	     recApplies)
    end
