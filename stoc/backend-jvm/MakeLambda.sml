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

	(* remember the actual function. We use a stack because functions may
	 be nested *)
	val lambdaStack = ref (toplevel::nil)

	(* remember whether the actual function can be stored in a static
	 method *)
	val staticStack = ref (true::nil)

	(* non top-level functions must be pickled explicitly *)
	(* xxx store in corresponding classes *)
	val pickleFn: id StampHash.t=StampHash.new()

	(* map function arguments to ids *)
	val lambdas:id StampHash.t=StampHash.new ()

	(* In this stack, we remember the names of functions. *)
	val lambdaIdsStack = ref [Id ((0,0),toplevel,InId)]:id list ref

	(* map names (i.e. ids) to function arguments. Remember that
	 functions are represented by their OneArg which is unique. *)
	val ids:stamp StampHash.t=StampHash.new ()

	(* map Pairs of (crosswise recursive function) stamps and number
	 of parameters to the stamp of the class in which the function code
	     is stored and the position in its apply method *)
	val recApplies: (stamp * int) StampIntHash.t = StampIntHash.new ()

	(* set of Pairs of (not crosswise recursive function) stamps and number
	 of parameters for which static applies are created *)
	val staticPossible = StampIntSet.new ()

	(* remember which (non-dummy) apply functions are created. *)
	val normalApplies = StampIntSet.new ()

	(* return the current function *)
	fun top () = hd(!lambdaStack)

	(* Called before a new apply method is created.
	 Assume that the method can be static. *)
	fun newApply () =
	    staticStack := (true::(!staticStack))

	(* called on entering a new function *)
	fun push (id' as Id(_,stamp',_)) =
	    (if top() = toplevel
		 then ()
	     else
		 StampHash.insert (pickleFn, stamp', id');
	     lambdaStack := (stamp'::(!lambdaStack));
	     newApply ())

	(* called after finishing a apply method. *)
	fun endApply () =
	    staticStack := tl(!staticStack)

	(*called on leaving a function *)
	fun pop () =
	    (lambdaStack := tl(!lambdaStack);
	     endApply ())

	(* test whether a static apply is created for a function *)
	fun isStatic (stampparms: stamp* int) =
	    StampIntSet.member (staticPossible,stampparms)

	(* called on entering a new function *)
	fun pushFun ids = lambdaIdsStack:=(ids::(!lambdaIdsStack))

	(* called on leaving a function *)
	fun popFun () = lambdaIdsStack:=tl(!lambdaIdsStack)

	(* get the id with the name of the current function. *)
	fun getFun () = hd (!lambdaIdsStack)

	(* get the id with the name of the surrounding function.  *)
	fun getOuterFun () = case !lambdaIdsStack of
	    _::id'::_ => id'
	  | _ => raise Mitch

	(* name the current function. Both the function and its
	 corresponding name can be found on top of the stack. *)
	fun setId () =
	    StampHash.insert(lambdas,top(),hd(!lambdaIdsStack))

	(* get the name of a function *)
	fun getId id' =
	    case StampHash.lookup(lambdas, id')
		of NONE => Id ((0,0),toplevel,InId)
	      | SOME id'' => id''

	(* called when the actual method cannot be declared static *)
	fun noSapply () =
	    staticStack:= false::(tl(!staticStack))

	(* checks whether the actual function can be declared static *)
	fun sapplyPossible parms =
	    if hd(!staticStack) then
		(StampIntSet.insert
		 (staticPossible,
		  ((case getId (hd (!lambdaStack))
			of Id (_,stamp',_) => stamp'),
		   parms));
		 true)
	    else false

	(* checks whether a function call is self recursive. *)
	fun isSelfCall stamp' =
	    (case StampHash.lookup(lambdas, top()) of
		 NONE => false
	       | SOME (Id (_,stamp'',_)) => stamp'=stamp'')

	(* return the function stamp that corresponds to a name (stamp) *)
	fun getLambda stamp' =
	    case StampHash.lookup(ids, stamp') of
		NONE => toplevel
	      | SOME stamp'' => stamp''

	(* We already have a hashtable that maps functions to their names.
	 Now we create a hashtable that does vice versa. *)
	fun createIdsLambdaTable () =
	    StampHash.appi
	    (fn (stamp', Id(_,stamp'',_)) =>
	     StampHash.insert (ids, stamp'', stamp'))
	    lambdas

	(* create a field name for a stamp *)
	fun fname stamp' = "f"^(Stamp.toString stamp')

	(* create a full qualified name for a stamp. *)
	fun fieldname stamp'=Class.getLiteralName()^"/"^(fname stamp')

	(* We need instances of classes to ensure that the class information
	 is stored in the pickle. Instances of inner functions are created at
	 runtime, so we have to build dummy instances of inner functions. *)
	fun generatePickleFn startwert =
	    let
		fun codePickle (stamp',_,acc) =
		    Aload 0::
		    Ldc (JVMString (classNameFromStamp stamp'))::
		    Invokestatic MForName::
		    Putfield (fieldname stamp',
			      [Classsig CClass])::
		    acc
	    in
		StampHash.foldi codePickle startwert pickleFn
	    end

	(* These are the dummy fields for generatePickleFn *)
	fun makePickleFields startwert =
	    let
		fun pickleFields (stamp',_, acc) =
		    Field ([FPublic, FFinal],
			   fname stamp',
			   [Classsig CClass])::acc
	    in
		StampHash.foldi pickleFields startwert pickleFn
	    end

	(* Return the class in which apply is defined. Note that this is
	 not necessarily the class that corresponds to the stamp as crosswise
	 recursive functions are merged in one single apply *)
	fun getClass (stampint as (stamp', _)) =
	    case StampIntHash.lookup (recApplies, stampint) of
		SOME (stamp'', _) => stamp''
	      | NONE => stamp'

	(* Return the apply method of a stamp and its number of parameters. *)
	fun getMethod (sp as (stamp', parms)) =
	    (case StampIntHash.lookup (recApplies, sp) of
		 SOME (stamp'', pos) =>
		     RecApply (parms, stamp'', pos)
	       | NONE =>
		     (case StampIntHash.lookup (recApplies, (stamp', 1)) of
			  SOME (stamp'', pos) =>
			      RecApply (1, stamp'', pos)
			| NONE =>
			      (if StampIntSet.member (normalApplies, sp) then
				   Apply (applyName (false, parms), parms)
			       else Apply (applyName (false, 1), 1))))

	local
	    val actual = ref illegalStamp
	    val dest = ref illegalStamp
	    val counter = ref 0

	    fun insertInner (OneArg (Id (_,stamp',_)), _) =
		(actual := stamp';
		 StampIntHash.insert (recApplies, (stamp', 1),(!dest, !counter));
		 counter := !counter + 1)
	      | insertInner (TupArgs ids, _) =
		let
		    val l = length ids
		    val i = if l<=4 then l else 1
		in
		    if isSome (StampIntHash.lookup (recApplies, (!actual, i)))
			then ()
		    else
			(StampIntHash.insert (recApplies, (!actual, i), (!dest, !counter));
			 counter := !counter +1)
		end
	      | insertInner (RecArgs _, _) =
		if isSome (StampIntHash.lookup (recApplies, (!actual, 1)))
		    then ()
		else
		    (StampIntHash.insert (recApplies, (!actual, 1), (!dest, !counter));
		     counter := !counter + 1)

	    fun insertRec' (Id (_,stamp',_), exp') =
		(case exp' of
		     FunExp (_,_,idabodys as (OneArg (Id (_,stamp'',_)),_)::_) =>
			 (if !dest=illegalStamp then
			      dest := stamp'' else ();
			      counter := 0;
			      app insertInner idabodys)
		   | _ => ())
	in
	    fun insertRec idexps =
		(dest := illegalStamp;
		 counter := 0;
		 app insertRec' idexps)
	end

    end
