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

	(* remember whether static versions of functions are created *)
	(* xxx extend by number of parameters *)
	val staticPossible:StampSet.t = StampSet.new ()

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
	val innerApplies: (stamp * int) StampIntHash.t = StampIntHash.new ()

	(* set of Pairs of (other function) stamps and number of parameters
	 for which static applies are created *)
	val normalApplies: bool StampIntHash.t = StampIntHash.new ()

	(* return the current function *)
	fun top () = hd(!lambdaStack)

	(* called on entering a new function *)
	(* xxx seperate staticstack for methods *)
	fun push (id' as Id(_,stamp',_)) =
	    (if top() = toplevel
		 then ()
	     else
		 StampHash.insert (pickleFn, stamp', id');
	     lambdaStack := (stamp'::(!lambdaStack));
	     staticStack := (true::(!staticStack)))

	(*called on leaving a function *)
	(* xxx seperate staticstack for methods *)
	fun pop () =
	    (lambdaStack := tl(!lambdaStack);
	     staticStack := tl(!staticStack))

	(* test whether a static apply is created for a function *)
	fun isStatic stamp' =
	    StampSet.member (staticPossible,stamp')

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
	fun sapplyPossible () =
	    if hd(!staticStack) then
		(StampSet.insert
		 (staticPossible,
		  (case getId (hd (!lambdaStack))
		       of Id (_,stamp',_) => stamp'));
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

(* 
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

	fun makePickleFields startwert =
	    let
		fun pickleFields (stamp',_, acc) =
		    Field ([FPublic, FFinal],
			   fname stamp',
			   [Classsig CClass])::acc
	    in
		StampHash.foldi pickleFields startwert pickleFn
	    end

		fun getClass stamp' =
		    case StampHash.lookup (innerApplies, stamp') of
			SOME (stamp'', _) => stamp''
		      | NONE => stamp'

		fun getMethod (sp as (stamp', parms)) =
		    (case StampHash.lookup (innerApplies, sp) of
			 SOME (stamp'', pos) =>
			     RecApply (parms, _, stamp'', pos)
		       | NONE =>
			     (case StampHash.lookup (innerApplies, (stamp', 1)) of
				  SOME (stamp'', pos) =>
				      RecApply (1, _, stamp'', pos)
				| NONE =>
				      (case StampIntHash.lookup
					   (normalApplies, sp) of
					   SOME isstatic =>
					       let
						   val name =
						       (if isstatic then "sapply"
							else "apply")^
							    (if parms = 1
								 then ""
							     else
								 Int.toString parms)
					       in
						   Apply (name, parms, isstatic)
					       end
					 | NONE => case StampIntHash.lookup
					       (normalApplies, (stamp', 1)) of
					       SOME isstatic =>
						   Apply
						   (if isstatic then "sapply" else "apply",
							1,
							isstatic)
					     | NONE => Apply ("apply", 1, false))))

		let
		    val dest = ref illegalStamp
		    val actual = ref illegalStamp
		    val counter = ref 0

		    fun insertInner (OneArg stamp', body'::rest) =
			(actual := stamp';
			 StampIntHash.insert (recApplies, (stamp', 1),(!dest, !counter));
			 counter := !counter + 1;
			 insertInner rest)
		      | insertInner nil = ()

		    fun insertRec' ((Id (_,stamp',_), exp')::rest) =
			(case exp' of
			     FunExp (_,_,idabodys as (OneArg stamp'',_)::_) =>
				 (if !dest=illegalStamp then
				      dest := stamp'' else ();
				      insertInner idabodys;
				      insertRec' rest)
			   | _ => insertRec' rest)
		      | insertRec' nil = ()
		in
		    fun insertRec idexps =
			(destStamp := illegalStamp;
			 counter := 0;
			 insertRec' idexps)
		end

    end
