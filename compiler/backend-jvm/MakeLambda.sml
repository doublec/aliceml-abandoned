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

(* Funktionenstack *)
functor MakeLambda(structure StampSet:IMP_SET
		       where type item=IntermediateGrammar.stamp
			     structure StampHash:IMP_MAP
				 where type key=IntermediateGrammar.stamp
				       val toplevel:IntermediateGrammar.stamp) =
    struct
	open ImperativeGrammar
	open Common

	type stamp=IntermediateGrammar.stamp

(*	fun printstack (x::xs) = (print (Int.toString x^", "); printstack xs)
	  | printstack nil = print "\n"*)

	val lambdaStack = ref (toplevel::nil)
	val staticStack = ref (true::nil)
	val staticPossible:StampSet.t = StampSet.new ()

	(* non top-level functions must be pickled explicitly *)
	val pickleFn: id StampHash.t=StampHash.new()

	(* Abbildung von Lambda-Argumenten auf ids *)
	val lambdas:id StampHash.t=StampHash.new ()
	val lambdaIdsStack = ref [Id ((0,0),toplevel,InId)]:id list ref

	(* Abbildung von ids auf Lambda-Argumente *)
	val ids:stamp StampHash.t=StampHash.new ()

	(* Abbildung von Stamps auf Namen *)
	val stampName: string StampHash.t=StampHash.new ()

	(* Lambda.top () liefert stets die aktuelle Funktion *)
	fun top () = hd(!lambdaStack)

	fun push (id' as Id(_,stamp',_)) =
	    ((*printstack (stamp'::(!lambdaStack));*)
	     if top() = toplevel
		 then ()
	     else
		 StampHash.insert (pickleFn, stamp', id');
	     lambdaStack := (stamp'::(!lambdaStack));
	     staticStack := (true::(!staticStack)))

	fun pop () =
	    ((*printstack (tl (!lambdaStack));*)
	     lambdaStack := tl(!lambdaStack);
	     staticStack := tl(!staticStack))

	fun isStatic stamp' =
	    StampSet.member (staticPossible,stamp')

	fun pushFun ids = lambdaIdsStack:=(ids::(!lambdaIdsStack))
	fun popFun () = lambdaIdsStack:=tl(!lambdaIdsStack)
	fun getFun () = hd (!lambdaIdsStack)
	fun getOuterFun () = case !lambdaIdsStack of
	    _::id'::_ => id'
	  | _ => raise Match

	fun setId () =
	    StampHash.insert(lambdas,top(),hd(!lambdaIdsStack))

	fun getId id' =
	    case StampHash.lookup(lambdas, id')
		of NONE => Id ((0,0),toplevel,InId)
	      | SOME id'' => id''

	fun noSapply () =
	    staticStack:= false::(tl(!staticStack))

	fun sapplyPossible () =
	    if hd(!staticStack) then
		(StampSet.insert
		 (staticPossible,
		  (case getId (hd (!lambdaStack))
		       of Id (_,stamp',_) => stamp'));
		 true)
	    else false

	(* feststellen, ob eine Applikation selbstrekursiv ist. xxx *)
	fun isSelfCall stamp' =
	    case StampHash.lookup(lambdas, top()) of
		NONE => false
	      | SOME (Id (_,stamp'',_)) => stamp'=stamp''

	fun getLambda stamp' =
	    case StampHash.lookup(ids, stamp') of
		NONE => toplevel
	      | SOME stamp'' => stamp''

	fun createIdsLambdaTable () =
	    StampHash.appi
	    (fn (stamp', Id(_,stamp'',_)) =>
	     StampHash.insert (ids, stamp'', stamp'))
	    lambdas

	fun assignName (stamp', name) =
	    StampHash.insert (stampName, stamp', name)

	fun getName stamp' =
	    case StampHash.lookup (stampName,stamp') of
		NONE => Stamp.toString stamp'
	      | SOME name => name^(Stamp.toString stamp')

	fun fname stamp' = "f"^(Stamp.toString stamp')
	fun fieldname stamp'=Class.getLiteralName()^"/"^(fname stamp')

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
    end
