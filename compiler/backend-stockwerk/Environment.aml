(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Environment :> ENVIRONMENT =
    struct
	type scope = Pickle.idRef StampMap.t
	type globals = FlatGrammar.id list
	type index = int

	type t = {stack: (scope * index * index * globals) list ref,
		  scope: scope ref,
		  localIndex: index ref,
		  globalIndex: index ref,
		  globals: globals ref}

	fun new () =
	    {stack = ref nil, scope = ref (StampMap.new ()),
	     localIndex = ref 0, globalIndex = ref 0, globals = ref nil}

	fun startFn {stack, scope, localIndex, globalIndex, globals} =
	    (stack := (!scope, !localIndex, !globalIndex, !globals)::(!stack);
	     scope := StampMap.new ();
	     localIndex := 0;
	     globalIndex := 0;
	     globals := nil)

	fun endFn {stack, scope, localIndex, globalIndex, globals} =
	    case !stack of
		(scope', localIndex', globalIndex', globals')::rest =>
		    (Vector.fromList (List.rev (!globals)), !localIndex)
		    before (scope := scope';
			    localIndex := localIndex';
			    globalIndex := globalIndex';
			    globals := globals';
			    stack := rest)
	      | nil => raise Crash.Crash "Environment.endFn"

	fun declare ({stack, scope, localIndex, globalIndex, globals},
		     FlatGrammar.Id (_, stamp, _)) =
	    let
		val i = !localIndex
	    in
		localIndex := i + 1;
		StampMap.insertDisjoint (!scope, stamp, Pickle.Local i);
		i
	    end

	fun fresh {stack, scope, localIndex, globalIndex, globals} =
	    let
		val i = !localIndex
	    in
		localIndex := i + 1;
		i
	    end

	fun lookup ({stack, scope, localIndex, globalIndex, globals},
		    id as FlatGrammar.Id (_, stamp, _)) =
	    case StampMap.lookup (!scope, stamp) of
		SOME idRef => idRef
	      | NONE =>
		    let
			val i = !globalIndex
			val idRef = Pickle.Global i
		    in
			globals := id::(!globals);
			globalIndex := i + 1;
			StampMap.insertDisjoint (!scope, stamp, idRef);
			idRef
		    end

	fun lookupStamp ({stack, scope, localIndex, globalIndex, globals},
			 stamp) =
	    StampMap.lookupExistent (!scope, stamp)
    end
