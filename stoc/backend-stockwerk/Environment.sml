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
	type scope = PickleGrammar.idRef StampMap.t
	type globals = FlatGrammar.id list
	type index = int

	type t = {stack: (scope * index * index * globals) list ref,
		  scope: scope ref,
		  localIndex: index ref,
		  globalIndex: index ref,
		  globals: globals ref,
		  shared: PickleGrammar.instr StampMap.t}

	fun new () =
	    {stack = ref nil, scope = ref (StampMap.new ()),
	     localIndex = ref 0, globalIndex = ref 0, globals = ref nil,
	     shared = StampMap.new ()}

	fun startFn {stack, scope, localIndex, globalIndex, globals, shared} =
	    (stack := (!scope, !localIndex, !globalIndex, !globals)::(!stack);
	     scope := StampMap.new ();
	     localIndex := 0;
	     globalIndex := 0;
	     globals := nil)

	fun endFn {stack, scope, localIndex, globalIndex, globals, shared} =
	    case !stack of
		(scope', localIndex', globalIndex', globals')::rest =>
		    (Vector.fromList (List.rev (!globals)), !localIndex)
		    before (scope := scope';
			    localIndex := localIndex';
			    globalIndex := globalIndex';
			    globals := globals';
			    stack := rest)
	      | nil => raise Crash.Crash "Environment.endFn"

	fun declare ({stack, scope, localIndex, globalIndex, globals, shared},
		     FlatGrammar.Id (_, stamp, _)) =
	    let
		val i = !localIndex
	    in
		localIndex := i + 1;
		StampMap.insertDisjoint (!scope, stamp, PickleGrammar.Local i);
		i
	    end

	fun fresh {stack, scope, localIndex, globalIndex, globals, shared} =
	    let
		val i = !localIndex
	    in
		localIndex := i + 1;
		i
	    end

	fun lookup ({stack, scope, localIndex, globalIndex, globals, shared},
		    id as FlatGrammar.Id (_, stamp, _)) =
	    case StampMap.lookup (!scope, stamp) of
		SOME idRef => idRef
	      | NONE =>
		    let
			val i = !globalIndex
			val idRef = PickleGrammar.Global i
		    in
			globals := id::(!globals);
			globalIndex := i + 1;
			StampMap.insertDisjoint (!scope, stamp, idRef);
			idRef
		    end

	fun lookupStamp ({stack, scope, localIndex, globalIndex, globals,
			  shared}, stamp) =
	    StampMap.lookupExistent (!scope, stamp)

	fun lookupShared ({stack, scope, localIndex, globalIndex, globals,
			   shared}, stamp) =
	    StampMap.lookup (shared, stamp)

	fun declareShared ({stack, scope, localIndex, globalIndex, globals,
			    shared}, stamp, instr) =
	    StampMap.insertDisjoint (shared, stamp, instr)
    end
