(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure CodeStore :> CODE_STORE =
    struct
	open IL

	type stamp = ImperativeGrammar.stamp

	datatype id = datatype ImperativeGrammar.id

	datatype reg =
	    SFld of index
	  | Fld of index
	  | Arg of index
	  | Loc of index
	  | Prebound of string
	withtype index = int

	type class = stamp

	structure Map =
	    MakeHashImpMap(type t = class
			   val hash = Stamp.hash)
	structure ScopedMap =
	    MakeHashScopedImpMap(type t = stamp
				 val hash = Stamp.hash)

	type classAttrState = (extends * implements) option ref
	type scope = reg ScopedMap.t
	type classDeclsState = classDecl list ref
	type regState = scope * index ref * index ref
	type instrsState = IL.instr list ref

	val namespace: dottedname ref = ref nil
	val classes: (classAttrState * scope * classDeclsState) Map.t ref =
	    ref (Map.new ())
	val env: (class * IL.id * int * regState * instrsState) list ref =
	    ref nil

	val global = Stamp.new ()
	val preboundScope =
	    let
		open Prebound
		val scope = ScopedMap.new ()
	    in
		ScopedMap.insert (scope, stamp_false, Prebound "false");
		ScopedMap.insert (scope, stamp_true, Prebound "true");
		ScopedMap.insert (scope, stamp_nil, Prebound "nil");
		ScopedMap.insert (scope, stamp_cons, Prebound "cons");
		ScopedMap.insert (scope, stamp_ref, Prebound "ref");
		ScopedMap.insert (scope, stamp_Match, Prebound "Match");
		ScopedMap.insert (scope, stamp_Bind, Prebound "Bind");
		scope
	    end

	fun className class = !namespace @ ["P" ^ Stamp.toString class]
	fun sfldName i = "V" ^ Int.toString i
	fun fldName i = "G" ^ Int.toString i

	fun init dottedname =
	    (namespace := dottedname;
	     classes := Map.new ();
	     env := [(global, "main", 0,
		      (ScopedMap.copy preboundScope, ref 0, ref 0), ref nil)])

	fun defineClass (stamp, extends, implements) =
	    let
		val classAttr = SOME (extends, implements)
		val constructor =
		    Method (".ctor", (Public, Instance),
			    nil, VoidTy, (nil, false),
			    [Ldarg 0,
			     Tail, Call (true, extends, ".ctor", nil, VoidTy),
			     Ret])
	    in
		case Map.lookup (!classes, stamp) of
		    SOME (classAttrRef, _, classDeclsRef) =>
			if Option.isSome (!classAttrRef) then
			    Crash.crash "CodeStore.defineClass"
			else
			    (classAttrRef := classAttr;
			     classDeclsRef := constructor::(!classDeclsRef))
		  | NONE =>
			Map.insertDisjoint (!classes, stamp,
					    (ref classAttr,
					     ScopedMap.copy preboundScope,
					     ref [constructor]))
	    end

	fun defineMethod (stamp, id, args) =
	    let
		val (scope, classDeclsRef) =
		    case Map.lookup (!classes, stamp) of
			SOME (_, scope, classDeclsRef) =>
			    (scope, classDeclsRef)
		      | NONE =>
			    let
				val scope = ScopedMap.copy preboundScope
				val classDeclsRef = ref nil
			    in
				Map.insertDisjoint
				(!classes, stamp,
				 (ref NONE, scope, classDeclsRef));
				(scope, classDeclsRef)
			    end
	    in
		ScopedMap.insertScope scope;
		List.foldl (fn (Id (_, stamp, _), i) =>
			    (ScopedMap.insertDisjoint (scope, stamp, Arg i);
			     i + 1)) 1 args;
		env := (stamp, id, List.length args, (scope, ref 0, ref 0),
			ref nil)::(!env)
	    end

	fun emit instr =
	    case !env of
		(_, _, _, _, instrsRef)::_ =>
		    instrsRef := instr::(!instrsRef)
	      | nil => Crash.crash "CodeStore.emit"

	local
	    fun currentClosure () =
		case !env of
		    (stamp, _, _, _, _)::_ => className stamp
		  | _ => Crash.crash "CodeStore.currentClosure"

	    fun lookup ((_, _, _, (scope, ri, _), _)::envr, stamp) =
		(case ScopedMap.lookup (scope, stamp) of
		     SOME reg => reg
		   | NONE =>
			 let
			     val i = !ri
			     val reg = Fld i
			 in   (*--** generate SFld? *)
			     lookup (envr, stamp);
			     ScopedMap.insertDisjoint (scope, stamp, reg);
			     ri := i + 1;
			     reg
			 end)
	      | lookup (nil, stamp) =
		Crash.crash ("CodeStore.lookup: " ^ Stamp.toString stamp)
	in
	    fun emitStamp stamp =
		case lookup (!env, stamp) of
		    SFld i =>
			emit (Ldsfld (currentClosure (), sfldName i,
				      StockWerk.StockWertTy))
		  | Fld i =>
			(emit (Ldarg 0);
			 emit (Ldfld (currentClosure (), fldName i,
				      StockWerk.StockWertTy)))
		  | Loc i => emit (Ldloc i)
		  | Arg i => emit (Ldarg i)
		  | Prebound name =>
			emit (Ldsfld (StockWerk.Prebound, name,
				      StockWerk.StockWertTy))
	end

	fun emitId (Id (_, stamp, _)) = emitStamp stamp

	fun declareLocal (Id (_, stamp, _)) =
	    case !env of
		(_, _, _, (scope, _, ri), _)::_ =>
		    (case ScopedMap.lookup (scope, stamp) of
			 SOME (Loc i) => emit (Stloc i)
		       | SOME _ => Crash.crash "CodeStore.declareLocal 1"
		       | NONE =>
			     let
				 val i = !ri
			     in
				 ScopedMap.insertDisjoint
				 (scope, stamp, Loc i);
				 ri := i + 1;
				 emit (Stloc i)
			     end)
	      | nil => Crash.crash "CodeStore.declareLocal 2"

	fun closeMethod () =
	    case !env of
		(stamp, id, nargs, (scope, _, ref nlocs), ref instrs)::envr =>
		    let
			val (_, _, classDeclsRef) =
			    Map.lookupExistent (!classes, stamp)
			val delta = ScopedMap.splitScope scope
			val className' = className stamp
			val _ = env := envr
			val method =
			    Method (id, (Public, Virtual),
				    List.tabulate
				    (nargs, fn _ => StockWerk.StockWertTy),
				    StockWerk.StockWertTy,
				    (List.tabulate
				     (nlocs, fn _ => StockWerk.StockWertTy),
				     false), List.rev instrs)
			val newClassDecls =
			    ScopedMap.foldi
			    (fn (stamp, reg, classDecls) =>
			     case reg of
				 SFld i =>
				     (emit Dup;
				      emitStamp stamp;
				      emit (Stsfld (className', sfldName i,
						    StockWerk.StockWertTy));
				      ScopedMap.insertDisjoint (scope, stamp,
								reg);
				      Field (sfldName i, (Public, true, false),
					     StockWerk.StockWertTy)::classDecls)
			       | Fld i =>
				     (emit Dup;
				      emitStamp stamp;
				      emit (Stfld (className', fldName i,
						   StockWerk.StockWertTy));
				      ScopedMap.insertDisjoint (scope, stamp,
								reg);
				      Field (fldName i, (Public, false, false),
					     StockWerk.StockWertTy)::classDecls)
			   | _ => classDecls) (!classDeclsRef) delta
		    in
			classDeclsRef := method::newClassDecls
		    end
	      | nil => Crash.crash "CodeStore.closeMethod"

	fun close () =
	    let
		val _ = emit Ret
		val mainMethod =
		    case !env of
			[(_, id, 0, (_, _, ref n), ref instrs)] =>
			    GlobalMethod
			    (id, true, nil, VoidTy, true,
			     (List.tabulate (n, fn _ => StockWerk.StockWertTy),
			      false), List.rev instrs)
		      | _ => Crash.crash "CodeStore.close"
	    in
		Map.foldi
		(fn (stamp, (ref classAttr, scope, ref classDecls), program) =>
		 let
		     val (extends, implements) = Option.valOf classAttr
		 in
		     (*--** Map.deleteExistent (!classes, stamp); *)
		     Class (className stamp, (true, RegularClass),
			    extends, implements, classDecls)::program
		 end) [mainMethod] (!classes)
	    end
    end
