(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(*
 * Optimierungsideen:
 *
 * Voraussetzung: Fuer Pickling muss erkannt werden, welche
 * Referenzen Code auf andere Closures macht.  Idee: Code wird nicht
 * in assemblierter Form in ein PEFile geschrieben (denn Code kann nicht
 * reflektiert werden), sondern in Zwischendarstellung als StockWert
 * gespeichert (bei jeder Closure in einem statischen Feld).
 * Bei Serialisierung wird der transitive Abschluss aller Closures
 * gebildet.  Closures werden ausserdem mit ihrer Definition
 * rausgeschrieben (also mit den Werten ihrer statischen Felder).
 * Bei Deserialisierung eines Token wird (bei Bedarf) der zugehoerige
 * Code in Zwischendarstellung assembliert (eine Form von JITting).
 *
 * Dann kann folgende Optimierung durchgefuehrt werden:
 *
 * Die freien Variablen werden klassifiziert nach statischen und
 * dynamischen freien Variablen (statisch = auf toplevel definiert,
 * also die V-Register in Mozart; dynamisch = nicht auf toplevel
 * definiert, also die G-Register in Mozart) und entsprechend in
 * statischen oder Instanzenfeldern gespeichert.
 *
 * Geboxte Literale sollen auch in V-Registern gespeichert
 * werden, zaehlen also auch als `statische freie Variablen'.
 * V-Register werden als zum Code gehoerig betrachtet.
 *
 * Vorteile:
 * -- Da sich alle Instanzen von Prozeduren die statischen freien
 *    Variablen teilen, wird die (wiederholte) Ausfuehrung einer
 *    Prozedurdefinition effizienter.
 * -- Der Heapverbrauch sinkt, da Literale nur einmal geboxt werden
 *    muessen.
 * Nachteile:
 * -- Es muss der Overhead in Kauf genommen werden, dass der Code
 *    jeder Prozedur in zwei Darstellungen gespeichert wird.
 *)

(*
 * Anzahl der generierten Klassen reduzieren:
 *
 * Eine Toplevel-Prozedur, die ausschliesslich an Designatorposition von
 * Applikationen verwendet wird, heisst Hilfsprozedur (Auxiliary).  Berechne
 * den Dominanzgraphen des Aufrufgraphen der Toplevel-Prozeduren.  Jede
 * dominierende nicht-Hilfsprozedur, die ausschliesslich Hilfsprozeduren
 * unter sich hat, kann die Definitionen ihrer Hilfsprozeduren in ihre
 * eigene Klasse mit aufnehmen.
 *
 * Entsprechend dominierte Hilfsprozeduren sollen mit dem Flag AuxiliaryOf
 * annotiert werden.
 *)

structure CodeGenPhase :> CODE_GEN_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = IL

	open I
	open O
	open CodeStore

	local
	    val count = ref 0
	in
	    fun newLabel () =
		let
		    val n = !count + 1
		in
		    count := n; n
		end
	end

	fun emitRegion (s, region) =
	    emit (Comment (s ^ " at " ^ Source.regionToString region))

	datatype expMode =
	    PREPARE
	  | FILL
	  | BOTH

	fun emitAwait () =
	    let
		val label = newLabel ()
	    in
		emit Dup; emit (Isinst Alice.TransientTy);
		emit (B (FALSE, label));
		emit (Castclass Alice.TransientTy);
		emit (Callvirt (Alice.Transient, "Await",
				nil, System.ObjectTy));
		emit (Label label)
	    end

	structure IntMap =
	     MakeHashImpMap(type t = int
			    val equals = op=
			    fun hash i = i)

	fun parseTests (_, _, ~1, min, max) = (min, max)
	  | parseTests (tests, map, i, min, max) =
	    let
		val (j, gen, body) = Vector.sub (tests, i)
	    in
		IntMap.insertWith #1 (map, j, (newLabel (), gen, body));
		parseTests (tests, map, i - 1,
			    Int.min (min, j), Int.max (max, j))
	    end

	fun declareArgs (OneArg idDef, _) = declareLocal idDef
	  | declareArgs (TupArgs idDefs, await) =
	    let
		val indexIdDefList =
		    Vector.foldli (fn (i, idDef, rest) =>
				   case idDef of
				       IdDef _ => (i, idDef)::rest
				     | Wildcard => rest) nil (idDefs, 0, NONE)
	    in
		if await then emitAwait () else ();
		case indexIdDefList of
		    nil => emit Pop
		  | (i, idDef)::indexIdDefList =>
			(emit (Castclass (ArrayTy System.ObjectTy));
			 List.app (fn (i, idDef) =>
				   (emit Dup; emit (LdcI4 i); emit LdelemRef;
				    declareLocal idDef))
			 (List.rev indexIdDefList);
			 emit (LdcI4 i); emit LdelemRef; declareLocal idDef)
	    end
	  | declareArgs (ProdArgs labelIdDefVec, await) =
	    declareArgs (TupArgs (Vector.map #2 labelIdDefVec), await)

	fun emitBox (ty, dottedname) =
	    let
		val index = allocateLocal ty
	    in
		emit (Stloc index); emit (Ldloca index); emit (Box dottedname)
	    end

	fun genLit (WordLit w) =
	    (emit (LdcI4 (LargeWord.toInt w)); emitBox (Int32Ty, System.Int32))
	  | genLit (IntLit i) =
	    (emit (LdcI4 (LargeInt.toInt i)); emitBox (Int32Ty, System.Int32))
	  | genLit (CharLit c) =
	    (emit (LdcI4 (Char.ord c)); emitBox (CharTy, System.Char))
	  | genLit (StringLit s) = emit (Ldstr s)
	  | genLit (RealLit r) =
	    (emit (LdcR8 r); emitBox (Float64Ty, System.Double))

	(*--** remove global state *)
	val sharedLabels: label StampMap.t = StampMap.new ()
	val handlerCont: (label * body) option ref = ref NONE

	fun genStm (ValDec (_, idDef, exp)) =
	    (genExp (exp, BOTH); declareLocal idDef)
	  | genStm (RecDec (_, idDefExpVec)) =
	    (Vector.app (fn (idDef, exp) =>
			 (genExp (exp, PREPARE); declareLocal idDef))
	     idDefExpVec;
	     Vector.app (fn (idDef, exp) =>
			 case idDef of
			     IdDef id =>
				 (emitId id; genExp (exp, FILL))
			   | Wildcard => ()) idDefExpVec)
	  | genStm (RefAppDec (_, idDef, id)) =
	    (emitId id; emitAwait (); emit (Castclass Alice.CellTy);
	     emit (Ldfld (Alice.Cell, "Value", System.ObjectTy));
	     declareLocal idDef)
	  | genStm (TupDec (_, idDefs, id)) =
	    (emitId id; declareArgs (TupArgs idDefs, true))
	  | genStm (ProdDec (info, labelIdDefVec, id)) =
	    (emitId id; declareArgs (ProdArgs labelIdDefVec, true))
	  | genStm (TryStm (_, tryBody, idDef, handleBody)) =
	    let
		val label1 = newLabel ()
		val label2 = newLabel ()
		val label3 = newLabel ()
		val oldHandlerCont = !handlerCont
	    in
		handlerCont := NONE;
		emit (Try (label1, label2, Alice.Exception, label2, label3));
		emit (Label label1); genBody tryBody;
		emit (Label label2);
		emit (Ldfld (Alice.Exception, "Value", System.ObjectTy));
		declareLocal idDef; genBody handleBody;
		emit (Label label3);
		case !handlerCont of
		    SOME (label, contBody) =>
			(emit (Label label); genBody contBody)
		  | NONE => ();
		handlerCont := oldHandlerCont
	    end
	  | genStm (EndTryStm (_, body) | EndHandleStm (_, body)) =
	    (case !handlerCont of
		 SOME (label, _) => emit (Leave label)
	       | NONE =>
		     let
			 val label = newLabel ()
		     in
			 handlerCont := SOME (label, body); emit (Leave label)
		     end)
	  | genStm (stm as TestStm (_, id, tests, elseBody)) =
	    (emitId id; emitAwait (); genTests (tests, elseBody))
	  | genStm (RaiseStm (info, id)) =
	    let
		val ((line, _), _) = #region info
	    in
		emitId id; emit (LdcI4 line);
		emit (Newobj (Alice.Exception, [System.ObjectTy, Int32Ty]));
		emit Throw
	    end
	  | genStm (ReraiseStm (_, _)) = emit Rethrow
	  | genStm (SharedStm (_, body, stamp)) =
	    (case StampMap.lookup (sharedLabels, stamp) of
		 NONE =>
		     let
			 val label = newLabel ()
		     in
			 StampMap.insertDisjoint (sharedLabels, stamp, label);
			 emit (Label label); genBody body
		     end
	       | SOME label => emit (Br label))
	  | genStm (ReturnStm (_, exp)) = (*--** tail calls *)
	    (genExp (exp, BOTH); emit Ret)
	  | genStm (IndirectStm (_, ref bodyOpt)) = genBody (valOf bodyOpt)
	  | genStm (ExportStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	and genTests (LitTests #[], elseBody) = genBody elseBody
	  | genTests (LitTests litBodyVec, elseBody) =
	    (case Vector.sub (litBodyVec, 0) of
		 (WordLit _, _) =>
		     let
			 fun toInt (WordLit w, body) =
			     (LargeWord.toInt w, ignore, body)
			   | toInt (_, _) =
			     raise Crash.Crash "CodeGenPhase.genTests 1"
		     in
			 emit (Castclass System.Int32Ty);
			 emit (Unbox System.Int32); emit LdindI4;
			 genSwitchTestStm (Vector.map toInt litBodyVec,
					   fn () => genBody elseBody)
		     end
	       | (IntLit _, _) =>
		     let
			 fun toInt (IntLit i, body) =
			     (LargeInt.toInt i, ignore, body)
			   | toInt (_, _) =
			     raise Crash.Crash "CodeGenPhase.genTests 2"
		     in
			 emit (Castclass System.Int32Ty);
			 emit (Unbox System.Int32); emit LdindI4;
			 genSwitchTestStm (Vector.map toInt litBodyVec,
					   fn () => genBody elseBody)
		     end
	       | (CharLit _, _) =>
		     let
			 fun toInt (CharLit c, body) =
			     (WideChar.ord c, ignore, body)
			   | toInt (_, _) =
			     raise Crash.Crash "CodeGenPhase.genTests 3"
		     in
			 emit (Castclass System.CharTy);
			 emit (Unbox System.Char); emit LdindU2;
			 genSwitchTestStm (Vector.map toInt litBodyVec,
					   fn () => genBody elseBody)
		     end
	       | (StringLit _, _) =>
		     let
			 val max = Vector.length litBodyVec - 1
			 fun gen (i, (StringLit s, body)) =
			     let
				 val elseLabel = newLabel ()
				 val regState = saveRegState ()
			     in
				 if i < max then emit Dup else ();
				 emit (Ldstr s);
				 emit (Call (false, System.String, "Equals",
					     [System.StringTy,
					      System.StringTy], BoolTy));
				 emit (B (FALSE, elseLabel));
				 if i < max then emit Pop else ();
				 genBody body; emit (Label elseLabel);
				 restoreRegState regState
			     end
			   | gen (_, (_, _)) =
			     raise Crash.Crash "CodeGenPhase.genTests 4"
		     in
			 emit (Castclass System.StringTy);
			 Vector.appi gen (litBodyVec, 0, NONE);
			 genBody elseBody
		     end
	       | (RealLit _, _) =>
		     let
			 val max = Vector.length litBodyVec - 1
			 fun gen (i, (RealLit r, body)) =
			     let
				 val elseLabel = newLabel ()
				 val regState = saveRegState ()
			     in
				 if i < max then emit Dup else ();
				 emit (LdcR8 r); emit (B (NE_UN, elseLabel));
				 if i < max then emit Pop else ();
				 genBody body; emit (Label elseLabel);
				 restoreRegState regState
			     end
			   | gen (_, (_, _)) =
			     raise Crash.Crash "CodeGenPhase.genTests 4"
		     in
			 emit (Castclass System.DoubleTy);
			 emit (Unbox System.Double); emit LdindR8;
			 Vector.appi gen (litBodyVec, 0, NONE);
			 genBody elseBody
		     end)
	  | genTests (TagTests tagBodyVec, elseBody) =
	    let
		fun toInt (_, n, NONE, body) = (n, fn () => emit Pop, body)
		  | toInt (_, n, SOME args, body) =
		    (n, fn () => (emit (Castclass Alice.TagValTy);
				  emit (Ldfld (Alice.TagVal, "Value",
					       System.ObjectTy));
				  declareArgs (args, true)), body)
		val thenLabel = newLabel ()
		val contLabel = newLabel ()
	    in
		emit Dup; emit Dup; emit (Isinst Alice.TagValTy);
		emit (B (TRUE, thenLabel)); emit (Castclass System.Int32Ty);
		emit (Unbox System.Int32); emit LdindI4; emit (Br contLabel);
		emit (Label thenLabel); emit (Castclass Alice.TagValTy);
		emit (Call (true, Alice.TagVal, "GetTag", nil, Int32Ty));
		emit (Label contLabel);
		genSwitchTestStm (Vector.map toInt tagBodyVec,
				  fn () => (emit Pop; genBody elseBody))
	    end
	  | genTests (ConTests conBodyVec, elseBody) =
	    let
		val max = Vector.length conBodyVec - 1
		fun gen (i, (con, conArgs, body)) =
		    let
			val elseLabel = newLabel ()
			val regState = saveRegState ()
		    in
			if i < max then emit Dup else ();
			case con of
			    Con id => (emitId id; emitAwait ())
			  | StaticCon _ =>
				raise Crash.Crash
				    ("CodeGenPhase.genTests: " ^
				     "StaticCon not implemented");   (*--** *)
			emit (B (NE_UN, elseLabel));
			if i < max then emit Pop else ();
			case conArgs of
			    SOME args =>
				(emit (Castclass Alice.ConValTy);
				 emit (Ldfld (Alice.ConVal, "Value",
					      System.ObjectTy));
				 declareArgs (args, true))
			  | NONE => emit Pop;
			genBody body; emit (Label elseLabel);
			restoreRegState regState
		    end
		val contLabel = newLabel ()
	    in
		emit Dup; emit Dup; emit (Isinst Alice.ConValTy);
		emit (B (FALSE, contLabel)); emit (Castclass Alice.ConValTy);
		emit (Call (true, Alice.ConVal, "GetId", nil,
			    System.ObjectTy));
		emit (Label contLabel);
		Vector.appi gen (conBodyVec, 0, NONE);
		emit Pop; genBody elseBody
	    end
	  | genTests (VecTests vecBodyVec, elseBody) =
	    let
		fun toInt (idDefs, body) =
		    (Vector.length idDefs,
		     fn () => declareArgs (TupArgs idDefs, false), body)
	    in
		emit (Castclass (ArrayTy System.ObjectTy));
		emit Dup; emit Ldlen;
		genSwitchTestStm (Vector.map toInt vecBodyVec,
				  fn () => (emit Pop; genBody elseBody))
	    end
	and genSwitchTestStm (intGenBodyVec, elseBodyFun) =
	    let
		val map = IntMap.new ()
		val i = #1 (Vector.sub (intGenBodyVec, 0))
		val (min, max) =
		    parseTests (intGenBodyVec, map,
				Vector.length intGenBodyVec - 1, i, i)
	    in
		if IntMap.size map > 1
		    andalso max - min + 1 <= IntMap.size map * 4
		then
		    let
			val elseLabel = newLabel ()
			val labels =
			    List.tabulate (max - min + 1,
					   fn i =>
					   case IntMap.lookup (map, i + min) of
					       SOME (label, _, _) => label
					     | NONE => elseLabel)
		    in
			if min = 0 then () else (emit (LdcI4 min); emit Sub);
			emit (Switch labels);
			let
			    val regState = saveRegState ()
			in
			    emit (Label elseLabel); elseBodyFun ();
			    restoreRegState regState
			end;
			IntMap.app (fn (label, gen, body) =>
				    let
					val regState = saveRegState ()
				    in
					emit (Label label);
					gen (); genBody body;
					restoreRegState regState
				    end) map
		    end
		else
		    let
			val intGenBodyList = Vector.toList intGenBodyVec
			val intGenBodyList = List.rev intGenBodyList
			val (i, gen, body) = List.hd intGenBodyList
			val intGenBodyList = List.rev (List.tl intGenBodyList)
		    in
			List.app (fn (i, gen, body) =>
				  let
				      val regState = saveRegState ()
				      val elseLabel = newLabel ()
				  in
				      emit Dup; emit (LdcI4 i);
				      emit (B (NE_UN, elseLabel)); emit Pop;
				      gen (); genBody body;
				      emit (Label elseLabel);
				      restoreRegState regState
				  end) intGenBodyList;
			let
			    val regState = saveRegState ()
			    val elseLabel = newLabel ()
			in
			    emit (LdcI4 i); emit (B (NE_UN, elseLabel));
			    gen (); genBody body; emit (Label elseLabel);
			    restoreRegState regState
			end;
			let
			    val regState = saveRegState ()
			in
			    elseBodyFun (); restoreRegState regState
			end
		    end
	    end
	and genExp (LitExp (_, lit), PREPARE) = genLit lit
	  | genExp (PrimExp (_, name), PREPARE) =
	    let
		val (dottedname, id) = Builtins.lookupField name
	    in
		emit (Ldsfld (dottedname, id, System.ObjectTy))
	    end
	  | genExp (NewExp _, PREPARE) =
	    let
		val index = allocateLocal System.GuidTy
	    in
		emit (Call (false, System.Guid, "NewGuid",
			    nil, System.GuidTy));
		emit (Stloc index); emit (Ldloca index);
		emit (Box System.Guid)
	    end
	  | genExp (VarExp (_, id), PREPARE) = emitId id
	  | genExp (TagExp (_, _, n), PREPARE) =
	    (emit (LdcI4 n); emitBox (Int32Ty, System.Int32))
	  | genExp (ConExp (_, Con id), PREPARE) = emitId id
	  | genExp (ConExp (_, StaticCon _), _) =   (*--** implement *)
	    raise Crash.Crash "CodeGenPhase.genExp: ConExp/StaticCon"
	  | genExp (TupExp (info, #[]), PREPARE) =
	    genExp (PrimExp (info, "unit"), PREPARE)
	  | genExp (TupExp (_, #[]), FILL) = ()
	  | genExp (TupExp (info, #[]), BOTH) =
	    genExp (PrimExp (info, "unit"), BOTH)
	  | genExp (TupExp (_, ids), PREPARE) =
	    (emit (LdcI4 (Vector.length ids)); emit (Newarr System.ObjectTy))
	  | genExp (TupExp (_, ids), FILL) =
	    let
		val max = Vector.length ids - 1
	    in
		Vector.appi (fn (i, id) =>
			     (if i = max then () else emit Dup;
				  emit (LdcI4 i); emitId id; emit StelemRef))
		(ids, 0, NONE)
	    end
	  | genExp (TupExp (_, ids), BOTH) =
	    (emit (LdcI4 (Vector.length ids)); emit (Newarr System.ObjectTy);
	     Vector.appi (fn (i, id) =>
			  (emit Dup; emit (LdcI4 i); emitId id;
			   emit StelemRef)) (ids, 0, NONE))
	  | genExp (ProdExp (info, labelIdVec), mode) =
	    genExp (TupExp (info, Vector.map #2 labelIdVec), mode)
	  | genExp (VecExp (info, ids), mode) =
	    genExp (TupExp (info, ids), mode)
	  | genExp (FunExp (info, stamp, _, args, body), PREPARE) =
	    (emitRegion ("FunExp", #region info);
	     emit (Newobj (className stamp, nil));
	     case args of
		 TupArgs #[] =>
		     defineClass (stamp, Alice.Procedure0, nil)
	       | (TupArgs #[_, _] |
		  ProdArgs #[_, _]) =>
		     defineClass (stamp, Alice.Procedure2, nil)
	       | (TupArgs #[_, _, _] |
		  ProdArgs #[_, _, _]) =>
		     defineClass (stamp, Alice.Procedure3, nil)
	       | (TupArgs #[_, _, _, _] |
		  ProdArgs #[_, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure4, nil)
	       | (TupArgs #[_, _, _, _, _] |
		  ProdArgs #[_, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure5, nil)
	       | (TupArgs #[_, _, _, _, _, _] |
		  ProdArgs #[_, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure6, nil)
	       | (TupArgs #[_, _, _, _, _, _, _] |
		  ProdArgs #[_, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure7, nil)
	       | (TupArgs #[_, _, _, _, _, _, _, _] |
		  ProdArgs #[_, _, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure8, nil)
	       | (TupArgs #[_, _, _, _, _, _, _, _, _] |
		  ProdArgs #[_, _, _, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure9, nil)
	       | _ =>
		     defineClass (stamp, Alice.Procedure, nil))
	  | genExp (FunExp (_, stamp, _, args, body), FILL) =
	    (case args of
		 OneArg idDef =>
		     (defineMethod (stamp, "Apply", [idDef]);
		      genBody body; closeMethod ())
	       | (TupArgs (idDefs as #[]) |
		  TupArgs (idDefs as #[_, _]) |
		  TupArgs (idDefs as #[_, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _, _, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _, _, _, _, _]) |
		  TupArgs (idDefs as #[_, _, _, _, _, _, _, _, _])) =>
		     (defineMethod (stamp, "Apply", Vector.toList idDefs);
		      genBody body; closeMethod ())
	       | (ProdArgs (labelIdDefVec as #[_, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _, _, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _, _, _, _, _]) |
		  ProdArgs (labelIdDefVec as #[_, _, _, _, _, _, _, _, _])) =>
		     (defineMethod (stamp, "Apply",
				    Vector.foldr (fn ((_, id), rest) =>
						  id::rest) nil labelIdDefVec);
		      genBody body; closeMethod ())
	       | _ =>
		     let
			 val info = {region = Source.nowhere}
			 val id = Id (info, Stamp.new (), Name.InId)
		     in
			 defineMethod (stamp, "Apply", [IdDef id]); emitId id;
			 declareArgs (args, true); genBody body; closeMethod ()
		     end;
	     emit Pop)
	  | genExp (PrimAppExp (_, name, ids), BOTH) =
	    let
		val dottedname = Builtins.lookupClass name
	    in
		Vector.app emitId ids;
		emit (Call (false, dottedname, "StaticApply",
			    List.tabulate (Vector.length ids,
					   fn _ => System.ObjectTy),
			    System.ObjectTy))
	    end
	  | genExp (VarAppExp (_, id1, OneArg id2), BOTH) =
	    (emitId id1; emit (Castclass Alice.ProcedureTy);
	     emitId id2;
	     emit (Callvirt (Alice.Procedure, "Apply",
			     [System.ObjectTy], System.ObjectTy)))
	  | genExp (VarAppExp
		    (_, id, (TupArgs (ids as #[]) |
			     TupArgs (ids as #[_, _]) |
			     TupArgs (ids as #[_, _, _]) |
			     TupArgs (ids as #[_, _, _, _]) |
			     TupArgs (ids as #[_, _, _, _, _]) |
			     TupArgs (ids as #[_, _, _, _, _, _]) |
			     TupArgs (ids as #[_, _, _, _, _, _, _]) |
			     TupArgs (ids as #[_, _, _, _, _, _, _, _]) |
			     TupArgs (ids as #[_, _, _, _, _, _, _, _, _]))),
		    BOTH) =
	    (emitId id; emit (Castclass Alice.ProcedureTy);
	     Vector.app emitId ids;
	     emit (Callvirt (Alice.Procedure, "Apply",
			     List.tabulate (Vector.length ids,
					    fn _ => System.ObjectTy),
			     System.ObjectTy)))
	  | genExp (VarAppExp (info, id, TupArgs ids), BOTH) =
	    (emitId id; emit (Castclass Alice.ProcedureTy);
	     genExp (TupExp (info, ids), BOTH);
	     emit (Callvirt (Alice.Procedure, "Apply",
			     [System.ObjectTy], System.ObjectTy)))
	  | genExp (VarAppExp (info, id, ProdArgs labelIdVec), mode) =
	    genExp (VarAppExp (info, id,
			       TupArgs (Vector.map #2 labelIdVec)), mode)
	  | genExp (TagAppExp (_, _, n, _), PREPARE) =
	    (emit (LdcI4 n); emit (Newobj (Alice.TagVal, [Int32Ty])))
	  | genExp (TagAppExp (_, _, _, args), FILL) =
	    (genArgs args;
	     emit (Stfld (Alice.TagVal, "Value", System.ObjectTy)))
	  | genExp (TagAppExp (_, _, n, args), BOTH) =
	    (emit (LdcI4 n); genArgs args;
	     emit (Newobj (Alice.TagVal, [Int32Ty, System.ObjectTy])))
	  | genExp (ConAppExp (_, Con id, _), PREPARE) =
	    (emitId id; emit (Newobj (Alice.ConVal, [System.ObjectTy])))
	  | genExp (ConAppExp (_, Con _, args), FILL) =
	    (genArgs args;
	     emit (Stfld (Alice.ConVal, "Value", System.ObjectTy)))
	  | genExp (ConAppExp (_, Con id, args), BOTH) =
	    (emitId id; genArgs args;
	     emit (Newobj (Alice.ConVal, [System.ObjectTy, System.ObjectTy])))
	  | genExp (ConAppExp (_, StaticCon _, _), _) =   (*--** implement *)
	    raise Crash.Crash "CodeGenPhase.genExp: ConAppExp/StaticCon"
	  | genExp (RefAppExp (_, _), PREPARE) =
	    emit (Newobj (Alice.Cell, nil))
	  | genExp (RefAppExp (_, id), FILL) =
	    (emitId id; emit (Stfld (Alice.Cell, "Value", System.ObjectTy)))
	  | genExp (RefAppExp (_, id), BOTH) =
	    (emitId id; emit (Newobj (Alice.Cell, [System.ObjectTy])))
	  | genExp (SelAppExp (_, _, _, n, id), BOTH) =
	    (emitId id; emitAwait ();
	     emit (Castclass (ArrayTy System.ObjectTy));
	     emit (LdcI4 n); emit LdelemRef)
	  | genExp (FunAppExp (info, id, _, args), expMode) =
	    genExp (VarAppExp (info, id, args), expMode)
	  | genExp (exp, PREPARE) =
	    raise Crash.Crash "CodeGenPhase.genExp: not admissible"
	  | genExp (_, FILL) = emit Pop
	  | genExp (exp, BOTH) =
	    (genExp (exp, PREPARE); emit Dup; genExp (exp, FILL))
	and genArgs (OneArg id) = emitId id
	  | genArgs (TupArgs ids) =
	    genExp (TupExp ({region = Source.nowhere}, ids), BOTH)
	  | genArgs (ProdArgs labelIdVec) =
	    genExp (ProdExp ({region = Source.nowhere}, labelIdVec), BOTH)
	and genBody (stm::stms) =
	    (case #liveness (infoStm stm) of
		 ref (Kill set) => kill set
	       | ref _ => ();
	     genStm stm; genBody stms)
	  | genBody nil = ()

	fun translate () (desc, component as (imports, (body, exportSign))) =
	    (init nil;
	     Vector.app (fn (idDef, _, url) =>
			 (emit (Ldarg 0); emit (Castclass Alice.KomponistTy);
			  emit (Ldstr (Url.toString url));
			  emit (Call (true, Alice.Komponist, "Import",
				      [System.StringTy], System.ObjectTy));
			  declareLocal idDef)) imports;
	     genBody body;
	     (close(), exportSign))
    end
