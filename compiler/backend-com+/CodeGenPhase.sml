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

	fun emitRegion (s, ((a, b), _)) =
	    emit (Comment (s ^ " at " ^ Int.toString a ^ "." ^ Int.toString b))

	datatype expMode =
	    PREPARE
	  | FILL
	  | BOTH

	fun emitAwait () =
	    let
		val label = newLabel ()
	    in
		emit Dup; emit (Isinst Alice.FutureTy);
		emit (B (FALSE, label));
		emit (Castclass Alice.FutureTy);
		emit (Callvirt (Alice.Future, "Await", nil, System.ObjectTy));
		emit (Label label)
	    end

	structure IntMap =
	     MakeHashImpMap(type t = int
			    val equals = op=
			    fun hash i = i)

	fun parseTests ((test, body)::rest, map, min, max, toInt) =
	    let
		val i = toInt test
	    in
		IntMap.insertWith #1 (map, i, (newLabel (), test, body));
		parseTests (rest, map, Int.min (min, i), Int.max (max, i),
			    toInt)
	    end
	  | parseTests (nil, _, min, max, _) = (min, max)

	fun declareArgs (OneArg id, _) = declareLocal id
	  | declareArgs (TupArgs nil, await) =
	    (if await then emitAwait () else (); emit Pop)
	  | declareArgs (TupArgs ids, await) =
	    let
		val max = List.length ids - 1
	    in
		if await then emitAwait () else ();
		List.appi (fn (i, id) =>
			   (if i = max then () else emit Dup;
			    emit (LdcI4 i); emit LdelemRef;
			    declareLocal id)) ids
	    end
	  | declareArgs (RecArgs labelIdList, await) =
	    declareArgs (TupArgs (List.map #2 labelIdList), await)

	fun genTest (LitTest (WordLit w), elseLabel) =
	    (emit (Castclass System.Int32Ty); emit Dup;
	     emit (Unbox System.Int32); emit (LdcI4 (LargeWord.toInt w));
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (LitTest (IntLit i), elseLabel) =
	    (emit (Castclass System.Int32Ty); emit Dup;
	     emit (Unbox System.Int32); emit (LdcI4 (LargeInt.toInt i));
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (LitTest (CharLit c), elseLabel) =
	    (emit (Castclass System.CharTy); emit Dup;
	     emit (Unbox System.Char); emit (LdcI4 (WideChar.ord c));
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (LitTest (StringLit s), elseLabel) =
	    (emit (Castclass System.StringTy); emit Dup; 
	     emit (Ldstr s);
	     emit (Call (false, System.String, "Equals",
			 [System.StringTy, System.StringTy], BoolTy));
	     emit (B (FALSE, elseLabel)); emit Pop)
	  | genTest (LitTest (RealLit s), elseLabel) =
	    (emit (Castclass System.DoubleTy); emit Dup;
	     emit (Unbox System.Double); emit (LdcR8 s);
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (TagTest (_, n), elseLabel) =
	    (emit Dup; emit (Isinst System.Int32Ty);
	     emit (B (FALSE, elseLabel)); emit (Castclass System.Int32Ty);
	     emit Dup; emit (Unbox System.Int32); emit (LdcI4 n);
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (TagAppTest (_, n, args), elseLabel) =
	    (emit Dup; emit (Isinst Alice.TagValTy);
	     emit (B (FALSE, elseLabel)); emit (Castclass Alice.TagValTy);
	     emit Dup;
	     emit (Call (true, Alice.TagVal, "GetTag", nil, Int32Ty));
	     emit (LdcI4 n); emit (B (NE_UN, elseLabel));
	     emit (Ldfld (Alice.TagVal, "Value", System.ObjectTy));
	     declareArgs (args, true))
	  | genTest (ConTest id, elseLabel) =
	    (emit Dup; emitId id; emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (ConAppTest (id, args), elseLabel) =
	    (emit Dup; emit (Isinst Alice.ConValTy);
	     emit (B (FALSE, elseLabel)); emit (Castclass Alice.ConValTy);
	     emit Dup;
	     emit (Call (true, Alice.ConVal, "GetId", nil, System.ObjectTy));
	     emitId id; emit (B (NE_UN, elseLabel));
	     emit (Ldfld (Alice.ConVal, "Value", System.ObjectTy));
	     declareArgs (args, true))
	  | genTest (RefAppTest id, elseLabel) =
	    (emit (Castclass Alice.CellTy);
	     emit (Call (true, Alice.Cell, "Access", nil, System.ObjectTy));
	     declareLocal id)
	  | genTest (TupTest nil, elseLabel) = emit Pop
	  | genTest (TupTest ids, elseLabel) =
	    (emit (Castclass (ArrayTy System.ObjectTy));
	     declareArgs (TupArgs ids, false))
	  | genTest (RecTest labelIdList, elseLabel) =
	    genTest (TupTest (List.map #2 labelIdList), elseLabel)
	  | genTest (LabTest (_, n, id), elseLabel) =
	    (emit (Castclass (ArrayTy System.ObjectTy));
	     emit (LdcI4 n); emit LdelemRef; declareLocal id)
	  | genTest (VecTest ids, elseLabel) =
	    (emit (Castclass (ArrayTy System.ObjectTy)); emit Dup;
	     emit Dup; emit Ldlen; emit (LdcI4 (List.length ids));
	     emit (B (NE_UN, elseLabel));
	     declareArgs (TupArgs ids, false))

	fun genLit (WordLit w) =
	    (emit (LdcI4 (LargeWord.toInt w)); emit (Box System.Int32))
	  | genLit (IntLit i) =
	    (emit (LdcI4 (LargeInt.toInt i)); emit (Box System.Int32))
	  | genLit (CharLit c) =
	    (emit (LdcI4 (Char.ord c)); emit (Box System.Char))
	  | genLit (StringLit s) = emit (Ldstr s)
	  | genLit (RealLit s) =
	    (emit (LdcR8 s); emit (Box System.Double))

	(*--** remove global state *)
	val sharedLabels: label StampMap.t = StampMap.new ()

	fun genStm (ValDec (_, id, exp)) =
	    (genExp (exp, BOTH); declareLocal id)
	  | genStm (RecDec (_, idExpList)) =
	    (List.app (fn (id, exp) =>
		       (genExp (exp, PREPARE); declareLocal id)) idExpList;
	     List.app (fn (id, exp) =>
		       (emitId id; genExp (exp, FILL))) idExpList)
	  | genStm (EvalStm (_, exp)) = (genExp (exp, BOTH); emit Pop)
	  | genStm (HandleStm (_, tryBody, id, catchBody, contBody, stamp)) =
	    let
		val label1 = newLabel ()
		val label2 = newLabel ()
		val label3 = newLabel ()
	    in
		StampMap.insertDisjoint (sharedLabels, stamp, label3);
		emit (Try (label1, label2, Alice.Exception, label2, label3));
		emit (Label label1); genBody tryBody;
		emit (Label label2);
		emit (Ldfld (Alice.Exception, "Value", System.ObjectTy));
		declareLocal id; genBody catchBody;
		emit (Label label3); genBody contBody
	    end
	  | genStm (EndHandleStm (_, stamp)) =
	    emit (Leave (StampMap.lookupExistent (sharedLabels, stamp)))
	  | genStm (stm as TestStm (_, id, testBodyList, elseBody)) =
	    genTestStm (id, testBodyList, fn () => genBody elseBody)
	  | genStm (RaiseStm (info, id)) =
	    let
		val ((line, _), _) = #region info
	    in
		emitId id; (*--** emit (LdcI4 line); *)
		emit (Newobj (Alice.Exception,
			      [System.ObjectTy (*--** , Int32Ty *)]));
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
	  | genStm (ReturnStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	  | genStm (IndirectStm (_, ref bodyOpt)) = genBody (valOf bodyOpt)
	  | genStm (ExportStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	and genTestStm (id, testBodyList, elseBodyFun) =
	    (emitId id; emitAwait (); genTestStm' (testBodyList, elseBodyFun))
	and genTestStm' (testBodyList as (LitTest (WordLit _), _)::_::_,
			 elseBodyFun) =
	    let
		fun toInt (LitTest (WordLit w)) = LargeWord.toInt w
		  | toInt _ = raise Crash.Crash "CodeGenTest.genTestStm' 1"
		fun getInt () = (emit (Castclass System.Int32Ty);
				 emit (Unbox System.Int32))
		fun gen (_, body) = genBody body
	    in
		genSwitchTestStm (toInt, getInt, gen,
				  testBodyList, elseBodyFun)
	    end
	  | genTestStm' (testBodyList as (LitTest (IntLit _), _)::_::_,
			 elseBodyFun) =
	    let
		fun toInt (LitTest (IntLit i)) = LargeInt.toInt i
		  | toInt _ = raise Crash.Crash "CodeGenTest.genTestStm' 2"
		fun getInt () = (emit (Castclass System.Int32Ty);
				 emit (Unbox System.Int32))
		fun gen (_, body) = genBody body
	    in
		genSwitchTestStm (toInt, getInt, gen,
				  testBodyList, elseBodyFun)
	    end
	  | genTestStm' (testBodyList as (LitTest (CharLit _), _)::_::_,
			 elseBodyFun) =
	    let
		fun toInt (LitTest (CharLit c)) = WideChar.ord c
		  | toInt _ = raise Crash.Crash "CodeGenTest.genTestStm' 3"
		fun getInt () = (emit (Castclass System.CharTy);
				 emit (Unbox System.Char))
		fun gen (_, body) = genBody body
	    in
		genSwitchTestStm (toInt, getInt, gen,
				  testBodyList, elseBodyFun)
	    end
	  | genTestStm' (testBodyList as (TagTest (_, _), _)::_::_,
			 elseBodyFun) =
	    genTagTestStm (testBodyList, elseBodyFun)
	  | genTestStm' (testBodyList as (TagAppTest (_, _, _), _)::_::_,
			 elseBodyFun) =
	    genTagTestStm (testBodyList, elseBodyFun)
	  | genTestStm' (testBodyList, elseBodyFun) =
	    genSequentialTestStm (testBodyList, elseBodyFun)
	and genTagTestStm (testBodyList, elseBodyFun) =
	    let
		fun toInt (TagTest (_, n)) = n
		  | toInt (TagAppTest (_, n, _)) = n
		  | toInt _ = raise Crash.Crash "CodeGenTest.genTagTestStm"
		fun getInt () =
		    let
			val thenLabel = newLabel ()
			val contLabel = newLabel ()
		    in
			emit Dup; emit Dup; emit (Isinst Alice.TagValTy);
			emit (B (TRUE, thenLabel));
			emit (Castclass System.Int32Ty);
			emit (Unbox System.Int32); emit (Br contLabel);
			emit (Label thenLabel);
			emit (Castclass Alice.TagValTy);
			emit (Call (true, Alice.TagVal, "GetTag", nil,
				    Int32Ty));
			emit (Label contLabel)
		    end
		fun gen (TagAppTest (_, _, args), body) =
		    (emit (Ldfld (Alice.TagVal, "Value", System.ObjectTy));
		     declareArgs (args, true); genBody body)
		  | gen (_, body) = (emit Pop; genBody body)
	    in
		genSwitchTestStm (toInt, getInt, gen,
				  testBodyList, elseBodyFun)
	    end
	and genSwitchTestStm (toInt, getInt, gen, testBodyList, elseBodyFun) =
	    let
		val map = IntMap.new ()
		val i = toInt (#1 (List.hd testBodyList))
		val (min, max) = parseTests (testBodyList, map, i, i, toInt)
	    in
		if max - min + 1 <= IntMap.size map * 4 then
		    let
			val elseLabel = newLabel ()
			val labels =
			    List.tabulate (max - min + 1,
					   fn i =>
					   case IntMap.lookup (map, i + min) of
					       SOME (label, _, _) => label
					     | NONE => elseLabel)
		    in
			getInt ();
			if min = 0 then () else (emit (LdcI4 min); emit Sub);
			emit (Switch labels);
			let
			    val regState = saveRegState ()
			in
			    emit (Label elseLabel); elseBodyFun ();
			    restoreRegState regState
			end;
			IntMap.app (fn (label, test, body) =>
				    let
					val regState = saveRegState ()
				    in
					emit (Label label); gen (test, body);
					restoreRegState regState
				    end) map
		    end
		else genSequentialTestStm (testBodyList, elseBodyFun)
	    end
	and genSequentialTestStm (testBodyList, elseBodyFun) =
	    (List.app (fn (test, body) =>
		       let
			   val elseLabel = newLabel ()
			   val regState = saveRegState ()
		       in
			   genTest (test, elseLabel); genBody body;
			   emit (Label elseLabel); restoreRegState regState
		       end) testBodyList;
	     emit Pop; elseBodyFun ())
	and genExp (LitExp (_, lit), PREPARE) = genLit lit
	  | genExp (PrimExp (_, name), PREPARE) =
	    let
		val (dottedname, id) = Builtins.lookupField name
	    in
		emit (Ldsfld (dottedname, id, System.ObjectTy))
	    end
	  | genExp (NewExp (_, _), PREPARE) =
	    emit (Newobj (System.Guid, nil))
	  | genExp (VarExp (_, id), PREPARE) = emitId id
	  | genExp (TagExp (_, _, n, NONE), PREPARE) =
	    (emit (LdcI4 n); emit (Box System.Int32))
	  | genExp (TagExp (_, _, n, SOME _), PREPARE) =
	    (emit (LdcI4 n);
	     emit (Newobj (Alice.TagConstructor, [Int32Ty])))
	  | genExp (ConExp (_, id, NONE), PREPARE) = emitId id
	  | genExp (ConExp (_, id, SOME _), PREPARE) =
	    (emitId id;
	     emit (Newobj (Alice.ConConstructor, [System.ObjectTy])))
	  | genExp (RefExp _, PREPARE) =
	    emit (Ldsfld (Alice.Prebound.General, "ref", System.ObjectTy))
	  | genExp (TupExp (_, nil), PREPARE) = emit Ldnull
	  | genExp (TupExp (_, nil), FILL) = ()
	  | genExp (TupExp (_, nil), BOTH) = emit Ldnull
	  | genExp (TupExp (_, ids), PREPARE) =
	    (emit (LdcI4 (List.length ids)); emit (Newarr System.ObjectTy))
	  | genExp (TupExp (_, ids), FILL) =
	    let
		val max = List.length ids - 1
	    in
		List.appi (fn (i, id) =>
			   (if i = max then () else emit Dup;
			    emit (LdcI4 i); emitId id; emit StelemRef)) ids
	    end
	  | genExp (TupExp (_, ids), BOTH) =
	    (emit (LdcI4 (List.length ids)); emit (Newarr System.ObjectTy);
	     List.appi (fn (i, id) =>
			(emit Dup; emit (LdcI4 i); emitId id;
			 emit StelemRef)) ids)
	  | genExp (RecExp (info, labelIdList), mode) =
	    genExp (TupExp (info, List.map #2 labelIdList), mode)
	  | genExp (SelExp (_, _, n), BOTH) =
	    (emit (LdcI4 n); emit (Newobj (Alice.Selector, [Int32Ty])))
	  | genExp (VecExp (info, ids), mode) =
	    genExp (TupExp (info, ids), mode)
	  | genExp (FunExp (info, stamp, _, args, body), PREPARE) =
	    (emitRegion ("FunExp", #region info);
	     emit (Newobj (className stamp, nil));
	     case args of
		 TupArgs nil =>
		     defineClass (stamp, Alice.Procedure0, nil)
	       | (TupArgs [_, _] |
		  RecArgs [_, _]) =>
		     defineClass (stamp, Alice.Procedure2, nil)
	       | (TupArgs [_, _, _] |
		  RecArgs [_, _, _]) =>
		     defineClass (stamp, Alice.Procedure3, nil)
	       | (TupArgs [_, _, _, _] |
		  RecArgs [_, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure4, nil)
	       | (TupArgs [_, _, _, _, _] |
		  RecArgs [_, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure5, nil)
	       | (TupArgs [_, _, _, _, _, _] |
		  RecArgs [_, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure6, nil)
	       | (TupArgs [_, _, _, _, _, _, _] |
		  RecArgs [_, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure7, nil)
	       | (TupArgs [_, _, _, _, _, _, _, _] |
		  RecArgs [_, _, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure8, nil)
	       | (TupArgs [_, _, _, _, _, _, _, _, _] |
		  RecArgs [_, _, _, _, _, _, _, _, _]) =>
		     defineClass (stamp, Alice.Procedure9, nil)
	       | _ =>
		     defineClass (stamp, Alice.Procedure, nil))
	  | genExp (FunExp (_, stamp, _, args, body), FILL) =
	    (case args of
		 OneArg id =>
		     (defineMethod (stamp, "Apply", [id]);
		      genBody body; closeMethod ())
	       | (TupArgs (ids as nil) |
		  TupArgs (ids as [_, _]) |
		  TupArgs (ids as [_, _, _]) |
		  TupArgs (ids as [_, _, _, _]) |
		  TupArgs (ids as [_, _, _, _, _]) |
		  TupArgs (ids as [_, _, _, _, _, _]) |
		  TupArgs (ids as [_, _, _, _, _, _, _]) |
		  TupArgs (ids as [_, _, _, _, _, _, _, _]) |
		  TupArgs (ids as [_, _, _, _, _, _, _, _, _])) =>
		     (defineMethod (stamp, "Apply", nil);
		      genBody body; closeMethod ())
	       | (RecArgs (labelIdList as [_, _]) |
		  RecArgs (labelIdList as [_, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _, _, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _, _, _, _, _]) |
		  RecArgs (labelIdList as [_, _, _, _, _, _, _, _, _])) =>
		     (defineMethod (stamp, "Apply", List.map #2 labelIdList);
		      genBody body; closeMethod ())
	       | _ =>
		     let
			 val info = {region = Source.nowhere}
			 val id = Id (info, Stamp.new (), Name.InId)
			 val test =
			     case args of
				 OneArg _ =>
				     raise Crash.Crash
					 "CodeGenPhase.genExp: FunExp"
			       | TupArgs ids => TupTest ids
			       | RecArgs labelIdList => RecTest labelIdList
		     in
			 defineMethod (stamp, "Apply", [id]);
			 genTestStm (id, [(test, body)], fn () => ());
			 closeMethod ()
		     end;
	     emit Pop)
	  | genExp (PrimAppExp (_, name, ids), BOTH) =
	    let
		val dottedname = Builtins.lookupClass name
	    in
		List.app emitId ids;
		emit (Call (false, dottedname, "StaticApply",
			    List.map (fn _ => System.ObjectTy) ids,
			    System.ObjectTy))
	    end
	  | genExp (VarAppExp (_, id1, OneArg id2), BOTH) =
	    (emitId id1; emit (Castclass Alice.ProcedureTy);
	     emitId id2;
	     emit (Callvirt (Alice.Procedure, "Apply",
			     [System.ObjectTy], System.ObjectTy)))
	  | genExp (VarAppExp (_, id,
			       (TupArgs (ids as nil) |
				TupArgs (ids as [_, _]) |
				TupArgs (ids as [_, _, _]) |
				TupArgs (ids as [_, _, _, _]) |
				TupArgs (ids as [_, _, _, _, _]) |
				TupArgs (ids as [_, _, _, _, _, _]) |
				TupArgs (ids as [_, _, _, _, _, _, _]) |
				TupArgs (ids as [_, _, _, _, _, _, _, _]) |
				TupArgs (ids as [_, _, _, _, _, _, _, _, _]))),
		    BOTH) =
	    (emitId id; emit (Castclass Alice.ProcedureTy);
	     List.app emitId ids;
	     emit (Callvirt (Alice.Procedure, "Apply",
			     List.map (fn _ => System.ObjectTy) ids,
			     System.ObjectTy)))
	  | genExp (VarAppExp (info, id, TupArgs ids), BOTH) =
	    (emitId id; emit (Castclass Alice.ProcedureTy);
	     genExp (TupExp (info, ids), BOTH);
	     emit (Callvirt (Alice.Procedure, "Apply",
			     [System.ObjectTy], System.ObjectTy)))
	  | genExp (VarAppExp (info, id, RecArgs labelIdList), mode) =
	    genExp (VarAppExp (info, id,
			       TupArgs (List.map #2 labelIdList)), mode)
	  | genExp (TagAppExp (_, _, n, _), PREPARE) =
	    (emit (LdcI4 n); emit (Newobj (Alice.TagVal, [Int32Ty])))
	  | genExp (TagAppExp (_, _, _, args), FILL) =
	    (genArgs args;
	     emit (Stfld (Alice.TagVal, "Value", System.ObjectTy)))
	  | genExp (TagAppExp (_, _, n, args), BOTH) =
	    (emit (LdcI4 n); genArgs args;
	     emit (Newobj (Alice.TagVal, [Int32Ty, System.ObjectTy])))
	  | genExp (ConAppExp (_, id, _), PREPARE) =
	    (emitId id; emit (Newobj (Alice.ConVal, [System.ObjectTy])))
	  | genExp (ConAppExp (_, _, args), FILL) =
	    (genArgs args;
	     emit (Stfld (Alice.ConVal, "Value", System.ObjectTy)))
	  | genExp (ConAppExp (_, id, args), BOTH) =
	    (emitId id; genArgs args;
	     emit (Newobj (Alice.ConVal, [System.ObjectTy, System.ObjectTy])))
	  | genExp (RefAppExp (_, _), PREPARE) =
	    emit (Newobj (Alice.Cell, nil))
	  | genExp (RefAppExp (_, id), FILL) =
	    (emitId id;
	     emit (Call (true, Alice.Cell, "Assign",
			 [System.ObjectTy], VoidTy)))
	  | genExp (RefAppExp (_, id), BOTH) =
	    (emitId id; emit (Newobj (Alice.Cell, [System.ObjectTy])))
	  | genExp (SelAppExp (_, _, n, id), BOTH) =
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
	  | genArgs (RecArgs labelIdList) =
	    genExp (RecExp ({region = Source.nowhere}, labelIdList), BOTH)
	and genBody (stm::stms) =
	    (case #liveness (infoStm stm) of
		 ref (Kill set) => kill set
	       | ref _ => ();
	     genStm stm; genBody stms)
	  | genBody nil = ()

	fun translate () (desc, component as (imports, (body, _))) =
	    (init ["Test"];
	     Assert.assert (List.length imports = 0);   (*--** implement *)
	     genBody body; close())
    end
