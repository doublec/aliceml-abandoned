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

(*
 * Optimierungsideen:
 *
 * Voraussetzung: Fuer Pickling/Marshaling muss erkannt werden, welche
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
 * Literale und RecordArities sollen auch in V-Registern gespeichert
 * werden, zaehlen also auch als `statische freie Variablen'.  V-Register
 * werden als zum Code gehoerig betrachtet.
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
 * unter sich hat, kann die Definitionen ihrer Hilfsprozeduren in ihrer
 * eigenen Klasse aufnehmen.
 *
 * Entsprechend dominierte Hilfsprozeduren sollen mit dem Flag AuxiliaryOf
 * annotiert werden.
 *)

structure CodeGenPhase :> CODE_GEN_PHASE =
    struct
	structure I = ImperativeGrammar
	structure O = IL

	structure System =
	    struct
		val Object = ["System", "Object"]
		val ObjectTy = O.ClassTy Object
		val Int32 = ["System", "Int32"]
		val Int32Ty = O.ClassTy Int32
		val String = ["System", "String"]
		val StringTy = O.ClassTy String
	    end

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

	fun emitCoord (s, ((a, b), _)) =
	    emit (Comment (s ^ " at " ^ Int.toString a ^ "." ^ Int.toString b))

	fun emitRecordArity labelIdList =
	    (emit (LdcI4 (List.length labelIdList));
	     emit (Newarr System.ObjectTy);
	     Misc.List_appi (fn (i, (label, _)) =>
			     (emit Dup; emit (LdcI4 i);
			      case Label.toInt label of
				  SOME i =>
				      (emit (LdcI4 i);
				       emit (Newobj (System.Int32, [Int32Ty])))
				| NONE =>
				      emit (Ldstr (Label.toString label));
			      emit (StelemRef))) labelIdList;
	     emit (Call (false, StockWerk.RecordArity, "MakeRecordArity",
			 [ArrayTy System.ObjectTy], StockWerk.RecordArityTy)))

	datatype expMode =
	    PREPARE
	  | FILL
	  | BOTH

	fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	fun genTestInt (dottedname, ty, i, elseLabel) =
	    (emit Dup; emit (Isinst dottedname); emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass dottedname);
	     emit (Ldfld (dottedname, "Value", ty)); emit (LdcI4 i);
	     emit (B (NE_UN, elseLabel)); emit Pop)

	fun genTest (LitTest (WordLit w), elseLabel) =
	    genTestInt (StockWerk.Word, Int32Ty, LargeWord.toInt w, elseLabel)
	  | genTest (LitTest (IntLit i), elseLabel) =
	    genTestInt (StockWerk.Int, Int32Ty, Int.fromLarge i, elseLabel)
	  | genTest (LitTest (CharLit c), elseLabel) =
	    genTestInt (StockWerk.Char, CharTy, Char.ord c, elseLabel)
	  | genTest (LitTest (StringLit s), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.String);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.String);
	     emit (Ldfld (StockWerk.String, "Value", System.StringTy));
	     emit (Ldstr s);
	     emit (Call (false, System.String, "Equals",
			 [System.StringTy, System.StringTy], BoolTy));
	     emit (B (FALSE, elseLabel)); emit Pop)
	  | genTest (LitTest (RealLit s), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Real);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.Real);
	     emit (Ldfld (StockWerk.Real, "Value", Float32Ty));
	     emit (LdcR4 s); emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (ConTest (id, NONE, _), elseLabel) =
	    (emit Dup; emitId id; emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (ConTest (id1, SOME id2, _), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.ConVal);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.ConVal); emit Dup;
	     emit (Ldfld (StockWerk.ConVal, "Con", StockWerk.StockWertTy));
	     emitId id1; emit (B (NE_UN, elseLabel));
	     emit (Ldfld (StockWerk.ConVal, "Val", StockWerk.StockWertTy));
	     declareLocal id2)
	  | genTest (RefTest id, elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Ref);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Ref);
	     emit (Call (true, StockWerk.Ref, "Access", nil,
			 StockWerk.StockWertTy));
	     declareLocal id)
	  | genTest (TupTest nil, elseLabel) =
	    (emit Dup;
	     emit (Ldsfld (StockWerk.Prebound, "unit", StockWerk.StockWertTy));
	     emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (TupTest [id1, id2], elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple2);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Tuple2); emit Dup;
	     emit (Ldfld (StockWerk.Tuple2, "Value1", StockWerk.StockWertTy));
	     declareLocal id1;
	     emit (Ldfld (StockWerk.Tuple2, "Value2", StockWerk.StockWertTy));
	     declareLocal id2)
	  | genTest (TupTest [id1, id2, id3], elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple3);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Tuple3); emit Dup;
	     emit (Ldfld (StockWerk.Tuple3, "Value1", StockWerk.StockWertTy));
	     declareLocal id1; emit Dup;
	     emit (Ldfld (StockWerk.Tuple3, "Value2", StockWerk.StockWertTy));
	     declareLocal id2;
	     emit (Ldfld (StockWerk.Tuple3, "Value3", StockWerk.StockWertTy));
	     declareLocal id3)
	  | genTest (TupTest [id1, id2, id3, id4], elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple4);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Tuple4); emit Dup;
	     emit (Ldfld (StockWerk.Tuple4, "Value1", StockWerk.StockWertTy));
	     declareLocal id1; emit Dup;
	     emit (Ldfld (StockWerk.Tuple4, "Value2", StockWerk.StockWertTy));
	     declareLocal id2; emit Dup;
	     emit (Ldfld (StockWerk.Tuple4, "Value3", StockWerk.StockWertTy));
	     declareLocal id3;
	     emit (Ldfld (StockWerk.Tuple4, "Value4", StockWerk.StockWertTy));
	     declareLocal id4)
	  | genTest (TupTest ids, elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Tuple);
	     emit (Ldfld (StockWerk.Tuple, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     Misc.List_appi (fn (i, id) =>
			     (emit Dup; emit (LdcI4 i); emit LdelemRef;
			      declareLocal id)) ids;
	     emit Pop)
	  | genTest (RecTest labelIdList, elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Record);
	     emit (B (FALSE, elseLabel));
	     emit (Castclass StockWerk.Record);
	     emit (Ldfld (StockWerk.Record, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     Misc.List_appi (fn (i, (_, id)) =>
			     (emit Dup; emit (LdcI4 i); emit LdelemRef;
			      declareLocal id)) labelIdList;
	     emit Pop)
	  | genTest (LabTest (label, id), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Record);
	     emit (B (FALSE, elseLabel));
	     case Label.toInt label of
		 SOME i =>
		     (emit (LdcI4 i);
		      emit (Call (true, StockWerk.StockWert, "CondSelect",
				  [Int32Ty], StockWerk.StockWertTy)))
	       | NONE =>
		     (emit (Ldstr (Label.toString label));
		      emit (Call (true, StockWerk.StockWert, "CondSelect",
				  [System.StringTy], StockWerk.StockWertTy)));
	     declareLocal id)
	  | genTest (VecTest ids, elseLabel) =
	    let
		val thenLabel = newLabel ()
	    in
		emit Dup; emit (Isinst StockWerk.Vector);
		emit (B (FALSE, elseLabel));
		emit (Castclass StockWerk.Vector); emit Dup;
		emit (Ldfld (StockWerk.Vector, "Values",
			     ArrayTy StockWerk.StockWertTy));
		emit Dup; emit Ldlen; emit (LdcI4 (List.length ids));
		emit (B (EQ, thenLabel)); emit Pop; emit (Br elseLabel);
		emit (Label thenLabel);
		Misc.List_appi (fn (i, id) =>
				(emit Dup; emit (LdcI4 i); emit LdelemRef;
				 declareLocal id)) ids;
		emit Pop
	    end

	fun genLit (WordLit w) =
	    (emit (LdcI4 (LargeWord.toInt w));
	     emit (Newobj (StockWerk.Word, [Int32Ty])))
	  | genLit (IntLit i) =
	    (emit (LdcI4 (Int.fromLarge i));
	     emit (Newobj (StockWerk.Int, [Int32Ty])))
	  | genLit (CharLit c) =
	    (emit (LdcI4 (Char.ord c));
	     emit (Newobj (StockWerk.Char, [CharTy])))
	  | genLit (StringLit s) =
	    (emit (Ldstr s);
	     emit (Newobj (StockWerk.String, [System.StringTy])))
	  | genLit (RealLit s) =
	    (emit (LdcR4 s);
	     emit (Newobj (StockWerk.Real, [Float32Ty])))

	(*--** in EvalStm and declarations of unused variables,
	 * remove all of exp but side-effects *)

	fun genStm (ValDec (_, id, exp, _)) =
	    (genExp (exp, BOTH); declareLocal id)
	  | genStm (RecDec (_, idExpList, _)) =
	    (List.app (fn (id, exp) =>
		       (genExp (exp, PREPARE); declareLocal id)) idExpList;
	     List.app (fn (id, exp) =>
		       (emitId id; genExp (exp, FILL))) idExpList)
	  | genStm (EvalStm (_, exp)) = (genExp (exp, BOTH); emit Pop)
	  | genStm (HandleStm (_, tryBody, id, catchBody, contBody, shared)) =
	    let
		val label1 = newLabel ()
		val label2 = newLabel ()
		val label3 = newLabel ()
	    in
		shared := label3;
		emit (Try (label1, label2, StockWerk.ExceptionWrapper,
			   label2, label3));
		emit (Label label1); genBody tryBody;
		emit (Label label2);
		emit (Ldfld (StockWerk.ExceptionWrapper, "Value",
			     StockWerk.StockWertTy));
		declareLocal id; genBody catchBody;
		emit (Label label3); genBody contBody
	    end
	  | genStm (EndHandleStm (_, shared)) = emit (Leave (!shared))
	  | genStm (stm as TestStm (_, id, _, _, _)) =
	    let
		val (testBodyList, elseBody) = gatherTests ([stm], id)
	    in
		genTestStm (id, testBodyList, fn () => genBody elseBody)
	    end
	  | genStm (RaiseStm ((((i, _), (_, _)), _), id)) =
	    (emitId id; emit (LdcI4 i);
	     emit (Newobj (StockWerk.ExceptionWrapper,
			   [StockWerk.StockWertTy, Int32Ty])); emit Throw)
	  | genStm (ReraiseStm (_, _)) = emit Rethrow
	  | genStm (SharedStm (_, body, shared as ref 0)) =
	    let
		val label = newLabel ()
	    in
		emit (Label label); shared := label; genBody body
	    end
	  | genStm (SharedStm (_, _, ref i)) = emit (Br i)
	  | genStm (ReturnStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	  | genStm (IndirectStm (_, ref bodyOpt)) = genBody (valOf bodyOpt)
	  | genStm (ExportStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	and gatherTests ([TestStm (_, id, test, thenBody, elseBody)], id') =
	    if idEq (id, id') then
		let
		    val (testBodyList, elseBody') = gatherTests (elseBody, id')
		in
		    ((test, thenBody)::testBodyList, elseBody')
		end
	    else (nil, elseBody)
	  | gatherTests (body, _) = (nil, body)
	and genTestStm (_, nil, elseBodyFun) = elseBodyFun ()
	  | genTestStm (id, testBodyList, elseBodyFun) =
	    let
		val retryLabel = newLabel ()
		val falseLabel = newLabel ()
	    in
		emitId id; emit (Label retryLabel);
		List.app (fn (test, body) =>
			  let
			      val elseLabel = newLabel ()
			      val regState = saveRegState ()
			  in
			      genTest (test, elseLabel); genBody body;
			      emit (Label elseLabel); restoreRegState regState
			  end) testBodyList;
		emit Dup; emit (Isinst StockWerk.Transient);
		emit (B (FALSE, falseLabel));
		emit (Callvirt (StockWerk.StockWert, "Await", nil,
				StockWerk.StockWertTy));
		emit (Br retryLabel); emit (Label falseLabel);
		emit Pop; elseBodyFun ()
	    end
	and genExp (LitExp (_, lit), PREPARE) = genLit lit
	  | genExp (PrimExp (_, name), PREPARE) =
	    let
		val (id, ty) = Builtins.lookup name
	    in
		emit (Ldsfld (StockWerk.Prebound, id, ty))
	    end
	  | genExp (NewExp (_, NONE, _), PREPARE) =
	    emit (Newobj (StockWerk.GenerativeCon, nil))
	  | genExp (NewExp (_, SOME s, _), PREPARE) =
	    (emit (Ldstr s);
	     emit (Call (false, StockWerk.NongenerativeCon, "Make",
			 [System.StringTy],
			 StockWerk.NongenerativeConTy)))
	  | genExp (VarExp (_, id), PREPARE) = emitId id
	  | genExp (ConExp (_, id as Id (_, stamp, _), _), PREPARE) = emitId id
	  | genExp (RefExp _, PREPARE) =
	    emit (Ldsfld (StockWerk.Prebound, "ref", StockWerk.StockWertTy))
	  | genExp (TupExp (_, nil), PREPARE) =
	    emit (Ldsfld (StockWerk.Prebound, "unit", StockWerk.StockWertTy))
	  | genExp (TupExp (_, nil), FILL) = ()
	  | genExp (TupExp (_, nil), BOTH) =
	    emit (Ldsfld (StockWerk.Prebound, "unit", StockWerk.StockWertTy))
	  | genExp (TupExp (_, [id1, id2]), PREPARE) =
	    emit (Newobj (StockWerk.Tuple2, nil))
	  | genExp (TupExp (_, [id1, id2]), FILL) =
	    (emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple2, "Value1", StockWerk.StockWertTy));
	     emitId id2;
	     emit (Stfld (StockWerk.Tuple2, "Value2", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, [id1, id2]), BOTH) =
	    (emit (Newobj (StockWerk.Tuple2, nil));
	     emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple2, "Value1", StockWerk.StockWertTy));
	     emit Dup; emitId id2;
	     emit (Stfld (StockWerk.Tuple2, "Value2", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, [id1, id2, id3]), PREPARE) =
	    emit (Newobj (StockWerk.Tuple3, nil))
	  | genExp (TupExp (_, [id1, id2, id3]), FILL) =
	    (emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple3, "Value1", StockWerk.StockWertTy));
	     emit Dup; emitId id2;
	     emit (Stfld (StockWerk.Tuple3, "Value2", StockWerk.StockWertTy));
	     emitId id3;
	     emit (Stfld (StockWerk.Tuple3, "Value3", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, [id1, id2, id3]), BOTH) =
	    (emit (Newobj (StockWerk.Tuple3, nil));
	     emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple3, "Value1", StockWerk.StockWertTy));
	     emit Dup; emitId id2;
	     emit (Stfld (StockWerk.Tuple3, "Value2", StockWerk.StockWertTy));
	     emit Dup; emitId id3;
	     emit (Stfld (StockWerk.Tuple3, "Value3", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, [id1, id2, id3, id4]), PREPARE) =
	    emit (Newobj (StockWerk.Tuple4, nil))
	  | genExp (TupExp (_, [id1, id2, id3, id4]), FILL) =
	    (emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple4, "Value1", StockWerk.StockWertTy));
	     emit Dup; emitId id2;
	     emit (Stfld (StockWerk.Tuple4, "Value2", StockWerk.StockWertTy));
	     emit Dup; emitId id3;
	     emit (Stfld (StockWerk.Tuple4, "Value3", StockWerk.StockWertTy));
	     emitId id4;
	     emit (Stfld (StockWerk.Tuple4, "Value4", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, [id1, id2, id3, id4]), BOTH) =
	    (emit (Newobj (StockWerk.Tuple4, nil));
	     emit Dup; emitId id1;
	     emit (Stfld (StockWerk.Tuple4, "Value1", StockWerk.StockWertTy));
	     emit Dup; emitId id2;
	     emit (Stfld (StockWerk.Tuple4, "Value2", StockWerk.StockWertTy));
	     emit Dup; emitId id3;
	     emit (Stfld (StockWerk.Tuple4, "Value3", StockWerk.StockWertTy));
	     emit Dup; emitId id4;
	     emit (Stfld (StockWerk.Tuple4, "Value4", StockWerk.StockWertTy)))
	  | genExp (TupExp (_, ids), PREPARE) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     emit (Newobj (StockWerk.Tuple, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (TupExp (_, ids), FILL) =
	    (emit (Ldfld (StockWerk.Tuple, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     Misc.List_appi (fn (i, id) =>
			     (emit Dup; emit (LdcI4 i); emitId id;
			      emit StelemRef)) ids;
	     emit Pop)
	  | genExp (TupExp (_, ids), BOTH) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     Misc.List_appi (fn (i, id) =>
			     (emit Dup; emit (LdcI4 i); emitId id;
			      emit StelemRef)) ids;
	     emit (Newobj (StockWerk.Tuple, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (RecExp (_, labelIdList), PREPARE) =
	    (emitRecordArity labelIdList;
	     emit (LdcI4 (List.length labelIdList));
	     emit (Newarr StockWerk.StockWertTy);
	     emit (Newobj (StockWerk.Record, [StockWerk.RecordArityTy,
					      ArrayTy StockWerk.StockWertTy])))
	  | genExp (RecExp (_, labelIdList), FILL) =
	    (emit (Ldfld (StockWerk.Record, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     Misc.List_appi (fn (i, (_, id)) =>
			     (emit Dup; emit (LdcI4 i); emitId id;
			      emit StelemRef)) labelIdList;
	     emit Pop)
	  | genExp (RecExp (_, labelIdList), BOTH) =
	    (emitRecordArity labelIdList;
	     emit (LdcI4 (List.length labelIdList));
	     emit (Newarr StockWerk.StockWertTy);
	     Misc.List_appi (fn (i, (_, id)) =>
			     (emit Dup; emit (LdcI4 i); emitId id;
			      emit StelemRef)) labelIdList;
	     emit (Newobj (StockWerk.Record, [StockWerk.RecordArityTy,
					      ArrayTy StockWerk.StockWertTy])))
	  | genExp (SelExp (_, label), BOTH) =
	    (case Label.toInt label of
		 SOME i =>
		     (emit (LdcI4 i);
		      emit (Newobj (StockWerk.IntSelector, [Int32Ty])))
	       | NONE =>
		     (emit (Ldstr (Label.toString label));
		      emit (Newobj (StockWerk.StringSelector,
				    [System.StringTy]))))
	  | genExp (VecExp (_, ids), PREPARE) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     emit (Newobj (StockWerk.Vector, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (VecExp (_, ids), FILL) =
	    (emit (Ldfld (StockWerk.Vector, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     Misc.List_appi (fn (i, id) =>
			     (emit Dup; emit (LdcI4 i); emitId id;
			      emit StelemRef)) ids;
	     emit Pop)
	  | genExp (FunExp ((coord, _), stamp, _, args, body), PREPARE) =
	    (emitCoord ("FunExp", coord); emit (Newobj (className stamp, nil));
	     case args of
		 TupArgs nil =>
		     defineClass (stamp, StockWerk.Procedure0, nil)
	       | TupArgs [_, _] =>
		     defineClass (stamp, StockWerk.Procedure2, nil)
	       | TupArgs [_, _, _] =>
		     defineClass (stamp, StockWerk.Procedure3, nil)
	       | TupArgs [_, _, _, _] =>
		     defineClass (stamp, StockWerk.Procedure4, nil)
	       | _ =>
		     defineClass (stamp, StockWerk.Procedure, nil))
	  | genExp (FunExp (_, stamp, _, args, body), FILL) =
	    (case args of
		 OneArg id =>
		     (defineMethod (stamp, "Apply", [id]);
		      genBody body; closeMethod ())
	       | TupArgs nil =>
		     (defineMethod (stamp, "Apply0", nil);
		      genBody body; closeMethod ())
	       | TupArgs (ids as [_, _]) =>
		     (defineMethod (stamp, "Apply2", ids);
		      genBody body; closeMethod ())
	       | TupArgs (ids as [_, _, _]) =>
		     (defineMethod (stamp, "Apply3", ids);
		      genBody body; closeMethod ())
	       | TupArgs (ids as [_, _, _, _]) =>
		     (defineMethod (stamp, "Apply4", ids);
		      genBody body; closeMethod ())
	       | _ =>
		     let
			 val info = (Source.nowhere, NONE)
			 val id = Id (info, Stamp.new (), Name.InId)
			 val test =
			     case args of
				 OneArg _ =>
				     raise Crash.Crash "CodeGen.genExp: FunExp"
			       | TupArgs ids => TupTest ids
			       | RecArgs labelIdList => RecTest labelIdList
			 fun elseBodyFun () =
			     (emit (Ldsfld (StockWerk.Prebound,
					    "General$Match",
					    StockWerk.StockWertTy));
			      emit (Newobj (StockWerk.ExceptionWrapper,
					    [StockWerk.StockWertTy]));
			      emit Throw)
		     in
			 defineMethod (stamp, "Apply", [id]);
			 genTestStm (id, [(test, body)], elseBodyFun);
			 closeMethod ()
		     end;
	     emit Pop)
	  | genExp (AppExp (_, id1, OneArg id2), BOTH) =
	    (emitId id1; emitId id2;
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (_, id, TupArgs nil), BOTH) =
	    (emitId id;
	     emit (Callvirt (StockWerk.StockWert, "Apply0",
			     nil, StockWerk.StockWertTy)))
	  | genExp (AppExp (_, id, TupArgs [id1, id2]), BOTH) =
	    (emitId id; emitId id1; emitId id2;
	     emit (Callvirt (StockWerk.StockWert, "Apply2",
			     [StockWerk.StockWertTy, StockWerk.StockWertTy],
			     StockWerk.StockWertTy)))
	  | genExp (AppExp (_, id, TupArgs [id1, id2, id3]), BOTH) =
	    (emitId id; emitId id1; emitId id2; emitId id3;
	     emit (Callvirt (StockWerk.StockWert, "Apply3",
			     [StockWerk.StockWertTy, StockWerk.StockWertTy,
			      StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (_, id, TupArgs [id1, id2, id3, id4]), BOTH) =
	    (emitId id; emitId id1; emitId id2; emitId id3; emitId id4;
	     emit (Callvirt (StockWerk.StockWert, "Apply4",
			     [StockWerk.StockWertTy, StockWerk.StockWertTy,
			      StockWerk.StockWertTy, StockWerk.StockWertTy],
			     StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, TupArgs ids), BOTH) =
	    (emitId id; genExp (TupExp (coord, ids), BOTH);
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, RecArgs labelIdList), BOTH) =
	    (emitId id; genExp (RecExp (coord, labelIdList), BOTH);
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (SelAppExp (_, label, id), BOTH) =
	    (emitId id;
	     case Label.toInt label of
		 SOME i =>
		     (emit (LdcI4 i);
		      emit (Callvirt (StockWerk.StockWert, "Select", [Int32Ty],
				      StockWerk.StockWertTy)))
	       | NONE =>
		     (emit (Ldstr (Label.toString label));
		      emit (Callvirt (StockWerk.StockWert, "Select",
				      [System.StringTy],
				      StockWerk.StockWertTy))))
	  | genExp (ConAppExp (_, id, _, _), PREPARE) =
	    (emitId id;
	     emit (Newobj (StockWerk.ConVal, [StockWerk.StockWertTy])))
	  | genExp (ConAppExp (_, _, args, _), FILL) =
	    (genArgs args;
	     emit (Stfld (StockWerk.ConVal, "Val", StockWerk.StockWertTy)))
	  | genExp (RefAppExp (_, _), PREPARE) =
	    emit (Newobj (StockWerk.Ref, nil))
	  | genExp (RefAppExp (_, args), FILL) =
	    (genArgs args;
	     emit (Call (true, StockWerk.Ref, "Assign",
			 [StockWerk.StockWertTy], VoidTy)))
	  | genExp (PrimAppExp (_, name, ids), BOTH) =
	    raise Crash.Crash "CodeGenPhase.genExp: PrimAppExp"   (*--** *)
	  | genExp (AdjExp (_, id1, id2), BOTH) =
	    raise Crash.Crash "CodeGenPhase.genExp: AdjExp"   (*--** *)
	  | genExp (exp, PREPARE) =
	    raise Crash.Crash "CodeGenPhase.genExp: not admissible"
	  | genExp (_, FILL) = emit Pop
	  | genExp (exp, BOTH) =
	    (genExp (exp, PREPARE); emit Dup; genExp (exp, FILL))
	and genArgs (OneArg id) = emitId id
	  | genArgs (TupArgs ids) =   (*--** type below *)
	    genExp (TupExp ((Source.nowhere, NONE), ids), BOTH)
	  | genArgs (RecArgs idExpList) =   (*--** type below *)
	    genExp (RecExp ((Source.nowhere, NONE), idExpList), BOTH)
	and genBody (stm::stms) =
	    (case infoStm stm of
		 (_, ref (Kill set)) => kill set
	       | (_, ref _) => ();
	     genStm stm; genBody stms)
	  | genBody nil = ()

	fun genComponent (component as (imports, (body, _))) =
	    (LivenessAnalysisPhase.annotate component;
	     init ["Test"];
	     List.app (fn (id, _, url) =>
		       (emit (Ldarg 0);
			emit (Ldstr (Url.toString url));
			emit (Call (true, StockWerk.Komponist, "Import",
				    [System.StringTy], StockWerk.StockWertTy));
			declareLocal id)) imports;
	     genBody body; close())
    end
