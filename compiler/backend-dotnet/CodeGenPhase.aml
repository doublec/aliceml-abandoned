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
 * gespeichert (bei jeder Closure in einem statischen Feld):
 * -- Verzeichnis aller TypeToken
 * -- Verzeichnis aller FieldToken
 * -- Verzeichnis aller MethodToken
 * -- IL-Code (wobei Tokens als Indizes in obige Verzeichnisse
 *    repraesentiert werden)
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
 * Entsprechend dominierte Hilfsprozeduren sollen mit dem flag AuxiliaryOF
 * annotiert werden.
 *)

structure CodeGenPhase :> CODE_GEN_PHASE =
    struct
	structure I = ImperativeGrammar
	structure O = IL

	open I
	open O
	open CodeStore

	structure System =
	    struct
		val String = ["System", "String"]
		val StringTy = ClassTy String
	    end

	local
	    fun appi' (x::xr, f, i) = (f (i, x); appi' (xr, f, i + 1))
	      | appi' (nil, _, _) = ()
	in
	    fun appi f xs = appi' (xs, f, 0)
	end

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

	fun emitCoord (l, r) =   (*--** for debugging *)
	    emit (Comment (Int.toString l ^ "-" ^ Int.toString r))

	fun emitRecordArity labs =
	    (emit (LdcI4 (List.length labs)); emit (Newarr System.StringTy);
	     appi (fn (i, lab) =>
		   (emit Dup; emit (LdcI4 i); emit (Ldstr lab);
		    emit (StelemRef))) labs;
	     emit (Call (false, StockWerk.RecordArity, "MakeRecordArity",
			 [ArrayTy System.StringTy], StockWerk.RecordArityTy)))

	datatype expMode =
	    PREPARE
	  | FILL
	  | BOTH

	fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	fun genTestIntLit (dottedname, i, elseLabel) =
	    (emit Dup; emit (Isinst dottedname); emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass dottedname);
	     emit (Ldfld (dottedname, "Value", Int32Ty)); emit (LdcI4 i);
	     emit (B (NE_UN, elseLabel)); emit Pop)

	fun genTest (LitTest (WordLit w), elseLabel) =
	    genTestIntLit (StockWerk.Word, LargeWord.toInt w, elseLabel)
	  | genTest (LitTest (IntLit i), elseLabel) =
	    genTestIntLit (StockWerk.Int, LargeInt.toInt i, elseLabel)
	  | genTest (LitTest (CharLit c), elseLabel) =
	    genTestIntLit (StockWerk.Char, Char.ord c, elseLabel)
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
	  | genTest (ConTest (id, NONE), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Name);
	     emit (B (FALSE, elseLabel)); emit Dup;
	     emitId id; emit (B (NE_UN, elseLabel)); emit Pop)
	  | genTest (ConTest (id1, SOME id2), elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.ConVal);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.ConVal); emit Dup;
	     emit (Ldfld (StockWerk.ConVal, "Con", StockWerk.ConstructorTy));
	     emitId id1; emit (B (NE_UN, elseLabel));
	     emit (Ldfld (StockWerk.ConVal, "Val", StockWerk.StockWertTy));
	     declareLocal id2)
	  | genTest (RefTest id, elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Ref);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.Ref);
	     emit (Call (true, StockWerk.Ref, "Access", nil,
			 StockWerk.StockWertTy));
	     declareLocal id)
	  | genTest (TupTest nil, elseLabel) =
	    (emit Dup;
	     emit (Ldsfld (StockWerk.Prebound, "unit", StockWerk.StockWertTy));
	     emit (B (NE_UN, elseLabel)))
	  | genTest (TupTest [id1, id2], elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple2);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.Tuple2); emit Dup;
	     emit (Ldfld (StockWerk.Tuple2, "Value1", StockWerk.StockWertTy));
	     declareLocal id1;
	     emit (Ldfld (StockWerk.Tuple2, "Value2", StockWerk.StockWertTy));
	     declareLocal id2)
	  | genTest (TupTest [id1, id2, id3], elseLabel) =
	    (emit Dup; emit (Isinst StockWerk.Tuple3);
	     emit (B (FALSE, elseLabel));
	     emit Dup; emit (Castclass StockWerk.Tuple3); emit Dup;
	     emit (Ldfld (StockWerk.Tuple3, "Value1", StockWerk.StockWertTy));
	     declareLocal id1; emit Dup;
	     emit (Ldfld (StockWerk.Tuple3, "Value2", StockWerk.StockWertTy));
	     declareLocal id2;
	     emit (Ldfld (StockWerk.Tuple3, "Value3", StockWerk.StockWertTy));
	     declareLocal id3)
	  | genTest (TupTest ids, elseLabel) =
	    let
		val thenLabel = newLabel ()
	    in
		emit Dup; emit (Isinst StockWerk.Tuple);
		emit (B (FALSE, elseLabel));
		emit Dup; emit (Castclass StockWerk.Tuple); emit Dup;
		emit (Ldfld (StockWerk.Tuple, "Values",
			     ArrayTy StockWerk.StockWertTy));
		emit Dup; emit Ldlen; emit (LdcI4 (List.length ids));
		emit (B (EQ, thenLabel)); emit Pop; emit (Br elseLabel);
		emit (Label thenLabel);
		appi (fn (i, id) =>
		      (emit Dup; emit (LdcI4 i); emit LdelemRef;
		       declareLocal id)) ids; emit Pop; emit Pop
	    end
	  | genTest (RecTest labIdList, elseLabel) =
	    let
		val thenLabel = newLabel ()
	    in
		emit Dup; emit (Isinst StockWerk.Record);
		emit (B (FALSE, elseLabel));
		emit Dup; emit (Castclass StockWerk.Record); emit Dup;
		emit (Ldfld (StockWerk.Record, "RecordArity",
			     StockWerk.RecordArityTy));
		emitRecordArity (List.map #1 labIdList);
		emit (B (EQ, thenLabel)); emit Pop; emit (Br elseLabel);
		emit (Label thenLabel);
		emit (Ldfld (StockWerk.Record, "Values",
			     ArrayTy StockWerk.StockWertTy));
		appi (fn (i, (_, id)) =>
		      (emit Dup; emit (LdcI4 i); emit LdelemRef;
		       declareLocal id)) labIdList; emit Pop
	    end
	  | genTest (LabTest (lab, id), elseLabel) =
	    (*--** *)
	    Crash.crash "CodeGenPhase.genTest: LabTest"
	  | genTest (VecTest ids, elseLabel) =
	    let
		val thenLabel = newLabel ()
	    in
		emit Dup; emit (Isinst StockWerk.Vector);
		emit (B (FALSE, elseLabel));
		emit Dup; emit (Castclass StockWerk.Vector); emit Dup;
		emit (Ldfld (StockWerk.Vector, "Values",
			     ArrayTy StockWerk.StockWertTy));
		emit Dup; emit Ldlen; emit (LdcI4 (List.length ids));
		emit (B (EQ, thenLabel)); emit Pop; emit (Br elseLabel);
		emit (Label thenLabel);
		appi (fn (i, id) =>
		      (emit Dup; emit (LdcI4 i); emit LdelemRef;
		       declareLocal id)) ids; emit Pop
	    end

	fun genLit (WordLit w) =
	    (emit (LdcI4 (LargeWord.toInt w));
	     emit (Newobj (StockWerk.Word, [Int32Ty])))
	  | genLit (IntLit i) =
	    (emit (LdcI4 (LargeInt.toInt i));
	     emit (Newobj (StockWerk.Int, [Int32Ty])))
	  | genLit (CharLit c) =
	    (emit (LdcI4 (Char.ord c));
	     emit (Newobj (StockWerk.Char, [Int32Ty])))
	  | genLit (StringLit s) =
	    (emit (Ldstr s);
	     emit (Newobj (StockWerk.String, [System.StringTy])))
	  | genLit (RealLit s) =
	    (emit (LdcR4 s);
	     emit (Newobj (StockWerk.Real, [Float32Ty])))

	fun genStm (ValDec (_, id, exp, _)) =
	    (genExp (exp, BOTH); declareLocal id)
	  | genStm (RecDec (_, idExpList, _)) =
	    (List.app (fn (id, exp) =>
		       (genExp (exp, PREPARE); declareLocal id)) idExpList;
	     List.app (fn (id, exp) =>
		       (emitId id; genExp (exp, FILL))) idExpList)
	  | genStm (ConDec (_, id as Id (_, _, ExId s), false, _)) =
	    (emit (Ldstr s);   (*--** only on toplevel *)
	     emit (Newobj (StockWerk.NamedName, [System.StringTy]));
	     declareLocal id)
	  | genStm (ConDec (_, id as Id (_, _, ExId s), true, _)) =
	    (emit (Ldstr s);   (*--** only on toplevel *)
	     emit (Newobj (StockWerk.NamedConstructor, [System.StringTy]));
	     declareLocal id)
	  | genStm (ConDec (_, id, false, _)) =
	    (emit (Newobj (StockWerk.Name, nil)); declareLocal id)
	  | genStm (ConDec (_, id, true, _)) =
	    (emit (Newobj (StockWerk.Constructor, nil)); declareLocal id)
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
		val (testBodyFunList, elseBody) = gatherTests ([stm], id)
	    in
		genTestStm (id, testBodyFunList, elseBody)
	    end
	  | genStm (RaiseStm ((i, _), id)) =
	    (emitId id; emit (LdcI4 i);
	     emit (Newobj (StockWerk.ExceptionWrapper,
			   [StockWerk.StockWertTy, Int32Ty])); emit Throw)
	  | genStm (SharedStm (_, body, shared as ref 0)) =
	    let
		val label = newLabel ()
	    in
		emit (Label label);
		shared := label;
		genBody body
	    end
	  | genStm (SharedStm (_, _, ref i)) = emit (Br i)
	  | genStm (ReturnStm (_, exp)) = (genExp (exp, BOTH); emit Ret)
	  | genStm (IndirectStm (_, ref bodyOpt)) = genBody (valOf bodyOpt)
	  | genStm (ExportStm (_, _)) = ()
	and gatherTests ([TestStm (_, id, test, thenBody, elseBody)], id') =
	    if idEq (id, id') then
		let
		    val (testBodyFunList, elseBody') =
			gatherTests (elseBody, id')
		    fun thenBodyFun () = genBody thenBody
		in
		    ((test, thenBodyFun)::testBodyFunList, elseBody')
		end
	    else (nil, elseBody)
	  | gatherTests (body, _) = (nil, body)
	and genTestStm (_, nil, elseBody) = genBody elseBody
	  | genTestStm (id, testBodyFunList, elseBody) =
	    let
		val retryLabel = newLabel ()
		val falseLabel = newLabel ()
	    in
		emitId id; emit (Label retryLabel);
		List.app (fn (test, bodyFun) =>
			  let
			      val elseLabel = newLabel ()
			  in
			      genTest (test, elseLabel);
			      bodyFun (); emit (Label elseLabel)
			  end) testBodyFunList;
		emit Dup; emit (Isinst StockWerk.Transient);
		emit (B (FALSE, falseLabel));
		emit (Callvirt (StockWerk.StockWert, "Await", nil,
				StockWerk.StockWertTy));
		emit (Br retryLabel); emit (Label falseLabel);
		emit Pop; genBody elseBody
	    end
	and genExp (LitExp (_, lit), PREPARE) = genLit lit
	  | genExp (PrimExp (_, name), PREPARE) =
	    emit (Ldsfld (StockWerk.Prebound, Builtins.lookup name,
			  StockWerk.StockWertTy))
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
	  | genExp (TupExp (_, ids), PREPARE) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     emit (Newobj (StockWerk.Tuple, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (TupExp (_, ids), FILL) =
	    (emit (Ldfld (StockWerk.Tuple, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     appi (fn (i, id) =>
		   (emit Dup; emit (LdcI4 i); emitId id; emit StelemRef)) ids;
	     emit Pop)
	  | genExp (TupExp (_, ids), BOTH) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     appi (fn (i, id) =>
		   (emit Dup; emit (LdcI4 i); emitId id; emit StelemRef)) ids;
	     emit (Newobj (StockWerk.Tuple, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (RecExp (_, labIdList), _) =
	    (emitRecordArity (List.map #1 labIdList);
	     emit (LdcI4 (List.length labIdList));
	     emit (Newarr StockWerk.StockWertTy);
	     appi (fn (i, (_, id)) =>
		   (emit Dup; emit (LdcI4 i); emitId id; emit StelemRef))
	     labIdList;
	     emit (Newobj (StockWerk.Record, [StockWerk.RecordArityTy,
					      ArrayTy StockWerk.StockWertTy])))
	  | genExp (SelExp (_, s), BOTH) =
	    (emit (Ldstr s);
	     emit (Newobj (StockWerk.Selector, [System.StringTy])))
	  | genExp (VecExp (_, ids), PREPARE) =
	    (emit (LdcI4 (List.length ids));
	     emit (Newarr StockWerk.StockWertTy);
	     emit (Newobj (StockWerk.Vector, [ArrayTy StockWerk.StockWertTy])))
	  | genExp (VecExp (_, ids), FILL) =
	    (emit (Ldfld (StockWerk.Vector, "Values",
			  ArrayTy StockWerk.StockWertTy));
	     appi (fn (i, id) =>
		   (emit Dup; emit (LdcI4 i); emitId id; emit StelemRef)) ids;
	     emit Pop)
	  | genExp (FunExp (_, _, argsBodyList), PREPARE) =
	    (case argsBodyList of
		 (OneArg (id as Id (_, stamp, _)), _)::_ =>
		     (emit (Newobj (className stamp, nil));
		      defineClass (stamp, StockWerk.Procedure, nil))
	       | _ => Crash.crash "CodeGenPhase.genExp")
	  | genExp (FunExp (_, _, argsBodyList), FILL) =
	    (case argsBodyList of
		 (OneArg (id as Id (_, stamp, _)), body)::rest =>
		     (genFunBody (stamp, id, body, rest); emit Pop)
	       | _ => Crash.crash "CodeGenPhase.genExp")
	  | genExp (AppExp (_, id1, OneArg id2), BOTH) =
	    (emitId id1; emitId id2;
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, TupArgs nil), BOTH) =
	    (emitId id;
	     emit (Callvirt (StockWerk.StockWert, "Apply0",
			     nil, StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, TupArgs [id1, id2]), BOTH) =
	    (emitId id; emitId id1; emitId id2;
	     emit (Callvirt (StockWerk.StockWert, "Apply2",
			     [StockWerk.StockWertTy, StockWerk.StockWertTy],
			     StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, TupArgs [id1, id2, id3]), BOTH) =
	    (emitId id; emitId id1; emitId id2; emitId id3;
	     emit (Callvirt (StockWerk.StockWert, "Apply3",
			     [StockWerk.StockWertTy, StockWerk.StockWertTy,
			      StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, TupArgs ids), BOTH) =
	    (emitId id; genExp (TupExp (coord, ids), BOTH);
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (AppExp (coord, id, RecArgs labIdList), BOTH) =
	    (emitId id; genExp (RecExp (coord, labIdList), BOTH);
	     emit (Callvirt (StockWerk.StockWert, "Apply",
			     [StockWerk.StockWertTy], StockWerk.StockWertTy)))
	  | genExp (SelAppExp (_, s, id), BOTH) =
	    (emit (Ldstr s);
	     emit (Callvirt (StockWerk.StockWert, "Select", [System.StringTy],
			     StockWerk.StockWertTy)))
	  | genExp (ConAppExp (_, id, _), PREPARE) =
	    (emitId id; emit (Castclass StockWerk.Constructor);
	     emit (Newobj (StockWerk.ConVal, [StockWerk.ConstructorTy])))
	  | genExp (ConAppExp (_, _, args), FILL) =
	    (genArgs args;
	     emit (Stfld (StockWerk.ConVal, "Val", StockWerk.StockWertTy)))
	  | genExp (RefAppExp (_, _), PREPARE) =
	    emit (Newobj (StockWerk.Ref, nil))
	  | genExp (RefAppExp (_, args), FILL) =
	    (genArgs args;
	     emit (Call (true, StockWerk.Ref, "Assign",
			 [StockWerk.StockWertTy], VoidTy)))
	  | genExp (PrimAppExp (_, name, ids), BOTH) =
	    (*--** *)
	    Crash.crash "CodeGenPhase.genExp: PrimAppExp"
	  | genExp (AdjExp (_, id1, id2), BOTH) =
	    (*--** *)
	    Crash.crash "CodeGenPhase.genExp: AdjExp"
	  | genExp (exp, PREPARE) =
	    Crash.crash "CodeGenPhase.genExp: not admissible"
	  | genExp (_, FILL) = emit Pop
	  | genExp (exp, BOTH) =
	    (genExp (exp, PREPARE); emit Dup; genExp (exp, FILL))
	and genFunBody (stamp, id, body, argsBodyList) =
	    let
		val testBodyFunList =
		    List.map
		    (fn (args, body) =>
		     case args of
			 OneArg _ => Crash.crash "CodeGen.genFunBody"
		       | TupArgs (ids as (nil | [_, _] | [_, _, _])) =>
			 let
			     val name =
				 "Apply" ^ Int.toString (List.length ids)
			 in
			     defineMethod (stamp, name, ids);
			     genBody body; closeMethod ();
			     (TupTest ids,
			      fn () =>
			      (emit Pop; emit (Ldarg 0); List.map emitId ids;
			       emit Tail;
			       emit (Call (true, className stamp, name,
					   List.map (fn _ =>
						     StockWerk.StockWertTy)
					   ids, StockWerk.StockWertTy));
			      emit Ret))
			 end
		       | TupArgs ids =>
			     (TupTest ids, fn () => genBody body)
		       | RecArgs labIdList =>
			     (RecTest labIdList, fn () => genBody body))
		    argsBodyList
	    in
		defineMethod (stamp, "Apply", [id]);
		genTestStm (id, testBodyFunList, body);
		closeMethod ()
	    end
	and genArgs (OneArg id) = emitId id
	  | genArgs (TupArgs ids) = genExp (TupExp (Source.nowhere, ids), BOTH)
	  | genArgs (RecArgs idExpList) =
	    genExp (RecExp (Source.nowhere, idExpList), BOTH)
	and genBody stms = List.app genStm stms

	fun genProgram program = (init ["Test"]; genBody program; close())
    end
