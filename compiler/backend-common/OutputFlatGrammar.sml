(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure OutputFlatGrammar :> OUTPUT_FLAT_GRAMMAR =
    struct
	structure I = FlatGrammar

	open I

	datatype format =
	    SEQ of format list
	  | S of string
	  | I of int
	  | IN
	  | EX
	  | NL
	  | ID of id
	  | CO of string
	  | NULL
	  | SEP of format * format list

	fun format f =
	    let
		val indent = ref 0
		fun format' (SEQ fs) = String.concat (List.map format' fs)
		  | format' (S s) = s
		  | format' (I n) = Int.toString n
		  | format' IN = (indent := !indent + 1; "")
		  | format' EX = (indent := !indent - 1; "")
		  | format' NL =
		    let
			val n = !indent * 2
			val ntabs = n div 8
			val nspaces = n mod 8
		    in
			"\n" ^
			String.concat (List.tabulate (ntabs, fn _ => "\t")) ^
			String.concat (List.tabulate (nspaces, fn _ => " "))
		    end
		  | format' (ID (Id (_, stamp, Name.InId))) =
		    "$" ^ Stamp.toString stamp
		  | format' (ID (Id (_, stamp, Name.ExId s))) =
		    s ^ "$" ^ Stamp.toString stamp
		  | format' (CO s) = "   (* " ^ s ^ " *)"
		  | format' NULL = ""
		  | format' (SEP (f, f1::fr)) =
		    List.foldl (fn (fi, rest) => rest ^ format' f ^ format' fi)
		    (format' f1) fr
		  | format' (SEP (_, nil)) = ""
	    in
		format' f
	    end

	fun visit (stamp, shared) =
	    not (StampSet.member (shared, stamp)) before
	    StampSet.insert (shared, stamp)

	fun insert (x, ys as (y::yr)) =
	    (case Stamp.compare (x, y) of
		 LESS => x::ys
	       | EQUAL => ys
	       | GREATER => y::insert (x, yr))
	  | insert (x, nil) = [x]

	val sort = StampSet.fold insert nil

	fun outputLiveness (ref (Unknown | LoopStart | LoopEnd)) = NULL
	  | outputLiveness (ref (Use set)) =
	    SEQ [S (List.foldl (fn (stamp, s) =>
				s ^ " " ^ Stamp.toString stamp)
		    "(* use" (sort set)), S " *)", NL]
	  | outputLiveness (ref (Kill set)) =
	    SEQ [S (List.foldl (fn (stamp, s) =>
				s ^ " " ^ Stamp.toString stamp)
		    "(* kill" (sort set)), S " *)", NL]

	fun outputInfo ({liveness, ...}: stm_info) = outputLiveness liveness

	fun outputLit (WordLit w) = SEQ [S "0w", S (LargeWord.toString w)]
	  | outputLit (IntLit i) = S (LargeInt.toString i)
	  | outputLit (CharLit c) =
	    SEQ [S "#\"", S (WideChar.toString c), S "\""]
	  | outputLit (StringLit s) =
	    SEQ [S "\"", S (String.toString s), S "\""]
	  | outputLit (RealLit r) = S r

	fun outputTag NONE = S "tag0"
	  | outputTag (SOME Unary) = S "tag1"
	  | outputTag (SOME (TupArity _) | SOME (RowArity _)) = S "tag+"

	fun outputCon NONE = S "con0"
	  | outputCon (SOME Unary) = S "con1"
	  | outputCon (SOME (TupArity _) | SOME (RowArity _)) = S "con+"

	fun outputArgs (OneArg id) = ID id
	  | outputArgs (TupArgs ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputArgs (RowArgs labelIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (label, id) =>
				       SEQ [S (Label.toString label), S "=",
					    ID id]) labelIdList),
		 S "}"]

	fun outputTest (LitTest lit) = outputLit lit
	  | outputTest (TagTest (label, n)) =
	    SEQ [S "tag ", S (Label.toString label), S "/", I n]
	  | outputTest (TagAppTest (label, n, args)) =
	    SEQ [S "(tag ", S (Label.toString label), S "/", I n, S ") ",
		 outputArgs args]
	  | outputTest (ConTest id) =
	    SEQ [S "con ", ID id]
	  | outputTest (ConAppTest (id, args)) =
	    SEQ [S "(con ", ID id, S ") ", outputArgs args]
	  | outputTest (StaticConTest stamp) =
	    SEQ [S "con ", S (Stamp.toString stamp)]
	  | outputTest (StaticConAppTest (stamp, args)) =
	    SEQ [S "(con ", S (Stamp.toString stamp), S ") ", outputArgs args]
	  | outputTest (VecTest ids) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]

	fun outputStm (ValDec (_, id, exp), _) =
	    SEQ [S "val ", ID id, S " = ", IN, outputExp exp, EX]
	  | outputStm (RecDec (_, idExpList), _) =
	    SEQ [S "rec", IN,
		 SEQ (List.map (fn (id, exp) =>
				SEQ [NL, S "val ", ID id, S " = ",
				     IN, outputExp exp, EX]) idExpList), EX]
	  | outputStm (RefAppDec (_, id, id'), _) =
	    SEQ [S "val ref ", ID id, S " = ", ID id']
	  | outputStm (TupDec (_, ids, id), _) =
	    SEQ [S "val (", SEP (S ", ", List.map ID ids), S ") = ", ID id]
	  | outputStm (RowDec (_, labelIdList, id), _) =
	    SEQ [S "val {",
		 SEP (S ", ", List.map (fn (label, id) =>
					SEQ [S (Label.toString label), S "=",
					     ID id]) labelIdList),
		 S "} = ", ID id]
	  | outputStm (EvalStm (_, exp), _) =
	    SEQ [S "eval ", IN, outputExp exp, EX]
	  | outputStm (HandleStm (_, body1, id, body2, body3, stamp), shared) =
	    SEQ [S "try ", S (Stamp.toString stamp), IN, NL,
		 outputBody (body1, shared), EX, NL,
		 S "catch ", ID id, IN, NL, outputBody (body2, shared), EX, NL,
		 S "cont", IN, NL, outputBody (body3, shared), EX]
	  | outputStm (EndHandleStm (_, stamp), _) =
	    SEQ [S "leave ", S (Stamp.toString stamp)]
	  | outputStm (TestStm (_, id, testBodyList, body), shared) =
	    SEQ [S "case ", ID id, S " of", IN, NL,
		 SEQ (List.map (fn (test, body) =>
				SEQ [outputTest test, S " =>", IN, NL,
				     outputBody (body, shared), EX, NL])
		      testBodyList),
		 S "else", IN, NL, outputBody (body, shared), EX, EX]
	  | outputStm (RaiseStm (_, id), _) = SEQ [S "raise ", ID id]
	  | outputStm (ReraiseStm (_, id), _) = SEQ [S "reraise ", ID id]
	  | outputStm (SharedStm (_, body, stamp), shared) =
	    if visit (stamp, shared) then
		SEQ [S "label ", S (Stamp.toString stamp), S ":", NL,
		     outputBody (body, shared)]
	    else
		SEQ [S "goto ", S (Stamp.toString stamp)]
	  | outputStm (ReturnStm (_, exp), _) =
	    SEQ [S "return ", IN, outputExp exp, EX]
	  | outputStm (IndirectStm (_, ref bodyOpt), shared) =
	    SEQ [S "indirect", NL, outputBody (valOf bodyOpt, shared)]
	  | outputStm (ExportStm (_, exp), _) =
	    SEQ [S "export ", IN, outputExp exp, EX]
	and outputExp (LitExp (_, lit)) = outputLit lit
	  | outputExp (PrimExp (_, name)) = SEQ [S "prim \"", S name, S "\""]
	  | outputExp (NewExp (_, conArity)) = outputCon conArity
	  | outputExp (VarExp (_, id)) = ID id
	  | outputExp (TagExp (_, label, n, conArity)) =
	    SEQ [outputTag conArity, S " ", S (Label.toString label),
		 S "/", I n]
	  | outputExp (ConExp (_, id, conArity)) =
	    SEQ [outputCon conArity, S " ", ID id]
	  | outputExp (StaticConExp (_, stamp, conArity)) =
	    SEQ [outputCon conArity, S " ", S (Stamp.toString stamp)]
	  | outputExp (RefExp _) = SEQ [S "ref"]
	  | outputExp (TupExp (_, ids)) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputExp (RowExp (_, labelIdList)) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (label, id) =>
				       SEQ [S (Label.toString label), S "=",
					    ID id]) labelIdList),
		 S "}"]
	  | outputExp (SelExp (_, label, n)) =
	    SEQ [S "#", S (Label.toString label), S "/", I n]
	  | outputExp (VecExp (_, ids)) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]
	  | outputExp (FunExp (_, _, _, args, body)) =
	    SEQ [NL, S "fn ", outputArgs args, S " =>",
		 IN, NL, outputBody (body, StampSet.new ()), EX]
	  | outputExp (PrimAppExp (_, name, ids)) =
	    SEQ [S "prim \"", S name, S "\" ", SEP (S ", ", List.map ID ids)]
	  | outputExp (VarAppExp (_, id, args)) =
	    SEQ [ID id, S " ", outputArgs args]
	  | outputExp (TagAppExp (_, label, n, args)) =
	    SEQ [S "(tag ", S (Label.toString label), S "/", I n, S ") ",
		 outputArgs args]
	  | outputExp (ConAppExp (_, id, args)) =
	    SEQ [S "(con ", ID id, S ") ", outputArgs args]
	  | outputExp (StaticConAppExp (_, stamp, args)) =
	    SEQ [S "(con ", S (Stamp.toString stamp), S ") ", outputArgs args]
	  | outputExp (RefAppExp (_, id)) =
	    SEQ [S "ref ", ID id]
	  | outputExp (SelAppExp (_, label, n, id)) =
	    SEQ [S "#", S (Label.toString label), S "/", I n, S " ", ID id]
	  | outputExp (FunAppExp (_, id, _, args)) =
	    SEQ [ID id, S " ", outputArgs args]
	and outputBody (stms, shared) =
	    SEP (NL, List.map (fn stm =>
			       SEQ [outputInfo (infoStm stm),
				    outputStm (stm, shared)]) stms)

	fun outputComponent (importList, (body, _)) =
	    format (SEQ [SEQ (List.map
			      (fn (id, _, url) =>
			       SEQ [S "import ", ID id,
				    S " from ", S (Url.toString url), S "\n"])
			      importList), outputBody (body, StampSet.new ()),
			 NL])
    end
