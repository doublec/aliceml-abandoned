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
		  | format' IN = (indent := !indent + 1; "")
		  | format' EX = (indent := !indent - 1; "")
		  | format' NL =
		    "\n" ^ String.concat (List.tabulate
					  (!indent, fn _ => "  "))
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

	fun insert (x, ys as (y::yr)): int list =
	    if x < y then x::ys else y::insert (x, yr)
	  | insert (x, nil) = [x]

	val sort = StampSet.fold insert nil

	fun outputLiveness (ref (Unknown | LoopStart | LoopEnd)) = NULL
	  | outputLiveness (ref (Use set)) =
	    if StampSet.isEmpty set then SEQ [S "(* use *)", NL]
	    else
		SEQ [S (List.foldl (fn (stamp, s) =>
				    s ^ " " ^ Stamp.toString stamp)
			"(* use" (sort set)), S " *)", NL]
	  | outputLiveness (ref (Kill set)) =
	    if StampSet.isEmpty set then SEQ [S "(* kill *)", NL]
	    else
		SEQ [S (List.foldl (fn (stamp, s) =>
				    s ^ " " ^ Stamp.toString stamp)
			"(* kill" (sort set)), S "*)", NL]

	fun outputInfo ({liveness, ...}: stm_info) = outputLiveness liveness

	fun outputLit (WordLit w) = "word " ^ LargeWord.toString w
	  | outputLit (IntLit i) = "int " ^ LargeInt.toString i
	  | outputLit (CharLit c) = "char " ^ Char.toCString c
	  | outputLit (StringLit s) = "string \"" ^ String.toCString s ^ "\""
	  | outputLit (RealLit r) = "real " ^ (*LargeReal.toString*) r

	fun outputTag Nullary = S "tag0"
	  | outputTag Unary = S "tag1"
	  | outputTag (Tuple _ | Record _) = S "tag+"

	fun outputCon Nullary = S "con0"
	  | outputCon Unary = S "con1"
	  | outputCon (Tuple _ | Record _) = S "con+"

	fun outputArgs (OneArg id) = ID id
	  | outputArgs (TupArgs ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputArgs (RecArgs labelIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (label, id) =>
				       SEQ [S (Label.toString label ^ "="),
					    ID id]) labelIdList),
		 S "}"]

	fun outputTest (LitTest lit) = S (outputLit lit)
	  | outputTest (TagTest label) =
	    SEQ [outputTag Nullary, S " ", S (Label.toString label)]
	  | outputTest (TagAppTest (label, args, conArity)) =
	    SEQ [S "(", outputTag conArity, S " ", S (Label.toString label),
		 S ") ", outputArgs args]
	  | outputTest (ConTest id) =
	    SEQ [outputCon Nullary, S " ", ID id]
	  | outputTest (ConAppTest (id, args, conArity)) =
	    SEQ [S "(", outputCon conArity, S " ", ID id, S ") ",
		 outputArgs args]
	  | outputTest (RefAppTest id) = SEQ [S "ref ", ID id]
	  | outputTest (TupTest ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputTest (RecTest labelIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (label, id) =>
				       SEQ [S (Label.toString label ^ "="),
					    ID id]) labelIdList),
		 S "}"]
	  | outputTest (LabTest (label, id)) =
	    SEQ [S ("{" ^ Label.toString label ^ "="), ID id, S "...}"]
	  | outputTest (VecTest ids) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]

	fun outputStm (ValDec (_, id, exp), _) =
	    SEQ [S "val ", ID id, S " = ", IN, outputExp exp, EX]
	  | outputStm (RecDec (_, idExpList), _) =
	    SEQ [S "rec", IN,
		 SEQ (List.map (fn (id, exp) =>
				SEQ [NL, S "val ", ID id, S " = ",
				     IN, outputExp exp, EX]) idExpList), EX]
	  | outputStm (EvalStm (_, exp), _) =
	    SEQ [S "eval ", IN, outputExp exp, EX]
	  | outputStm (HandleStm (_, body1, id, body2, body3, stamp), shared) =
	    SEQ [S "try", CO (Stamp.toString stamp), IN, NL,
		 outputBody (body1, shared), EX, NL,
		 S "catch ", ID id, IN, NL, outputBody (body2, shared), EX, NL,
		 S "cont", IN, NL, outputBody (body3, shared), EX]
	  | outputStm (EndHandleStm (_, stamp), _) =
	    S ("(* leave " ^ Stamp.toString stamp ^ " *)")
	  | outputStm (TestStm (_, id, testBodyList, body), shared) =
	    SEQ [S "case ", ID id, S " of", IN, NL,
		 SEQ (List.map (fn (test, body) =>
				SEQ [outputTest test, S " =>", IN, NL,
				     outputBody (body, shared), EX, NL])
		      testBodyList),
		 S "else", IN, NL, outputBody (body, shared), EX]
	  | outputStm (RaiseStm (_, id), _) = SEQ [S "raise ", ID id]
	  | outputStm (ReraiseStm (_, id), _) = SEQ [S "reraise ", ID id]
	  | outputStm (SharedStm (_, body, stamp), shared) =
	    if visit (stamp, shared) then
		SEQ [S ("label " ^ (Stamp.toString stamp) ^ ":"), NL,
		     outputBody (body, shared)]
	    else
		SEQ [S ("goto " ^ (Stamp.toString stamp))]
	  | outputStm (ReturnStm (_, exp), _) =
	    SEQ [S "return ", IN, outputExp exp, EX]
	  | outputStm (IndirectStm (_, ref bodyOpt), shared) =
	    SEQ [S "indirect", NL, outputBody (valOf bodyOpt, shared)]
	  | outputStm (ExportStm (_, exp), _) =
	    SEQ [S "export ", IN, outputExp exp, EX]
	and outputExp (LitExp (_, lit)) = S (outputLit lit)
	  | outputExp (PrimExp (_, s)) = S ("prim \"" ^ s ^ "\"")
	  | outputExp (NewExp (_, conArity)) = outputCon conArity
	  | outputExp (VarExp (_, id)) = ID id
	  | outputExp (TagExp (_, label, conArity)) =
	    SEQ [outputTag conArity, S " ", S (Label.toString label)]
	  | outputExp (ConExp (_, id, conArity)) =
	    SEQ [outputCon conArity, S " ", ID id]
	  | outputExp (StaticConExp (_, stamp, conArity)) =
	    SEQ [outputCon conArity, S " ", S (Stamp.toString stamp)]
	  | outputExp (RefExp _) = SEQ [S "ref"]
	  | outputExp (TupExp (_, ids)) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputExp (RecExp (_, labelIdList)) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (label, id) =>
				       SEQ [S (Label.toString label ^ "="),
					    ID id]) labelIdList),
		 S "}"]
	  | outputExp (SelExp (_, label)) =
	    SEQ [S ("#" ^ Label.toString label)]
	  | outputExp (VecExp (_, ids)) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]
	  | outputExp (FunExp (_, _, _, args, body)) =
	    SEQ [NL, S "fn ", outputArgs args, S " =>",
		 IN, NL, outputBody (body, StampSet.new ()), EX]
	  | outputExp (PrimAppExp (_, s, ids)) =
	    SEQ [S (s ^ " "), SEP (S ", ", List.map ID ids)]
	  | outputExp (VarAppExp (_, id, args)) =
	    SEQ [ID id, S " ", outputArgs args]
	  | outputExp (TagAppExp (_, label, args, conArity)) =
	    SEQ [S "(", outputTag conArity, S " ", S (Label.toString label),
		 S ") ", outputArgs args]
	  | outputExp (ConAppExp (_, id, args, conArity)) =
	    SEQ [S "(", outputCon conArity, S " ", ID id, S ") ",
		 outputArgs args]
	  | outputExp (StaticConAppExp (_, stamp, args, conArity)) =
	    SEQ [S "(", outputCon conArity, S " ", S (Stamp.toString stamp),
		 S ") ", outputArgs args]
	  | outputExp (RefAppExp (_, id)) =
	    SEQ [S "ref ", ID id]
	  | outputExp (SelAppExp (_, label, id)) =
	    SEQ [S ("#" ^ Label.toString label ^ " "), ID id]
	  | outputExp (AdjExp (_, id1, id2)) =
	    SEQ [S "adj ", ID id1, S ", ", ID id2]
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
				    S (" from " ^ Url.toString url ^ "\n")])
			      importList), outputBody (body, StampSet.new ()),
			 NL])
    end
