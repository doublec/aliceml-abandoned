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

structure OutputImperativeGrammar :> OUTPUT_IMPERATIVE_GRAMMAR =
    struct
	structure I = ImperativeGrammar

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
		  | format' (ID (Id (_, stamp, InId))) =
		    "$" ^ Stamp.toString stamp
		  | format' (ID (Id (_, stamp, ExId s))) =
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

	local
	    val count = ref 0
	in
	    fun gen () =
		let
		    val n = !count + 1
		in
		    count := n; n
		end
	end

	fun outputInfo (_, ref (Unknown | LoopStart | LoopEnd)) = NULL
	  | outputInfo (_, ref (Use set)) = SEQ [S "use ...", NL]
	  | outputInfo (_, ref (Kill set)) =
	    if StampSet.isEmpty set then NULL
	    else
		SEQ [S (StampSet.fold (fn (stamp, s) =>
				       s ^ " " ^ Stamp.toString stamp)
			"kill" set), NL]

	fun outputLit (WordLit w) = "word " ^ LargeWord.toString w
	  | outputLit (IntLit i) = "int " ^ LargeInt.toString i
	  | outputLit (CharLit c) = "char " ^ Char.toCString c
	  | outputLit (StringLit s) = "string " ^ String.toCString s
	  | outputLit (RealLit r) = "real " ^ (*LargeReal.toString*) r

	fun outputArgs (OneArg id) = ID id
	  | outputArgs (TupArgs ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputArgs (RecArgs labIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (lab, id) =>
				       SEQ [S (lab ^ "="), ID id]) labIdList),
		 S "}"]

	fun outputTest (LitTest lit) = S (outputLit lit)
	  | outputTest (ConTest (id, NONE)) = SEQ [S "nam ", ID id]
	  | outputTest (ConTest (id1, SOME id2)) =
	    SEQ [S "(con ", ID id1, S ") ", ID id2]
	  | outputTest (RefTest id) = SEQ [S "(con ref) ", ID id]
	  | outputTest (TupTest ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputTest (RecTest labIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (lab, id) =>
				       SEQ [S (lab ^ "="), ID id]) labIdList),
		 S "}"]
	  | outputTest (LabTest (lab, id)) =
	    SEQ [S ("{" ^ lab ^ "="), ID id, S "...}"]
	  | outputTest (VecTest ids) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]

	fun outputStm (ValDec (_, id, exp, isToplevel)) =
	    SEQ [S "val ", ID id, S " = ", IN, outputExp exp, EX,
		 if isToplevel then CO "toplevel" else NULL]
	  | outputStm (RecDec (_, idExpList, isToplevel)) =
	    SEQ [S "rec", IN, if isToplevel then CO "toplevel" else NULL,
		 SEQ (List.map (fn (id, exp) =>
				SEQ [NL, S "val ", ID id, S " = ",
				     IN, outputExp exp, EX]) idExpList), EX]
	  | outputStm (EvalStm (_, exp)) =
	    SEQ [S "eval ", IN, outputExp exp, EX]
	  | outputStm (HandleStm (_, body1, id, body2, body3, shared)) =
	    (shared := gen ();
	     SEQ [S "try", CO (Int.toString (!shared)), IN, NL,
		  outputBody body1, EX, NL,
		  S "catch ", ID id, IN, NL, outputBody body2, EX, NL,
		  S "cont", IN, NL, outputBody body3, EX])
	  | outputStm (EndHandleStm (_, ref i)) =
	    CO ("leave " ^ Int.toString i)
	  | outputStm (TestStm (_, id, test, body1, body2)) =
	    SEQ [S "case ", ID id, S " of ", IN, outputTest test, NL,
		 outputBody body1, EX, NL, S "else", IN, NL, outputBody body2,
		 EX]
	  | outputStm (RaiseStm (_, id)) =
	    SEQ [S "raise ", ID id]
	  | outputStm (SharedStm (_, body, shared as ref 0)) =
	    (shared := gen ();
	     SEQ [S ("label " ^ (Int.toString (!shared)) ^ ":"), NL,
		  outputBody body])
	  | outputStm (SharedStm (_, _, ref i)) =
	    SEQ [S ("goto " ^ (Int.toString i))]
	  | outputStm (ReturnStm (_, exp)) =
	    SEQ [S "return ", IN, outputExp exp, EX]
	  | outputStm (IndirectStm (_, ref bodyOpt)) =
	    outputBody (valOf bodyOpt)
	  | outputStm (ExportStm (_, ids)) =
	    SEQ [S "export ", IN, SEP (S ", ", List.map ID ids)]
	and outputExp (LitExp (_, lit)) = S (outputLit lit)
	  | outputExp (PrimExp (_, s)) = S ("prim \"" ^ s ^ "\"")
	  | outputExp (NewExp (_, false)) = SEQ [S "con"]
	  | outputExp (NewExp (_, true)) = SEQ [S "nam"]
	  | outputExp (VarExp (_, id)) = ID id
	  | outputExp (ConExp (_, id, false)) = SEQ [S "nam ", ID id]
	  | outputExp (ConExp (_, id, true)) = SEQ [S "con ", ID id]
	  | outputExp (RefExp _) = SEQ [S "con ref"]
	  | outputExp (TupExp (_, ids)) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputExp (RecExp (_, labIdList)) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (lab, id) =>
				       SEQ [S (lab ^ "="), ID id]) labIdList),
		 S "}"]
	  | outputExp (SelExp (_, lab)) = SEQ [S ("#" ^ lab)]
	  | outputExp (VecExp (_, ids)) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]
	  | outputExp (FunExp (_, _, s, argsBodyList)) =
	    SEQ [NL, S "fn ",
		 SEP (SEQ [NL, S "| "],
		      List.map (fn (args, body) =>
				SEQ [outputArgs args, S " =>", IN, NL,
				     outputBody body, EX]) argsBodyList)]
	  | outputExp (AppExp (_, id, args)) =
	    SEQ [ID id, S " ", outputArgs args]
	  | outputExp (SelAppExp (_, lab, id)) =
	    SEQ [S ("#" ^ lab ^ " "), ID id]
	  | outputExp (ConAppExp (_, id, args)) =
	    SEQ [S "(con ", ID id, S ") ", outputArgs args]
	  | outputExp (RefAppExp (_, args)) =
	    SEQ [S "(con ref) ", outputArgs args]
	  | outputExp (PrimAppExp (_, s, ids)) =
	    SEQ [S (s ^ " "), SEP (S ", ", List.map ID ids)]
	  | outputExp (AdjExp (_, id1, id2)) =
	    SEQ [S "adj ", ID id1, S ", ", ID id2]
	and outputBody stms =
	    SEP (NL,
		 List.map (fn stm =>
			   SEQ [outputInfo (infoStm stm), outputStm stm]) stms)

	fun outputComponent (idStringList, _, body) =
	    format (SEQ [SEQ (List.map
			      (fn (id, string) =>
			       SEQ [S "import ", ID id, S (" from " ^ string)])
			      idStringList), outputBody body, NL])
    end
