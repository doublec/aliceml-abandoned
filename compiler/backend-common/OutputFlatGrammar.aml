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

	fun outputLit (WordLit w) = "word " ^ LargeWord.toString w
	  | outputLit (IntLit i) = "int " ^ LargeInt.toString i
	  | outputLit (CharLit c) = "char " ^ Char.toCString c
	  | outputLit (StringLit s) = "string " ^ String.toCString s
	  | outputLit (RealLit r) = "real " ^ (*LargeReal.toString*) r

	fun outputArgs (OneArg id) = ID id
	  | outputArgs (TupArgs ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputArgs (RecArgs stringIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (s, id) =>
				       SEQ [S (s ^ "="), ID id]) stringIdList),
		 S "}"]

	fun outputTest (LitTest lit) = S (outputLit lit)
	  | outputTest (ConTest (id, NONE)) = SEQ [S "nam ", ID id]
	  | outputTest (ConTest (id1, SOME id2)) =
	    SEQ [S "(con ", ID id1, S ") ", ID id2]
	  | outputTest (TupTest ids) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputTest (RecTest stringIdList) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (s, id) =>
				       SEQ [S (s ^ "="), ID id]) stringIdList),
		 S "}"]
	  | outputTest (LabTest (s, id)) =
	    SEQ [S ("{" ^ s ^ "="), ID id, S "...}"]
	  | outputTest (VecTest ids) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]

	fun outputStm (ValDec (_, id, exp, isToplevel)) =
	    SEQ [S "val ", ID id, S " = ", IN, outputExp exp, EX,
		 if isToplevel then CO "toplevel" else NULL]
	  | outputStm (PrimDec (_, id, s, isToplevel)) =
	    SEQ [S "prim ", ID id, S (" = \"" ^ s ^ "\""),
		 if isToplevel then CO "toplevel" else NULL]
	  | outputStm (RecDec (_, idExpList, isToplevel)) =
	    SEQ [S "rec", IN, if isToplevel then CO "toplevel" else NULL,
		 SEQ (List.map (fn (id, exp) =>
				SEQ [NL, S "val ", ID id, S " = ",
				     IN, outputExp exp, EX]) idExpList), EX]
	  | outputStm (ConDec (_, id, false, isToplevel)) =
	    SEQ [S "con ", ID id, if isToplevel then CO "toplevel" else NULL]
	  | outputStm (ConDec (_, id, true, isToplevel)) =
	    SEQ [S "nam ", ID id, if isToplevel then CO "toplevel" else NULL]
	  | outputStm (EvalStm (_, exp)) =
	    SEQ [S "eval ", IN, outputExp exp, EX]
	  | outputStm (HandleStm (_, body1, id, body2)) =
	    SEQ [S "try", IN, NL, outputBody body1, EX, NL,
		 S "catch ", ID id, IN, NL, outputBody body2, EX]
	  | outputStm (EndHandleStm (_, body)) =
	    SEQ [S "untry", IN, NL, outputBody body, EX]
	  | outputStm (TestStm (_, id, test, body1, body2)) =
	    SEQ [S "case ", ID id, S " of ", IN, outputTest test, NL,
		 outputBody body1, EX, NL, S "else", IN, NL, outputBody body2,
		 EX]
	  | outputStm (RaiseStm (_, id)) =
	    SEQ [S "raise ", ID id]
	  | outputStm (SharedStm (_, body, shared)) =
	    if !shared = 0 then
		(shared := gen ();
		 SEQ [S ("label " ^ (Int.toString (!shared)) ^ ":"), NL,
		      outputBody body])
	    else
		SEQ [S ("goto " ^ (Int.toString (!shared)))]
	  | outputStm (ReturnStm (_, exp)) =
	    SEQ [S "return ", IN, outputExp exp, EX]
	  | outputStm (IndirectStm (_, ref bodyOpt)) =
	    outputBody (valOf bodyOpt)
	  | outputStm (ExportStm (_, ids)) =
	    SEQ [S "export ", IN, SEP (S ", ", List.map ID ids)]
	and outputExp (LitExp (_, lit)) = S (outputLit lit)
	  | outputExp (VarExp (_, id)) = ID id
	  | outputExp (ConExp (_, id, false)) = SEQ [S "nam ", ID id]
	  | outputExp (ConExp (_, id, true)) = SEQ [S "con ", ID id]
	  | outputExp (TupExp (_, ids)) =
	    SEQ [S "(", SEP (S ", ", List.map ID ids), S ")"]
	  | outputExp (RecExp (_, labIdList)) =
	    SEQ [S "{", SEP (S ", ",
			     List.map (fn (Lab (_, s), id) =>
				       SEQ [S (s ^ "="), ID id]) labIdList),
		 S "}"]
	  | outputExp (SelExp (_, Lab (_, s))) = SEQ [S ("#" ^ s)]
	  | outputExp (VecExp (_, ids)) =
	    SEQ [S "#[", SEP (S ", ", List.map ID ids), S "]"]
	  | outputExp (FunExp (_, s, argsBodyList)) =
	    SEQ [NL, S "fn ", IN,
		 SEP (SEQ [NL, S "| "],
		      List.map (fn (args, body) =>
				SEQ [outputArgs args, S " =>", IN, NL,
				     outputBody body, EX]) argsBodyList)]
	  | outputExp (AppExp (_, id, args)) =
	    SEQ [ID id, S " ", outputArgs args]
	  | outputExp (SelAppExp (_, Lab (_, s), id)) =
	    SEQ [S ("#" ^ s ^ " "), ID id]
	  | outputExp (ConAppExp (_, id1, id2)) =
	    SEQ [S "(con ", ID id1, S ") ", ID id2]
	  | outputExp (PrimAppExp (_, s, ids)) =
	    SEQ [S (s ^ " "), SEP (S ", ", List.map ID ids)]
	  | outputExp (AdjExp (_, id1, id2)) =
	    SEQ [S "adj ", ID id1, S ", ", ID id2]
	and outputBody stms = SEP (NL, List.map outputStm stms)

	fun outputProgram body = format (SEQ [outputBody body, NL])
    end
