(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
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
	    SEQ of format vector
	  | S of string
	  | I of int
	  | IN
	  | EX
	  | NL
	  | ID of id
	  | IDDEF of idDef
	  | CO of string
	  | NULL
	  | SEP of format * format vector

	fun format f =
	    let
		val indent = ref 0
		fun format' (SEQ fs) =
		    String.concat (Vector.toList (Vector.map format' fs))
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
		    "$" ^ Stamp.toString stamp ^ "[" ^ s ^ "]"
		  | format' (IDDEF (IdDef id)) = format' (ID id)
		  | format' (IDDEF Wildcard) = "_"
		  | format' (CO s) = "   (* " ^ s ^ " *)"
		  | format' NULL = ""
		  | format' (SEP (f, #[])) = ""
		  | format' (SEP (f, fs)) =
		    Vector.foldli
		    (fn (_, fi, rest) => rest ^ format' f ^ format' fi)
		    (format' (Vector.sub (fs, 0))) (fs, 1, NONE)
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

	fun outputLiveness (ref Unknown) = NULL
	  | outputLiveness (ref (Use set)) =
	    SEQ #[S (List.foldl (fn (stamp, s) =>
				 s ^ " " ^ Stamp.toString stamp)
		     "(* use" (sort set)), S " *)", NL]
	  | outputLiveness (ref (Kill set)) =
	    SEQ #[S (List.foldl (fn (stamp, s) =>
				 s ^ " " ^ Stamp.toString stamp)
		     "(* kill" (sort set)), S " *)", NL]

	fun outputInfo ({liveness, ...}: stm_info) = outputLiveness liveness

	fun outputLit (WordLit w) = SEQ #[S "0w", S (LargeWord.toString w)]
	  | outputLit (IntLit i) = S (LargeInt.toString i)
	  | outputLit (CharLit c) =
	    SEQ #[S "#\"", S (WideChar.toString c), S "\""]
	  | outputLit (StringLit s) =
	    SEQ #[S "\"", S (String.toString s), S "\""]
	  | outputLit (RealLit r) = S (LargeReal.toString r)

	fun outputArgs outputX (OneArg x) = outputX x
	  | outputArgs outputX (TupArgs xs) =
	    SEQ #[S "(", SEP (S ", ", Vector.map outputX xs), S ")"]
	  | outputArgs outputX (ProdArgs labelXVector) =
	    SEQ #[S "{", SEP (S ", ",
			      Vector.map (fn (label, x) =>
					  SEQ #[S (Label.toString label),
						S "=", outputX x])
			      labelXVector), S "}"]

	fun outputStm (ValDec (_, idDef, exp), _) =
	    SEQ #[S "val ", IDDEF idDef, S " = ", IN, outputExp exp, EX]
	  | outputStm (RefAppDec (_, idDef, id'), _) =
	    SEQ #[S "val ref ", IDDEF idDef, S " = ", ID id']
	  | outputStm (TupDec (_, idDefs, id), _) =
	    SEQ #[S "val (", SEP (S ", ", Vector.map IDDEF idDefs),
		  S ") = ", ID id]
	  | outputStm (ProdDec (_, labelIdDefVector, id), _) =
	    SEQ #[S "val {",
		  SEP (S ", ",
		       Vector.map (fn (label, idDef) =>
				   SEQ #[S (Label.toString label), S "=",
					 IDDEF idDef]) labelIdDefVector),
		  S "} = ", ID id]
	  | outputStm (TryStm (_, tryBody, idDef, handleBody), shared) =
	    let
		val handleOutput = outputBody (handleBody, shared)
	    in
		SEQ #[S "try", IN, NL, outputBody (tryBody, shared), EX, NL,
		      S "handle ", IDDEF idDef, IN, NL, handleOutput]
	    end
	  | outputStm (EndTryStm (_, body), shared) =
	    SEQ #[S "(* end try *)", outputBody (body, shared)]
	  | outputStm (EndHandleStm (_, body), shared) =
	    SEQ #[S "(* end handle *)", EX, NL, outputBody (body, shared)]
	  | outputStm (TestStm (_, id, tests, body), shared) =
	    SEQ #[S "case ", ID id, S " of", IN, NL,
		  outputTests (tests, shared),
		  S "else", IN, NL, outputBody (body, shared), EX, EX]
	  | outputStm (RaiseStm (_, id), _) = SEQ #[S "raise ", ID id]
	  | outputStm (ReraiseStm (_, id), _) = SEQ #[S "reraise ", ID id]
	  | outputStm (SharedStm (_, body, stamp), shared) =
	    if visit (stamp, shared) then
		SEQ #[S "label ", S (Stamp.toString stamp), S ":", NL,
		      outputBody (body, shared)]
	    else
		SEQ #[S "goto ", S (Stamp.toString stamp)]
	  | outputStm (ReturnStm (_, exp), _) =
	    SEQ #[S "return ", IN, outputExp exp, EX]
	  | outputStm (IndirectStm (_, ref bodyOpt), shared) =
	    SEQ #[S "indirect", NL, outputBody (valOf bodyOpt, shared)]
	  | outputStm (ExportStm (_, exp), _) =
	    SEQ #[S "export ", IN, outputExp exp, EX]
	and outputTests (LitTests litBodyVector, shared) =
	    SEQ (Vector.map (fn (lit, body) =>
			     SEQ #[outputLit lit, S " =>", IN, NL,
				   outputBody (body, shared), EX, NL])
		 litBodyVector)
	  | outputTests (TagTests tagBodyVector, shared) =
	    SEQ (Vector.map (fn (label, n, conArgs, body) =>
			     SEQ #[case conArgs of
				       NONE =>
					   SEQ #[S "tag ",
						 S (Label.toString label),
						 S "/", I n]
				     | SOME args =>
					   SEQ #[S "(tag ",
						 S (Label.toString label),
						 S "/", I n, S ") ",
						 outputArgs IDDEF args],
				   S " =>", IN, NL,
				   outputBody (body, shared), EX, NL])
		 tagBodyVector)
	  | outputTests (ConTests conBodyVector, shared) =
	    SEQ (Vector.map (fn (con, conArgs, body) =>
			     SEQ #[case (con, conArgs) of
				       (Con id, NONE) =>
					   SEQ #[S "con ", ID id]
				     | (Con id, SOME args) =>
					   SEQ #[S "(con ", ID id, S ") ",
						 outputArgs IDDEF args]
				     | (StaticCon stamp, NONE) =>
					   SEQ #[S "con ",
						 S (Stamp.toString stamp)]
				     | (StaticCon stamp, SOME args) =>
					   SEQ #[S "(con ",
						 S (Stamp.toString stamp),
						 S ") ", outputArgs IDDEF args],
				   S " =>", IN, NL,
				   outputBody (body, shared), EX, NL])
		 conBodyVector)
	  | outputTests (VecTests idDefsBodyVector, shared) =
	    SEQ (Vector.map (fn (idDefs, body) =>
			     SEQ #[S "#[",
				   SEP (S ", ", Vector.map IDDEF idDefs),
				   S "]", S " =>", IN, NL,
				   outputBody (body, shared), EX, NL])
		 idDefsBodyVector)
	and outputExp (LitExp (_, lit)) = outputLit lit
	  | outputExp (PrimExp (_, name)) = SEQ #[S "prim \"", S name, S "\""]
	  | outputExp (NewExp _) = S "new"
	  | outputExp (VarExp (_, id)) = ID id
	  | outputExp (TagExp (_, label, n)) =
	    SEQ #[S "tag ", S (Label.toString label), S "/", I n]
	  | outputExp (ConExp (_, con)) =
	    SEQ #[S "con ",
		  case con of
		      Con id => ID id
		    | StaticCon stamp => S (Stamp.toString stamp)]
	  | outputExp (TupExp (_, ids)) =
	    SEQ #[S "(", SEP (S ", ", Vector.map ID ids), S ")"]
	  | outputExp (ProdExp (_, labelIdVector)) =
	    SEQ #[S "{", SEP (S ", ",
			      Vector.map (fn (label, id) =>
					  SEQ #[S (Label.toString label),
						S "=", ID id]) labelIdVector),
		  S "}"]
	  | outputExp (VecExp (_, ids)) =
	    SEQ #[S "#[", SEP (S ", ", Vector.map ID ids), S "]"]
	  | outputExp (FunExp (_, _, _, args, body)) =
	    SEQ #[NL, S "fn ", outputArgs IDDEF args, S " =>",
		  IN, NL, outputBody (body, StampSet.new ()), EX]
	  | outputExp (PrimAppExp (_, name, ids)) =
	    SEQ #[S "prim \"", S name, S "\" (",
		  SEP (S ", ", Vector.map ID ids), S ")"]
	  | outputExp (VarAppExp (_, id, args)) =
	    SEQ #[ID id, S " ", outputArgs ID args]
	  | outputExp (TagAppExp (_, label, n, args)) =
	    SEQ #[S "(tag ", S (Label.toString label), S "/", I n, S ") ",
		  outputArgs ID args]
	  | outputExp (ConAppExp (_, con, args)) =
	    SEQ #[S "(con ",
		  case con of
		      Con id => ID id
		    | StaticCon stamp => S (Stamp.toString stamp), S ") ",
		  outputArgs ID args]
	  | outputExp (RefAppExp (_, id)) =
	    SEQ #[S "ref ", ID id]
	  | outputExp (SelAppExp (_, _, label, n, id)) =
	    SEQ #[S "#", S (Label.toString label), S "/", I n, S " ", ID id]
	  | outputExp (FunAppExp (_, id, _, args)) =
	    SEQ #[ID id, S " ", outputArgs ID args]
	and outputBody (stms, shared) =
	    SEP (NL, Vector.fromList (List.map
				      (fn stm =>
				       SEQ #[outputInfo (infoStm stm),
					    outputStm (stm, shared)]) stms))

	fun outputComponent (importVector, (body, _)) =
	    format (SEQ #[SEQ (Vector.map
			       (fn (idDef, _, url) =>
				SEQ #[S "import ", IDDEF idDef,
				      S " from ", S (Url.toString url),
				      S "\n"]) importVector),
			  outputBody (body, StampSet.new ()), NL])
    end
