structure ElaborationError :> ELABORATION_ERROR =
  struct

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

    val par = paragraph


    type typ    = Type.t
    type var    = Type.var
    type kind   = Type.kind
    type inf	= Inf.t
    type id     = AbstractGrammar.id
    type longid = AbstractGrammar.longid

    type unify_error  = typ * typ * typ * typ
    type inf_mismatch = Inf.mismatch

    datatype error =
	(* Expressions *)
	  VecExpUnify		of unify_error
	| AppExpFunUnify	of unify_error
	| AppExpArgUnify	of unify_error
	| AndExpUnify		of unify_error
	| OrExpUnify		of unify_error
	| IfExpCondUnify	of unify_error
	| IfExpBranchUnify	of unify_error
	| WhileExpCondUnify	of unify_error
	| RaiseExpUnify		of unify_error
	| HandleExpUnify	of unify_error
	| AnnExpUnify		of unify_error
	| MatchPatUnify		of unify_error
	| MatchExpUnify		of unify_error
	(* Patterns *)
	| ConPatFewArgs		of longid
	| ConPatManyArgs	of longid
	| ConPatUnify		of unify_error
	| VecPatUnify		of unify_error
	| AsPatUnify		of unify_error
	| AltPatUnify		of unify_error
	| GuardPatUnify		of unify_error
	| AnnPatUnify		of unify_error
	(* Types *)
	| StarTypKind		of kind
	| AppTypFunKind		of kind
	| AppTypArgKind		of kind * kind
	| RefTypKind		of kind
	(* Declarations *)
	| ValDecUnify		of unify_error
	| ValDecLift		of id * var
	(* Long ids *)
	| ModLongidInf		of longid * inf
	(* Modules *)
	| SelModInf		of inf
	| AppModFunMismatch	of inf
	| AppModArgMismatch	of inf_mismatch
	| AnnModMismatch	of inf_mismatch
	(* Interfaces *)
	| GroundInfKind		of Inf.kind

    datatype warning =
	  NotGeneralized	of id * typ


    (* Pretty printing *)

    fun ppLab'(AbstractGrammar.Lab(_,l)) = l

    fun ppId'(AbstractGrammar.Id(_,_,name)) = Name.toString name
    fun ppId x = "`" ^ ppId' x ^ "'"

    fun ppLongid'(AbstractGrammar.ShortId(_,x))  = ppId' x
      | ppLongid'(AbstractGrammar.LongId(_,y,l)) = ppLongid' y ^ "." ^ ppLab' l
    fun ppLongid y = "`" ^ ppLongid' y ^ "'"


    fun ppLab l = "`" ^ Lab.toString l ^ "'"


    fun ppUnify2(d1, d2, (t1,t2,t3,t4)) =
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppTyp t2))
	)

    fun ppUnify4(d1, d2, (t1,t2,t3,t4)) =
	if t3 = t1 andalso t4 = t2 then
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppTyp t2))
	)
	else
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppTyp t2)) ^/^
	    par["because","type"] ^^
	    nest(break ^^ below(PPType.ppTyp t3)) ^/^
	    par["does","not","unify","with"] ^^
	    nest(break ^^ below(PPType.ppTyp t4))
	)

    fun ppMismatch(d, im) =
        vbox(
	    d ^/^
	    par(ppMismatch' im)
	)

    and ppMismatch'(Inf.MissingVal l) =
	    ["value",ppLab l,"is","missing"]
      | ppMismatch'(Inf.MissingTyp  l) =
	    ["type",ppLab l,"is","missing"]
      | ppMismatch'(Inf.MissingMod  l) =
	    ["module",ppLab l,"is","missing"]
      | ppMismatch'(Inf.MissingInf  l) =
	    ["signature",ppLab l,"is","missing"]
      | ppMismatch'(Inf.ManifestVal l) =
	    ["value",ppLab l,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestTyp l) =
	    ["type",ppLab l,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestMod l) =
	    ["module",ppLab l,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestInf l) =
	    ["signature",ppLab l,
	     "does","not","match","manifest","specification"]
      | ppMismatch'(Inf.MismatchVal(l,t1,t2)) =
	    ["value",ppLab l,"has","incompatible","type"]
      | ppMismatch'(Inf.MismatchTyp(l,k1,k2)) =
	    ["type",ppLab l,"has","incompatible","arity"]
      | ppMismatch'(Inf.MismatchMod(l, Inf.Incompatible _)) =
	    ["module",ppLab l,"has","incompatible","signature"]
      | ppMismatch'(Inf.MismatchMod(l, im as Inf.IncompatibleArg _)) =
	    ["module",ppLab l,"has","incompatible","signature,","because"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchMod(l,im)) =
	    ["module",ppLab l,"has","incompatible","signature,",
	     "because","nested"] @ ppMismatch' im
      | ppMismatch'(Inf.MismatchInf(l, Inf.Incompatible _)) =
	    ["signature",ppLab l,"is","incompatible"]
      | ppMismatch'(Inf.MismatchInf(l, im as Inf.IncompatibleArg _)) =
	    ["signature",ppLab l,"is","incompatible","because"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchInf(l,im)) =
	    ["signature",ppLab l,"is","incompatible,","because","nested"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.Incompatible(j1,j2)) =
	    ["signatures","are","incompatible"]
      | ppMismatch'(Inf.IncompatibleArg(p1,p2)) =
	    ["applied","signature","arguments","are","incompatible"]

    fun ppError(VecExpUnify ue) =
	ppUnify2(
	  par["inconsistent","types","in","vector","expression:"],
	  par["does","not","agree","with","previous","element","type"], ue)
      | ppError(AppExpFunUnify ue) =
	ppUnify2(
	  par["applied","value","is","not","a","function:"],
	  par["does","not","match","function","type"], ue)
      | ppError(AppExpArgUnify ue) =
	ppUnify4(
	  par["argument","type","mismatch:"],
	  par["does","not","match","argument","type"], ue)
      | ppError(AndExpUnify ue) =
	ppUnify2(
	  par["operand","of","`andalso'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | ppError(OrExpUnify ue) =
	ppUnify2(
	  par["operand","of","`orelse'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | ppError(IfExpCondUnify ue) =
	ppUnify2(
	  par["operand","of","`if'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | ppError(IfExpBranchUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`if':"],
	  par["does","not","agree","with","type"], ue)
      | ppError(WhileExpCondUnify ue) =
	ppUnify2(
	  par["operand","of","`while'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | ppError(RaiseExpUnify ue) =
	ppUnify2(
	  par["operand","of","`raise'","is","not","an","exception:"],
	  par["does","not","match","type"], ue)
      | ppError(HandleExpUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`handle':"],
	  par["does","not","agree","with","type"], ue)
      | ppError(AnnExpUnify ue) =
	ppUnify4(
	  par["expression","does","not","match","annotation:"],
	  par["does","not","match","type"], ue)
      | ppError(MatchPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","`case'","patterns:"],
	  par["does","not","agree","with","previous","type"], ue)
      | ppError(MatchExpUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`case':"],
	  par["does","not","agree","with","previous","type"], ue)
      | ppError(ConPatFewArgs y) =
	  par["missing","argument","to","constructor",ppLongid y,"in","pattern"]
      | ppError(ConPatManyArgs y) =
	  par["surplus","argument","to","constructor",ppLongid y,"in","pattern"]
      | ppError(ConPatUnify ue) =
	ppUnify4(
	  par["ill-typed","constructor","argument:"],
	  par["does","not","match","argument","type"], ue)
      | ppError(VecPatUnify ue) =
	ppUnify2(
	  par["inconsistent","types","in","vector","pattern:"],
	  par["does","not","agree","with","previous","element","type"], ue)
      | ppError(AsPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","`as'","pattern:"],
	  par["does","not","agree","with","type"], ue)
      | ppError(AltPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","pattern","alternatives:"],
	  par["does","not","agree","with","previous","type"], ue)
      | ppError(GuardPatUnify ue) =
	ppUnify2(
	  par["pattern","guard","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | ppError(AnnPatUnify ue) =
	ppUnify4(
	  par["pattern","does","not","match","annotation:"],
	  par["does","not","match","type"], ue)
      | ppError(StarTypKind k) =
	  par["missing","arguments","in","type","expression"]
      | ppError(AppTypFunKind k) =
	  par["type","expression","is","not","a","type","function"]
      | ppError(AppTypArgKind(k1,k2)) =
	  par["missing","arguments","in","type","expression"]
      | ppError(RefTypKind k) =
	  par["missing","arguments","in","type","expression"]
      | ppError(ValDecUnify ue) =
	ppUnify4(
	  par["expression","does","not","match","pattern","type:"],
	  par["does","not","match","type"], ue)
      | ppError(ValDecLift(x,a)) =
	  par["could not generalize","type","of",ppId x,
	      "due","to","value","restriction",
	      "although","it","contains","explicit","type","variables"]
      | ppError(ModLongidInf(y,j)) =
	  par["module",ppLongid y,"is","not","a","structure"]
      | ppError(SelModInf j) =
	  par["module","expression","is","not","a","structure"]
      | ppError(AppModFunMismatch j) =
	  par["applied","module","is","not","a","functor"]
	  (* UNFINISHED: print actual signature j *)
      | ppError(AppModArgMismatch im) =
	ppMismatch(
	  par["module","expression","does","not","match",
	      "functor","parameter","signature:"], im)
      | ppError(AnnModMismatch im) =
	ppMismatch(
	  par["module","expression","does","not","match","signature:"], im)
      | ppError(GroundInfKind k) =
	  par["missing","arguments","in","signature","expression"]

    fun ppWarning(NotGeneralized(x,t)) =
	vbox(
	    par["type","of",ppId x,"cannot","be","generalized","due","to",
		"value","restriction:"] ^^
	    nest(break ^^ below(PPType.ppTyp t))
	)


    fun errorToString err = PrettyPrint.toString(ppError err, 75)
    fun warningToString w = PrettyPrint.toString(ppWarning w, 75)

    fun error(i, err) = Error.error(i, errorToString err)
    fun warn(i, w)    = Error.warn(i, warningToString w)

  end
