structure ElaborationError :> ELABORATION_ERROR =
  struct

  (* Pretty printer *)

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

    val par = paragraph

  (* Types *)

    type lab    = Label.t
    type typ    = Type.t
    type var    = Type.var
    type kind   = Type.kind
    type inf	= Inf.t
    type fix    = Fixity.t
    type id     = AbstractGrammar.id
    type longid = AbstractGrammar.longid

    type unify_error  = typ * typ * typ * typ
    type inf_mismatch = Inf.mismatch

    datatype error =
	(* Expressions *)
	  VecExpUnify		of unify_error
	| AppExpFunUnify	of unify_error
	| AppExpArgUnify	of unify_error
	| CompExpNoRow		of typ
	| CompExpUnify		of unify_error
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
	| VecPatUnify		of unify_error
	| AppPatArrTyp		of typ
	| AppPatFunUnify	of unify_error
	| AppPatUnify		of unify_error
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
	| StrModUnclosed	of lab * int * typ
	| SelModInf		of inf
	| AppModFunMismatch	of inf
	| AppModArgMismatch	of inf_mismatch
	| AnnModMismatch	of inf_mismatch
	(* Interfaces *)
	| GroundInfKind		of Inf.kind
	| CompInfMismatch	of inf_mismatch
	| SingInfPath
	(* Imports *)
	| ValItemMismatch	of lab * typ * typ
	| ConItemMismatch	of lab * typ * typ
	| TypItemMismatch	of lab * kind * kind
	| ModItemMismatch	of lab * inf_mismatch
	| InfItemMismatch	of lab * inf_mismatch
	| FixItemMismatch	of lab * fix * fix
	(* Components *)
	| CompUnclosed		of lab * int * typ

    datatype warning =
	  NotGeneralized	of id * typ


  (* Pretty printing *)

    fun ppQuoted s	= "`" ^ s ^ "'"

    fun ppLab'(AbstractGrammar.Lab(_,l)) = Label.toString l

    fun ppId'(AbstractGrammar.Id(_,_,n)) = Name.toString n
    fun ppId x = ppQuoted(ppId' x)

    fun ppLongid'(AbstractGrammar.ShortId(_,x))  = ppId' x
      | ppLongid'(AbstractGrammar.LongId(_,y,l)) = ppLongid' y ^ "." ^ ppLab' l
    fun ppLongid y = ppQuoted(ppLongid' y)


    fun ppLab a = ppQuoted(Label.toString a)


    fun ppUnify2(d1, d2, (t1,t2,t3,t4)) =
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppTyp t2))
	)

    fun ppUnify4(d1, d2, (t1,t2,t3,t4)) =
	let
	    val td1 = PPType.ppTyp t1
	    val td2 = PPType.ppTyp t2
	    val td3 = PPType.ppTyp t3
	    val td4 = PPType.ppTyp t4
	in
	    if td3 = td1 andalso td4 = td2 then
		vbox(
		    d1 ^^
		    nest(break ^^ below td1) ^/^
		    d2 ^^
		    nest(break ^^ below td2)
		)
	    else
		vbox(
		    d1 ^^
		    nest(break ^^ below td1) ^/^
		    d2 ^^
		    nest(break ^^ below td2) ^/^
		    par["because","type"] ^^
		    nest(break ^^ below td3) ^/^
		    par["does","not","unify","with"] ^^
		    nest(break ^^ below td4)
		)
	end

    fun ppMismatch(d, im) =
        vbox(
	    d ^/^
	    par(ppMismatch' im)
	)

    and ppMismatch'(Inf.MissingVal a) =
	    ["value",ppLab a,"is","missing"]
      | ppMismatch'(Inf.MissingTyp  a) =
	    ["type",ppLab a,"is","missing"]
      | ppMismatch'(Inf.MissingMod  a) =
	    ["module",ppLab a,"is","missing"]
      | ppMismatch'(Inf.MissingInf  a) =
	    ["signature",ppLab a,"is","missing"]
      | ppMismatch'(Inf.MissingFix  a) =
	    ["fixity","of",ppLab a,"is","unspecified"]
      | ppMismatch'(Inf.ManifestVal a) =
	    ["value",ppLab a,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestTyp a) =
	    ["type",ppLab a,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestMod a) =
	    ["module",ppLab a,"does","not","match","manifest","specification"]
      | ppMismatch'(Inf.ManifestInf a) =
	    ["signature",ppLab a,
	     "does","not","match","manifest","specification"]
      | ppMismatch'(Inf.MismatchVal(a,t1,t2)) =
	    ["value",ppLab a,"has","incompatible","type"]
      | ppMismatch'(Inf.MismatchTyp(a,k1,k2)) =
	    ["type",ppLab a,"has","incompatible","arity"]
      | ppMismatch'(Inf.MismatchMod(a, Inf.Incompatible _)) =
	    ["module",ppLab a,"has","incompatible","signature"]
      | ppMismatch'(Inf.MismatchMod(a, im as Inf.IncompatibleArg _)) =
	    ["module",ppLab a,"has","incompatible","signature,","because"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchMod(a,im)) =
	    ["module",ppLab a,"has","incompatible","signature,",
	     "because","nested"] @ ppMismatch' im
      | ppMismatch'(Inf.MismatchInf(a, Inf.Incompatible _)) =
	    ["signature",ppLab a,"is","incompatible"]
      | ppMismatch'(Inf.MismatchInf(a, im as Inf.IncompatibleArg _)) =
	    ["signature",ppLab a,"is","incompatible","because"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchInf(a,im)) =
	    ["signature",ppLab a,"is","incompatible,","because","nested"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchFix(a,q1,q2)) =
	    ["fixity","of",ppLab a,"is","different"]
      | ppMismatch'(Inf.MismatchValSort(a,w1,w2)) =
	    ["value",ppLab a,"is","not","a","constructor"]
      | ppMismatch'(Inf.MismatchTypSort(a,w1,w2)) =
	    ["type",ppLab a,"is","not","an","open","datatype"]
      | ppMismatch'(Inf.MismatchDom im) =
	    ["functor","signature","is","incompatible","because","argument"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.MismatchRan im) =
	    ["functor","signature","is","incompatible","because","result"]
	    @ ppMismatch' im
      | ppMismatch'(Inf.Incompatible(j1,j2)) =
	    ["signatures","are","incompatible"]
      | ppMismatch'(Inf.IncompatibleArg(p1,p2)) =
	    ["applied","signature","arguments","are","incompatible"]


    fun ppUnclosed(d, (a,n,t)) =
	vbox(
	    d ^^
	    nest(break ^^
		fbox(nest(
		    text(Label.toString a) ^/^
		    text ":" ^/^
		    below(PPType.ppTyp t)
		))
	    ) ^/^
	    par["contains","free","type","variable",
		"or","unresolved","record","type"]
	)


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
      | ppError(CompExpNoRow t) =
	vbox(
	    par["specialization","type","is","not","a","record:"] ^^
	    nest(break ^^ PPType.ppTyp t)
	)
      | ppError(CompExpUnify ue) =
	ppUnify4(
	  par["mismatch","on","record","update:"],
	  par["does","not","match","type"], ue)
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
      (* Patterns *)
      | ppError(MatchPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","`case'","patterns:"],
	  par["does","not","agree","with","previous","type"], ue)
      | ppError(MatchExpUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`case':"],
	  par["does","not","agree","with","previous","type"], ue)
      | ppError(VecPatUnify ue) =
	ppUnify2(
	  par["inconsistent","types","in","vector","pattern:"],
	  par["does","not","agree","with","previous","element","type"], ue)
      | ppError(AppPatArrTyp t) =
	  par["missing","argument","to","constructor","in","pattern"]
      | ppError(AppPatFunUnify ue) =
	  par["surplus","argument","to","constructor","in","pattern"]
      | ppError(AppPatUnify ue) =
	ppUnify4(
	  par["ill-typed","constructor","argument:"],
	  par["does","not","match","argument","type"], ue)
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
      (* Types *)
      | ppError(StarTypKind k) =
	  par["missing","arguments","in","type","expression"]
      | ppError(AppTypFunKind k) =
	  par["type","expression","is","not","a","type","function"]
      | ppError(AppTypArgKind(k1,k2)) =
	  par["missing","arguments","in","type","expression"]
      | ppError(RefTypKind k) =
	  par["missing","arguments","in","type","expression"]
      (* Declarations *)
      | ppError(ValDecUnify ue) =
	ppUnify4(
	  par["expression","does","not","match","pattern","type:"],
	  par["does","not","match","type"], ue)
      | ppError(ValDecLift(x,a)) =
	  par["could not generalize","type","of",ppId x,
	      "due","to","value","restriction",
	      "although","it","contains","explicit","type","variables"]
      (* Modules *)
      | ppError(ModLongidInf(y,j)) =
	  par["module",ppLongid y,"is","not","a","structure"]
      | ppError(StrModUnclosed lnt) =
	ppUnclosed(
	  par["structure","is","not","closed:"], lnt)
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
      (* Interfaces *)
      | ppError(GroundInfKind k) =
	  par["missing","arguments","in","signature","expression"]
      | ppError(CompInfMismatch im) =
	ppMismatch(
	  par["inconsistency","at","signature","specialization:"], im)
      | ppError(SingInfPath) =
	  par["module","expression","is","not","a","path"]
      (* Imports *)
      | ppError(ValItemMismatch(a,t1,t2)) =
	vbox(
	    par["type","annotation","of","value",ppLab a] ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    par["does","not","match","component","export","type"] ^^
	    nest(break ^^ below(PPType.ppTyp t2))
	)
      | ppError(ConItemMismatch(a,t1,t2)) =
	vbox(
	    par["type","of","constructor",ppLab a] ^^
	    nest(break ^^ below(PPType.ppTyp t1)) ^/^
	    par["does","not","match","component","export","type"] ^^
	    nest(break ^^ below(PPType.ppTyp t2))
	)
      | ppError(TypItemMismatch(a,k1,k2)) =
	  par["type",ppLab a,"exported","by","component",
	      "has","incompatible","arity"]
      | ppError(ModItemMismatch(a,im)) =
	ppMismatch(
	  par["module",ppLab a,"exported","by","component","does","not","match",
	      "signature,","because"], im)
      | ppError(InfItemMismatch(a,im)) =
	ppMismatch(
	  par["signature",ppLab a,"exported","by","component","is",
	      "incompatible,","because"], im)
      | ppError(FixItemMismatch(a,f1,f2)) =
	  par["fixity","status","for",ppLab a,"does","not","match","export"]
      (* Components *)
      | ppError(CompUnclosed ant) =
	ppUnclosed(
	  par["component","is","not","closed:"], ant)

    fun ppWarning(NotGeneralized(x,t)) =
	vbox(
	    par["type","of",ppId x,"cannot","be","generalized","due","to",
		"value","restriction:"] ^^
	    nest(break ^^ PPType.ppTyp t)
	)

  (* Export *)

    fun errorToString e   = PrettyPrint.toString(ppError e, 75)
    fun warningToString w = PrettyPrint.toString(ppWarning w, 75)

    fun error(region, e)  = Error.error(region, errorToString e)
    fun warn(region, w)   = Error.warn(region, warningToString w)

  end
