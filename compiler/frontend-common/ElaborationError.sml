structure ElaborationError :> ELABORATION_ERROR =
  struct

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

    val par = paragraph


    type typ    = Type.t
    type kind   = Type.kind
    type longid = AbstractGrammar.longid

    type unify_error = typ * typ * typ * typ

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
	| TypDecUnify		of unify_error
	| DatDecUnify		of unify_error
	(* Specifications *)
	| TypSpecUnify		of unify_error
	| DatSpecUnify		of unify_error


    (* Pretty printing *)

    fun ppLab'(AbstractGrammar.Lab(_,l)) = l

    fun ppId'(AbstractGrammar.Id(_,_, AbstractGrammar.ExId x)) = x
      | ppId'(AbstractGrammar.Id(_,_, AbstractGrammar.InId))   = "?"

    fun ppLongid'(AbstractGrammar.ShortId(_,x))  = ppId' x
      | ppLongid'(AbstractGrammar.LongId(_,y,l)) = ppLongid' y ^ "." ^ ppLab' l

    fun ppLongid y = "`" ^ ppLongid' y ^ "'"


    fun ppUnify2(d1, d2, (t1,t2,t3,t4)) =
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppType t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppType t2))
	)

    fun ppUnify4(d1, d2, (t1,t2,t3,t4)) =
	vbox(
	    d1 ^^
	    nest(break ^^ below(PPType.ppType t1)) ^/^
	    d2 ^^
	    nest(break ^^ below(PPType.ppType t2)) ^/^
	    par["Reason: could","not","unify"] ^^
	    nest(break ^^ below(PPType.ppType t3)) ^/^
	    par["and"] ^^
	    nest(break ^^ below(PPType.ppType t4))
	)

    fun pp(VecExpUnify ue) =
	ppUnify2(
	  par["inconsistent","types","in","vector","expression:"],
	  par["does","not","agree","with","previous","element","type"], ue)
      | pp(AppExpFunUnify ue) =
	ppUnify2(
	  par["applied","value","is","not","a","function:"],
	  par["does","not","match","function","type"], ue)
      | pp(AppExpArgUnify ue) =
	ppUnify4(
	  par["argument","type","mismatch:"],
	  par["does","not","match","argument","type"], ue)
      | pp(AndExpUnify ue) =
	ppUnify2(
	  par["operand","of","`andalso'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | pp(OrExpUnify ue) =
	ppUnify2(
	  par["operand","of","`orelse'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | pp(IfExpCondUnify ue) =
	ppUnify2(
	  par["operand","of","`if'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | pp(IfExpBranchUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`if':"],
	  par["does","not","agree","with","type"], ue)
      | pp(WhileExpCondUnify ue) =
	ppUnify2(
	  par["operand","of","`while'","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | pp(RaiseExpUnify ue) =
	ppUnify2(
	  par["operand","of","`raise'","is","not","an","exception:"],
	  par["does","not","match","type"], ue)
      | pp(HandleExpUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`handle':"],
	  par["does","not","agree","with","type"], ue)
      | pp(AnnExpUnify ue) =
	ppUnify4(
	  par["expression","does","not","match","annotation:"],
	  par["does","not","match","type"], ue)
      | pp(MatchPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","`case'","patterns:"],
	  par["does","not","agree","with","previous","type"], ue)
      | pp(MatchExpUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","branches","of","`case':"],
	  par["does","not","agree","with","previous","type"], ue)
      | pp(ConPatFewArgs y) =
	  par["missing","argument","to","constructor",ppLongid y,"in","pattern"]
      | pp(ConPatManyArgs y) =
	  par["surplus","argument","to","constructor",ppLongid y,"in","pattern"]
      | pp(ConPatUnify ue) =
	ppUnify4(
	  par["ill-typed","constructor","argument:"],
	  par["does","not","match","argument","type"], ue)
      | pp(VecPatUnify ue) =
	ppUnify2(
	  par["inconsistent","types","in","vector","pattern:"],
	  par["does","not","agree","with","previous","element","type"], ue)
      | pp(AsPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","`as'","pattern:"],
	  par["does","not","agree","with","type"], ue)
      | pp(AltPatUnify ue) =
	ppUnify4(
	  par["inconsistent","types","in","pattern","alternatives:"],
	  par["does","not","agree","with","previous","type"], ue)
      | pp(GuardPatUnify ue) =
	ppUnify2(
	  par["pattern","guard","is","not","a","boolean:"],
	  par["does","not","match","type"], ue)
      | pp(AnnPatUnify ue) =
	ppUnify4(
	  par["pattern","does","not","match","annotation:"],
	  par["does","not","match","type"], ue)
      | pp(StarTypKind k) =
	  par["missing","arguments","in","type","expression"]
      | pp(AppTypFunKind k) =
	  par["type","expression","is","not","a","type","function"]
      | pp(AppTypArgKind(k1,k2)) =
	  par["missing","arguments","in","type","expression"]
      | pp(RefTypKind k) =
	  par["missing","arguments","in","type","expression"]
      | pp(ValDecUnify ue) =
	ppUnify4(
	  par["expression","does","not","match","pattern","type:"],
	  par["does","not","match","type"], ue)
      | pp(TypDecUnify ue) =
	  par["missing","arguments","in","type","expression",
	      "in","type","declaration"]
      | pp(DatDecUnify ue) =
	  par["missing","arguments","in","type","expression",
	      "in","datatype","declaration"]
      | pp(TypSpecUnify ue) =
	  par["missing","arguments","in","type","expression",
	      "in","type","specification"]
      | pp(DatSpecUnify ue) =
	  par["missing","arguments","in","type","expression",
	      "in","datatype","specification"]


    fun toString err = PrettyPrint.toString(pp err, 75)

    fun error(i, err) = Error.error(i, toString err)

  end
