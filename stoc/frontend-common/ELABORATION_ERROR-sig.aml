signature ELABORATION_ERROR =
  sig

    type typ    = Type.t
    type kind   = Type.kind
    type inf	= Inf.t
    type id     = AbstractGrammar.id
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
	(* Long ids *)
	| ModLongidInf		of longid * inf
	(* Modules *)
	| SelModInf		of inf

    datatype warning =
	  NotGeneralized	of id * typ

    val error :	Source.position * error -> 'a
    val warn :	Source.position * warning -> unit

  end
