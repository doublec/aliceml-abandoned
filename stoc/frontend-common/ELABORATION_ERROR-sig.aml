signature ELABORATION_ERROR =
  sig

    type lab    = Lab.t
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
	| StrModUnclosed	of lab * int * typ
	| SelModInf		of inf
	| AppModFunMismatch	of inf
	| AppModArgMismatch	of inf_mismatch
	| AnnModMismatch	of inf_mismatch
	(* Interfaces *)
	| GroundInfKind		of Inf.kind
	| CompInfMismatch	of inf_mismatch
	| SingInfPath
	(* Components *)
	| CompUnclosed		of lab * int * typ

    datatype warning =
	  NotGeneralized	of id * typ

    val error :	Source.region * error -> 'a
    val warn :	Source.region * warning -> unit

  end
