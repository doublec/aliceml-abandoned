signature ELABORATION_ERROR =
  sig

    type lab       = Label.t
    type typ       = Type.t
    type var       = Type.var
    type kind      = Type.kind
    type inf	   = Inf.t
    type fix       = Fixity.t
    type valid     = AbstractGrammar.valid
    type modlongid = AbstractGrammar.modlongid

    type unify_error  = typ * typ * typ * typ
    type inf_mismatch = Inf.mismatch

    datatype error =
	(* Expressions *)
	  VecExpUnify		of unify_error
	| TagExpConUnify	of unify_error
	| TagExpArgUnify	of unify_error
	| TagExpRowUnify	of unify_error
	| ConExpConUnify	of unify_error
	| ConExpArgUnify	of unify_error
	| UpdExpUnify		of unify_error
	| SelExpUnify		of unify_error
	| AppExpFunUnify	of unify_error
	| AppExpArgUnify	of unify_error
	| AndExpUnify		of unify_error
	| OrExpUnify		of unify_error
	| IfExpCondUnify	of unify_error
	| IfExpBranchUnify	of unify_error
	| RaiseExpUnify		of unify_error
	| HandleExpUnify	of unify_error
	| AnnExpUnify		of unify_error
	| MatchPatUnify		of unify_error
	| MatchExpUnify		of unify_error
	(* Patterns *)
	| TagPatConUnify	of unify_error
	| TagPatArgUnify	of unify_error
	| TagPatRowUnify	of unify_error
	| ConPatConUnify	of unify_error
	| ConPatArgUnify	of unify_error
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
	| PervasiveTypUnknown	of string
	(* Declarations *)
	| ValDecUnify		of unify_error
	| ValDecLift		of valid * var
	(* Long ids *)
	| ModlongidInf		of modlongid * inf
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
	| PervasiveInfUnknown	of string
	(* Imports *)
	| ValImpUnbound		of lab
	| ConImpUnbound		of lab
	| TypImpUnbound		of lab
	| ModImpUnbound		of lab
	| InfImpUnbound		of lab
	| FixImpUnbound		of lab
	| ValImpMismatch	of lab * typ * typ
	| ConImpMismatch	of lab * typ * typ
	| TypImpMismatch	of lab * kind * kind
	| ModImpMismatch	of lab * inf_mismatch
	| InfImpMismatch	of lab * inf_mismatch
	| FixImpMismatch	of lab * fix * fix
	(* Components *)
	| CompUnclosed		of lab * int * typ

    datatype warning =
	  NotGeneralized	of valid * typ

    val error :	Source.region * error -> 'a
    val warn :	Source.region * warning -> unit

  end
