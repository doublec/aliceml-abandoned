structure ElaborationError :> ELABORATION_ERROR =
  struct

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


    fun toString(VecExpUnify ue) =
	"inconsistent types in vector expression"
      | toString(AppExpFunUnify	ue) =
	"applied value is not a function"
      | toString(AppExpArgUnify	ue) =
	"argument type mismatch"
      | toString(AndExpUnify ue) =
	"operand has not type bool"
      | toString(OrExpUnify ue) =
	"operand has not type bool"
      | toString(IfExpCondUnify	ue) =
	"expression has not type bool"
      | toString(IfExpBranchUnify ue) =
	"inconsistent types in branches of if expression"
      | toString(WhileExpCondUnify ue) =
	"expression has not type bool"
      | toString(RaiseExpUnify ue) =
	"operand has not type exn"
      | toString(HandleExpUnify	ue) =
	"inconsistent types in handle epxression"
      | toString(AnnExpUnify ue) =
	"expression does not match annotated type"
      | toString(MatchPatUnify ue) =
	"inconsistent types in match patterns"
      | toString(MatchExpUnify ue) =
	"inconsistent types in match branches"
      | toString(ConPatFewArgs y) =
	"too few arguments in constructor pattern"
      | toString(ConPatManyArgs	y) =
	"too many arguments in constructor pattern"
      | toString(ConPatUnify ue) =
	"constructor argument type mismatch"
      | toString(VecPatUnify ue) =
	"inconsistent types in vector pattern"
      | toString(AsPatUnify ue) =
	"inconsistent types in as pattern"
      | toString(AltPatUnify ue) =
	"inconsistent types in pattern alternatives"
      | toString(GuardPatUnify ue) =
	"expression has not type bool"
      | toString(AnnPatUnify ue) =
	"pattern does not match annotated type"
      | toString(StarTypKind k) =
	"missing arguments in type expression"
      | toString(AppTypFunKind k) =
	"type expression is not a type function"
      | toString(AppTypArgKind(k1,k2)) =
	"missing arguments in type expression"
      | toString(RefTypKind k) =
	"missing arguments in type expression"
      | toString(ValDecUnify ue) =
	"expression type does not match pattern"
      | toString(TypDecUnify ue) =
	"missing arguments in type expression"
      | toString(DatDecUnify ue) =
	"missing arguments in type expression"
      | toString(TypSpecUnify ue) =
	"missing arguments in type expression"
      | toString(DatSpecUnify ue) =
	"missing arguments in type expression"


    fun error(i, err) = Error.error(i, toString err)

  end
