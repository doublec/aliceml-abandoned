signature INTERMEDIATE_GRAMMAR =
  sig

    (* Generic *)

    type info
    type sign

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word		(* modulo arithmetic *)
	| IntLit    of LargeInt.int		(* integer arithmetic *)
	| CharLit   of WideChar.char		(* character *)
	| StringLit of WideString.string	(* character string *)
(*	| RealLit   of LargeReal.real		(* floating point *)
UNFINISHED: obsolete after bootstrapping:
*)	| RealLit   of string			(* floating point *)

    (* Identifiers *)

    datatype lab    = Lab     of info * Label.t
    datatype id     = Id      of info * Stamp.t * Name.t
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit		(* literal *)
	| PrimExp   of info * string		(* primitive value *)
	| NewExp    of info * string option * bool (* new constructor *)
				(* bool : is n-ary *)
	| VarExp    of info * longid		(* variable *)
	| ConExp    of info * longid * bool	(* constructor *)
				(* bool : is n-ary *)
	| RefExp    of info			(* reference constructor *)
	| TupExp    of info * exp list		(* tuple *)
	| RowExp    of info * exp field list	(* record / module *)
			(* all labels distinct *)
	| SelExp    of info * lab		(* field selector *)
	| VecExp    of info * exp list		(* vector *)
	| FunExp    of info * match list	(* function / functor *)
	| AppExp    of info * exp * exp		(* application *)
	| AdjExp    of info * exp * exp		(* record adjunction *)
	| UpExp     of info * exp		(* up cast *)
	| AndExp    of info * exp * exp		(* conjunction *)
	| OrExp     of info * exp * exp		(* disjunction *)
	| IfExp     of info * exp * exp * exp	(* conditional *)
	| WhileExp  of info * exp * exp		(* conditional loop *)
	| SeqExp    of info * exp list		(* sequential *)
	| CaseExp   of info * exp * match list	(* case switch *)
	| RaiseExp  of info * exp		(* exception raise *)
	| HandleExp of info * exp * match list	(* exception handler *)
	| LetExp    of info * dec list * exp	(* local binding *)

    and 'a field = Field of info * lab * 'a

    and match    = Match of info * pat * exp

    (* Patterns (always linear) *)

    and pat =
	  WildPat   of info			(* wildcard *)
	| LitPat    of info * lit		(* literal *)
	| VarPat    of info * id		(* variable *)
	| ConPat    of info * longid * bool	(* constructed *)
			(* bool : is n-ary *)
			(* pat present iff longid has arguments *)
	| RefPat    of info			(* reference *)
	| TupPat    of info * pat list		(* tuple *)
	| RowPat    of info * pat field list	(* record *)
			(* all labels distinct *)
	| VecPat    of info * pat list		(* vector *)
	| AppPat    of info * pat * pat		(* constructor application *)
			(* first must be ConPat or RefPat *)
	| AsPat     of info * pat * pat		(* conjunction *)
	| AltPat    of info * pat list		(* disjunction *)
			(* all patterns bind same ids *)
	| NegPat    of info * pat		(* negation *)
	| GuardPat  of info * pat * exp		(* guard *)
	| WithPat   of info * pat * dec list	(* local bindings *)

    (* Declarations *)

    and dec =
	  ValDec    of info * pat * exp		(* value / module *)
	  		(* if inside RecDec, then
			 * (1) pat may not contain AltPat, NegPat, GuardPat,
			 *     WithPat
			 * (2) exp may only contain LitExp, VarExp, ConExp,
			 *     RefExp, TupExp, RowExp, VecExp, FunExp, AppExp
			 * (3) AppExps may only contain ConExp or RefExp
			 *     as first argument
			 * (4) if an VarPat on the LHS structurally corresponds
			 *     to an VarExp on the RHS then the RHS id may not
			 *     be bound on the LHS *)
	| RecDec    of info * dec list		(* recursive definition *)

    (* Components *)

    type component = (id * sign * Url.t) list * (exp * sign)


    (* Operations *)

    val infoLab :	lab	-> info
    val infoId :	id	-> info
    val infoLongid :	longid	-> info
    val infoExp :	exp	-> info
    val infoField :	'a field-> info
    val infoMatch :	match	-> info
    val infoPat :	pat	-> info
    val infoDec :	dec	-> info

  end
