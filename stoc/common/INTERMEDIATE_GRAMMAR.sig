signature INTERMEDIATE_GRAMMAR =
  sig

    (* Generic *)

    type lab_info
    type id_info
    type longid_info
    type exp_info
    type pat_info
    type 'a field_info
    type match_info
    type dec_info

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

    datatype lab    = Lab     of lab_info * Label.t
    datatype id     = Id      of id_info * Stamp.t * Name.t
    datatype longid = ShortId of longid_info * id
		    | LongId  of longid_info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of exp_info * lit			(* literal *)
	| PrimExp   of exp_info * string		(* primitive value *)
	| NewExp    of exp_info * string option * bool	(* new constructor *)
				(* bool : is n-ary *)
	| VarExp    of exp_info * longid		(* variable *)
	| ConExp    of exp_info * longid * bool		(* constructor *)
				(* bool : is n-ary *)
	| RefExp    of exp_info				(* reference *)
	| TupExp    of exp_info * exp list		(* tuple *)
	| RowExp    of exp_info * exp field list	(* record / module *)
			(* all labels distinct *)
	| SelExp    of exp_info * lab			(* field selector *)
	| VecExp    of exp_info * exp list		(* vector *)
	| FunExp    of exp_info * match list		(* function / functor *)
	| AppExp    of exp_info * exp * exp		(* application *)
	| AdjExp    of exp_info * exp * exp		(* record adjunction *)
	| UpExp     of exp_info * exp			(* up cast *)
	| AndExp    of exp_info * exp * exp		(* conjunction *)
	| OrExp     of exp_info * exp * exp		(* disjunction *)
	| IfExp     of exp_info * exp * exp * exp	(* conditional *)
	| WhileExp  of exp_info * exp * exp		(* conditional loop *)
	| SeqExp    of exp_info * exp list		(* sequential *)
	| CaseExp   of exp_info * exp * match list	(* case switch *)
	| RaiseExp  of exp_info * exp			(* exception raise *)
	| HandleExp of exp_info * exp * match list	(* exception handler *)
	| LetExp    of exp_info * dec list * exp	(* local binding *)

    and 'a field = Field of 'a field_info * lab * 'a

    and match    = Match of match_info * pat * exp

    (* Patterns (always linear) *)

    and pat =
	  WildPat   of pat_info				(* wildcard *)
	| LitPat    of pat_info * lit			(* literal *)
	| VarPat    of pat_info * id			(* variable *)
	| ConPat    of pat_info * longid * bool		(* constructed *)
			(* bool : is n-ary *)
			(* pat present iff longid has arguments *)
	| RefPat    of pat_info				(* reference *)
	| TupPat    of pat_info * pat list		(* tuple *)
	| RowPat    of pat_info * pat field list	(* record *)
			(* all labels distinct *)
	| VecPat    of pat_info * pat list		(* vector *)
	| AppPat    of pat_info * pat * pat		(* construction *)
			(* first must be ConPat or RefPat *)
	| AsPat     of pat_info * pat * pat		(* conjunction *)
	| AltPat    of pat_info * pat list		(* disjunction *)
			(* all patterns bind same ids *)
	| NegPat    of pat_info * pat			(* negation *)
	| GuardPat  of pat_info * pat * exp		(* guard *)
	| WithPat   of pat_info * pat * dec list	(* local bindings *)

    (* Declarations *)

    and dec =
	  ValDec    of dec_info * pat * exp		(* value / module *)
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
	| RecDec    of dec_info * dec list		(* recursion *)

    (* Components *)

    type comp = (id * sign * Url.t) list * (exp * sign)
    type t = comp


    (* Operations *)

    val stamp :		id	-> Stamp.t
    val name :		id	-> Name.t
    val lab :		lab	-> Label.t

    val infoLab :	lab	-> lab_info
    val infoId :	id	-> id_info
    val infoLongid :	longid	-> longid_info
    val infoExp :	exp	-> exp_info
    val infoField :	'a field -> 'a field_info
    val infoMatch :	match	-> match_info
    val infoPat :	pat	-> pat_info
    val infoDec :	dec	-> dec_info

  end
