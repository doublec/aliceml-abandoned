signature INTERMEDIATE_GRAMMAR =
  sig

    (* Generic *)

    type info
    type sign

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word
	| IntLit    of LargeInt.int
	| CharLit   of WideChar.char
	| StringLit of WideString.string
(*	| RealLit   of LargeReal.real
UNFINISHED: obsolete after bootstrapping:
*)	| RealLit   of string

    (* Identifiers *)

    type stamp      = Stamp.t

    datatype name   = ExId of string | InId

    datatype lab    = Lab     of info * string
    datatype id     = Id      of info * stamp * name
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit
	| PrimExp   of info * string
	| NewExp    of info * string option * bool (* is n-ary *)
	| VarExp    of info * longid
	| ConExp    of info * longid * bool (* is n-ary *)
	| RefExp    of info
	| TupExp    of info * exp list
	| RowExp    of info * exp field list
			(* all labels distinct *)
	| SelExp    of info * lab
	| VecExp    of info * exp list
	| FunExp    of info * id * exp
	| AppExp    of info * exp * exp
	| AdjExp    of info * exp * exp
	| AndExp    of info * exp * exp
	| OrExp     of info * exp * exp
	| IfExp     of info * exp * exp * exp
	| WhileExp  of info * exp * exp
	| SeqExp    of info * exp list
	| CaseExp   of info * exp * match list
	| RaiseExp  of info * exp
	| HandleExp of info * exp * match list
	| LetExp    of info * dec list * exp

    and 'a field = Field of info * lab * 'a

    and match    = Match of info * pat * exp

    (* Patterns (always linear) *)

    and pat =
	  WildPat   of info
	| LitPat    of info * lit
	| VarPat    of info * id
	| ConPat    of info * longid * pat option * bool (* is n-ary *)
			(* pat present iff longid has arguments *)
	| RefPat    of info * pat
	| TupPat    of info * pat list
	| RowPat    of info * pat field list * bool (* dots *)
			(* all labels distinct *)
	| VecPat    of info * pat list
	| AsPat     of info * pat * pat
	| AltPat    of info * pat list
			(* all patterns bind same ids *)
	| NegPat    of info * pat
	| GuardPat  of info * pat * exp
	| WithPat   of info * pat * dec list

    (* Declarations *)

    and dec =
	  ValDec    of info * pat * exp
	  		(* if inside RecDec, then
			 * (1) pat may not contain AltPat, NegPat, GuardPat,
			 *     WithPat
			 * (2) exp may only contain LitExp, VarExp, ConExp,
			 *     RefExp, TupExp, RowExp, VecExp, FunExp, AppExp
			 * (3) AppExps may only contain ConExp or RefExp
			 *     as first argument
			 * (4) if an VarExp on the LHS structurally corresponds
			 *     to an VarExp on the RHS then the RHS id may not
			 *     be bound on the LHS *)
	| RecDec    of info * dec list

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
