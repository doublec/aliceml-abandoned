(*
 * Note:
 *   I would like to use WideChar and WideString for literals, but SML/NJ
 *   does not support it.
 *)

signature INTERMEDIATE_GRAMMAR =
  sig

    (* Generic *)

    type info

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word
	| IntLit    of LargeInt.int
	| CharLit   of Char.char
	| StringLit of String.string
	| RealLit   of string

    (* Identifiers *)

    type stamp      = int

    datatype name   = ExId of string | InId

    datatype lab    = Lab     of info * string
    datatype id     = Id      of info * stamp * name
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit
	| VarExp    of info * longid
	| ConExp    of info * longid * exp option
	| RefExp    of info * exp option
	| TupExp    of info * exp list
	| RecExp    of info * exp field list
			(* all labels distinct *)
	| SelExp    of info * lab * exp option
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
	| ConPat    of info * longid * pat option
			(* pat present iff longid has arguments *)
	| RefPat    of info * pat
	| TupPat    of info * pat list
	| RecPat    of info * pat field list * bool (* dots *)
			(* all labels distinct *)
	| AsPat     of info * pat * pat
	| AltPat    of info * pat list
			(* all paterns bind same ids *)
	| NegPat    of info * pat
	| GuardPat  of info * pat * exp
	| WithPat   of info * pat * dec list

    (* Declarations *)

    and dec =
	  ValDec    of info * pat * exp * bool (* recursive *)
	  		(* if dec is recursive, then
			 * (1) pat may not contain WithPat
			 * (2) exp may only contain LitExp, VarExp, ConExp,
			 *     RefExp, TupExp, RecExp, FunExp *)
	| ConDec    of info * id * bool (* has args *)

    (* Programs *)

    type program = dec list


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
