(*
 * Note:
 *   I would like to use WideChar and WideString for literals, but SML/NJ
 *   does not support it.
 *)

functor MakeIntermediateGrammar(type info) :>
  INTERMEDIATE_GRAMMAR where type info = info =
  struct

    (* Generic *)

    type info = info

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word
	| IntLit    of LargeInt.int
	| CharLit   of Char.char
	| StringLit of String.string
	| RealLit   of string

    (* Identifiers *)

    datatype lab    = Lab     of info * string
    datatype id     = Id      of info * Name.t
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * id

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


    (* Projections *)

    fun infoLab(Lab(i,_))		= i
    fun infoId(Id(i,_))			= i
    fun infoLongid(ShortId(i,_))	= i
      | infoLongid(LongId(i,_,_))	= i

    fun infoExp(LitExp(i,_))		= i
      | infoExp(VarExp(i,_))		= i
      | infoExp(ConExp(i,_,_))		= i
      | infoExp(RefExp(i,_))		= i
      | infoExp(TupExp(i,_))		= i
      | infoExp(RecExp(i,_))		= i
      | infoExp(SelExp(i,_,_))		= i
      | infoExp(FunExp(i,_,_))		= i
      | infoExp(AppExp(i,_,_))		= i
      | infoExp(AdjExp(i,_,_))		= i
      | infoExp(AndExp(i,_,_))		= i
      | infoExp(OrExp(i,_,_))		= i
      | infoExp(IfExp(i,_,_,_))		= i
      | infoExp(WhileExp(i,_,_))	= i
      | infoExp(SeqExp(i,_))		= i
      | infoExp(CaseExp(i,_,_))		= i
      | infoExp(RaiseExp(i,_))		= i
      | infoExp(HandleExp(i,_,_))	= i
      | infoExp(LetExp(i,_,_))		= i

    fun infoField(Field(i,_,_))		= i
    fun infoMatch(Match(i,_,_))		= i

    fun infoPat(WildPat(i))		= i
      | infoPat(LitPat(i,_))		= i
      | infoPat(VarPat(i,_))		= i
      | infoPat(ConPat(i,_,_))		= i
      | infoPat(RefPat(i,_))		= i
      | infoPat(TupPat(i,_))		= i
      | infoPat(RecPat(i,_,_))		= i
      | infoPat(AsPat(i,_,_))		= i
      | infoPat(AltPat(i,_))		= i
      | infoPat(NegPat(i,_))		= i
      | infoPat(GuardPat(i,_,_))	= i
      | infoPat(WithPat(i,_,_))		= i

    fun infoDec(ValDec(i,_,_,_))	= i
      | infoDec(ConDec(i,_,_))		= i

  end
