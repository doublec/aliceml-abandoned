functor MakeIntermediateGrammar(type info) :>
  INTERMEDIATE_GRAMMAR where type info = info =
  struct

    (* Generic *)

    type info = info

    (* Literals *)

    datatype lit =	(* Add type name annotation later. *)
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
	| VarExp    of info * longid
	| ConExp    of info * longid * bool
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
	| ConPat    of info * longid * pat option
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
	| ConDec    of info * id * bool (* has args *)
	| PrimDec   of info * id * string
	| RecDec    of info * dec list

    (* Programs *)

    type program = dec list * id list


    (* Projections *)

    fun infoLab(Lab(i,_))		= i
    fun infoId(Id(i,_,_))		= i
    fun infoLongid(ShortId(i,_))	= i
      | infoLongid(LongId(i,_,_))	= i

    fun infoExp(LitExp(i,_))		= i
      | infoExp(VarExp(i,_))		= i
      | infoExp(ConExp(i,_,_))		= i
      | infoExp(RefExp(i))		= i
      | infoExp(TupExp(i,_))		= i
      | infoExp(RowExp(i,_))		= i
      | infoExp(SelExp(i,_))		= i
      | infoExp(VecExp(i,_))		= i
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
      | infoPat(RowPat(i,_,_))		= i
      | infoPat(VecPat(i,_))		= i
      | infoPat(AsPat(i,_,_))		= i
      | infoPat(AltPat(i,_))		= i
      | infoPat(NegPat(i,_))		= i
      | infoPat(GuardPat(i,_,_))	= i
      | infoPat(WithPat(i,_,_))		= i

    fun infoDec(ValDec(i,_,_))		= i
      | infoDec(ConDec(i,_,_))		= i
      | infoDec(PrimDec(i,_,_))		= i
      | infoDec(RecDec(i,_))		= i

  end
