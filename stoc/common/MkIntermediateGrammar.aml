functor Intermediate(type info
		     val dummy: info) :> INTERMEDIATE where type info = info =
  struct

    (* Generic *)

    type info = info

    (* Literals *)

    datatype lit =
	  WordLit   of word
	| IntLit    of int
	| CharLit   of char
	| StringLit of string
	| RealLit   of string

    (* Identifiers *)

    type stamp      = int

    datatype name   = ExId of string | InId

    datatype lab    = Lab     of info * string
    datatype id     = Id      of info * stamp * name
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * id

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit
	| VarExp    of info * longid
	| ConExp    of info * longid * exp option
	| TupExp    of info * exp list
	| RecExp    of info * exp field list
			(* all labels distinct *)
	| SelExp    of info * lab
	| FunExp    of info * id * exp
	| AppExp    of info * exp * exp
	| AdjExp    of info * exp * exp
	| AndExp    of info * exp * exp
	| OrExp     of info * exp * exp
	| IfExp     of info * exp * exp * exp
	| WhileExp  of info * exp * exp
	| SeqExp    of info * exp list
	| CaseExp   of info * exp * match list * longid (* failure exception *)
	| RaiseExp  of info * exp
	| HandleExp of info * exp * id * exp
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
	| TupPat    of info * pat list
	| RecPat    of info * pat field list * bool (* dots *)
			(* all labels distinct *)
	| AsPat     of info * id * pat
	| AltPat    of info * pat list
			(* all paterns bind same ids *)
	| NegPat    of info * pat
	| GuardPat  of info * pat * exp
	| WithPat   of info * pat * dec list

    (* Declarations *)

    and dec =
	  ValDec    of info * id list * exp
	  		(* all ids distinct *)
	| ConDec    of info * id * bool (* has args *)


    (* Predefined *)

    val stamp_false	= ~1
    val stamp_true	= ~2
    val stamp_nil	= ~3
    val stamp_cons	= ~4
    val stamp_ref	= ~5
    val stamp_Match	= ~6
    val stamp_Bind	= ~7
    val stamp_eq	= ~8
    val stamp_assign	= ~9

    val id_false	= Id(dummy, stamp_false, ExId "false")
    val id_true		= Id(dummy, stamp_true,  ExId "true")
    val id_nil		= Id(dummy, stamp_nil,   ExId "nil")
    val id_cons		= Id(dummy, stamp_cons,  ExId "::")
    val id_ref		= Id(dummy, stamp_ref,   ExId "ref")
    val id_Match	= Id(dummy, stamp_Match, ExId "Match")
    val id_Bind		= Id(dummy, stamp_Bind,  ExId "Bind")
    val id_eq		= Id(dummy, stamp_eq,    ExId "=")
    val id_assign	= Id(dummy, stamp_assign,ExId ":=")

    val longid_false	= ShortId(dummy, id_false)
    val longid_true	= ShortId(dummy, id_true)
    val longid_nil	= ShortId(dummy, id_nil)
    val longid_cons	= ShortId(dummy, id_cons)
    val longid_ref	= ShortId(dummy, id_ref)
    val longid_Match	= ShortId(dummy, id_Match)
    val longid_Bind	= ShortId(dummy, id_Bind)
    val longid_eq	= ShortId(dummy, id_eq)
    val longid_assign	= ShortId(dummy, id_assign)


    (* Projections *)

    fun info_lab(Lab(i,_))		= i
    fun info_id(Id(i,_,_))		= i
    fun info_longid(ShortId(i,_))	= i
      | info_longid(LongId(i,_,_))	= i

    fun info_exp(LitExp(i,_))		= i
      | info_exp(VarExp(i,_))		= i
      | info_exp(ConExp(i,_,_))		= i
      | info_exp(TupExp(i,_))		= i
      | info_exp(RecExp(i,_))		= i
      | info_exp(SelExp(i,_))		= i
      | info_exp(FunExp(i,_,_))		= i
      | info_exp(AppExp(i,_,_))		= i
      | info_exp(AdjExp(i,_,_))		= i
      | info_exp(AndExp(i,_,_))		= i
      | info_exp(OrExp(i,_,_))		= i
      | info_exp(IfExp(i,_,_,_))	= i
      | info_exp(WhileExp(i,_,_))	= i
      | info_exp(SeqExp(i,_))		= i
      | info_exp(CaseExp(i,_,_,_))	= i
      | info_exp(RaiseExp(i,_))		= i
      | info_exp(HandleExp(i,_,_,_))	= i
      | info_exp(LetExp(i,_,_))		= i

    fun info_field(Field(i,_,_))	= i
    fun info_match(Match(i,_,_))	= i

    fun info_pat(WildPat(i))		= i
      | info_pat(LitPat(i,_))		= i
      | info_pat(VarPat(i,_))		= i
      | info_pat(ConPat(i,_,_))		= i
      | info_pat(TupPat(i,_))		= i
      | info_pat(RecPat(i,_,_))		= i
      | info_pat(AsPat(i,_,_))		= i
      | info_pat(AltPat(i,_))		= i
      | info_pat(NegPat(i,_))		= i
      | info_pat(GuardPat(i,_,_))	= i
      | info_pat(WithPat(i,_,_))	= i

    fun info_dec(ValDec(i,_,_))		= i
      | info_dec(ConDec(i,_,_))		= i

  end
