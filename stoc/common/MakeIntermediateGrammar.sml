functor Intermediate(type info
                     val output_info: TextIO.outstream * info -> unit)
  : INTERMEDIATE =
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
	| SelExp    of info * lab
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
	| HandleExp of info * exp * id * exp
	| LetExp    of info * dec list * exp

    and 'a field = Field of info * lab * 'a

    and match    = Match of info * pat * exp

    (* Patterns *)

    and pat =
	  LitPat    of info * lit
	| VarPat    of info * id
	| ConPat    of info * longid * pat option
	| TupPat    of info * pat list
	| RecPat    of info * pat field list * bool (* dots *)
	| AsPat     of info * id * pat
	| AltPat    of info * pat list
	| NegPat    of info * pat
	| GuardPat  of info * pat * exp
	| WithPat   of info * pat * dec

    (* Declarations *)

    and dec =
	  ValDec    of info * id list * exp
	| ConDec    of info * id * bool (* has args *)


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
      | info_exp(CaseExp(i,_,_))	= i
      | info_exp(RaiseExp(i,_))		= i
      | info_exp(HandleExp(i,_,_,_))	= i
      | info_exp(LetExp(i,_,_))		= i

    fun info_field(Field(i,_,_))	= i
    fun info_match(Match(i,_,_))	= i

    fun info_pat(LitPat(i,_))		= i
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


    (* Externalisation *)

    val output				= TextIO.output
    val output1				= TextIO.output1

    fun f(q,s)				= ( output(q,s) ; output1(q,#"(") )
    fun m(q)				= output1(q,#" ")
    fun r(q)				= output1(q,#")")

    fun output_bool(q,b)		= output(q, Bool.toString b)
    fun output_int(q,n)			= output(q, Int.toString n)
    fun output_string(q,s)		= ( output1(q,#"\"")
					  ; output(q, String.toCString s)
					  ; output1(q, #"\"")
					  )

    fun output_option output_z (q,NONE) = ( f(q,"NONE") ; r(q) )
      | output_option output_z (q,SOME z) =
					  ( f(q,"SOME")
					  ; output_z(q,z)
					  ; r(q)
					  )

    fun output_list output_z (q,zs)	= ( output1(q,#"[")
					  ; List.app (fn z =>
					         ( output_z(q,z) ; m(q) )) zs
					  ; output1(q,#"]")
					  )

    fun output_lit(q, WordLit w)	= ( f(q,"WordLit")
					  ; output_int(q,Word.toInt w)
					  ; r(q)
					  )
      | output_lit(q, IntLit n)		= ( f(q,"IntLit")
					  ; output_int(q,n)
					  ; r(q)
					  )
      | output_lit(q, CharLit c)	= ( f(q,"CharLit")
					  ; output_string(q,String.str c)
					  ; r(q)
					  )
      | output_lit(q, StringLit s)	= ( f(q,"StringLit")
					  ; output_string(q,s)
					  ; r(q)
					  )
      | output_lit(q, RealLit x)	= ( f(q,"RealLit")
					  ; output_string(q,x)
					  ; r(q)
					  )

    val output_stamp			= output_int

    fun output_name(q, ExId s)		= ( f(q,"ExId")
 					  ; output_string(q,s)
					  ; r(q)
					  )
      | output_name(q, InId)		= ( f(q,"InId") ; r(q) )

    fun output_lab(q, Lab(i,s))		= ( f(q,"Lab")
					  ; output_info(q,i) ; m(q)
					  ; output_string(q,s) ; r(q)
					  )

    fun output_id(q, Id(i,s,n))		= ( f(q,"Id")
					  ; output_info(q,i) ; m(q)
					  ; output_stamp(q,s) ; m(q)
					  ; output_name(q,n) ; r(q)
					  )

    fun output_longid(q, ShortId(i,x))	= ( f(q,"ShortId")
					  ; output_info(q,i) ; m(q)
					  ; output_id(q,x) ; r(q)
					  )
      | output_longid(q, LongId(i,y,x))	= ( f(q,"LongId")
					  ; output_info(q,i) ; m(q)
					  ; output_longid(q,y) ; m(q)
					  ; output_id(q,x) ; r(q)
					  )

    fun output_field output_z (q, Field(i,l,z)) =
					  ( f(q,"Field")
					  ; output_info(q,i) ; m(q)
					  ; output_lab(q,l) ; m(q)
					  ; output_z(q,z) ; r(q)
					  )

    fun output_exp(q, LitExp(i,l))	= ( f(q,"LitExp")
					  ; output_info(q,i) ; m(q)
					  ; output_lit(q,l) ; r(q)
					  )
      | output_exp(q, VarExp(i,y))	= ( f(q,"VarExp")
					  ; output_info(q,i) ; m(q)
					  ; output_longid(q,y) ; r(q)
					  )
      | output_exp(q, ConExp(i,y,eo))	= ( f(q,"ConExp")
					  ; output_info(q,i) ; m(q)
					  ; output_longid(q,y) ; m(q)
					  ; output_option output_exp(q,eo); r(q)
					  )
      | output_exp(q, TupExp(i,es))	= ( f(q,"TupExp")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_exp (q,es) ; r(q)
					  )
      | output_exp(q, RecExp(i,fs))	= ( f(q,"RecExp")
					  ; output_info(q,i) ; m(q)
					  ; output_list(output_field output_exp)
					               (q,fs) ; r(q)
					  )
      | output_exp(q, SelExp(i,l))	= ( f(q,"SelExp")
					  ; output_info(q,i) ; m(q)
					  ; output_lab(q,l) ; r(q)
					  )
      | output_exp(q, FunExp(i,x,e))	= ( f(q,"FunExp")
					  ; output_info(q,i) ; m(q)
					  ; output_id(q,x) ; m(q)
					  ; output_exp(q,e) ; r(q)
					  )
      | output_exp(q, AppExp(i,e1,e2))	= ( f(q,"AppExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, AdjExp(i,e1,e2))	= ( f(q,"AdjExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, AndExp(i,e1,e2))	= ( f(q,"AndExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, OrExp(i,e1,e2))	= ( f(q,"OrExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, IfExp(i,e1,e2,e3)) =
					  ( f(q,"IfExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; m(q)
					  ; output_exp(q,e3) ; r(q)
					  )
      | output_exp(q, WhileExp(i,e1,e2)) =
					  ( f(q,"WhileExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, SeqExp(i,es))	= ( f(q,"SeqExp")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_exp (q,es) ; r(q)
					  )
      | output_exp(q, CaseExp(i,e,ms))	= ( f(q,"CaseExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e) ; m(q)
					  ; output_list output_match (q,ms)
					  ; r(q)
					  )
      | output_exp(q, RaiseExp(i,e))	= ( f(q,"RaiseExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e) ; r(q)
					  )
      | output_exp(q, HandleExp(i,e1,x,e2)) =
					  ( f(q,"HandleExp")
					  ; output_info(q,i) ; m(q)
					  ; output_exp(q,e1) ; m(q)
					  ; output_id(q,x) ; m(q)
					  ; output_exp(q,e2) ; r(q)
					  )
      | output_exp(q, LetExp(i,ds,e))	= ( f(q,"LetExp")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_dec (q,ds) ; m(q)
					  ; output_exp(q,e) ; r(q)
					  )

    and output_match(q, Match(i,p,e))	= ( f(q,"Match")
					  ; output_info(q,i) ; m(q)
					  ; output_pat(q,p) ; m(q)
					  ; output_exp(q,e) ; r(q)
					  )

    and output_pat(q, LitPat(i,l))	= ( f(q,"LitPat")
					  ; output_info(q,i) ; m(q)
					  ; output_lit(q,l) ; r(q)
					  )
      | output_pat(q, VarPat(i,x))	= ( f(q,"VarPat")
					  ; output_info(q,i) ; m(q)
					  ; output_id(q,x) ; r(q)
					  )
      | output_pat(q, ConPat(i,y,po))	= ( f(q,"ConPat")
					  ; output_info(q,i) ; m(q)
					  ; output_longid(q,y) ; m(q)
					  ; output_option output_pat(q,po); r(q)
					  )
      | output_pat(q, TupPat(i,ps))	= ( f(q,"TupPat")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_pat (q,ps) ; r(q)
					  )
      | output_pat(q, RecPat(i,fs,b))	= ( f(q,"RecPat")
					  ; output_info(q,i) ; m(q)
					  ; output_list(output_field output_pat)
					               (q,fs) ; m(q)
					  ; output_bool(q,b) ; r(q)
					  )
      | output_pat(q, AsPat(i,x,p))	= ( f(q,"AsPat")
					  ; output_info(q,i) ; m(q)
					  ; output_id(q,x) ; m(q)
					  ; output_pat(q,p) ; r(q)
					  )
      | output_pat(q, AltPat(i,ps))	= ( f(q,"AltPat")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_pat (q,ps) ; r(q)
					  )
      | output_pat(q, NegPat(i,p))	= ( f(q,"NegPat")
					  ; output_info(q,i) ; m(q)
					  ; output_pat(q,p) ; r(q)
					  )
      | output_pat(q, GuardPat(i,p,e))	= ( f(q,"GuardPat")
					  ; output_info(q,i) ; m(q)
					  ; output_pat(q,p) ; m(q)
					  ; output_exp(q,e) ; r(q)
					  )
      | output_pat(q, WithPat(i,p,d))	= ( f(q,"WithPat")
					  ; output_info(q,i) ; m(q)
					  ; output_pat(q,p) ; m(q)
					  ; output_dec(q,d) ; r(q)
					  )

    and output_dec(q, ValDec(i,xs,e))	= ( f(q,"ValDec")
					  ; output_info(q,i) ; m(q)
					  ; output_list output_id (q,xs); m(q)
					  ; output_exp(q,e) ; r(q)
					  )
      | output_dec(q, ConDec(i,x,b))	= ( f(q,"ConDec")
					  ; output_info(q,i) ; m(q)
					  ; output_id(q,x) ; m(q)
					  ; output_bool(q,b) ; r(q)
					  )

  end
