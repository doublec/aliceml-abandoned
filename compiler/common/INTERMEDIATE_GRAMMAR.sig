signature INTERMEDIATE =
  sig

    (* Generic *)

    type info

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

    datatype id     = Id     of info * stamp * name
    datatype longid = Longid of info * id list * id
    datatype lab    = Lab    of info * string

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit
	| VarExp    of info * longid
	| ConExp    of info * longid * exp option
	| RefExp    of info
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
	| CaseExp   of info * exp * match list	(* always total *)
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
	| RefPat    of info * pat
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


    (* Operations *)

    val info_id:     id		-> info
    val info_longid: longid	-> info
    val info_lab:    lab	-> info
    val info_exp:    exp	-> info
    val info_field:  'a field	-> info
    val info_match:  match	-> info
    val info_pat:    pat	-> info
    val info_dec:    dec	-> info

    val output_lit:    TextIO.outstream * lit		-> unit
    val output_id:     TextIO.outstream * id		-> unit
    val output_longid: TextIO.outstream * longid	-> unit
    val output_lab:    TextIO.outstream * lab		-> unit
    val output_exp:    TextIO.outstream * exp		-> unit
    val output_field: (TextIO.outstream * 'a -> unit) ->
                       TextIO.outstream * 'a field	-> unit
    val output_match:  TextIO.outstream * match		-> unit
    val output_pat:    TextIO.outstream * pat		-> unit
    val output_dec:    TextIO.outstream * dec		-> unit

  end
