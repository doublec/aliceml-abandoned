signature ABSTRACT_GRAMMAR =
  sig

    (* Generic *)

    type info

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word		(* word *)
	| IntLit    of LargeInt.int		(* integer *)
	| CharLit   of WideChar.char		(* character *)
	| StringLit of WideString.string	(* string *)
	| RealLit   of LargeReal.real		(* floating point *)

    (* Identifiers *)

    type     stamp  = Stamp.t
    datatype name   = datatype Name.name
    datatype lab    = Lab     of info * string
    datatype id     = Id      of info * stamp * name
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit		(* literal *)
	| PrimExp   of info * string * typ	(* primitive values *)
	| VarExp    of info * longid		(* variable *)
	| ConExp    of info * int * longid	(* constructor *)
	| RefExp    of info			(* reference constructor *)
	| TupExp    of info * exp list		(* tuple *)
	| RowExp    of info * exp row		(* row (record) *)
	| SelExp    of info * lab		(* row selector *)
	| VecExp    of info * exp list		(* vector *)
	| FunExp    of info * id  * exp		(* function *)
	| AppExp    of info * exp * exp		(* application *)
	| CompExp   of info * exp * exp		(* adjunction *)
	| AndExp    of info * exp * exp		(* short-circuit conjunction *)
	| OrExp     of info * exp * exp		(* short-circuit disjunction *)
	| IfExp     of info * exp * exp * exp	(* conditional *)
	| WhileExp  of info * exp * exp		(* while loop *)
	| SeqExp    of info * exp list		(* sequential expressions *)
	| CaseExp   of info * exp * match list	(* case *)
	| RaiseExp  of info * exp		(* exception raising *)
	| HandleExp of info * exp * match list	(* exception handling *)
	| AnnExp    of info * exp * typ		(* type annotation *)
	| LetExp    of info * dec list * exp	(* let *)

    and 'a row   = Row   of info * 'a field list * bool
    and 'a field = Field of info * lab * 'a

    and match    = Match of info * pat * exp

    (* Patterns *)

    and pat =
	  JokPat    of info			(* joker (wildcard) *)
	| LitPat    of info * lit		(* literal *)
	| VarPat    of info * id		(* variable *)
	| ConPat    of info * longid * pat list	(* constructor (fully applied)*)
	| RefPat    of info * pat		(* reference *)
	| TupPat    of info * pat list		(* tuple *)
	| RowPat    of info * pat row		(* row (record) *)
	| VecPat    of info * pat list		(* vector *)
	| AsPat     of info * pat * pat		(* as (layered) pattern *)
	| AltPat    of info * pat list		(* alternative pattern *)
	| NegPat    of info * pat		(* negated pattern *)
	| GuardPat  of info * pat * exp		(* guarded pattern *)
	| AnnPat    of info * pat * typ		(* type annotation *)
	| WithPat   of info * pat * dec list	(* local declarations *)

    (* Types *)

    and typ =
	  AbsTyp    of info			(* abstract type *)
	| VarTyp    of info * id		(* variable *)
	| ConTyp    of info * longid		(* constructor *)
	| FunTyp    of info * id * typ		(* type function *)
	| AppTyp    of info * typ * typ		(* constructor application *)
	| RefTyp    of info * typ		(* reference type *)
	| TupTyp    of info * typ list		(* tuple (cartesian) type *)
	| RowTyp    of info * typ row		(* row (record) type *)
	| ArrTyp    of info * typ * typ		(* arrow (function) type *)
	| SumTyp    of info * con list		(* sum type (datatype) *)
	| ExtTyp    of info			(* extensible sum type *)
	| AllTyp    of info * id * typ		(* universal quantification *)
	| ExTyp     of info * id * typ		(* existential quantification *)
	| SingTyp   of info * longid		(* singleton type *)

    and con =   Con of info * id * typ list	(* data constructor *)

    (* Modules *)

    and mod =
	  VarMod    of info * id		(* module id *)
	| StrMod    of info * dec list		(* structure *)
	| SelMod    of info * mod * lab		(* selection *)
	| FunMod    of info * id * inf * mod	(* functor *)
	| AppMod    of info * mod * mod		(* application *)
	| AnnMod    of info * mod * inf		(* annotation (ascription) *)
	| LetMod    of info * dec list * mod	(* let *)

    (* Interfaces *)

    and inf =
	  AnyInf    of info			(* top interface *)
	| AbsInf    of info			(* abstract interface *)
	| ConInf    of info * longid		(* interface constructor *)
	| SigInf    of info * spec list		(* signature *)
	| FunInf    of info * id * inf * inf	(* interface function *)
	| AppInf    of info * inf * mod		(* interface application *)
	| CompInf   of info * inf * inf		(* composition *)
	| ArrInf    of info * id * inf * inf	(* arrow (functor) interface *)
	| SingInf   of info * mod		(* singleton interface *)

    (* Declarations *)

    and dec =
	  ValDec    of info * pat * exp		(* values *)
	| ConDec    of info * con * typ		(* constructor *)
	| TypDec    of info * id * typ		(* type *)
	| DatDec    of info * id * typ		(* data type *)
	| ModDec    of info * id * mod		(* module *)
	| InfDec    of info * id * inf		(* interface *)
	| RecDec    of info * dec list		(* recursive declarations *)
	| TypvarDec of info * id * dec list	(* scoped type variable *)
	| LocalDec  of info * dec list		(* local declarations *)

    (* Specifications *)

    and spec =
	  ValSpec   of info * id * typ		(* value *)
	| ConSpec   of info * con * typ		(* constructor *)
	| TypSpec   of info * id * typ		(* type *)
	| DatSpec   of info * id * typ		(* data type *)
	| ModSpec   of info * id * inf		(* module *)
	| InfSpec   of info * id * inf		(* interface *)
	| RecSpec   of info * spec list		(* recursive specifications *)
	| LocalSpec of info * spec list		(* local specifications *)
	| ExtSpec   of info * inf		(* extension (include) *)

    (* Programs *)

    type program = dec list

    (* Operations *)

    val stamp :		id	-> stamp
    val name :		id	-> name
    val lab :		lab	-> string
    val idToLab :	id	-> lab

    val infoLab :	lab	-> info
    val infoId :	id	-> info
    val infoLongid :	longid	-> info
    val infoExp :	exp	-> info
    val infoRow :	'a row	-> info
    val infoField :	'a field -> info
    val infoMatch :	match	-> info
    val infoPat :	pat	-> info
    val infoTyp :	typ	-> info
    val infoCon :	con	-> info
    val infoMod :	mod	-> info
    val infoInf :	inf	-> info
    val infoDec :	dec	-> info
    val infoSpec :	spec	-> info

  end
