signature ABSTRACT_GRAMMAR =
  sig

    (* Generic *)

    type fix_info
    type lab_info
    type id_info
    type longid_info
    type exp_info
    type pat_info
    type 'a row_info
    type 'a field_info
    type match_info
    type typ_info
    type con_info
    type mod_info
    type inf_info
    type dec_info
    type spec_info
    type imp_info
    type ann_info
    type comp_info

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word		(* word *)
	| IntLit    of LargeInt.int		(* integer *)
	| CharLit   of WideChar.char		(* character *)
	| StringLit of WideString.string	(* string *)
	| RealLit   of LargeReal.real		(* floating point *)

    (* Fixity *)

    datatype fix = Fix of fix_info * Fixity.t

    (* Identifiers *)

    datatype lab    = Lab     of lab_info * Label.t
    datatype id     = Id      of id_info * Stamp.t * Name.t
    datatype longid = ShortId of longid_info * id
		    | LongId  of longid_info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of exp_info * lit		(* literal *)
	| PrimExp   of exp_info * string * typ	(* builtin values *)
	| VarExp    of exp_info * longid	(* variable *)
	| TagExp    of exp_info * lab * int	(* tag (constructor) *)
	| ConExp    of exp_info * longid * int	(* (generative) constructor *)
	| RefExp    of exp_info			(* reference constructor *)
	| TupExp    of exp_info * exp list	(* tuple *)
	| ProdExp   of exp_info * exp row	(* row (record) *)
	| SelExp    of exp_info * lab		(* row selector *)
	| VecExp    of exp_info * exp list	(* vector *)
	| FunExp    of exp_info * match list	(* function *)
	| AppExp    of exp_info * exp * exp	(* application *)
	| CompExp   of exp_info * exp * exp	(* adjunction *)
	| AndExp    of exp_info * exp * exp	(* short-circuit conjunction *)
	| OrExp     of exp_info * exp * exp	(* short-circuit disjunction *)
	| IfExp     of exp_info * exp * exp * exp (* conditional *)
	| WhileExp  of exp_info * exp * exp	(* while loop *)
	| SeqExp    of exp_info * exp list	(* sequential expressions *)
	| CaseExp   of exp_info * exp * match list (* case *)
	| RaiseExp  of exp_info * exp		(* exception raising *)
	| HandleExp of exp_info * exp * match list (* exception handling *)
	| AnnExp    of exp_info * exp * typ	(* type annotation *)
	| LetExp    of exp_info * dec list * exp (* let *)
	| PackExp   of exp_info * mod		(* package introduction *)

    and 'a row   = Row   of 'a row_info * 'a field list * bool
    and 'a field = Field of 'a field_info * lab * 'a list

    and match    = Match of match_info * pat * exp

    (* Patterns *)

    and pat =
	  JokPat    of pat_info			(* joker (wildcard) *)
	| LitPat    of pat_info * lit		(* literal *)
	| VarPat    of pat_info * id		(* variable *)
	| TagPat    of pat_info * lab * int	(* tag (fully applied) *)
	| ConPat    of pat_info * longid * int	(* constructor (fully applied)*)
	| RefPat    of pat_info			(* reference (fully applied) *)
	| TupPat    of pat_info * pat list	(* tuple *)
	| ProdPat   of pat_info * pat row	(* row (record) *)
	| VecPat    of pat_info * pat list	(* vector *)
	| AppPat    of pat_info * pat * pat	(* constructor application *)
	| AsPat     of pat_info * pat * pat	(* as (layered) pattern *)
	| AltPat    of pat_info * pat list	(* alternative pattern *)
	| NegPat    of pat_info * pat		(* negated pattern *)
	| GuardPat  of pat_info * pat * exp	(* guarded pattern *)
	| AnnPat    of pat_info * pat * typ	(* type annotation *)
	| WithPat   of pat_info * pat * dec list (* local declarations *)

    (* Types *)

    and typ =
	  VarTyp    of typ_info * id		(* variable *)
	| ConTyp    of typ_info * longid	(* constructor *)
	| FunTyp    of typ_info * id * typ	(* type function *)
	| AppTyp    of typ_info * typ * typ	(* constructor application *)
	| RefTyp    of typ_info * typ		(* reference type *)
	| TupTyp    of typ_info * typ list	(* tuple (cartesian) type *)
	| ProdTyp   of typ_info * typ row	(* product (record) type *)
	| SumTyp    of typ_info * typ row	(* sum type (datatype) *)
	| ArrTyp    of typ_info * typ * typ	(* arrow (function) type *)
	| AllTyp    of typ_info * id * typ	(* universal quantification *)
	| ExTyp     of typ_info * id * typ	(* existential quantification *)
	| PackTyp   of typ_info * inf		(* package type *)
	| SingTyp   of typ_info * longid	(* singleton type *)
	| AbsTyp    of typ_info			(* abstract type *)
	| ExtTyp    of typ_info			(* extensible sum type *)

    (* Modules *)

    and mod =
	  PrimMod   of mod_info * string * inf	(* builtin modules *)
	| VarMod    of mod_info * id		(* module id *)
	| StrMod    of mod_info * dec list	(* structure *)
	| SelMod    of mod_info * mod * lab	(* selection *)
	| FunMod    of mod_info * id * inf * mod (* functor *)
	| AppMod    of mod_info * mod * mod	(* application *)
	| AnnMod    of mod_info * mod * inf	(* annotation *)
	| UpMod     of mod_info * mod * inf	(* coercion *)
	| LetMod    of mod_info * dec list * mod (* let *)
	| UnpackMod of mod_info * exp * inf	(* package elimination *)

    (* Interfaces *)

    and inf =
	  TopInf    of inf_info			(* top interface *)
	| ConInf    of inf_info * longid	(* interface constructor *)
	| SigInf    of inf_info * spec list	(* signature *)
	| FunInf    of inf_info * id * inf * inf (* interface function *)
	| AppInf    of inf_info * inf * mod	(* interface application *)
	| CompInf   of inf_info * inf * inf	(* composition *)
	| ArrInf    of inf_info * id * inf * inf (* arrow (functor) interface *)
	| LetInf    of inf_info * dec list * inf (* let *)
	| SingInf   of inf_info * mod		(* singleton interface *)
	| AbsInf    of inf_info			(* abstract interface *)

    (* Declarations *)

    and dec =
	  ValDec    of dec_info * pat * exp	(* values *)
	| ConDec    of dec_info * id * typ * int (* constructor *)
	| TypDec    of dec_info * id * typ	(* type *)
	| ModDec    of dec_info * id * mod	(* module *)
	| InfDec    of dec_info * id * inf	(* interface *)
	| FixDec    of dec_info * id * fix	(* fixity *)
	| VarDec    of dec_info * id * dec	(* scoped type variable *)
	| RecDec    of dec_info * dec list	(* recursive declarations *)
	| LocalDec  of dec_info * dec list	(* local declarations *)

    (* Specifications *)

    and spec =
	  ValSpec   of spec_info * id * typ	(* value *)
	| ConSpec   of spec_info * id * typ * int (* constructor *)
	| TypSpec   of spec_info * id * typ	(* type *)
	| ModSpec   of spec_info * id * inf	(* module *)
	| InfSpec   of spec_info * id * inf	(* interface *)
	| FixSpec   of spec_info * id * fix	(* fixity *)
	| RecSpec   of spec_info * spec list	(* recursive specifications *)
	| ExtSpec   of spec_info * inf		(* extension (include) *)

    (* Import *)

    and imp =
	  ValImp of imp_info * id * (typ_info,typ) desc	(* value *)
	| ConImp of imp_info * id * (typ_info,typ) desc * int (* constructor *)
	| TypImp of imp_info * id * (typ_info,typ) desc (* type *)
	| ModImp of imp_info * id * (inf_info,inf) desc (* module *)
	| InfImp of imp_info * id * (inf_info,inf) desc (* interface *)
	| FixImp of imp_info * id * (fix_info,fix) desc (* fixity *)
	| RecImp of imp_info * imp list			(* recursive items *)

    and ('info,'a) desc =
	  NoDesc   of 'info
	| SomeDesc of 'info * 'a

    (* Components *)

    and ann  = ImpAnn of ann_info * imp list * Url.t

    and comp = Comp of comp_info * ann list * dec list

    type t = comp


    (* Operations *)

    val stamp :		id	-> Stamp.t
    val name :		id	-> Name.t
    val lab :		lab	-> Label.t
    val idToLab :	id	-> lab
    val labToId :	lab	-> id

    val infoLab :	lab	-> lab_info
    val infoId :	id	-> id_info
    val infoLongid :	longid	-> longid_info
    val infoExp :	exp	-> exp_info
    val infoRow :	'a row	-> 'a row_info
    val infoField :	'a field -> 'a field_info
    val infoMatch :	match	-> match_info
    val infoPat :	pat	-> pat_info
    val infoTyp :	typ	-> typ_info
    val infoMod :	mod	-> mod_info
    val infoInf :	inf	-> inf_info
    val infoDec :	dec	-> dec_info
    val infoSpec :	spec	-> spec_info
    val infoImp :	imp	-> imp_info
    val infoAnn :	ann	-> ann_info
    val infoDesc :	('a,'b) desc -> 'a
    val infoComp :	comp	-> comp_info

  end
