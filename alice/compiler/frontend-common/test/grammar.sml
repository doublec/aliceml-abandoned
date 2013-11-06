type word
type int
type char
type string
type real
datatype bool = false | true
datatype 'a list = nil | :: of 'a * 'a list

structure Stamp = struct type t end
structure Name  = struct type t end
structure Label = struct type t end

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
    type com_info

    (* Literals *)

    datatype lit =
	  WordLit   of word		(* word *)
	| IntLit    of int		(* integer *)
	| CharLit   of char		(* character *)
	| StringLit of string		(* string *)
	| RealLit   of real		(* floating point *)

    (* Fixity *)

    datatype fix = Fix of fix_info

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
	| AdjExp    of exp_info * exp * exp	(* adjunction *)
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
	| AdjInf    of inf_info * inf * inf	(* composition *)
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

    and ann = ImpAnn of ann_info * imp list * string

    and com = Com of com_info * ann list * dec list

    type t = com


    (* Operations *)

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
    val infoCom :	com	-> com_info

  end


functor MakeAbstractGrammar(type fix_info
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
			    type com_info
			    val labToIdInfo: lab_info -> id_info
			    val idToLabInfo: id_info -> lab_info
			   ) =
  struct

    (* Generic *)

    type fix_info	= fix_info
    type lab_info	= lab_info
    type id_info	= id_info
    type longid_info	= longid_info
    type exp_info	= exp_info
    type pat_info	= pat_info
    type 'a row_info	= 'a row_info
    type 'a field_info	= 'a field_info
    type match_info	= match_info
    type typ_info	= typ_info
    type con_info	= con_info
    type mod_info	= mod_info
    type inf_info	= inf_info
    type dec_info	= dec_info
    type spec_info	= spec_info
    type imp_info	= imp_info
    type ann_info	= ann_info
    type com_info	= com_info

    (* Literals *)

    datatype lit =
	  WordLit   of word		(* word *)
	| IntLit    of int		(* integer *)
	| CharLit   of char		(* character *)
	| StringLit of string		(* string *)
	| RealLit   of real		(* floating point *)

    (* Fixity *)

    datatype fix = Fix of fix_info

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
	| AdjExp    of exp_info * exp * exp	(* adjunction *)
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
	| ProdTyp   of typ_info * typ row	(* row (record) type *)
	| ArrTyp    of typ_info * typ * typ	(* arrow (function) type *)
	| SumTyp    of typ_info * typ row	(* sum type (datatype) *)
	| AllTyp    of typ_info * id * typ	(* universal quantification *)
	| ExTyp     of typ_info * id * typ	(* existential quantification *)
	| PackTyp   of typ_info * inf		(* package type *)
	| SingTyp   of typ_info * longid	(* singleton type *)
	| AbsTyp    of typ_info			(* abstract type *)
	| ExtTyp    of typ_info			(* extensible sum type *)

    and con =   Con of con_info * id * typ list	(* data constructor *)

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
	| AdjInf    of inf_info * inf * inf	(* composition *)
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

    and ann = ImpAnn of ann_info * imp list * string

    and com = Com of com_info * ann list * dec list

    type t = com


    (* Projections *)

    fun infoLab(Lab(i,_))		= i
    fun infoId(Id(i,_,_))		= i
    fun infoLongid(ShortId(i,_))	= i
      | infoLongid(LongId(i,_,_))	= i

    fun infoExp(LitExp(i,_))		= i
      | infoExp(PrimExp(i,_,_))		= i
      | infoExp(VarExp(i,_))		= i
      | infoExp(TagExp(i,_,_))		= i
      | infoExp(ConExp(i,_,_))		= i
      | infoExp(RefExp(i))		= i
      | infoExp(TupExp(i,_))		= i
      | infoExp(ProdExp(i,_))		= i
      | infoExp(SelExp(i,_))		= i
      | infoExp(VecExp(i,_))		= i
      | infoExp(FunExp(i,_))		= i
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
      | infoExp(AnnExp(i,_,_))		= i
      | infoExp(LetExp(i,_,_))		= i
      | infoExp(PackExp(i,_))		= i

    fun infoRow(Row(i,_,_))		= i
    fun infoField(Field(i,_,_))		= i
    fun infoMatch(Match(i,_,_))		= i

    fun infoPat(JokPat(i))		= i
      | infoPat(LitPat(i,_))		= i
      | infoPat(VarPat(i,_))		= i
      | infoPat(TagPat(i,_,_))		= i
      | infoPat(ConPat(i,_,_))		= i
      | infoPat(RefPat(i))		= i
      | infoPat(TupPat(i,_))		= i
      | infoPat(ProdPat(i,_))		= i
      | infoPat(VecPat(i,_))		= i
      | infoPat(AppPat(i,_,_))		= i
      | infoPat(AsPat(i,_,_))		= i
      | infoPat(AltPat(i,_))		= i
      | infoPat(NegPat(i,_))		= i
      | infoPat(GuardPat(i,_,_))	= i
      | infoPat(AnnPat(i,_,_))		= i
      | infoPat(WithPat(i,_,_))		= i

    fun infoTyp(VarTyp(i,_))		= i
      | infoTyp(ConTyp(i,_))		= i
      | infoTyp(FunTyp(i,_,_))		= i
      | infoTyp(AppTyp(i,_,_))		= i
      | infoTyp(RefTyp(i,_))		= i
      | infoTyp(TupTyp(i,_))		= i
      | infoTyp(ProdTyp(i,_))		= i
      | infoTyp(SumTyp(i,_))		= i
      | infoTyp(ArrTyp(i,_,_))		= i
      | infoTyp(AllTyp(i,_,_))		= i
      | infoTyp(ExTyp(i,_,_))		= i
      | infoTyp(PackTyp(i,_))		= i
      | infoTyp(SingTyp(i,_))		= i
      | infoTyp(ExtTyp(i))		= i
      | infoTyp(AbsTyp(i))		= i

    fun infoMod(PrimMod(i,_,_))		= i
      | infoMod(VarMod(i,_))		= i
      | infoMod(StrMod(i,_))		= i
      | infoMod(SelMod(i,_,_))		= i
      | infoMod(FunMod(i,_,_,_))	= i
      | infoMod(AppMod(i,_,_))		= i
      | infoMod(AnnMod(i,_,_))		= i
      | infoMod(UpMod(i,_,_))		= i
      | infoMod(LetMod(i,_,_))		= i
      | infoMod(UnpackMod(i,_,_))	= i

    fun infoInf(TopInf(i))		= i
      | infoInf(ConInf(i,_))		= i
      | infoInf(SigInf(i,_))		= i
      | infoInf(FunInf(i,_,_,_))	= i
      | infoInf(AppInf(i,_,_))		= i
      | infoInf(AdjInf(i,_,_))		= i
      | infoInf(ArrInf(i,_,_,_))	= i
      | infoInf(LetInf(i,_,_))		= i
      | infoInf(SingInf(i,_))		= i
      | infoInf(AbsInf(i))		= i

    fun infoDec(ValDec(i,_,_))		= i
      | infoDec(ConDec(i,_,_,_))	= i
      | infoDec(TypDec(i,_,_))		= i
      | infoDec(ModDec(i,_,_))		= i
      | infoDec(InfDec(i,_,_))		= i
      | infoDec(FixDec(i,_,_))		= i
      | infoDec(VarDec(i,_,_))		= i
      | infoDec(RecDec(i,_))		= i
      | infoDec(LocalDec(i,_))		= i

    fun infoSpec(ValSpec(i,_,_))	= i
      | infoSpec(ConSpec(i,_,_,_))	= i
      | infoSpec(TypSpec(i,_,_))	= i
      | infoSpec(ModSpec(i,_,_))	= i
      | infoSpec(InfSpec(i,_,_))	= i
      | infoSpec(FixSpec(i,_,_))	= i
      | infoSpec(RecSpec(i,_))		= i
      | infoSpec(ExtSpec(i,_))		= i

    fun infoImp(ValImp(i,_,_))		= i
      | infoImp(ConImp(i,_,_,_))	= i
      | infoImp(TypImp(i,_,_))		= i
      | infoImp(ModImp(i,_,_))		= i
      | infoImp(InfImp(i,_,_))		= i
      | infoImp(FixImp(i,_,_))		= i
      | infoImp(RecImp(i,_))		= i

    fun infoAnn(ImpAnn(i,_,_))		= i

    fun infoDesc(NoDesc(i))		= i
      | infoDesc(SomeDesc(i,_))		= i

    fun infoCom(Com(i,_,_))		= i

  end


structure AbstractInfo =
  struct
    type fix_info	= (int * int) * (int * int)
    type lab_info	= (int * int) * (int * int)
    type id_info	= (int * int) * (int * int)
    type longid_info	= (int * int) * (int * int)
    type exp_info	= (int * int) * (int * int)
    type pat_info	= (int * int) * (int * int)
    type 'a row_info	= (int * int) * (int * int)
    type 'a field_info	= (int * int) * (int * int)
    type match_info	= (int * int) * (int * int)
    type typ_info	= (int * int) * (int * int)
    type con_info	= (int * int) * (int * int)
    type mod_info	= (int * int) * (int * int)
    type inf_info	= (int * int) * (int * int)
    type dec_info	= (int * int) * (int * int)
    type spec_info	= (int * int) * (int * int)
    type imp_info	= (int * int) * (int * int)
    type ann_info	= (int * int) * (int * int)
    type com_info	= (int * int) * (int * int)

    fun labToIdInfo r	= r
    fun idToLabInfo r	= r
  end

structure AbstractGrammar = MakeAbstractGrammar(AbstractInfo)


signature TRANSLATION_PHASE =
  sig
    structure I : ABSTRACT_GRAMMAR = AbstractGrammar
    structure O : ABSTRACT_GRAMMAR = AbstractGrammar

    val translate : I.com -> O.com
  end

type com = AbstractGrammar.com
