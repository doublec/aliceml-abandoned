functor MakeAbstractGrammar(type info) :>
  ABSTRACT_GRAMMAR where type info = info =
  struct

    (* Generic *)

    type info = info

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
	| PrimExp   of info * string * typ	(* builtin values *)
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
	  PrimMod   of info * string * inf	(* builtin modules *)
	| VarMod    of info * id		(* module id *)
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

    (* Components *)

    and comp = Comp of info * imp list * dec list

    and imp  = Imp of info * spec list * string

    type component = comp


    (* Projections *)

    fun stamp(Id(_,x,_))		= x
    fun name(Id(_,_,n))			= n
    fun lab(Lab(_,a))			= a

    fun conToId(Con(_,x,_))		= x

    fun idToLab(Id(i,_,ExId n))		= Lab(i,n)
      | idToLab _			= Crash.crash "AbstractGrammar.idToLab"

    fun infoLab(Lab(i,_))		= i
    fun infoId(Id(i,_,_))		= i
    fun infoLongid(ShortId(i,_))	= i
      | infoLongid(LongId(i,_,_))	= i

    fun infoExp(LitExp(i,_))		= i
      | infoExp(PrimExp(i,_,_))		= i
      | infoExp(VarExp(i,_))		= i
      | infoExp(ConExp(i,_,_))		= i
      | infoExp(RefExp(i))		= i
      | infoExp(TupExp(i,_))		= i
      | infoExp(RowExp(i,_))		= i
      | infoExp(SelExp(i,_))		= i
      | infoExp(VecExp(i,_))		= i
      | infoExp(FunExp(i,_,_))		= i
      | infoExp(AppExp(i,_,_))		= i
      | infoExp(CompExp(i,_,_))		= i
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

    fun infoRow(Row(i,_,_))		= i
    fun infoField(Field(i,_,_))		= i
    fun infoMatch(Match(i,_,_))		= i

    fun infoPat(JokPat(i))		= i
      | infoPat(LitPat(i,_))		= i
      | infoPat(VarPat(i,_))		= i
      | infoPat(ConPat(i,_,_))		= i
      | infoPat(RefPat(i,_))		= i
      | infoPat(TupPat(i,_))		= i
      | infoPat(RowPat(i,_))		= i
      | infoPat(VecPat(i,_))		= i
      | infoPat(AsPat(i,_,_))		= i
      | infoPat(AltPat(i,_))		= i
      | infoPat(NegPat(i,_))		= i
      | infoPat(GuardPat(i,_,_))	= i
      | infoPat(AnnPat(i,_,_))		= i
      | infoPat(WithPat(i,_,_))		= i

    fun infoTyp(AbsTyp(i))		= i
      | infoTyp(VarTyp(i,_))		= i
      | infoTyp(ConTyp(i,_))		= i
      | infoTyp(FunTyp(i,_,_))		= i
      | infoTyp(AppTyp(i,_,_))		= i
      | infoTyp(RefTyp(i,_))		= i
      | infoTyp(TupTyp(i,_))		= i
      | infoTyp(RowTyp(i,_))		= i
      | infoTyp(ArrTyp(i,_,_))		= i
      | infoTyp(SumTyp(i,_))		= i
      | infoTyp(ExtTyp(i))		= i
      | infoTyp(AllTyp(i,_,_))		= i
      | infoTyp(ExTyp(i,_,_))		= i
      | infoTyp(SingTyp(i,_))		= i

    fun infoCon(Con(i,_,_))		= i

    fun infoMod(PrimMod(i,_,_))		= i
      | infoMod(VarMod(i,_))		= i
      | infoMod(StrMod(i,_))		= i
      | infoMod(SelMod(i,_,_))		= i
      | infoMod(FunMod(i,_,_,_))	= i
      | infoMod(AppMod(i,_,_))		= i
      | infoMod(AnnMod(i,_,_))		= i
      | infoMod(LetMod(i,_,_))		= i

    fun infoInf(AnyInf(i))		= i
      | infoInf(AbsInf(i))		= i
      | infoInf(ConInf(i,_))		= i
      | infoInf(SigInf(i,_))		= i
      | infoInf(FunInf(i,_,_,_))	= i
      | infoInf(AppInf(i,_,_))		= i
      | infoInf(CompInf(i,_,_))		= i
      | infoInf(ArrInf(i,_,_,_))	= i
      | infoInf(SingInf(i,_))		= i

    fun infoDec(ValDec(i,_,_))		= i
      | infoDec(ConDec(i,_,_))		= i
      | infoDec(TypDec(i,_,_))		= i
      | infoDec(DatDec(i,_,_))		= i
      | infoDec(ModDec(i,_,_))		= i
      | infoDec(InfDec(i,_,_))		= i
      | infoDec(RecDec(i,_))		= i
      | infoDec(TypvarDec(i,_,_))	= i
      | infoDec(LocalDec(i,_))		= i

    fun infoSpec(ValSpec(i,_,_))	= i
      | infoSpec(ConSpec(i,_,_))	= i
      | infoSpec(TypSpec(i,_,_))	= i
      | infoSpec(DatSpec(i,_,_))	= i
      | infoSpec(ModSpec(i,_,_))	= i
      | infoSpec(InfSpec(i,_,_))	= i
      | infoSpec(RecSpec(i,_))		= i
      | infoSpec(LocalSpec(i,_))	= i
      | infoSpec(ExtSpec(i,_))		= i

    fun infoComp(Comp(i,_,_))		= i
    fun infoImp(Imp(i,_,_))		= i

  end
