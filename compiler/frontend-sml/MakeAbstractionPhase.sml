(*
On Constructors:

The abstract language only provides unary contructors. Nullary constructors
are mapped into constructors of the special type zero. (We don't use unit,
because for imports we need to be able to recognise nullary constructors
from there type. This is not possible with unit since the user can legaly
declare unary constructors of argument type unit.)

Moreover, the abstract grammar has no constructor declarations. Instead,
it provides first-class constructors. We have to map this as well. First-class
constructors use the special type conarrow, which also encodes the constructors
syntactic arity, through the types succ and zero.

The following table shows how constructor related declarations, expressions,
and patterns are mapped into the abstract syntax (not showing type
declarations in the abstract grammar):

Declarations:
datatype t = A			val 'A = lab A : conarrow (zero -> t) zero
				val  A = 'A(fail)
           | B of int		val 'B = lab B : conarrow (int -> t) (succ zero)
				val  B = fun x -> 'B(x)
constructor C : u		val 'C = new : conarrow (zero -> u) zero
				val  C = 'C(fail)
constructor D of int : u	val 'D = new : conarrow (int -> u) (succ zero)
				val  D = fun x -> 'D(x)
datatype t = datatype M.t	val 'A = M.'A  val A = M.A
				val 'B = M.'B  val B = M.B
constructor E = C		val 'E = 'C  val E = C
__primitive
constructor F : u = "s"		val 'F = prim "s": conarrow (int -> u) zero
				val  F = 'F(fail)
__primitive
constructor G of int : u = "s"	val 'G = prim "s": conarrow(int -> u)(succ zero)
				val  G = fun x -> 'G(x)
__primitive
__reftype 'a r = ref of 'a	val 'ref = fail: conarrow('a->'a ref)(succ zero)
				val  ref = fun x -> Ref(x)

Expressions:
A				'A(fail)
B 0				'B(0)
B				B
C				'C(fail)
D 0				'D(0)
D				D
ref 0				Ref(0)
ref				ref

Patterns:
A				'A(_)
B 0				'B(0)
C				'C(_)
D 0				'D(0)
ref 0				'Ref(0)

Treatment of datatypes is still unsatisfactory for 3 reasons:

- TagExp's need to carry an optional longid
  (where an implicit type annotation is required).
- We had to introduce LabExp's
  (just to provide these implicit annotations in a very indirect way).
- Useless declarations and structure fields are generated for all datatype
  constructors, which are just FailExp's.

But still I see no better way. Ideally we should just abstract all appearances
of datatype constructors into TagExp's/TagPat's plus suitable type annotations
via AnnExp's/AnnPat's, but the corresponding type terms cannot be build solely
by syntactic transformations, since there is the problem of scoping and hiding
of type identifiers.

*)


functor MakeAbstractionPhase(
		val loadSign: Source.desc * Url.t -> Inf.sign
		structure Switches : SWITCHES
	) :> ABSTRACTION_PHASE =
  struct

    structure C   = BindEnv
    structure I   = InputGrammar
    structure O   = AbstractGrammar
    structure E   = AbstractionError

    open I
    open BindEnv


  (* Error handling *)

    val error = E.error
    val warn  = E.warn

    fun errorVId(E, vid', Error) =
	error((#1 o Option.valOf o lookupVal)(E, vid'), Error vid')


  (* Miscellanous helpers *)

    fun conName s  = "'" ^ s
    fun conVId vid = VId.fromString(conName(VId.toString vid))

    fun inventId i = O.Id(i, Stamp.new(), Name.InId)

    fun idToLab(O.Id(i, _, name))        = O.Lab(i, Label.fromName name)
    fun longidToLab(O.ShortId(_, id))    = idToLab id
      | longidToLab(O.LongId(_, _, lab)) = lab

    fun modlongidToMod(O.ShortId(i, modid))            = O.VarMod(i, modid)
      | modlongidToMod(O.LongId(i, modlongid, modlab)) =
	    O.SelMod(i, modlab, modlongidToMod modlongid)

    fun varToTyp varid = O.VarTyp(O.infoId varid, varid)


    val lab_zero	= Label.fromString "zero"	(*UGLY*)
    val lab_succ	= Label.fromString "succ"
    val lab_conarrow	= Label.fromString "conarrow"
    val strid_pervasive	= StrId.fromString
				(Name.toString(PervasiveType.name_pervasive))

    fun contyp E (i, lab) =
	case lookupStr(E, strid_pervasive)
	  of NONE            => error(i, E.StrIdUnbound strid_pervasive)
	   | SOME(_,stamp,_) =>
		O.ConTyp(i,
		  O.LongId(i,
		     O.ShortId(i, O.Id(i, stamp, PervasiveType.name_pervasive)),
		     O.Lab(i, lab)))

    fun aritytyp E (i, 0) = contyp E (i, lab_zero)
      | aritytyp E (i, k) = O.AppTyp(i, contyp E (i, lab_succ),
					aritytyp E (i,k-1))

    fun conarrowtyp E (i, argtyp, typ, k) =
	O.AppTyp(i,
	    O.AppTyp(i,
		contyp E (i, lab_conarrow),
		O.ArrTyp(i, argtyp, typ)),
		aritytyp E (i, k))


    fun tupexp(i, #[exp]) = exp
      | tupexp(i,   exps) = O.TupExp(i, exps)

    fun tuppat(i, #[pat]) = pat
      | tuppat(i,   pats) = O.TupPat(i, pats)

    fun annexp(exp, typs) =
	List.foldl (fn(typ,exp) => O.AnnExp(O.infoTyp typ, exp, typ)) exp typs

    fun funexp(valids, exp) =
	List.foldr (fn(valid,exp) =>
	     let val i = Source.over(O.infoId valid, O.infoExp exp) in
		O.FunExp(i, #[O.Match(i, O.VarPat(O.infoId valid, valid), exp)])
	     end
	    ) exp valids

    fun alltyp(varids, typ) =
	List.foldr (fn(varid,typ) =>
		O.AllTyp(O.infoTyp typ, varid, typ)
	    ) typ varids

    fun funtyp(varids, typ) =
	List.foldr (fn(varid,typ) =>
		O.FunTyp(O.infoTyp typ, varid, typ)
	    ) typ varids

    fun apptyp(typs, typ) =
	List.foldl (fn(typ1,typ2) =>
	      O.AppTyp(Source.over(O.infoTyp typ1, O.infoTyp typ2), typ2, typ1)
	    ) typ typs

    fun arrtyp(typs, typ) =
	List.foldr (fn(typ1,typ2) =>
	      O.ArrTyp(Source.over(O.infoTyp typ1, O.infoTyp typ2), typ1, typ2)
	    ) typ typs

    fun funinf(modid_infs, inf) =
	List.foldr (fn((modid,inf1),inf2) =>
	      O.FunInf(Source.over(O.infoId modid, O.infoInf inf2),
		       modid, inf1, inf2)
	    ) inf modid_infs

    fun vardec(varids, dec) =
	List.foldr (fn(varid,dec) =>
		O.VarDec(O.infoId varid, varid, dec)
	    ) dec varids


    fun lookupIdStatus(E, vid') =
	case lookupVal(E, vid')
	  of NONE             => V
	   | SOME(i,stamp,is) => is



  (* Syntactic arity of constructors *)

    fun typArity(O.TupTyp(_,typs'))	= Int.max(1, Vector.length typs')
      | typArity(O.ProdTyp(_,row'))	= Int.max(1, rowArity row')
      | typArity _			= 1

    and rowArity(O.Row(_,fields',_))	= Vector.length fields'




  (* Constants and identifiers *)

    fun trSCon E =
	fn SCon(i, SCon.INT n)		=> O.IntLit n
	 | SCon(i, SCon.WORD w)		=> O.WordLit w
	 | SCon(i, SCon.CHAR c)		=> O.CharLit c
	 | SCon(i, SCon.STRING s)	=> O.StringLit s
	 | SCon(i, SCon.REAL x)		=> O.RealLit x

    fun trLab E (Lab(i, lab)) = O.Lab(i, Label.fromString(Lab.toString lab))

    fun trTyVar E (tyvar as TyVar(i, tyvar')) =
	let
	    val (_,stamp) =
		case lookupVar(E, tyvar')
		  of SOME xx => xx
		   | NONE    => error(i, E.TyVarUnbound tyvar')
	in
	    O.Id(i, stamp, Name.ExId(TyVar.toString tyvar'))
	end

    fun trId (lookup,infoId,idId,toString,Unbound) E id =
	let
	    val i   = infoId id
	    val id' = idId id
	    val (_,stamp,x) = case lookup(E, id')
				of SOME xx => xx
				 | NONE    => error(i, Unbound id')
	in
	    ( O.Id(i, stamp, Name.ExId(toString id')), x )
	end

    val trVId   = trId(lookupVal, infoVId, idVId, VId.toString, E.VIdUnbound)
    val trTyCon = trId(lookupTy, infoTyCon, idTyCon,
			TyCon.toString, E.TyConUnbound)
    val trStrId = trId(lookupStr, infoStrId, idStrId,
			StrId.toString, E.StrIdUnbound)
    val trSigId = trId(lookupSig, infoSigId, idSigId,
			SigId.toString, E.SigIdUnbound)


    fun trId_bind (lookup,infoId,idId,toString,Shadowed) E id =
	let
	    val i     = infoId id
	    val id'   = idId id
	    val name  = toString id'
	    val stamp = Stamp.new()
	    val _     = if !Switches.Warn.shadowing
			andalso Option.isSome(lookup(E, id')) then
			   warn(i, Shadowed id')
			else ()
	in
	    ( O.Id(i, stamp, Name.ExId name), stamp )
	end


    val trTyVar_bind = trId_bind(lookupVar, infoTyVar, idTyVar, TyVar.toString,
				 E.TyVarShadowed)
    val trVId_bind   = trId_bind(lookupVal, infoVId,   idVId,   VId.toString,
				 E.VIdShadowed)
    val trTyCon_bind = trId_bind(lookupTy,  infoTyCon, idTyCon, TyCon.toString,
				 E.TyConShadowed)
    val trStrId_bind = trId_bind(lookupStr, infoStrId, idStrId, StrId.toString,
				 E.StrIdShadowed)
    val trSigId_bind = trId_bind(lookupSig, infoSigId, idSigId, SigId.toString,
				 E.SigIdShadowed)

    fun trVId_bind' E (VId(i, vid')) =
	let
	    val name = VId.toString vid'
	in
	    O.Lab(i, Label.fromString name)
	end

    fun trConVId_bind' E (VId(i, vid')) =
	let
	    val name  = conName(VId.toString vid')
	    val stamp = Stamp.new()
	in
	    ( O.Id(i, stamp, Name.ExId name), stamp )
	end


    (* With polymorphic recursion we could avoid the following code
       duplication... *)

    fun trLongStrId' E =
	fn SHORTLong(i, strid) =>
	   let
		val (modid',E') = trStrId E strid
	   in
		( SOME(O.ShortId(i,modid')), E' )
	   end

	 | DOTLong(i, longstrid, strid) =>
	   let
		val (modlongido',E') = trLongStrId' E longstrid
		val (modid',x)       = trStrId E' strid
		val  modlongid'      =
		     case modlongido'
		       of NONE            => O.ShortId(i, modid')
			| SOME modlongid' => O.LongId(i, modlongid',
							 idToLab modid')
	   in
		( SOME modlongid', x )
	   end

    fun trLongId trId E =
	fn SHORTLong(i, id) =>
	   let
		val (id',x) = trId E id
	   in
		( O.ShortId(i,id'), x )
	   end

	 | DOTLong(i, longstrid, id) =>
	   let
		val (modlongido',E') = trLongStrId' E longstrid
		val (id',x)          = trId E' id
		val  longid'         =
		     case modlongido'
		       of NONE            => O.ShortId(i, id')
			| SOME modlongid' => O.LongId(i, modlongid',idToLab id')
	   in
		( longid', x )
	   end

    fun trLongVId E   = trLongId trVId E
    fun trLongTyCon E = trLongId trTyCon E
    fun trLongStrId E = trLongId trStrId E
    fun trLongSigId E = trLongId trSigId E

    fun trConLongVId E longvid =
	case #1(trLongVId E longvid)
	  of O.LongId(i1, modid', O.Lab(i2, lab')) =>
		O.LongId(i1, modid',
		    O.Lab(i2, Label.fromString(conName(Label.toString lab'))))

	   | O.ShortId(i1, O.Id(i2, stamp, name)) =>
	     let
		val name' = conName(Name.toString name)
	     in
		case lookupVal(E, VId.fromString name')
		  of NONE => raise Crash.Crash "AbstractionPhase.trConLongVId"
		   | SOME(i, stamp', is) =>
			O.ShortId(i1, O.Id(i2, stamp', Name.ExId name'))
	     end


  (* Calculate sets of unguarded explicit type variables [Section 4.6] *)

    fun ? tyvarsX E  NONE    = []
      | ? tyvarsX E (SOME x) = tyvarsX E x

    fun unguardedTyVarsAtExp E (RECORDAtExp(_, exprow_opt)) =
	    ?unguardedTyVarsExpRow E exprow_opt

      | unguardedTyVarsAtExp E ( TUPLEAtExp(_, exps)
			       | VECTORAtExp(_, exps)
			       | SEQAtExp(_, exps) ) =
	    List.concat(List.map (unguardedTyVarsExp E) exps)

      | unguardedTyVarsAtExp E (LETAtExp(_, dec, exp)) =
	    unguardedTyVarsDec E dec @ unguardedTyVarsExp E exp

      | unguardedTyVarsAtExp E (PARAtExp(_, exp)) =
	    unguardedTyVarsExp E exp

      | unguardedTyVarsAtExp E _ = []

    and unguardedTyVarsExpRow E (ROWExpRow(_, lab, exp, exprow_opt)) =
	    unguardedTyVarsExp E exp @ ?unguardedTyVarsExpRow E exprow_opt

    and unguardedTyVarsExp E (ATEXPExp(_, atexp)) =
	    unguardedTyVarsAtExp E atexp

      | unguardedTyVarsExp E (APPExp(_, exp, atexp)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsAtExp E atexp

      | unguardedTyVarsExp E (TYPEDExp(_, exp, ty)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsTy E ty

      | unguardedTyVarsExp E ( ANDALSOExp(_, exp1, exp2)
			     | ORELSEExp(_, exp1, exp2) ) =
	    unguardedTyVarsExp E exp1 @ unguardedTyVarsExp E exp2

      | unguardedTyVarsExp E (HANDLEExp(_, exp, match)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsMatch E match

      | unguardedTyVarsExp E (RAISEExp(_, exp)) =
	    unguardedTyVarsExp E exp

      | unguardedTyVarsExp E (IFExp(_, exp1, exp2, exp3)) =
	    unguardedTyVarsExp E exp1 @ unguardedTyVarsExp E exp2 @
	    unguardedTyVarsExp E exp3

      | unguardedTyVarsExp E (CASEExp(_, exp, match)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsMatch E match

      | unguardedTyVarsExp E (FNExp(_, match)) =
	    unguardedTyVarsMatch E match

      | unguardedTyVarsExp E (PACKExp(_, longstrid)) =
	    []

    and unguardedTyVarsMatch E (Match(_, mrule, match_opt)) =
	    unguardedTyVarsMrule E mrule @ ?unguardedTyVarsMatch E match_opt

    and unguardedTyVarsMrule E (Mrule(_, pat, exp)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsExp E exp

    and unguardedTyVarsDec E (CONSTRUCTORDec(_, dconbind)) =
	    unguardedTyVarsDconBind E dconbind

      | unguardedTyVarsDec E (STRUCTUREDec(_, strbind)) =
	    unguardedTyVarsStrBind E strbind

      | unguardedTyVarsDec E ( LOCALDec(_, dec1, dec2)
			     | SEQDec(_, dec1, dec2) ) =
	    unguardedTyVarsDec E dec1 @ unguardedTyVarsDec E dec2

      | unguardedTyVarsDec E _ = []

    and unguardedTyVarsValBind E (PLAINValBind(_, pat, exp, valbind_opt)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsExp E exp @
	    ?unguardedTyVarsValBind E valbind_opt

      | unguardedTyVarsValBind E (RECValBind(_, valbind)) =
	    unguardedTyVarsValBind E valbind

    and unguardedTyVarsFvalBind E (FvalBind(_, match, fvalbind_opt)) =
	    unguardedTyVarsMatch E match @
	    ?unguardedTyVarsFvalBind E fvalbind_opt

    and unguardedTyVarsDconBind E (NEWDconBind(_, _, vid, ty_opt, tyvarseq,
						     longtycon, dconbind_opt)) =
	let
	    val  _   = insertScope E
	    val  _   = trTyVarSeq E tyvarseq
	    val  _   = insertScope E
	    val ids' = ?unguardedTyVarsTy E ty_opt
	    val  E'  = splitScope E
	    val  _   = deleteScope E
	    val  _   = union(E,E')
	in
	    ids' @ ?unguardedTyVarsDconBind E dconbind_opt
	end

      | unguardedTyVarsDconBind E (EQUALDconBind(_, _,vid, _,longvid,
								dconbind_opt)) =
	    ?unguardedTyVarsDconBind E dconbind_opt

    and unguardedTyVarsStrBind E (StrBind(_, strid, strexp, strbind_opt)) =
	    unguardedTyVarsStrExp E strexp @
	    ?unguardedTyVarsStrBind E strbind_opt

    and unguardedTyVarsAtPat E (RECORDAtPat(_, patrow_opt)) =
	    ?unguardedTyVarsPatRow E patrow_opt

      | unguardedTyVarsAtPat E ( TUPLEAtPat(_, pats)
			       | VECTORAtPat(_, pats)
			       | ALTAtPat(_, pats) ) =
	    List.concat(List.map (unguardedTyVarsPat E) pats)

      | unguardedTyVarsAtPat E (PARAtPat(_, pat)) =
	    unguardedTyVarsPat E pat

      | unguardedTyVarsAtPat E _ = []

    and unguardedTyVarsPatRow E (WILDCARDPatRow(_)) = []

      | unguardedTyVarsPatRow E (ROWPatRow(_, lab, pat, patrow_opt)) =
	    unguardedTyVarsPat E pat @ ?unguardedTyVarsPatRow E patrow_opt

    and unguardedTyVarsPat E (ATPATPat(_, atpat)) =
	    unguardedTyVarsAtPat E atpat

      | unguardedTyVarsPat E (APPPat(_, pat, atpat)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsAtPat E atpat

      | unguardedTyVarsPat E (TYPEDPat(_, pat, ty)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsTy E ty

      | unguardedTyVarsPat E (NONPat(_, pat)) =
	    unguardedTyVarsPat E pat

      | unguardedTyVarsPat E (ASPat(_, pat1, pat2)) =
	    unguardedTyVarsPat E pat1 @ unguardedTyVarsPat E pat2

      | unguardedTyVarsPat E (WHEREPat(_, pat, atexp)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsAtExp E atexp

      | unguardedTyVarsPat E (WITHVALPat(_, pat, valbind)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsValBind E valbind

      | unguardedTyVarsPat E (WITHFUNPat(_, pat, fvalbind)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsFvalBind E fvalbind

    and unguardedTyVarsTy E (TYVARTy(_, tyvar as TyVar(i,tyvar'))) =
	if Option.isSome(lookupVar(E, tyvar')) then
	    []
	else
	let
	    val (id',stamp) = trTyVar_bind E tyvar
	    val  _          = insertVar(E, tyvar', (i, stamp))
	in
	    [id']
	end

      | unguardedTyVarsTy E (RECORDTy(_, tyrow_opt)) =
	    ?unguardedTyVarsTyRow E tyrow_opt

      | unguardedTyVarsTy E (TUPLETy(_, tys)) =
	    List.concat(List.map (unguardedTyVarsTy E) tys)

      | unguardedTyVarsTy E (TYCONTy(_, tyseq, longtycon)) =
	    unguardedTyVarsTyseq E tyseq

      | unguardedTyVarsTy E (ARROWTy(_, ty, ty')) =
	    unguardedTyVarsTy E ty @ unguardedTyVarsTy E ty'

      | unguardedTyVarsTy E (PACKTy(_, longsigid)) =
	    []

      | unguardedTyVarsTy E (PARTy(_, ty)) =
	    unguardedTyVarsTy E ty

    and unguardedTyVarsTyRow E (ROWTyRow(_, lab, ty, tyrow_opt)) =
	    unguardedTyVarsTy E ty @ ?unguardedTyVarsTyRow E tyrow_opt

    and unguardedTyVarsTyseq E (Seq(_, tys)) =
	    List.concat(List.map (unguardedTyVarsTy E) tys)

    and unguardedTyVarsAtStrExp E (STRUCTAtStrExp(_, dec)) =
	    unguardedTyVarsDec E dec

      | unguardedTyVarsAtStrExp E (LONGSTRIDAtStrExp(_, longstrid)) =
	    []

      | unguardedTyVarsAtStrExp E (LETAtStrExp(_, dec, strexp)) =
	    unguardedTyVarsDec E dec @ unguardedTyVarsStrExp E strexp

      | unguardedTyVarsAtStrExp E (PARAtStrExp(_, strexp)) =
	    unguardedTyVarsStrExp E strexp

    and unguardedTyVarsStrExp E (ATSTREXPStrExp(_, atstrexp)) =
	    unguardedTyVarsAtStrExp E atstrexp

      | unguardedTyVarsStrExp E (APPStrExp(_, strexp, atstrexp)) =
	    unguardedTyVarsStrExp E strexp @ unguardedTyVarsAtStrExp E atstrexp

      | unguardedTyVarsStrExp E ( TRANSStrExp(_, strexp, _)
				| OPAQStrExp(_, strexp, _)
				| FCTStrExp(_, _, strexp) ) =
	    unguardedTyVarsStrExp E strexp

      | unguardedTyVarsStrExp E (UNPACKStrExp(_, exp, sigexp)) =
	    unguardedTyVarsExp E exp

      (*UNFINISHED: if we have LETSigExp then we must check sigexps as well*)



  (* Expressions *)

    and trAtExp E =
	fn SCONAtExp(i, scon)          => O.LitExp(i, trSCon E scon)
	 | LONGVIDAtExp(i, _, longvid) =>
	   (case trLongVId E longvid
	      of (vallongid', T 0) => O.TagExp(i, longidToLab vallongid',
						  SOME(trConLongVId E longvid),
						  O.FailExp(i))
	       | (vallongid', C 0) => O.ConExp(i, trConLongVId E longvid,
						  O.FailExp(i))
	       | (vallongid', _)   => O.VarExp(i, vallongid')
	   )
	 | RECORDAtExp(i, exprowo) =>
	   let
		val  _   = insertScope E
		val row' = trExpRowo E exprowo
		val  _   = deleteScope E
	   in
		O.ProdExp(i, row')
	   end
	 | UPDATEAtExp(i, atexp, exprow) =>
	   let
		val exp' = trAtExp E atexp
		val  _   = insertScope E
		val row' = trExpRowo E (SOME exprow)
		val  _   = deleteScope E
	   in
		O.UpdExp(i, exp', row')
	   end
	 | HASHAtExp(i, lab) =>
	   let
		val id'    = O.Id(i, Stamp.new(), Name.InId)
		val pat'   = O.VarPat(i, id')
		val exp'   = O.VarExp(i, O.ShortId(i, id'))
		val match' = O.Match(i, pat', O.SelExp(i, trLab E lab, exp'))
	   in
		O.FunExp(i, #[match'])
	   end
	 | TUPLEAtExp(i, exps)		=> O.TupExp(i, trExps E exps)
	 | VECTORAtExp(i, exps)		=> O.VecExp(i, trExps E exps)
	 | SEQAtExp(i, exps)		=> O.SeqExp(i, trExps E exps)
	 | LETAtExp(i, dec, exp)	=>
	   let
		val  _   = insertScope E
		val dec' = trDec E dec
		val exp' = trExp E exp
		val  _   = deleteScope E
	   in
		O.LetExp(i, dec', exp')
	   end
	 | PARAtExp(i, exp)		=> trExp E exp


    and trExpRowo E exprowo =
	let
	    val (i, fields') = trExpRowo' E exprowo
	in
	    O.Row(i, Vector.fromList fields', false)
	end
    and trExpRowo' E =
	fn NONE => (Source.nowhere, [])

	 | SOME(ROWExpRow(i, lab as Lab(i',lab'), exp, exprowo)) =>
	   let
		val i1'    = Source.over(i', infoExp exp)
		val field' = O.Field(i1', trLab E lab, trExp E exp)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
				error(i', E.ExpRowLabDuplicate lab')
		val (_,fields') = trExpRowo' E exprowo
	   in
		(i, field'::fields')
	   end



    and trExps E exps = Vector.fromList(List.map (trExp E) exps)

    and trExp E =
	fn exp as (ATEXPExp _|APPExp _)	=> trAppExp E (Infix.exp (infEnv E) exp)
 	 | TYPEDExp(i, exp, ty)		=> O.AnnExp(i,trExp E exp, trTy E ty)
	 | ANDALSOExp(i, exp1, exp2)	=> O.AndExp(i,trExp E exp1,trExp E exp2)
	 | ORELSEExp(i, exp1, exp2)	=> O.OrExp(i,trExp E exp1, trExp E exp2)
	 | HANDLEExp(i, exp, match)	=>
		O.HandleExp(i, trExp E exp, trMatcho E (SOME match))

	 | RAISEExp(i, exp)		=> O.RaiseExp(i, trExp E exp)
	 | IFExp(i, exp1, exp2, exp3)	=>
		O.IfExp(i, trExp E exp1, trExp E exp2, trExp E exp3)

	 | CASEExp(i, exp, match) =>
		O.CaseExp(i, trExp E exp, trMatcho E (SOME match))

	 | FNExp(i, match) =>
	   let
		val i'       = infoMatch match
		val matches' = trMatcho E (SOME match)
	   in
		O.FunExp(i, matches')
	   end

	 | PACKExp(i, longstrid) =>
	   let
		val (modlongid',E') = trLongStrId E longstrid
		val  mod'           = modlongidToMod modlongid'
	   in
		O.PackExp(i, mod')
	   end


    and trAppExp E =
	fn APPExp(i, exp, atexp) => (trAppliedAppExp E exp) (i, trAtExp E atexp)
	 | ATEXPExp(i, atexp)    => trAtExp E atexp
	 | exp                   => trExp E exp

    and trAppliedAppExp E =
	fn APPExp(i, exp, atexp) =>
	   let
		val exp1' = (trAppliedAppExp E exp) (i, trAtExp E atexp)
	   in
		fn(i',exp2') => O.AppExp(i', exp1', exp2')
	   end
	 | ATEXPExp(i, atexp) =>
	        trAppliedAtExp E atexp
	 | exp =>
	   let
	        val exp1' = trExp E exp
	   in
		fn(i',exp2') => O.AppExp(i', exp1', exp2')
	   end

    and trAppliedAtExp E =
	fn LONGVIDAtExp(i, _, longvid)	=>
	   (case trLongVId E longvid
	      of (vallongid', V) =>
		 ( fn(i',exp') => O.AppExp(i', O.VarExp(i, vallongid'), exp') )

	       | (vallongid', (T 0 | C 0)) =>
		 ( fn(i',exp') => error(i', E.ExpConArgSuperfluous) )

	       | (vallongid', T k) =>
		 ( fn(i',exp') => O.TagExp(i', longidToLab vallongid',
					   SOME(trConLongVId E longvid), exp') )
	       | (vallongid', C k) =>
		 ( fn(i',exp') => O.ConExp(i', trConLongVId E longvid, exp') )

	       | (vallongid', R) =>
		 ( fn(i',exp') => O.RefExp(i', exp') )
	   )
	 | HASHAtExp(i, lab) =>
	   let
		val lab' = trLab E lab
	   in
		fn(i',exp') => O.SelExp(i', lab', exp')
	   end
	 | PARAtExp(i, exp) =>
		trAppliedExp E exp
	 | atexp =>
	   let
		val exp1' = trAtExp E atexp
	   in
		fn(i',exp2') => O.AppExp(i', exp1', exp2')
	   end

    and trAppliedExp E =
	fn exp as (ATEXPExp _|APPExp _)	=>
		trAppliedAppExp E (Infix.exp (infEnv E) exp)
 	 | exp =>
	   let
		val exp1' = trExp E exp
	   in
		fn(i',exp2') => O.AppExp(i', exp1', exp2')
	   end


  (* Matches and patterns *)

    and trMatcho  E matcho = Vector.rev(Vector.fromList(trMatcho'(E,[]) matcho))
    and trMatcho'(E,acc) =
	fn NONE => acc
	 | SOME(Match(i, mrule, matcho)) =>
	   let
		val match' = trMrule E mrule
	   in
		trMatcho' (E, match'::acc) matcho
	   end

    and trMrule E (Mrule(i, pat, exp)) =
	let
		val E'   = BindEnv.new()
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E,E')
		val exp' = trExp E exp
		val  _   = deleteScope E
	in
		O.Match(i, pat', exp')
	end


    and trAtPat (E,E') =
	fn WILDCARDAtPat(i)	=> O.JokPat(i)
	 | SCONAtPat(i, scon)	=> O.LitPat(i, trSCon E scon)
	 | LONGVIDAtPat(_, _, longvid as SHORTLong(i, vid as VId(i',vid'))) =>
	   (case lookupIdStatus(E, vid')
	      of  T 0 => O.TagPat(i, idToLab(#1(trVId E vid)),
				     SOME(trConLongVId E longvid), O.JokPat(i))
	       |  C 0 => O.ConPat(i, trConLongVId E longvid, O.JokPat(i))
	       |  V   =>
		 let
		    (* If inside an alternative pattern then E' contains
		     * an upper scope where the variable is already bound.
		     * We have to reuse the stamp found there.
		     *)
		    val _ = if Option.isSome(lookupScopeVal(E', vid')) then
			       error(i', E.PatVIdDuplicate vid')
			    else ()
		    val (valid',stamp) =
			case lookupVal(E', vid')
			  of NONE            => trVId_bind E vid
			   | SOME(_,stamp,_) => ( O.Id(i', stamp,
						   Name.ExId(VId.toString vid'))
						, stamp )
		    val _ = insertVal(E', vid', (i',stamp,V))
		 in
		    O.VarPat(i, valid')
		 end
	       | (T _ | C _ | R) => error(i, E.PatConArgMissing)
	   )
	 | LONGVIDAtPat(i, _, longvid) =>
	   (case trLongVId E longvid
	      of (vallongid', T 0) => O.TagPat(i, longidToLab vallongid',
						  SOME(trConLongVId E longvid),
						  O.JokPat(i))
	       | (vallongid', C 0) => O.ConPat(i, trConLongVId E longvid,
						  O.JokPat(i))
	       | (vallongid', V)   => error(i, E.PatLongVIdVar)
	       | (vallongid', _)   => error(i, E.PatConArgMissing)
	   )
	 | RECORDAtPat(i, patrowo) =>
	   let
		val  _   = insertScope E
		val row' = trPatRowo (E,E') patrowo
		val  _   = deleteScope E
	   in
		O.ProdPat(i, row')
	   end

	 | TUPLEAtPat(i, pats)     => O.TupPat(i, trPats (E,E') pats)
	 | VECTORAtPat(i, pats)    => O.VecPat(i, trPats (E,E') pats)
	 | ALTAtPat(i, pats)       =>
	   let
		val  _    = insertScope E'
		val pat'  = trPat (E,E') (List.hd pats)
		val pats' = trAltPats (E,E') (List.tl pats)
		val  _    = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		O.AltPat(i, Vector.fromList(pat'::pats'))
	   end

	 | PARAtPat(i, pat) => trPat (E,E') pat


    and trPatRowo (E,E') patrowo =
	let
	    val (i, fields', dots') = trPatRowo' (E,E') patrowo
	in
	    O.Row(i, Vector.fromList fields', dots')
	end
    and trPatRowo' (E,E') =
	fn NONE => (Source.nowhere, [], false)

	 | SOME(WILDCARDPatRow(i)) => (i, [], true)

	 | SOME(ROWPatRow(i, lab as Lab(i',lab'), pat, patrowo)) =>
	   let
		val i1'    = Source.over(i', infoPat pat)
		val field' = O.Field(i1', trLab E lab, trPat (E,E') pat)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
				error(i', E.PatRowLabDuplicate lab')
		val (_,fields',dots') = trPatRowo' (E,E') patrowo
	   in
		(i, field'::fields', dots')
	   end


    and trPat (E,E') =
	fn pat as (ATPATPat _ | APPPat _) =>
		trAppPat (E,E') (Infix.pat (infEnv E) pat)

	 | TYPEDPat(i, pat, ty)	=> O.AnnPat(i, trPat (E,E') pat, trTy E ty)
	 | NONPat(i, pat)	=> O.NegPat(i, trPat (E,BindEnv.new()) pat)
	 | ASPat(i, pat1, pat2) => O.AsPat(i,trPat (E,E') pat1,trPat(E,E') pat2)
	 | WHEREPat(i, pat, atexp) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, cloneScope E')
		val exp' = trAtExp E atexp
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		O.GuardPat(i, pat', exp')
	   end

	 | WITHVALPat(i, pat, valbind) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, cloneScope E')
		val  _   = insertScope E'
		val decs'= trValBindo (E,E') (SOME valbind)
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		O.WithPat(i, pat', Vector.fromList decs')
	   end

	 | WITHFUNPat(i, pat, fvalbind) =>
	   let
		val  _      = insertScope E'
		val pat'    = trPat (E,E') pat
		val  _      = inheritScope(E, cloneScope E')
		val  _      = insertScope E'
		val valids' = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _      = inheritScope(E, cloneScope E')
		val exps'   = trFvalBindo_rhs E (SOME fvalbind)
		val decs'   = VectorPair.map
				(fn(valid',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId valid', valid'),
					  exp'))
				(valids',exps')
		val  _      = deleteScope E
		val  _      = deleteScope E
		val  _      = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _      = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		O.WithPat(i, pat', #[O.RecDec(infoFvalBind fvalbind, decs')])
	   end

    and trAppPat (E,E') =
	fn APPPat(i, pat, atpat) => (trAppliedAppPat (E,E') pat)
				    (i, trAtPat (E,E') atpat)
	 | ATPATPat(i, atpat)    => trAtPat (E,E') atpat
	 | pat                   => trPat (E,E') pat

    and trAppliedAppPat (E,E') =
	fn ATPATPat(i, atpat) => trAppliedAtPat (E,E') atpat
	 | pat                => error(I.infoPat pat, E.AppPatNonCon)

    and trAppliedAtPat (E,E') =
	fn LONGVIDAtPat(i, _, longvid) =>
	   (case trLongVId E longvid
	      of (vallongid', V) => error(i, E.AppPatNonCon)

	       | (vallongid', (T 0 | C 0)) =>
		 ( fn(i',pat') => error(i', E.PatConArgSuperfluous) )

	       | (vallongid', T k) =>
		 ( fn(i',pat') => O.TagPat(i', longidToLab vallongid',
					   SOME(trConLongVId E longvid), pat') )
	       | (vallongid', C k) =>
		 ( fn(i',pat') => O.ConPat(i', trConLongVId E longvid, pat') )

	       | (vallongid', R) =>
		 ( fn(i',pat') => O.RefPat(i', pat') )
	   )
	 | PARAtPat(i, pat) =>
		trAppliedPat (E,E') pat
	 | atpat =>
		error(I.infoAtPat atpat, E.AppPatNonCon)

    and trAppliedPat (E,E') =
	fn pat as (ATPATPat _|APPPat _)	=>
		trAppliedAppPat (E,E') (Infix.pat (infEnv E) pat)
	 | pat =>
		error(I.infoPat pat, E.AppPatNonCon)


    and trPats (E,E') pats =
	    Vector.map (trPat(E,E')) (Vector.fromList pats)

    and trAltPats (E,E') =
	    List.map(trAltPat(E,E'))

    and trAltPat (E,E') pat =
	let
	    val _    = insertScope E'
	    val pat' = trPat (E,E') pat
	    val E''  = splitScope E'
	    val _    = if BindEnv.sizeScope E' = BindEnv.sizeScope E'' then ()
		       else error(infoPat pat, E.AltPatInconsistent)
	    val _    = BindEnv.appiScopeVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat pat, E.AltPatInconsistent)
			    ) E'
	in
	    pat'
	end



  (* Types *)

    and trTy E =
	fn TYVARTy(i, tyvar as TyVar(i',tyvar')) =>
		O.VarTyp(i, trTyVar E tyvar)

	 | TYCONTy(i, tyseq, longtycon) =>
	   let
		val (typlongid',_) = trLongTyCon E longtycon
		val  typs'         = trTySeq E tyseq
	   in
		apptyp(typs', O.ConTyp(i, typlongid'))
	   end

	 | RECORDTy(i, tyrowo) =>
	   let
		val  _   = insertScope E
		val row' = trTyRowo E tyrowo
		val  _   = deleteScope E
	   in
		O.ProdTyp(i, row')
	   end

	 | TUPLETy(i, tys) =>
	   let
		val typs' = trTys E tys
	   in
		O.TupTyp(i, typs')
	   end

	 | ARROWTy(i, ty1, ty2) => O.ArrTyp(i, trTy E ty1, trTy E ty2)

	 | PACKTy(i, longsigid) =>
	   let
		val (inflongid',E') = trLongSigId E longsigid
		val  inf'           = O.ConInf(infoLong longsigid, inflongid')
	   in
		O.PackTyp(i, inf')
	   end

	 | PARTy(i, ty) => trTy E ty

    and trTys E tys = Vector.map (trTy E) (Vector.fromList tys)


    and trTyRowo E tyrowo =
	let
	    val (i, fields') = trTyRowo' E tyrowo
	in
	    O.Row(i, Vector.fromList fields', false)
	end
    and trTyRowo' E =
	fn NONE => (Source.nowhere, [])

	 | SOME(ROWTyRow(i, lab as Lab(i',lab'), ty, tyrowo)) =>
	   let
		val i1'    = Source.over(i', infoTy ty)
		val field' = O.Field(i1', trLab E lab, trTy E ty)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
				error(i', E.TyRowLabDuplicate lab')
		val (_,fields') = trTyRowo' E tyrowo
	   in
		(i, field'::fields')
	   end



    and trTySeq E (Seq(i, tys)) = List.map (trTy E) tys

    and trTyVarSeq E (Seq(i, tyvars)) = List.map (trSeqTyVar E) tyvars

    and trSeqTyVar E (tyvar as TyVar(i, tyvar')) =
	let
	    val (varid',stamp) = trTyVar_bind E tyvar
	    val  _             = insertDisjointVar(E, tyvar', (i, stamp))
				 handle CollisionVar _ =>
				     error(i, E.TyVarSeqDuplicate tyvar')
	in
	    varid'
	end


    (* Tyvarseqs at a val or fun *)

    and trValTyVarSeq E (Seq(i, tyvars)) = List.map (trValSeqTyVar E) tyvars

    and trValSeqTyVar E (tyvar as TyVar(i, tyvar')) =
	if Option.isSome(lookupVar(E, tyvar')) then
	    error(i, E.ValTyVarSeqDuplicate tyvar')
	else
	let
	    val (varid',stamp) = trTyVar_bind E tyvar
	    val  _             = insertVar(E, tyvar', (i, stamp))
	in
	    varid'
	end


    (* Extract type variables from a type (as implicitly quantified) *)

    and trAllTy E =
	fn TYVARTy(i, tyvar as TyVar(i',tyvar')) =>
	   if Option.isSome(lookupVar(E, tyvar')) then
		[]
	   else
	   let
		val (varid',stamp) = trTyVar_bind E tyvar
		val  _             = insertVar(E, tyvar', (i, stamp))
	   in
		[varid']
	   end

	 | TYCONTy(i, tyseq, longtycon) => trAllTySeq E tyseq
	 | RECORDTy(i, tyrowo)   => trAllTyRowo E tyrowo
	 | TUPLETy(i, tys)       => List.concat(List.map (trAllTy E) tys)
	 | ARROWTy(i, ty1, ty2)  => trAllTy E ty1 @ trAllTy E ty2
	 | PACKTy(i, longsigid)  => []
	 | PARTy(i, ty)          => trAllTy E ty

    and trAllTyRowo E =
	fn NONE                               => []
	 | SOME(ROWTyRow(i, lab, ty, tyrowo)) =>
		trAllTy E ty @ trAllTyRowo E tyrowo

    and trAllTySeq E (Seq(i, tys)) = List.concat(List.map (trAllTy E) tys)



  (* Declarations *)

    and trDec  E dec  = Vector.rev(Vector.fromList(trDec' (E,[]) dec))
    and trDec'(E,acc) =
	fn VALDec(i, tyvarseq, valbind) =>
	   let
		val  E'     = BindEnv.new()
		val  _      = insertScope E
		val varids' = trValTyVarSeq E tyvarseq @
			      unguardedTyVarsValBind E valbind
		val decs'   = (if List.null varids'
			       then trValBindo'(E,E',acc)
			       else trValBindo (E,E') ) (SOME valbind)
		val  _      = deleteScope E
		val  _      = union(E,E')
	   in
		if List.null varids'
		then decs'
		else List.map (fn dec' => vardec(varids', dec')) decs' @ acc
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end

	 | FUNDec(i, tyvarseq, fvalbind) =>
	   let
		val  E'      = BindEnv.new()
		val valids'  = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _       = union(E,E')
		val  _       = insertScope E
		val valids'' = trValTyVarSeq E tyvarseq @
			       unguardedTyVarsFvalBind E fvalbind
		val exps'    = trFvalBindo_rhs E (SOME fvalbind)
		val  _       = deleteScope E
		val decs'    = VectorPair.map
				(fn(valid',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId valid', valid'),
					  exp'))
				(valids',exps')
	   in
		vardec(valids'', O.RecDec(i, decs')) :: acc
	   end

	 | TYPEDec(i, typbind) =>
	   let
		val E'    = BindEnv.new()
		val decs' = trTypBindo' (E,E',acc) (SOME typbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | DATATYPEDec(i, datbind) =>
	   let
		val  E'             = BindEnv.new()
		val  _              = trDatBindo_lhs (E,E') (SOME datbind)
		val  _              = union(E,E')
		val (tdecs',cdecs') = trDatBindo_rhs (E,E') (SOME datbind)
		val  _              = union(E,E')
	   in
		cdecs' @ O.RecDec(i, tdecs') :: acc
	   end

	 | REPLICATIONDec(i, tycon as TyCon(i',tycon'), longtycon) =>
	   let
		val (typid',stamp)  = trTyCon_bind E tycon
		val (typlongid',E') = trLongTyCon E longtycon
		val  typlongido'    = case typlongid'
					of O.LongId(_,typlongid',_) =>
						SOME typlongid'
					 | O.ShortId _ => NONE
		val _               = insertTy(E, tycon', (i', stamp, E'))
	   in
		foldiVals (trOpenDecVal (E,i,typlongido'))
		  (O.TypDec(i, typid',
			    O.ConTyp(infoLong longtycon, typlongid')) :: acc)
		  E'
	   end

	 | CONSTRUCTORDec(i, dconbind) =>
	   let
		val E'    = BindEnv.new()
		val decs' = trDconBindo' (E,E',acc) (SOME dconbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | STRUCTUREDec(i, strbind) =>
	   let
		val E'    = BindEnv.new()
		val decs' = trStrBindo' (E,E',acc) (SOME strbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | SIGNATUREDec(i, sigbind) =>
	   let
		val E'    = BindEnv.new()
		val decs' = trSigBindo' (E,E',acc) (SOME sigbind)
		val _     = union(E,E')
	   in
		decs'
	   end

	 | LOCALDec(i, dec1, dec2) =>
	   let
		val  _     = insertScope E
		val decs1' = trDec E dec1
		val  _     = insertScope E
		val decs2' = trDec' (E, O.LocalDec(i, decs1')::acc) dec2
		val  E'    = splitScope E
		val  _     = deleteScope E
		val  _     = union(E,E')
	   in
		decs2'
	   end

	 | OPENDec(i, longstrid) =>
	   let
		val (modlongid', E') = trLongStrId E longstrid
		val   _              = unionInf(E,E')
	   in
		(foldiVals (trOpenDecVal(E,i,SOME modlongid'))
		(foldiTys  (trOpenDecTy (E,i,modlongid'))
		(foldiStrs (trOpenDecStr(E,i,modlongid'))
		(foldiSigs (trOpenDecSig(E,i,modlongid')) acc E') E') E') E')
	   end

	 | EMPTYDec(i) =>
		acc

	 | SEQDec(i, dec1, dec2) =>
		trDec' (E, trDec' (E,acc) dec1) dec2

	 | INFIXDec(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.LEFT)
		val fix'    = O.Fix(i, fix)
		val dec'    = O.FixDec(i, vallab', fix')
		val _       = insertInf(E, vid', (i', SOME(LEFT, n)))
	   in
		dec' :: acc
	   end

	 | INFIXRDec(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.RIGHT)
		val fix'    = O.Fix(i, fix)
		val dec'    = O.FixDec(i, vallab', fix')
		val _       = insertInf(E, vid', (i', SOME(RIGHT, n)))
	   in
		dec' :: acc
	   end

	 | NONFIXDec(i, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.NONFIX
		val fix'    = O.Fix(i, fix)
		val dec'    = O.FixDec(i, vallab', fix')
		val _       = insertInf(E, vid', (i', NONE))
	   in
		dec' :: acc
	   end

	 | PRIMITIVEVALDec(i, _, vid as VId(i',vid'), ty, s) =>
	   let
		val (valid',stamp) = trVId_bind E vid
		val  _             = insertScope E
		val  varids'       = trAllTy E ty
		val  typ'          = trTy E ty
		val  _             = deleteScope E
		val  pat'          = O.VarPat(i', valid')
		val  exp'          = O.PrimExp(i, s, typ')
		val  dec'          = O.ValDec(i, pat', exp')
		val  _             = insertVal(E, vid', (i, stamp, V))
	   in
		vardec(varids', dec') :: acc
	   end

	 | PRIMITIVETYPEDec(i, tyvarseq, tycon as TyCon(i',tycon'), s) =>
	   let
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq	(* ignore *)
		val  _             = deleteScope E
		val  dec'          = O.TypDec(i, typid', O.PrimTyp(i,s))
		val  _             = insertTy(E, tycon',
						 (i, stamp, BindEnv.new()))
	   in
		dec' :: acc
	   end

	 | PRIMITIVEDATATYPEDec(i, tyvarseq, tycon as TyCon(i',tycon'), s) =>
	   let
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq	(* ignore *)
		val  _             = deleteScope E
		val  dec'          = O.TypDec(i, typid', O.PrimTyp(i,s))
		val  _             = insertTy(E, tycon',
						 (i, stamp, BindEnv.new()))
	   in
		dec' :: acc
	   end

	 | PRIMITIVEREFTYPEDec(i, tyvar, tycon as TyCon(i1',tycon'),
				  _, vid as VId(i2',vid'), tyvar2) =>
	   let
		val (typid', stamp0) = trTyCon_bind E tycon
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  _               = insertScope E
		val  varid'          = trSeqTyVar E tyvar
		val  varid2'         = trTyVar E tyvar2
		val  _               = deleteScope E
		val  typ'            = O.RefTyp(i1', varToTyp varid')
		val  dec0'           = O.TypDec(i, typid',
						   O.FunTyp(i, varid', typ'))
		val  vartyp'         = varToTyp varid2'
		val  contyp'         = conarrowtyp E (i, vartyp',
						      O.RefTyp(i2', vartyp'), 1)
		val  exp1'           = O.FailExp(i2')
		val  dec1'           = O.ValDec(i, O.VarPat(i2', valid1'),
						   O.AnnExp(i, exp1', contyp'))
		val  id'             = O.Id(i2', Stamp.new(), Name.InId)
		val  pat'            = O.VarPat(i2', id')
		val  exp2'           = O.VarExp(i2', O.ShortId(i2', id'))
		val  match'          = O.Match(i2', pat', O.RefExp(i2', exp2'))
		val  dec2'           = O.ValDec(i, O.VarPat(i2', valid2'),
						   O.FunExp(i, #[match']))
		val  E'              = BindEnv.new()
		val  _               = insertVal(E', vid', (i2', stamp2, R))
		val  _               = insertVal(E', conVId vid',
						     (i2', stamp1, V))
		val  _               = insertTy(E, tycon', (i1', stamp0, E'))
		val  _               = union(E,E')
	   in
		dec2' :: vardec([varid2'], dec1') :: dec0' :: acc
	   end

	 | PRIMITIVECONSTRUCTORDec
		(i, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon, s) =>
	   let
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  vallongid1'     = O.ShortId(i, valid1')
		val  _               = insertScope E
		val (varids',typ12') = trTyVarSeqLongTyCon E
						(tyvarseq,longtycon)
		val (typ11',k)       = trTyo E tyo
		val  _               = deleteScope E
		val  typ1'           = conarrowtyp E (O.infoTyp typ12',
						      typ11', typ12', k)
		val  exp1'           = O.PrimExp(i, s, typ1')
		val  dec1'           = O.ValDec(i, O.VarPat(i', valid1'), exp1')
		val  exp2'           =
		     if k = 0 then
			O.ConExp(i, vallongid1', O.FailExp(i))
		     else
		     let
			val  validM'     = O.Id(i, Stamp.new(), Name.InId)
			val  vallongidM' = O.ShortId(i, validM')
			val  patM'       = O.VarPat(i, validM')
			val  expM'       = O.ConExp(i, vallongid1',
						    O.VarExp(i, vallongidM'))
		     in
			O.FunExp(i, #[O.Match(i, patM', expM')])
		     end
		val  dec2'           = O.ValDec(i, O.VarPat(i', valid2'), exp2')
		val  _               = insertVal(E, conVId vid', (i',stamp1,V))
		val  _               = insertVal(E, vid', (i', stamp2, C k))
	   in
		dec2' :: vardec(varids', dec1') :: acc
	   end

	 | PRIMITIVESTRUCTUREDec(i, strid as StrId(i',strid'), sigexp, s) =>
	   let
		val (modid',stamp) = trStrId_bind E strid
		val (inf',E')      = trSigExp E sigexp
		val  mod'          = O.PrimMod(i, s, inf')
		val  dec'          = O.ModDec(i, modid', mod')
		val  _             = insertStr(E, strid', (i, stamp, E'))
	   in
		dec' :: acc
	   end

	 | PRIMITIVESIGNATUREDec(i, sigid as SigId(i',sigid'), strpats, s) =>
	   let
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats	(* ignore *)
		val  _             = deleteScope E
		val  dec'          = O.InfDec(i', infid', O.PrimInf(i,s))
		val  _             = insertSig(E, sigid',
						    (i', stamp, BindEnv.new()))
	   in
		dec' :: acc
	   end

    and trOpenDecVal (E,i,modlongido') (vid', (_,stamp1,is), acc) =
	let
	    val name       = VId.toString vid'
	    val stamp2     = Stamp.new()
	    val valid1'    = O.Id(i, stamp1, Name.ExId name)
	    val valid2'    = O.Id(i, stamp2, Name.ExId name)
	    val vallongid' = case modlongido'
			       of NONE            => O.ShortId(i, valid1')
			        | SOME modlongid' =>
				   O.LongId(i, modlongid',
					    O.Lab(i, Label.fromString name))
	    val pat'       = O.VarPat(i, valid2')
	    val exp'       = O.VarExp(i, vallongid')
	    val _          = insertVal(E, vid', (i,stamp2,is))
	in
	    O.ValDec(i, pat', exp') :: acc
	end

    and trOpenDecTy (E,i,modlongid') (tycon', (_,stamp1,E'), acc) =
	let
	    val name       = TyCon.toString tycon'
	    val stamp2     = Stamp.new()
	    val typid'     = O.Id(i, stamp2, Name.ExId name)
	    val typlab'    = O.Lab(i, Label.fromString name)
	    val typlongid' = O.LongId(i, modlongid', typlab')
	    val typ'       = O.ConTyp(i, typlongid')
	    val _          = insertTy(E, tycon', (i,stamp2,E'))
	in
	    O.TypDec(i, typid', typ') :: acc
	end

    and trOpenDecStr (E,i,modlongid') (strid', (_,stamp1,E'), acc) =
	let
	    val name    = StrId.toString strid'
	    val stamp2  = Stamp.new()
	    val modid'  = O.Id(i, stamp2, Name.ExId name)
	    val modlab' = O.Lab(i, Label.fromString name)
	    val mod'    = modlongidToMod(O.LongId(i, modlongid', modlab'))
	    val _       = insertStr(E, strid', (i,stamp2,E'))
	in
	    O.ModDec(i, modid', mod') :: acc
	end

    and trOpenDecSig (E,i,modlongid') (sigid', (_,stamp1,E'), acc) =
	let
	    val name       = SigId.toString sigid'
	    val stamp2     = Stamp.new()
	    val infid'     = O.Id(i, stamp2, Name.ExId name)
	    val inflab'    = O.Lab(i, Label.fromString name)
	    val inflongid' = O.LongId(i, modlongid', inflab')
	    val inf'       = O.ConInf(i, inflongid')
	    val _          = insertSig(E, sigid', (i,stamp2,E'))
	in
	    O.InfDec(i, infid', inf') :: acc
	end



  (* Value bindings *)

    and trValBindo (E,E') valbindo = List.rev(trValBindo' (E,E',[]) valbindo)
    and trValBindo'(E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(_, pat, exp, valbindo)) =>
	   let
		val i    = Source.over(infoPat pat, infoExp exp)
		val pat' = trPat (E,E') pat
		val exp' = trExp E exp
		val dec' = O.ValDec(i, pat', exp')
	   in
		trValBindo' (E,E', dec'::acc) valbindo
	   end

	| SOME(RECValBind(i, valbind)) =>
	   let
		val pats' = trRecValBindo_lhs (E,E') (SOME valbind)
		val  _    = union(E,E')
		val exps' = trRecValBindo_rhs E (SOME valbind)
		val decs' = VectorPair.map
				(fn(pat',exp') =>
				 O.ValDec(Source.over(O.infoPat pat',
						      O.infoExp exp'),
					  pat', exp'))
				(pats',exps')
	   in
		O.RecDec(i, decs') :: acc
	   end


    and trRecValBindo_lhs (E,E') valbindo =
	    Vector.rev(Vector.fromList(trRecValBindo_lhs' (E,E',[]) valbindo))

    and trRecValBindo_lhs' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
	   let
		val pat' = trPat (E,E') pat
	   in
		trRecValBindo_lhs' (E,E', pat'::acc) valbindo
	   end

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_lhs' (E,E',acc) (SOME valbind)


    and trRecValBindo_rhs E valbindo =
	    Vector.rev(Vector.fromList(trRecValBindo_rhs' (E,[]) valbindo))

    and trRecValBindo_rhs' (E,acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
	   (* BUG: no check for admissibility *)
	   let
		val exp' = trExp E exp
	   in
		trRecValBindo_rhs' (E, exp'::acc) valbindo
	   end

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_rhs' (E,acc) (SOME valbind)



  (* Function bindings *)

    and trFvalBindo_lhs (E,E') fvalbindo =
	    Vector.rev(Vector.fromList(trFvalBindo_lhs' (E,E',[]) fvalbindo))

    and trFvalBindo_lhs'(E,E',acc) =
	fn NONE => acc
	 | SOME(FvalBind(i, fmatch, fvalbindo)) =>
	   let
		val valid' = trFmatch_lhs (E,E') fmatch
	   in
		trFvalBindo_lhs' (E,E', valid'::acc) fvalbindo
	   end


    and trFmatch_lhs (E,E') (Match(i, fmrule, fmatcho)) =
	   let
		val vid as VId(i',vid') = trFmrule_lhs E fmrule
		val (valid',stamp)      = trVId_bind E vid
		val _ = trFmatcho_lhs (E,vid) fmatcho
		val _ = insertDisjointVal(E', vid', (i',stamp,V))
			handle CollisionVal _ =>
			       error(i', E.FvalBindDuplicate vid')
	   in
		valid'
	   end

    and trFmatcho_lhs (E,vid1) =
	fn NONE => ()
	 | SOME(Match(i, fmrule, fmatcho)) =>
	   let
		val vid2 as VId(i',vid2') = trFmrule_lhs E fmrule
	   in
		if idVId vid1 = idVId vid2 then
		    trFmatcho_lhs (E,vid1) fmatcho
		else
		    error(i', E.FvalBindNameInconsistent vid2')
	   end

    and trFmrule_lhs E (Mrule(i, fpat, exp)) =
	   trFpat_lhs E fpat

    and trFpat_lhs E =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trFappPat_lhs E (Infix.pat (infEnv E) fpat)
	 | ( TYPEDPat(i, fpat, _)
	   | WHEREPat(i, fpat, _)
	   | WITHVALPat(i, fpat, _)
	   | WITHFUNPat(i, fpat, _) )	=> trFpat_lhs E fpat
	 | ( NONPat(i,_)
	   | ASPat(i,_,_) )		=> error(i, E.FvalBindPatInvalid)

    and trFappPat_lhs E =
	fn APPPat(i, fpat, atpat)	=> trFappPat_lhs E fpat
	 | ATPATPat(i, atpat)		=> trFatPat_lhs E atpat
	 | fpat				=> trFpat_lhs E fpat

    and trFatPat_lhs E =
	fn LONGVIDAtPat(i, _, SHORTLong(_, vid as VId(i', vid'))) =>
	   (case lookupIdStatus(E, vid')
	      of  V              => vid
	       | (R | C _ | T _) => error(i', E.FvalBindNameCon vid')
	   )

	 | ALTAtPat(i, fpats) =>
	   let
		val vids               = trFpats_lhs E fpats
		val vid as VId(_,vid') = List.hd vids
	   in
		case List.find (fn(VId(_,vid'')) => vid'<>vid'') (List.tl vids)
		  of NONE                => vid
		   | SOME(VId(i',vid2')) =>
			error(i', E.FvalBindNameInconsistent vid2')
	   end

	 | PARAtPat(i, fpat) =>
		trFpat_lhs E fpat

	 | atpat =>
		error(infoAtPat atpat, E.FvalBindNameMissing)

    and trFpats_lhs E = List.map(trFpat_lhs E)



    and trFvalBindo_rhs E fvalbindo =
	    Vector.rev(Vector.fromList(trFvalBindo_rhs' (E,[]) fvalbindo))

    and trFvalBindo_rhs'(E,acc) =
	fn NONE => acc
	 | SOME(FvalBind(i, fmatch, fvalbindo)) =>
	   let
		val exp' = trFmatch_rhs E fmatch
	   in
		trFvalBindo_rhs' (E, exp'::acc) fvalbindo
	   end

    and trFmatch_rhs E (Match(i, fmrule, fmatcho)) =
	   let
		val (match',arity) = trFmrule_rhs E fmrule
		val  matches'      = Vector.fromList
				     (match' :: trFmatcho_rhs (E,arity) fmatcho)
		val  i'            = O.infoMatch match'
	   in
		if arity = 1 then
		    O.FunExp(i', matches')
		else
		    let
			val valids'  = Vector.tabulate(arity, fn _ =>inventId i)
			val exps'    = Vector.map(fn valid' =>
					  O.VarExp(Source.nowhere,
					     O.ShortId(Source.nowhere, valid')))
					  valids'
			val tupexp'  = tupexp(i', exps')
			val caseexp' = O.CaseExp(i', tupexp', matches')
		    in
			funexp(Vector.toList valids', caseexp')
		    end
	   end

    and trFmatcho_rhs (E,arity) fmatcho =
	    List.rev(trFmatcho_rhs' (E,arity,[]) fmatcho)

    and trFmatcho_rhs' (E,arity,acc) =
	fn NONE => acc

	 | SOME(Match(i, fmrule, fmatcho)) =>
	   let
		val (match',arity') = trFmrule_rhs E fmrule
	   in
		if arity <> arity' then
		    error(infoMrule fmrule, E.FvalBindArityInconsistent)
		else
		    trFmatcho_rhs' (E, arity, match'::acc) fmatcho
	   end

    and trFmrule_rhs E (Mrule(i, fpat, exp)) =
	   let
		val  E'                = BindEnv.new()
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  _                 = inheritScope(E,E')
		val  exp'              = trExp E exp
		val  _                 = deleteScope E
	   in
		( O.Match(i, pat', annexp(exp',typs')), arity )
	   end

    and trFpat_rhs (E,E') =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trFappPat_rhs (E,E') (Infix.pat (infEnv E) fpat)

	 | TYPEDPat(i, fpat, ty) =>
	   let
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  typ'              = trTy E ty
	   in
		( pat', arity, typ'::typs' )
	   end

	 | WHEREPat(i, fpat, atexp) =>
	   let
		val  _   = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  _   = inheritScope(E, cloneScope E')
		val exp' = trAtExp E atexp
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		( O.GuardPat(i, pat', exp'), arity, typs' )
	   end

	 | WITHVALPat(i, pat, valbind) =>
	   let
		val  _   = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') pat
		val  _   = inheritScope(E, cloneScope E')
		val  _   = insertScope E'
		val decs'= Vector.fromList(trValBindo (E,E') (SOME valbind))
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		( O.WithPat(i, pat', decs'), arity, typs' )
	   end

	 | WITHFUNPat(i, pat, fvalbind) =>
	   let
		val  _      = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') pat
		val  _      = inheritScope(E, cloneScope E')
		val  _      = insertScope E'
		val valids' = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _      = inheritScope(E, cloneScope E')
		val exps'   = trFvalBindo_rhs E (SOME fvalbind)
		val decs'   = VectorPair.map
				(fn(valid',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId valid', valid'),
					  exp'))
				(valids',exps')
		val  _      = deleteScope E
		val  _      = deleteScope E
		val  _      = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _      = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		( O.WithPat(i, pat', #[O.RecDec(infoFvalBind fvalbind, decs')]),
		  arity, typs' )
	   end

	 | ( NONPat(i,_) | ASPat(i,_,_) ) =>
		error(i, E.FvalBindPatInvalid)

    and trFappPat_rhs (E,E') =
	fn fpat as APPPat _	=>
	   let
		val pats' = Vector.fromList(trAppliedFappPat_rhs (E,E') fpat)
	   in
		( tuppat(infoPat fpat, pats'), Vector.length pats', [] )
	   end
	 | ATPATPat(i, atpat)	=> trFatPat_rhs (E,E') atpat
	 | fpat			=> trFpat_rhs (E,E') fpat

    and trFatPat_rhs (E,E') =
	fn ALTAtPat(i, fpats) =>
	   let
		val  _                 = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') (List.hd fpats)
		val  pat'aritytyps's   = trAltFpats_rhs (E,E') (List.tl fpats)
		val (pats',arities,typs'') =
			 List.foldr (fn((p,a,ts), (pl,al,tl)) =>
					(p::pl, a::al, ts@tl)
				    ) ([],[],[]) pat'aritytyps's
		val  _ = mergeDisjointScope E'
			 handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		case List.find (fn(_,arity',_) => arity<>arity') pat'aritytyps's
		  of NONE =>
			( O.AltPat(i, Vector.fromList(pat'::pats')),
			  arity, typs' @ typs'' )
		   | SOME(pat',_,_) =>
			error(O.infoPat pat', E.FvalBindArityInconsistent)
	   end
	 | PARAtPat(i, fpat)	=> trFpat_rhs (E,E') fpat
	 | LONGVIDAtPat(i,_,_)	=> error(i, E.FvalBindArityZero)
	 | fatpat		=> error(infoAtPat fatpat, E.FvalBindPatInvalid)

    and trAltFpats_rhs (E,E') =
	    List.map(trAltFpat_rhs (E,E'))

    and trAltFpat_rhs (E,E') fpat =
	let
	    val _    = insertScope E'
	    val pat'aritytyps' = trFpat_rhs (E,E') fpat
	    val E''  = splitScope E'
	    val _    = if BindEnv.sizeScope E' = BindEnv.sizeScope E'' then ()
		       else error(infoPat fpat, E.AltPatInconsistent)
	    val _    = BindEnv.appiVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat fpat, E.AltPatInconsistent)
			    ) E'
	in
	    pat'aritytyps'
	end


    and trAppliedFpat_rhs (E,E') =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trAppliedFappPat_rhs (E,E') (Infix.pat (infEnv E) fpat)
	 | fpat => error(infoPat fpat, E.FvalBindPatInvalid)

    and trAppliedFappPat_rhs (E,E') =
	fn ATPATPat(i, fatpat)	  => trAppliedFatPat_rhs (E,E') fatpat
	 | APPPat(i, fpat, atpat) => trAppliedFappPat_rhs (E,E') fpat
				     @ [trAtPat (E,E') atpat]
	 | fpat => error(infoPat fpat, E.FvalBindPatInvalid)

    and trAppliedFatPat_rhs (E,E') =
	fn LONGVIDAtPat _	=> []
	 | PARAtPat(i, fpat)	=> trAppliedFpat_rhs (E,E') fpat
	 | fatpat => error(infoAtPat fatpat, E.FvalBindPatInvalid)



  (* Type and constructor bindings *)

    and trTypBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(NEWTypBind(_, tyvarseq, tycon as TyCon(i',tycon'), typbindo)) =>
	   let
		val  i             = Source.over(infoSeq tyvarseq, i')
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  _             = deleteScope E
		val  funtyp'       = funtyp(varids', O.AbsTyp(i',false))
		val  dec'          = O.TypDec(i, typid', funtyp')
		val  _             = insertDisjointTy(E', tycon',
						     (i', stamp, BindEnv.new()))
				     handle CollisionTy _ =>
					error(i', E.TypBindDuplicate tycon')
	   in
		trTypBindo' (E,E', dec'::acc) typbindo
	   end

	 | SOME(EQUALTypBind(_, tyvarseq, tycon as TyCon(i',tycon'), ty,
								typbindo)) =>
	   let
		val  i             = Source.over(infoSeq tyvarseq, infoTy ty)
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  typ'          = trTy E ty
		val  _             = deleteScope E
		val  funtyp'       = funtyp(varids', typ')
		val  dec'          = O.TypDec(i', typid', funtyp')
		val  _             = insertDisjointTy(E', tycon',
						   (i', stamp, BindEnv.new()))
				     handle CollisionTy _ =>
					error(i', E.TypBindDuplicate tycon')
	   in
		trTypBindo' (E,E', dec'::acc) typbindo
	   end


    and trDatBindo_lhs (E,E') =
	fn NONE => ()

	 | ( SOME(CLOSEDDatBind(i, tyvarseq, tycon, _, datbindo))
	   | SOME(OPENDatBind(i, tyvarseq, tycon, datbindo)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (typid',stamp)   = trTyCon_bind E tycon
		val  _               = insertDisjointTy(E', tycon',
						(i', stamp, BindEnv.new()))
				       handle CollisionTy _ =>
					   error(i', E.DatBindDuplicate tycon')
	   in
		trDatBindo_lhs (E,E') datbindo
	   end

    and trDatBindo_rhs (E,E') datbindo =
	let
	    val (tdecs',cdecs') = trDatBindo_rhs' (E,E',[],[]) datbindo
	in
	    (Vector.rev(Vector.fromList tdecs'), cdecs')
	end

    and trDatBindo_rhs'(E,E',acc1,acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(CLOSEDDatBind(_, tyvarseq, tycon, conbind, datbindo)) =>
	   let
		val  i'           = infoConBind conbind
		val  i            = Source.over(infoSeq tyvarseq, i')
		val (typid',E'')  = trTyCon E tycon	(* bound before *)
		val  _            = insertScope E
		val  varids'      = trTyVarSeq E tyvarseq
		val  i_typid      = O.infoId typid'
		val  contyp'      = O.ConTyp(i_typid, O.ShortId(i_typid,typid'))
		val  typ'         = apptyp(List.map varToTyp varids', contyp')
		val (flds',decs') = trConBindo (E,E'',varids',typ')
					       (SOME conbind)
		val  _            = deleteScope E
		val  sumtyp'      = O.SumTyp(i', O.Row(i', flds', false))
		val  dec'         = O.TypDec(i, typid', funtyp(varids',sumtyp'))
		val  _            = unionDisjoint(E',E'')
				    handle CollisionVal vid' =>
				      errorVId(E'', vid', E.DatBindConDuplicate)
	   in
		trDatBindo_rhs' (E,E', dec'::acc1, decs'@acc2) datbindo
	   end

	 | SOME(OPENDatBind(_, tyvarseq, tycon, datbindo)) =>
	   let
		val  i'          = infoTyCon tycon
		val  i           = Source.over(infoSeq tyvarseq, i')
		val (typid',E'') = trTyCon E tycon		(* bound before *)
		val  _           = insertScope E
		val  varids'     = trTyVarSeq E tyvarseq
		val  _           = deleteScope E
		val  funtyp'     = funtyp(varids', O.AbsTyp(i',true))
		val  dec'        = O.TypDec(i, typid', funtyp')
	   in
		trDatBindo_rhs' (E,E', dec'::acc1, acc2) datbindo
	   end


    and trConBindo (E,E',varids',typ') conbindo =
	let
	    val (fields',decs') = trConBindo' (E,E',varids',typ',[],[]) conbindo
	in
	    (Vector.rev(Vector.fromList fields'), decs')
	end

    and trConBindo'(E,E',varids',typ',acc1,acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(ConBind(i, _, vid as VId(i',vid'), tyo, conbindo)) =>
	   let
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  vallongid1'     = O.ShortId(i, valid1')
		val (typ11',k)       = trTyo E tyo
		val  vallab'         = idToLab valid2'
		val  field'          = O.Field(i, vallab', typ11')
		val  typ1'           = conarrowtyp E (O.infoTyp typ',
						      typ11', typ', k)
		val  exp1'           = O.LabExp(i, vallab', typ1')
		val  dec1'           = O.ValDec(i, O.VarPat(i', valid1'), exp1')
		val  exp2'           =
		     if k = 0 then
			O.TagExp(i, vallab', SOME vallongid1', O.FailExp(i))
		     else
		     let
			val  validM'     = O.Id(i, Stamp.new(), Name.InId)
			val  vallongidM' = O.ShortId(i, validM')
			val  patM'       = O.VarPat(i, validM')
			val  expM'       = O.TagExp(i, vallab',SOME vallongid1',
						    O.VarExp(i, vallongidM'))
		     in
			O.FunExp(i, #[O.Match(i, patM', expM')])
		     end
		val  dec2'           = O.ValDec(i, O.VarPat(i', valid2'), exp2')
	   in
		( insertDisjointVal(E', conVId vid', (i', stamp1, V))
		; insertDisjointVal(E', vid',  (i', stamp2, T k))
		) handle CollisionVal _ => error(i', E.ConBindDuplicate vid');
		trConBindo' (E,E',varids',typ', field'::acc1,
			     dec2' :: vardec(varids', dec1') :: acc2) conbindo
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end

    and trDconBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(NEWDconBind(_, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dconbindo)) =>
	   let
		val  i               = Source.over(i', infoLong longtycon)
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  vallongid1'     = O.ShortId(i, valid1')
		val  _               = insertScope E
		val (varids',typ12') = trTyVarSeqLongTyCon E
						(tyvarseq, longtycon)
		val (typ11',k)       = trTyo E tyo
		val  _               = deleteScope E
		val  typ1'           = conarrowtyp E (O.infoTyp typ12',
						      typ11', typ12', k)
		val  exp1'           = O.NewExp(i, typ1')
		val  dec1'           = O.ValDec(i, O.VarPat(i', valid1'), exp1')
		val  exp2'           =
		     if k = 0 then
			O.ConExp(i, vallongid1', O.FailExp(i))
		     else
		     let
			val  validM'     = O.Id(i, Stamp.new(), Name.InId)
			val  vallongidM' = O.ShortId(i, validM')
			val  patM'       = O.VarPat(i, validM')
			val  expM'       = O.ConExp(i, vallongid1',
						    O.VarExp(i, vallongidM'))
		     in
			O.FunExp(i, #[O.Match(i, patM', expM')])
		     end
		val  dec2'           = O.ValDec(i, O.VarPat(i', valid2'), exp2')
	   in
		( insertDisjointVal(E', conVId vid', (i', stamp1, V))
		; insertDisjointVal(E', vid',  (i', stamp2, C k))
		) handle CollisionVal _ => error(i', E.DconBindDuplicate vid');
		trDconBindo' (E,E',
			      dec2' :: vardec(varids', dec1') :: acc) dconbindo
	   end

	 | SOME(EQUALDconBind(_, _, vid as VId(i',vid'), _,
							longvid, dconbindo)) =>
	   let
		val  i               = Source.over(i', infoLong longvid)
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  vallongid1'     = trConLongVId E longvid
		val (vallongid2',is) = trLongVId E longvid
		val  i1              = infoLong longvid
		val  exp1'           = O.VarExp(i1, vallongid1')
		val  dec1'           = O.ValDec(i, O.VarPat(i', valid1'), exp1')
		val  exp2'           = O.VarExp(i1, vallongid2')
		val  dec2'           = O.ValDec(i, O.VarPat(i', valid2'), exp2')
	   in
		case is
		  of C _ =>
		      ( ( insertDisjointVal(E', conVId vid', (i', stamp1, V))
			; insertDisjointVal(E', vid', (i', stamp2, is))
			) handle CollisionVal _ =>
				error(i', E.DconBindDuplicate vid')
		      ; trDconBindo' (E,E', dec2'::dec1'::acc) dconbindo
		      )
		   | R =>
		      ( insertDisjointVal(E', vid', (i', stamp2, is))
			handle CollisionVal _ =>
				error(i', E.DconBindDuplicate vid')
		      ; trDconBindo' (E,E', dec2'::acc) dconbindo
		      )
		   | (T _ | V) => error(i, E.DconBindNonCon)
	   end


    and trTyo E (SOME typ) =
	let
	    val typ' = trTy E typ
	in
	    ( typ', typArity typ' )
	end
      | trTyo E  NONE      =
	let
	    val typ' = contyp E (Source.nowhere, lab_zero)
	in
	    ( typ', 0 )
	end


    and trTyVarSeqLongTyCon E (tyvarseq, longtycon) =
	let
	    val (typlongid',_) = trLongTyCon E longtycon
	    val  typ'          = O.ConTyp(O.infoLongid typlongid', typlongid')
	    val  varids'       = trTyVarSeq E tyvarseq
	    val  typs'         = List.map (fn varid' =>
	    			      O.VarTyp(O.infoId varid', varid')) varids'
	in
	    ( varids', apptyp(typs', typ') )
	end


  (* Structure and signature bindings *)

    and trStrBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(StrBind(_, strid as StrId(i',strid'), strexp, strbindo)) =>
	   let
		val  i             = Source.over(i', infoStrExp strexp)
		val (modid',stamp) = trStrId_bind E strid
		val (mod',E'')     = trStrExp E strexp
		val  dec'          = O.ModDec(i, modid', mod')
		val  _             = insertDisjointStr(E', strid', (i',stamp,E''))
				     handle CollisionStr _ =>
					error(i', E.StrBindDuplicate strid')
	   in
		trStrBindo' (E,E', dec'::acc) strbindo
	   end


    and trSigBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(SigBind(_, sigid as SigId(i',sigid'), strpats, sigexp,
								sigbindo)) =>
	   let
		val  i             = Source.over(i', infoSigExp sigexp)
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats
		val (inf',E'')     = trSigExp E sigexp
		val  _             = deleteScope E
		val  dec'          = O.InfDec(i,infid',funinf(modid_infs',inf'))
		val  _             = insertDisjointSig(E', sigid', (i',stamp,E''))
				     handle CollisionSig _ =>
					error(i', E.SigBindDuplicate sigid')
	   in
		trSigBindo' (E,E', dec'::acc) sigbindo
	   end


  (* Structure expressions *)

    and trAtStrExp E =
	fn STRUCTAtStrExp(i, dec) =>
	   let
		val _     = insertScope E
		val decs' = trDec E dec
		val E'    = splitScope E
	   in
		( O.StrMod(i, decs'), E' )
	   end

	 | LONGSTRIDAtStrExp(i, longstrid) =>
	   let
		val (modlongid',E') = trLongStrId E longstrid
	   in
		( modlongidToMod modlongid', E' )
	   end

	 | LETAtStrExp(i, dec, strexp) =>
	   let
		val  _        = insertScope E
		val  decs'    = trDec E dec
		val (mod',E') = trStrExp E strexp
		val  _        = deleteScope E
	   in
		( O.LetMod(i, decs', mod'), E' )
	   end

	 | PARAtStrExp(i, strexp) => trStrExp E strexp

    and trStrExp E =
	fn ATSTREXPStrExp(i, atstrexp) => trAtStrExp E atstrexp

	 | APPStrExp(i, strexp, atstrexp) =>
	   let
		val (mod1',E1') = trStrExp E strexp
		val (mod2',E2') = trAtStrExp E atstrexp
	   in
		( O.AppMod(i, mod1', mod2'), E1' )
	   end

	 | TRANSStrExp(i, strexp, sigexp) =>
	   let
		val (mod',E')  = trStrExp E strexp
		val (inf',E'') = trSigExp E sigexp
	   in
		( O.AnnMod(i, mod', inf'), E'' )
	   end

	 | OPAQStrExp (i, strexp, sigexp) =>
	   let
		val (mod',E')  = trStrExp E strexp
		val (inf',E'') = trSigExp E sigexp
	   in
		( O.UpMod(i, mod', inf'), E'' )
	   end

	 | FCTStrExp(i, strpat, strexp) =>
	   let
		val  _            = insertScope E
		val (modid',inf') = trStrPat E strpat
		val (mod',E')     = trStrExp E strexp
		val  _            = deleteScope E
	   in
		( O.FunMod(i, modid', inf', mod'), E' )
	   end

	 | UNPACKStrExp(i, exp, sigexp) =>
	   let
		val  exp'     = trExp E exp
		val (inf',E') = trSigExp E sigexp
	   in
		( O.UnpackMod(i, exp', inf'), E' )
	   end


    and trStrPat E (StrPat(i, strid as StrId(i', strid'), sigexp)) =
	let
	    val (modid',stamp) = trStrId_bind E strid
	    val (inf',E')      = trSigExp E sigexp
	    val  _             = insertStr(E, strid', (i', stamp, E'))
	in
	    (modid', inf')
	end

    and trStrPats E = List.map (trStrPat E)



  (* Signatures and specifications *)

    and trAtSigExp E =
	fn ANYAtSigExp(i) =>
	   let
		val E' = BindEnv.new()
	   in
		( O.TopInf(i), E' )
	   end

	 | SIGAtSigExp(i, spec) =>
	   let
		val _      = insertScope E
		val specs' = trSpec E spec
		val E'     = splitScope E
	   in
		( O.SigInf(i, specs'), E' )
	   end

	 | LONGSIGIDAtSigExp(i, sigid) =>
	   let
		val (inflongid',E') = trLongSigId E sigid
	   in
		( O.ConInf(i, inflongid'), E' )
	   end

	 | LETAtSigExp(i, dec, sigexp) =>
	   let
		val  _        = insertScope E
		val  decs'    = trDec E dec
		val (inf',E') = trSigExp E sigexp
		val  _        = deleteScope E
	   in
		( O.LetInf(i, decs', inf'), E' )
	   end

	 | PARAtSigExp(i, sigexp) => trSigExp E sigexp


    and trSigExp E =
	fn ATSIGEXPSigExp(i, atsigexp) => trAtSigExp E atsigexp

	 | APPSigExp(i, sigexp, atstrexp) =>
	   let
		val (inf',E') = trSigExp E sigexp
		val (mod',_)  = trAtStrExp E atstrexp
	   in
		( O.AppInf(i, inf', mod'), E' )
	   end

	 | FCTSigExp(i, strpat, sigexp) =>
	   let
		val  _             = insertScope E
		val (modid',inf1') = trStrPat E strpat
		val (inf2',E')     = trSigExp E sigexp
		val  _             = deleteScope E
	   in
		( O.ArrInf(i, modid', inf1', inf2'), E' )
	   end

	 | WHERESigExp(i, sigexp1, sigexp2) =>
	   let
		val (inf1',E1) = trSigExp E sigexp1
		val (inf2',E2) = trSigExp E sigexp2
		val  _         = unionCompose(E1, E2)
	   in
		( O.CompInf(i, inf1', inf2'), E1 )
	   end


    and trSpec  E spec = Vector.rev(Vector.fromList(trSpec' (E,[]) spec))
    and trSpec'(E,acc) =
	fn VALSpec(i, valdesc) =>
		trValDesco' (E,acc) (SOME valdesc)

	 | TYPESpec(i, typdesc) =>
		trTypDesco' (E,acc) (SOME typdesc)

	 | DATATYPESpec(i, datdesc) =>
	   let
		val  _                = trDatDesco_lhs E (SOME datdesc)
		val (tspecs',cspecs') = trDatDesco_rhs E (SOME datdesc)
	   in
		cspecs' @ O.RecSpec(i, tspecs') :: acc
	   end

	 | REPLICATIONSpec(i, tycon as TyCon(i', tycon'), longtycon) =>
	   let
		val (typid',stamp)  = trTyCon_bind E tycon
		val (typlongid',E') = trLongTyCon E longtycon
		val  typlongido'    = case typlongid'
					of O.ShortId _              => NONE
					 | O.LongId(_,typlongid',_) =>
						SOME typlongid'
		val  _              = insertDisjointTy(E, tycon', (i',stamp,E'))
				      handle CollisionTy _ =>
					error(i', E.SpecTyConDuplicate tycon')
	   in
		foldiVals (trOpenSpecVal (E,i,typlongido'))
		 (O.TypSpec(i, typid',
			       O.ConTyp(infoLong longtycon, typlongid')) :: acc)
		 E'
	   end

	 | CONSTRUCTORSpec(i, dcondesc) =>
		trDconDesco' (E,acc) (SOME dcondesc)

	 | STRUCTURESpec(i, strdesc) =>
		trStrDesco' (E,acc) (SOME strdesc)

	 | SIGNATURESpec(i, sigdesc) =>
		trSigDesco' (E,acc) (SOME sigdesc)

	 | INCLUDESpec(i, sigexp) =>
	   let
		val (inf',E') = trSigExp E sigexp
		val _ =
		    unionDisjoint(E,E')
		    handle CollisionInf x => error(i, E.SpecFixDuplicate x)
			 | CollisionVal x => error(i, E.SpecVIdDuplicate x)
			 | CollisionTy  x => error(i, E.SpecTyConDuplicate x)
			 | CollisionStr x => error(i, E.SpecStrIdDuplicate x)
			 | CollisionSig x => error(i, E.SpecSigIdDuplicate x)
	   in
		O.ExtSpec(i, inf') :: acc
	   end

	 | EMPTYSpec(i) =>
		acc

	 | SEQSpec(i, spec1, spec2) =>
		trSpec' (E, trSpec' (E,acc) spec1) spec2

	 | SHARINGTYPESpec(i, spec, longtycons) =>
	   let
		val specs'      = List.rev(trSpec' (E,[]) spec)
		val typlongids' = List.map (#1 o trLongTyCon E) longtycons
		val rspecs'     = List.rev(Sharing.shareTyp(specs',typlongids'))
	   in
		rspecs' @ acc
	   end

	 | SHARINGSIGNATURESpec(i, spec, longsigids) =>
	   let
		val specs'      = List.rev(trSpec' (E,[]) spec)
		val inflongids' = List.map (#1 o trLongSigId E) longsigids
		val rspecs'     = List.rev(Sharing.shareSig(specs',inflongids'))
	   in
		rspecs' @ acc
	   end

	 | SHARINGSpec(i, spec, longstrids) =>
	   let
		val specs'      = List.rev(trSpec' (E,[]) spec)
		val modlongids' = List.map (#1 o trLongStrId E) longstrids
		val rspecs'     = List.rev(Sharing.shareStr(specs',modlongids'))
	   in
		rspecs' @ acc
	   end

	 | INFIXSpec(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.LEFT)
		val fix'    = O.Fix(i, fix)
		val spec'   = O.FixSpec(i, vallab', fix')
		val _       = insertDisjointInf(E, vid', (i', SOME(LEFT, n)))
			      handle CollisionInf vid' =>
				error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end

	 | INFIXRSpec(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.RIGHT)
		val fix'    = O.Fix(i, fix)
		val spec'   = O.FixSpec(i, vallab', fix')
		val _       = insertDisjointInf(E, vid', (i', SOME(RIGHT, n)))
			      handle CollisionInf vid' =>
				error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end

	 | NONFIXSpec(i, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.NONFIX
		val fix'    = O.Fix(i, fix)
		val spec'   = O.FixSpec(i, vallab', fix')
		val _       = insertDisjointInf(E, vid', (i', NONE))
			      handle CollisionInf vid' =>
				error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end

	 | PRIMITIVETYPESpec(i, tyvarseq, tycon as TyCon(i',tycon'), s) =>
	   let
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq	(* ignore *)
		val  _             = deleteScope E
		val  spec'         = O.TypSpec(i, typid', O.PrimTyp(i,s))
		val  _             = insertTy(E, tycon',
						 (i, stamp, BindEnv.new()))
	   in
		spec' :: acc
	   end

	 | PRIMITIVESIGNATURESpec(i, sigid as SigId(i',sigid'), strpats, s) =>
	   let
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats	(* ignore *)
		val  _             = deleteScope E
		val  spec'         = O.InfSpec(i', infid', O.PrimInf(i,s))
		val  _             = insertSig(E, sigid',
						    (i', stamp, BindEnv.new()))
	   in
		spec' :: acc
	   end


    and trOpenSpecVal (E,i,modlongido') (vid', (_,stamp1,is), acc) =
	let
	    val name       = VId.toString vid'
	    val stamp2     = Stamp.new()
	    val valid1'    = O.Id(i, stamp1, Name.ExId name)
	    val valid2'    = O.Id(i, stamp2, Name.ExId name)
	    val vallongid' = case modlongido'
			       of NONE            => O.ShortId(i, valid1')
			        | SOME modlongid' =>
				   O.LongId(i, modlongid',
					    O.Lab(i, Label.fromString name))
	    val typ'       = O.SingTyp(i, vallongid')
	    val _          = insertDisjointVal(E, vid', (i,stamp2,is))
			     handle CollisionVal _ =>
				error(i, E.SpecVIdDuplicate vid')
	in
	    O.ValSpec(i, valid2', typ') :: acc
	end




  (* Descriptions *)

    and trValDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWValDesc(_, _, vid as VId(i',vid'), ty, valdesco)) =>
	   let
		val  i             = Source.over(i', infoTy ty)
		val (valid',stamp) = trVId_bind E vid
		val  _             = insertScope E
		val  varids'       = trAllTy E ty
		val  typ'          = alltyp(varids', trTy E ty)
		val  _             = deleteScope E
		val  spec'         = O.ValSpec(i, valid', typ')
		val  _             = insertDisjointVal(E, vid', (i', stamp, V))
				     handle CollisionVal vid' =>
					error(i', E.SpecVIdDuplicate vid')
	   in
		trValDesco' (E, spec'::acc) valdesco
	   end

	 | SOME(EQUALValDesc(i, _, vid as VId(i',vid'), _, longvid, valdesco))=>
	   let
		val (valid',stamp)  = trVId_bind E vid
		val (vallongid',is) = trLongVId E longvid
		val  typ'           = O.SingTyp(O.infoLongid vallongid',
						vallongid')
		val  spec'          = O.ValSpec(i, valid', typ')
		val  _              = insertDisjointVal(E, vid', (i', stamp, V))
				      handle CollisionVal vid' =>
					error(i', E.SpecVIdDuplicate vid')
	   in
		trValDesco' (E, spec'::acc) valdesco
	   end


    and trTypDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWTypDesc(_, tyvarseq, tycon as TyCon(i',tycon'), typdesco)) =>
	   let
		val  i             = Source.over(infoSeq tyvarseq, i')
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  _             = deleteScope E
		val  funtyp'       = funtyp(varids', O.AbsTyp(i',false))
		val  spec'         = O.TypSpec(i, typid', funtyp')
		val  _             = insertDisjointTy(E, tycon',
						   (i', stamp, BindEnv.new()))
				     handle CollisionTy _ =>
					error(i', E.SpecTyConDuplicate tycon')
	   in
		trTypDesco' (E, spec'::acc) typdesco
	   end

	 | SOME(EQUALTypDesc(_, tyvarseq, tycon as TyCon(i',tycon'),
							ty, typdesco)) =>
	   let
		val  i             = Source.over(infoSeq tyvarseq, infoTy ty)
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  typ'          = trTy E ty
		val  _             = deleteScope E
		val  funtyp'       = funtyp(varids', typ')
		val  spec'         = O.TypSpec(i, typid', funtyp')
		val  _             = insertDisjointTy(E, tycon',
						   (i', stamp, BindEnv.new()))
				     handle CollisionTy _ =>
					error(i', E.SpecTyConDuplicate tycon')
	   in
		trTypDesco' (E, spec'::acc) typdesco
	   end


    and trDatDesco_lhs E =
	fn NONE => ()

	 | ( SOME(CLOSEDDatDesc(i, tyvarseq, tycon, _, datdesco))
	   | SOME(OPENDatDesc(i, tyvarseq, tycon, datdesco)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (typid',stamp)   = trTyCon_bind E tycon
		val _                = insertDisjointTy(E, tycon',
						   (i', stamp, BindEnv.new()))
				       handle CollisionTy _ =>
					 error(i', E.SpecTyConDuplicate tycon')
	   in
		trDatDesco_lhs E datdesco
	   end

    and trDatDesco_rhs E datdesco =
	let
	    val (tspecs',cspecs') = trDatDesco_rhs' (E,[],[]) datdesco
	in
	    (Vector.rev(Vector.fromList tspecs'), cspecs')
	end

    and trDatDesco_rhs' (E,acc1,acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(CLOSEDDatDesc(_, tyvarseq, tycon, condesc, datdesco)) =>
	   let
		val  i'            = infoConDesc condesc
		val  i             = Source.over(infoSeq tyvarseq, i')
		val (typid',E')    = trTyCon E tycon	(* bound before *)
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  i_typid       = O.infoId typid'
		val  contyp'       = O.ConTyp(i_typid,O.ShortId(i_typid,typid'))
		val  typ'          = apptyp(List.map varToTyp varids', contyp')
		val (flds',specs') = trConDesco (E,E',varids',typ')
						(SOME condesc)
		val  _             = deleteScope E
		val  sumtyp'       = O.SumTyp(i', O.Row(i', flds', false))
		val  spec'         = O.TypSpec(i, typid',
						  funtyp(varids', sumtyp'))
		val  _             = unionDisjoint(E,E')
				     handle CollisionVal vid' =>
					errorVId(E', vid', E.SpecVIdDuplicate)
	   in
		trDatDesco_rhs' (E, spec'::acc1, specs'@acc2) datdesco
	   end

	 | SOME(OPENDatDesc(_, tyvarseq, tycon, datdesco)) =>
	   let
		val  i'         = infoTyCon tycon
		val  i          = Source.over(infoSeq tyvarseq, i')
		val (typid',E') = trTyCon E tycon
		val  _          = insertScope E
		val  varids'    = trTyVarSeq E tyvarseq
		val  _          = deleteScope E
		val  funtyp'    = funtyp(varids', O.AbsTyp(i',true))
		val  spec'      = O.TypSpec(i, typid', funtyp')
	   in
		trDatDesco_rhs' (E, spec'::acc1, acc2) datdesco
	   end


    and trConDesco (E,E',varids',typ') condesco =
	let
	    val (fields',specs') = trConDesco'(E,E',varids',typ',[],[]) condesco
	in
	    (Vector.rev(Vector.fromList fields'), specs')
	end

    and trConDesco'(E,E',varids',typ',acc1,acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(ConDesc(i, _, vid as VId(i',vid'), tyo, condesco)) =>
	   let
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val (typ11',k)       = trTyo E tyo
		val  vallab'         = idToLab valid2'
		val  field'          = O.Field(i, vallab', typ11')
		val  typ1'           = alltyp(varids',
					      conarrowtyp E
						(O.infoTyp typ', typ11',typ',k))
		val  spec1'          = O.ValSpec(i, valid1', typ1')
		val  typ2'           = alltyp(varids',
					      if k = 0 then typ' else
					      O.ArrTyp(O.infoTyp typ',
						       typ11', typ'))
		val  spec2'          = O.ValSpec(i, valid2', typ2')
	   in
		( insertDisjointVal(E', conVId vid', (i', stamp1, V))
		; insertDisjointVal(E', vid',  (i', stamp2, T k))
		) handle CollisionVal _ => error(i', E.ConDescDuplicate vid');
		trConDesco' (E,E',varids',typ', field'::acc1,
			     spec2'::spec1'::acc2) condesco
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end


    and trDconDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWDconDesc(_, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dcondesco)) =>
	   let
		val  i               = Source.over(i', infoLong longtycon)
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  _               = insertScope E
		val (varids',typ12') = trTyVarSeqLongTyCon E
						(tyvarseq, longtycon)
		val (typ11',k)       = trTyo E tyo
		val  _               = deleteScope E
		val  typ1'           = alltyp(varids',
					      conarrowtyp E (O.infoTyp typ12',
							     typ11', typ12',k))
		val  spec1'          = O.ValSpec(i, valid1', typ1')
		val  typ2'           = alltyp(varids',
					      if k = 0 then typ12' else
					      O.ArrTyp(O.infoTyp typ12',
						       typ11', typ12'))
		val  spec2'          = O.ValSpec(i, valid2', typ2')
	   in
		( insertDisjointVal(E, conVId vid', (i', stamp1, V))
		; insertDisjointVal(E, vid',  (i', stamp2, C k))
		) handle CollisionVal _ => error(i', E.SpecVIdDuplicate vid');
		trDconDesco' (E, spec2'::spec1'::acc) dcondesco
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end

	 | SOME(EQUALDconDesc(_, _, vid as VId(i',vid'), _, longvid,
								dcondesco)) =>
	   let
		val  i               = Source.over(i', infoLong longvid)
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  vallongid1'     = trConLongVId E longvid
		val (vallongid2',is) = trLongVId E longvid
		val  typ1'           = O.SingTyp(O.infoLongid vallongid1',
						vallongid1')
		val  spec1'          = O.ValSpec(i, valid1', typ1')
		val  typ2'           = O.SingTyp(O.infoLongid vallongid2',
						vallongid2')
		val  spec2'          = O.ValSpec(i, valid2', typ2')
		val  k               = case is
					 of C k => k
					  | _   => error(i, E.DconDescNonCon)
	   in
		( insertDisjointVal(E, conVId vid', (i', stamp1, V))
		; insertDisjointVal(E, vid',  (i', stamp2, C k))
		) handle CollisionVal _ => error(i', E.SpecVIdDuplicate vid');
		trDconDesco' (E, spec2'::spec1'::acc) dcondesco
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end



    and trStrDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWStrDesc(_, strid as StrId(i',strid'), sigexp, strdesco)) =>
	   let
		val  i             = Source.over(i', infoSigExp sigexp)
		val (modid',stamp) = trStrId_bind E strid
		val (inf',E')      = trSigExp E sigexp
		val  spec'         = O.ModSpec(i, modid', inf')
		val  _             = insertDisjointStr(E, strid', (i',stamp,E'))
				     handle CollisionStr strid' =>
					error(i', E.SpecStrIdDuplicate strid')
	   in
		trStrDesco' (E, spec'::acc) strdesco
	   end

	 | SOME(EQUALStrDesc(_, strid as StrId(i',strid'), sigexpo, longstrid,
								strdesco)) =>
	   let
		val  i              = Source.over(i', infoLong longstrid)
		val (modid',stamp)  = trStrId_bind E strid
		val (modlongid',E') = trLongStrId E longstrid
		val  mod'           = modlongidToMod modlongid'
		val (mod'',E'')     = case sigexpo
					of NONE => (mod',E')
					 | SOME sigexp =>
					let
					    val (inf',E'') = trSigExp E sigexp
					    val i''   = Source.over(
							infoSigExp sigexp,
							O.infoLongid modlongid')
					    val mod'' = O.AnnMod(i'', mod',inf')
					in
					    (mod'',E'')
					end
		val  inf'          = O.SingInf(O.infoMod mod'', mod'')
		val  spec'         = O.ModSpec(i, modid', inf')
		val  _             = insertDisjointStr(E, strid',(i',stamp,E''))
				      handle CollisionStr strid' =>
					error(i', E.SpecStrIdDuplicate strid')
	   in
		trStrDesco' (E, spec'::acc) strdesco
	   end



    and trSigDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWSigDesc(_, sigid as SigId(i',sigid'), strpats, sigdesco)) =>
	   let
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats
		val  inf'          = funinf(modid_infs', O.AbsInf(i'))
		val  _             = deleteScope E
		val  spec'         = O.InfSpec(i', infid', inf')
		val  _             = insertDisjointSig(E, sigid',
						    (i', stamp, BindEnv.new()))
				     handle CollisionSig _ =>
					error(i', E.SpecSigIdDuplicate sigid')
	   in
		trSigDesco' (E, spec'::acc) sigdesco
	   end

	 | SOME(EQUALSigDesc(_, sigid as SigId(i',sigid'), strpats, sigexp,
								sigdesco)) =>
	   let
		val  i             = Source.over(i', infoSigExp sigexp)
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats
		val (inf',E')      = trSigExp E sigexp
		val  inf''         = funinf(modid_infs', inf')
		val  _             = deleteScope E
		val  spec'         = O.InfSpec(i', infid', inf'')
		val  _             = insertDisjointSig(E, sigid', (i',stamp,E'))
				     handle CollisionSig _ =>
					error(i', E.SpecSigIdDuplicate sigid')
	   in
		trSigDesco' (E, spec'::acc) sigdesco
	   end



  (* Imports *)

    and trImp (E,E') imp = Vector.rev(Vector.fromList(trImp' (E,E',[]) imp))
    and trImp'(E,E',acc) =
	fn VALImp(i, valitem) =>
		trValItemo' (E,E',acc) (SOME valitem)

	 | TYPEImp(i, typitem) =>
		trTypItemo' (E,E',acc) (SOME typitem)

	 | DATATYPEImp(i, datitem) =>
	   let
		val   _             = trDatItemo_lhs (E,E') (SOME datitem)
		val (timps',cimps') = trDatItemo_rhs (E,E') (SOME datitem)
	   in
		cimps' @ O.RecImp(i, timps') :: acc
	   end

	 | CONSTRUCTORImp(i, dconitem) =>
		trDconItemo' (E,E',acc) (SOME dconitem)

	 | STRUCTUREImp(i, stritem) =>
		trStrItemo' (E,E',acc) (SOME stritem)

	 | SIGNATUREImp(i, sigitem) =>
		trSigItemo' (E,E',acc) (SOME sigitem)

	 | EMPTYImp(i) =>
		acc

	 | SEQImp(i, imp1, imp2) =>
		trImp' (E,E', trImp' (E,E',acc) imp1) imp2

	 | INFIXImp(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.LEFT)
		val fix'    = O.Fix(i, fix)
		val imp'    = O.FixImp(i, vallab', O.SomeDesc(i,fix'))
		val _       = insertDisjointInf(E, vid', (i', SOME(LEFT, n)))
			      handle CollisionInf vid' =>
				error(i', E.ImpFixDuplicate vid')
	   in
		imp' :: acc
	   end

	 | INFIXRImp(i, n, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.INFIX(n, Fixity.RIGHT)
		val fix'    = O.Fix(i, fix)
		val imp'    = O.FixImp(i, vallab', O.SomeDesc(i,fix'))
		val _       = insertDisjointInf(E, vid', (i', SOME(RIGHT, n)))
			      handle CollisionInf vid' =>
				error(i', E.ImpFixDuplicate vid')
	   in
		imp' :: acc
	   end

	 | NONFIXImp(i, vid as VId(i',vid')) =>
	   let
		val vallab' = trVId_bind' E vid
		val fix     = Fixity.NONFIX
		val fix'    = O.Fix(i, fix)
		val imp'    = O.FixImp(i, vallab', O.SomeDesc(i,fix'))
		val _       = insertDisjointInf(E, vid', (i', NONE))
			      handle CollisionInf vid' =>
				error(i', E.ImpFixDuplicate vid')
	   in
		imp' :: acc
	   end


    and trOpenImpVal (E,i) (vid', (_,stamp,is), acc) =
	let
	    val name   = VId.toString vid'
	    val valid' = O.Id(i, stamp, Name.ExId name)
	    val _      = insertDisjointVal(E, vid', (i,stamp,is))
			 handle CollisionVal _ =>
				error(i, E.ImpVIdDuplicate vid')
	in
	    O.ValImp(i, valid', O.NoDesc(i)) :: acc
	end



  (* Items *)

    and trValItemo' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINValItem(_, _, vid as VId(i',vid'), valitemo)) =>
	   let
		val (valid',stamp) = trVId_bind E vid
		val  imp'          = O.ValImp(i', valid', O.NoDesc(i'))
		val  _             = if isSome(lookupVal(E', vid')) then () else
					error(i', E.ValItemUnbound vid')
		val  _             = insertDisjointVal(E, vid', (i', stamp, V))
				     handle CollisionVal vid' =>
					error(i', E.ImpVIdDuplicate vid')
	   in
		trValItemo' (E,E', imp'::acc) valitemo
	   end

	 | SOME(DESCValItem(_, _, vid as VId(i',vid'), ty, valitemo)) =>
	   let
		val  i             = Source.over(i', infoTy ty)
		val (valid',stamp) = trVId_bind E vid
		val  _             = insertScope E
		val  varids'       = trAllTy E ty
		val  typ'          = alltyp(varids', trTy E ty)
		val  _             = deleteScope E
		val  desc'         = O.SomeDesc(O.infoTyp typ', typ')
		val  imp'          = O.ValImp(i, valid', desc')
		val  _             = if isSome(lookupVal(E', vid')) then () else
					error(i', E.ValItemUnbound vid')
		val  _             = insertDisjointVal(E, vid', (i', stamp, V))
				     handle CollisionVal vid' =>
					error(i', E.ImpVIdDuplicate vid')
	   in
		trValItemo' (E,E', imp'::acc) valitemo
	   end


    and trTypItemo' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINTypItem(_, tycon as TyCon(i',tycon'), typitemo)) =>
	   let
		val (typid',stamp) = trTyCon_bind E tycon
		val  imp'          = O.TypImp(i', typid', O.NoDesc(i'))
		val  E''           = BindEnv.new()
		val  _             = if isSome(lookupTy(E',tycon')) then () else
					error(i', E.TypItemUnbound tycon')
		val  _             = insertDisjointTy(E, tycon', (i',stamp,E''))
				     handle CollisionTy _ =>
					error(i', E.ImpTyConDuplicate tycon')
	   in
		trTypItemo' (E,E', imp'::acc) typitemo
	   end

	 | SOME(DESCTypItem(_, tyvarseq, tycon as TyCon(i',tycon'), typitemo)) =>
	   let
		val  i             = Source.over(infoSeq tyvarseq, i')
		val (typid',stamp) = trTyCon_bind E tycon
		val  _             = insertScope E
		val  varids'       = trTyVarSeq E tyvarseq
		val  _             = deleteScope E
		val  funtyp'       = funtyp(varids', O.AbsTyp(i',false))
		val  desc'         = O.SomeDesc(O.infoTyp funtyp', funtyp')
		val  imp'          = O.TypImp(i, typid', desc')
		val  E''           = BindEnv.new()
		val  _             = if isSome(lookupTy(E',tycon')) then () else
					error(i', E.TypItemUnbound tycon')
		val _              = insertDisjointTy(E, tycon', (i',stamp,E''))
				     handle CollisionTy _ =>
					error(i', E.SpecTyConDuplicate tycon')
	   in
		trTypItemo' (E,E', imp'::acc) typitemo
	   end


    and trDatItemo_lhs (E,E') =
	fn NONE => ()

	 | ( SOME(PLAINDatItem(i, tycon, datitemo))
	   | SOME(DESCDatItem(i, _, tycon, _, datitemo)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (typid',stamp)   = trTyCon_bind E tycon
		val  E''             = BindEnv.new()
		val  _               = insertDisjointTy(E,tycon',(i',stamp,E''))
				       handle CollisionTy _ =>
					 error(i', E.ImpTyConDuplicate tycon')
	   in
		trDatItemo_lhs (E,E') datitemo
	   end

    and trDatItemo_rhs (E,E') datitemo =
	let
	    val (timps',cimps') = trDatItemo_rhs' (E,E',[],[]) datitemo
	in
	    (Vector.rev(Vector.fromList timps'), cimps')
	end

    and trDatItemo_rhs' (E,E',acc1, acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(PLAINDatItem(i, tycon as TyCon(i', tycon'), datitemo)) =>
	   let
		val (typid',E'') = trTyCon E tycon
		val  imp'        = O.TypImp(i, typid', O.NoDesc(i))
		val  E'''        = case lookupTy(E', tycon')
				    of SOME(_,_,E''') => E'''
				     | NONE => error(i',E.DatItemUnbound tycon')
		val  acc2'       = foldiVals (trOpenImpVal (E'',i)) acc2 E'''
		val  _           = unionDisjoint(E,E'')
	   in
		trDatItemo_rhs' (E,E', imp'::acc1, acc2') datitemo
	   end

	 | SOME(DESCDatItem(_, tyvarseq, tycon as TyCon(_,tycon'), conitem,
								   datitemo)) =>
	   let
		val  i'           = infoConItem conitem
		val  i            = Source.over(infoSeq tyvarseq, i')
		val (typid',E'')  = trTyCon E tycon
		val  _            = insertScope E
		val  varids'      = trTyVarSeq E tyvarseq
		val  i_typid      = O.infoId typid'
		val  contyp'      = O.ConTyp(i_typid, O.ShortId(i_typid,typid'))
		val  typ'         = apptyp(List.map varToTyp varids', contyp')
		val (flds',imps') = trConItemo (E,E'',varids',typ')
					       (SOME conitem)
		val  _            = deleteScope E
		val  sumtyp'      = O.SumTyp(i', O.Row(i', flds', false))
		val  desc'        = O.SomeDesc(i', funtyp(varids', sumtyp'))
		val  imp'         = O.TypImp(i, typid', desc')
		val  _            = if isSome(lookupTy(E', tycon')) then () else
					error(i', E.TypItemUnbound tycon')
		val  _            = unionDisjoint(E,E'')
				    handle CollisionVal vid' =>
					errorVId(E'', vid', E.ImpVIdDuplicate)
	   in
		trDatItemo_rhs' (E,E', imp'::acc1, imps'@acc2) datitemo
	   end


    and trConItemo (E,E',varids',typ') conitemo =
	let
	    val (fields',imps') = trConItemo' (E,E',varids',typ',[],[]) conitemo
	in
	    (Vector.rev(Vector.fromList fields'), imps')
	end

    and trConItemo'(E,E',varids',typ',acc1,acc2) =
	fn NONE => (acc1,acc2)

	 | SOME(ConItem(i, _, vid as VId(i',vid'), tyo, conitemo)) =>
	   let
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val (typ11',k)       = trTyo E tyo
		val  vallab'         = idToLab valid2'
		val  field'          = O.Field(i, vallab', typ11')
		val  typ1'           = alltyp(varids',
					      conarrowtyp E
						(O.infoTyp typ', typ11',typ',k))
		val  imp1'           = O.ValImp(i, valid1', O.SomeDesc(i,typ1'))
		val  typ2'           = alltyp(varids',
					      if k = 0 then typ' else
					      O.ArrTyp(O.infoTyp typ',
						       typ11', typ'))
		val  imp2'           = O.ValImp(i, valid2', O.SomeDesc(i,typ2'))
	   in
		case lookupVal(E', vid')
		  of SOME(_,_,T _) => ()
		   | SOME(_,_,_)   => error(i', E.ConItemNonCon vid')
		   | NONE          => error(i', E.ConItemUnbound vid');
		( insertDisjointVal(E, conVId vid', (i', stamp1, V))
		; insertDisjointVal(E, vid',  (i', stamp2, T k))
		) handle CollisionVal _ => error(i', E.ConItemDuplicate vid');
		trConItemo' (E,E',varids',typ', field'::acc1,
			     imp2'::imp1'::acc2) conitemo
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end


    and trDconItemo' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINDconItem(_, _, vid as VId(i',vid'), dconitemo)) =>
	   let
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  imp1'           = O.ValImp(i', valid1', O.NoDesc(i'))
		val  imp2'           = O.ValImp(i', valid2', O.NoDesc(i'))
		val  k               = case lookupVal(E', vid')
				       of SOME(_,_,C k) => k
					| SOME(_,_,_)   =>
					  error(i', E.DconItemNonCon vid')
					| NONE =>
					  error(i', E.DconItemUnbound vid')
	   in
		( insertDisjointVal(E, conVId vid', (i', stamp1, V))
		; insertDisjointVal(E, vid',  (i', stamp2, C k))
		) handle CollisionVal _ => error(i', E.ImpVIdDuplicate vid');
		trDconItemo' (E,E', imp2'::imp1'::acc) dconitemo
	   end

	 | SOME(DESCDconItem(_, _, vid as VId(i',vid'), tyo, tyvarseq,longtycon,
								 dconitemo)) =>
	   let
		val  i               = Source.over(i', infoLong longtycon)
		val (valid1',stamp1) = trConVId_bind' E vid
		val (valid2',stamp2) = trVId_bind E vid
		val  _               = insertScope E
		val (varids',typ12') = trTyVarSeqLongTyCon E
						(tyvarseq, longtycon)
		val (typ11',k)       = trTyo E tyo
		val  _               = deleteScope E
		val  typ1'           = alltyp(varids',
					      conarrowtyp E (O.infoTyp typ12',
							     typ11', typ12',k))
		val  imp1'           = O.ValImp(i, valid1', O.SomeDesc(i,typ1'))
		val  typ2'           = alltyp(varids',
					      if k = 0 then typ12' else
					      O.ArrTyp(O.infoTyp typ12',
						       typ11', typ12'))
		val  imp2'           = O.ValImp(i, valid2', O.SomeDesc(i,typ2'))
		val  k               = case lookupVal(E', vid')
				       of SOME(_,_,C k) => k
					| SOME(_,_,_)   =>
					  error(i', E.DconItemNonCon vid')
					| NONE =>
					  error(i', E.DconItemUnbound vid')
	   in
		( insertDisjointVal(E, conVId vid', (i', stamp1, V))
		; insertDisjointVal(E, vid',  (i', stamp2, C k))
		) handle CollisionVal _ => error(i', E.ImpVIdDuplicate vid');
		trDconItemo' (E,E', imp2'::imp1'::acc) dconitemo
	   end



    and trStrItemo' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINStrItem(_, strid as StrId(i',strid'), stritemo)) =>
	   let
		val (modid',stamp) = trStrId_bind E strid
		val  imp'          = O.ModImp(i', modid', O.NoDesc(i'))
		val  E''           = case lookupStr(E', strid')
				     of SOME(_,_,E'') => E''
				     | NONE => error(i',E.StrItemUnbound strid')
		val  _             = insertDisjointStr(E, strid',(i',stamp,E''))
				     handle CollisionStr strid' =>
					error(i', E.ImpStrIdDuplicate strid')
	   in
		trStrItemo' (E,E', imp'::acc) stritemo
	   end

	 | SOME(DESCStrItem(_, strid as StrId(i',strid'), sigexp, stritemo)) =>
	   let
		val  i             = Source.over(i', infoSigExp sigexp)
		val (modid',stamp) = trStrId_bind E strid
		val (inf',E'')     = trSigExp E sigexp
		val  desc'         = O.SomeDesc(O.infoInf inf', inf')
		val  imp'          = O.ModImp(i, modid', desc')
		val  _             = if isSome(lookupStr(E',strid')) then() else
					error(i', E.StrItemUnbound strid')
		val  _             = insertDisjointStr(E, strid',(i',stamp,E''))
				     handle CollisionStr strid' =>
					error(i', E.ImpStrIdDuplicate strid')
	   in
		trStrItemo' (E,E', imp'::acc) stritemo
	   end



    and trSigItemo' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINSigItem(_, sigid as SigId(i',sigid'), sigitemo)) =>
	   let
		val (infid',stamp) = trSigId_bind E sigid
		val  imp'          = O.InfImp(i', infid', O.NoDesc(i'))
		val  E''           = case lookupSig(E', sigid')
				       of SOME(_,_,E'') => E''
					| NONE =>
					  error(i', E.SigItemUnbound sigid')
		val  _             = insertDisjointSig(E, sigid',(i',stamp,E''))
				     handle CollisionSig _ =>
					error(i', E.ImpSigIdDuplicate sigid')
	   in
		trSigItemo' (E,E', imp'::acc) sigitemo
	   end

	 | SOME(DESCSigItem(_, sigid as SigId(i',sigid'), strpats, sigitemo)) =>
	   let
		val (infid',stamp) = trSigId_bind E sigid
		val  _             = insertScope E
		val  modid_infs'   = trStrPats E strpats
		val  inf'          = funinf(modid_infs', O.AbsInf(i'))
		val  _             = deleteScope E
		val  desc'         = O.SomeDesc(O.infoInf inf', inf')
		val  imp'          = O.InfImp(i', infid', desc')
		val  E''           = case lookupSig(E', sigid')
				     of SOME(_,_,E'') => E''
				     | NONE => error(i',E.SigItemUnbound sigid')
		val  _             = insertDisjointSig(E, sigid',(i',stamp,E''))
				     handle CollisionSig _ =>
					error(i', E.ImpSigIdDuplicate sigid')
	   in
		trSigItemo' (E,E', imp'::acc) sigitemo
	   end



  (* Announcements *)

    and trAnn (E,desc) ann = Vector.rev(Vector.fromList(trAnn' (E,desc,[]) ann))
    and trAnn'(E,desc,acc) =
	fn IMPORTAnn(i, imp, s) =>
	   let
		val url   = Url.fromString s
		val s     = loadSign(desc,url)
		val E'    = BindEnvFromSig.envFromSig(i, s)
		val _     = insertScope E
		val imps' = trImp (E,E') imp
		val _     = mergeScope E
	   in
		O.ImpAnn(i, imps', url) :: acc
	   end

	 | EMPTYAnn(i) =>
		acc

	 | SEQAnn(i, ann1, ann2) =>
		trAnn' (E, desc, trAnn' (E,desc,acc) ann1) ann2


  (* Programs and components *)

    fun trProgramo  E programo =
	    Vector.rev(Vector.fromList(trProgramo' (E,[]) programo))

    and trProgramo'(E,acc) =
	fn NONE => acc

	 | SOME(Program(i, dec, programo)) =>
	   let
		val acc' = trDec' (E,acc) dec
	   in
		trProgramo' (E,acc') programo
	   end


    fun trComponent (E,desc) (Component(i, ann, programo)) =
	let
	    val anns' = trAnn (E,desc) ann
	    val decs' = trProgramo E programo
	in
	    O.Comp(i, anns', decs')
	end


    fun translate E (desc, component) = trComponent (E,desc) component

  end
