(*
 * Standard ML derived forms
 *
 * Definition, Appendix A
 *
 * Extensions and modifications:
 *   - recursive expressions:
 *	rec pat => exp     ==>     let val rec x as pat = exp in x end
 *     where x is a fresh identifier.
 *   - exception declarations have been made a derived form:
 *	exception exbind          ==>  constructor exbind'
 *	vid <of ty> <and exbind>  ==>  vid <of ty> : exn <and exbind'>
 *   - abstype has been made a derived form:
 *	abstype datbind <withtype tybind> with dec end
 *	  ==>
 *	<type typbind> local datatype datbind in type typbind' dec end
 *     where typbind' contains a binding t = t for each tycon t bound in
 *     datbind. Note that this results in a different treatment of equality.
 *   - include takes longsigids:
 *	include longsigid_1 ... longsigid_n
 *	==>
 *	include longsigid_1 ; ... ; include longsigid_n
 *
 * We did NOT introduce a sharing signature ... and signature ... derived form
 * similar to types, because we consider that completely broken.
 *
 * Notes:
 * - Two phrases named Fmatch and Fmrule have been added to factorize FvalBind.
 * - A phrase named TyReaDesc has been added to factorize type
 *   realisation signature expressions.
 * - In Fvalbinds we do not enforce that all optional type annotations are
 *   syntactically identical (as the Definition enforces, although this seems
 *   to be a mistake).
 * - The Definition is somewhat inaccurate about the derived forms of Exp
 *   [Definition, Appendix A, Figure 15] in that most forms are actually AtExp
 *   derived forms, as can be seen from the full grammar [Definition,
 *   Appendix B, Figure 20]. To achieve consistency, the equivalent forms must
 *   be put in parentheses in some case.
 * - The same goes for pattern derived forms [Definition, Appendix A, Figure 16;
 *   Appendix B, Figure 22].
 *)


structure DerivedForms :> DERIVED_FORMS =
  struct

    (* Import *)

    structure Grammar = InputGrammar
    structure G       = Grammar

    type Info      = Grammar.Info

    type Lab       = Grammar.Lab
    type VId       = Grammar.VId
    type StrId     = Grammar.StrId
    type FunId     = Grammar.FunId
    type LongVId   = Grammar.LongVId
    type LongTyCon = Grammar.LongTyCon
    type LongStrId = Grammar.LongStrId
    type LongSigId = Grammar.LongSigId
    type LongFunId = Grammar.LongFunId

    type Op        = Grammar.Op
    type AtExp     = Grammar.AtExp
    type AppExp    = Grammar.Exp
    type InfExp    = Grammar.Exp
    type Exp       = Grammar.Exp
    type ExpRow    = Grammar.ExpRow
    type Match     = Grammar.Match
    type Mrule     = Grammar.Mrule
    type Dec       = Grammar.Dec
    type ValBind   = Grammar.ValBind
    type FvalBind  = Grammar.FvalBind
    type ExBind    = Grammar.DconBind
    type Fmatch    = Grammar.Match
    type Fmrule    = Grammar.Mrule
    type Fpat      = Grammar.Pat
    type TypBind   = Grammar.TypBind
    type DatBind   = Grammar.DatBind
    type AtPat     = Grammar.AtPat
    type PatRow    = Grammar.PatRow
    type Pat       = Grammar.Pat
    type Ty        = Grammar.Ty
    type TyVarSeq  = Grammar.TyVarSeq
    type StrExp    = Grammar.StrExp
    type StrBind   = Grammar.StrBind
    type FunBind   = Grammar.FunBind
    type SigExp    = Grammar.SigExp
    type TyReaDesc = (Info * TyVarSeq * LongTyCon * Ty) list
    type Spec      = Grammar.Spec
    type ValDesc   = Grammar.ValDesc
    type TypDesc   = Grammar.TypDesc
    type DatDesc   = Grammar.DatDesc
    type ExDesc    = Grammar.DconDesc
    type FunDesc   = Grammar.FunDesc
    type Program   = Grammar.Program


    (* Additional types *)

    type AppExp = Exp
    type InfExp = Exp

    type Fmatch = Match
    type Fmrule = Mrule
    type Fpat   = Pat

    type TyReaDesc = (G.Info * G.TyVarSeq * G.LongTyCon * G.Ty) list

    (* Some helpers *)

    fun vid_NIL(I)             = G.VId(I, VId.fromString "nil")
    fun vid_CONS(I)            = G.VId(I, VId.fromString "::")
    fun longvid_CONS(I)        = G.SHORTLong(I, vid_CONS I)

    fun LONGVIDExp(I, longvid) = G.ATEXPExp(I, G.LONGVIDAtExp(I, G.WITHOp,
								 longvid))
    fun LONGVIDPat(I, longvid) = G.ATPATPat(I, G.LONGVIDAtPat(I, G.WITHOp,
								 longvid))
    fun VIDExp(I, vid)         = LONGVIDExp(I, G.SHORTLong(I, vid))
    fun VIDPat(I, vid)         = LONGVIDPat(I, G.SHORTLong(I, vid))

    fun NILExp(I)              = VIDExp(I, vid_NIL I)
    fun CONSExp(I)             = VIDExp(I, vid_CONS I)
    fun NILPat(I)              = VIDPat(I, vid_NIL I)

    fun tycon_EXN(I)           = G.TyCon(I, TyCon.fromString "exn")
    fun longtycon_EXN(I)       = G.SHORTLong(I, tycon_EXN(I))


    (* Functions to handle rewriting of withtype declarations *)

    fun equalTyCon(G.TyCon(_,tycon1), G.TyCon(_,tycon2)) = tycon1 = tycon2
    fun equalTyVar(G.TyVar(_,tyvar1), G.TyVar(_,tyvar2)) = tyvar1 = tyvar2

    fun lookupTyCon(tycon, G.TypBind(_, tyvarseq, tycon', ty, typbind_opt)) =
	    if equalTyCon(tycon, tycon') then
		(tyvarseq, ty)
	    else
	  	lookupTyCon(tycon, Option.valOf typbind_opt)
		(* may raise Option *)


    fun replaceTy (G.Seq(_,tyvars), G.Seq(_,tys)) (ty as G.TYVARTy(i, tyvar)) =
	let
	    fun loop(tyvar'::tyvars', ty'::tys') =
		    if equalTyVar(tyvar, tyvar') then
			ty'
		    else
			loop(tyvars', tys')
	      | loop([], _) =
		    ty
	      | loop(_, []) =
		    Error.error(i, "type sequence has wrong arity")
	in
	    loop(tyvars, tys)
	end

      | replaceTy tyvarseq_tyseq (G.RECORDTy(I, tyrow_opt)) =
	    G.RECORDTy(I, Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

      | replaceTy tyvarseq_tyseq (G.TYCONTy(I, tyseq', tycon)) =
	    G.TYCONTy(I, replaceTySeq tyvarseq_tyseq tyseq', tycon)

      | replaceTy tyvarseq_tyseq (G.ARROWTy(I, ty1, ty2)) =
	    G.ARROWTy(I, replaceTy tyvarseq_tyseq ty1,
			 replaceTy tyvarseq_tyseq ty2)

      | replaceTy tyvarseq_tyseq (G.PARTy(I, ty)) =
	    G.PARTy(I, replaceTy tyvarseq_tyseq ty)

    and replaceTyRow tyvarseq_tyseq (G.ROWTyRow(I, lab, ty, tyrow_opt)) =
	    G.ROWTyRow(I, lab, replaceTy tyvarseq_tyseq ty, 
			  Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

    and replaceTySeq tyvarseq_tyseq (G.Seq(I, tys)) =	  
	    G.Seq(I, List.map (replaceTy tyvarseq_tyseq) tys)


    fun rewriteTy typbind (ty as G.TYVARTy _) = ty

      | rewriteTy typbind (G.RECORDTy(I, tyrow_opt)) =
	    G.RECORDTy(I, Option.map (rewriteTyRow typbind) tyrow_opt)

      | rewriteTy typbind (ty as G.TYCONTy(I, tyseq, longtycon as G.DOTLong _))=
	    G.TYCONTy(I, rewriteTySeq typbind tyseq, longtycon)

      | rewriteTy typbind (ty as G.TYCONTy(I, tyseq,
					  longtycon as G.SHORTLong(_, tycon))) =
	let 
	    val tyseq' = rewriteTySeq typbind tyseq
	in
	    let
		val (tyvarseq', ty') = lookupTyCon(tycon, typbind)
	    in
		replaceTy (tyvarseq',tyseq') ty'
	    end
	    handle Option => G.TYCONTy(I, tyseq', longtycon)
	end

      | rewriteTy typbind (G.ARROWTy(I, ty1, ty2)) =
	    G.ARROWTy(I, rewriteTy typbind ty1, rewriteTy typbind ty2)

      | rewriteTy typbind (G.PARTy(I, ty)) =
	    G.PARTy(I, rewriteTy typbind ty)

    and rewriteTyRow typbind (G.ROWTyRow(I, lab, ty, tyrow_opt)) =
	    G.ROWTyRow(I, lab, rewriteTy typbind ty,
			  Option.map (rewriteTyRow typbind) tyrow_opt)

    and rewriteTySeq typbind (G.Seq(I, tys)) =
	    G.Seq(I, List.map (rewriteTy typbind) tys)

    fun rewriteConBind typbind (G.ConBind(I, op_opt, vid, ty_opt, conbind_opt))=
	    G.ConBind(I, op_opt, vid,
			 Option.map (rewriteTy typbind) ty_opt,
			 Option.map (rewriteConBind typbind) conbind_opt)

    fun rewriteDatBind typbind (G.CLOSEDDatBind(I, tyvarseq, tycon, conbind,
							      datbind_opt)) =
	    G.CLOSEDDatBind(I, tyvarseq, tycon, rewriteConBind typbind conbind,
			       Option.map (rewriteDatBind typbind) datbind_opt)
      | rewriteDatBind typbind (G.OPENDatBind(I, tyvarseq, tycon,
							      datbind_opt)) =
	    G.OPENDatBind(I, tyvarseq, tycon,
			     Option.map (rewriteDatBind typbind) datbind_opt)


    fun toTy tyvar = G.TYVARTy(G.infoTyVar tyvar, tyvar)

    fun toTypBind(G.NEWTypDesc(I, tyvarseq, tycon, typdesc_opt)) =
	let
	    val G.Seq(I',tyvarseq') = tyvarseq
	    val tyseq = G.Seq(I', List.map toTy tyvarseq')
	    val ty    = G.TYCONTy(I, tyseq, G.SHORTLong(I,tycon))
	in
	    G.TypBind(I, tyvarseq, tycon, ty, Option.map toTypBind typdesc_opt)
	end

      | toTypBind(G.EQUALTypDesc(I, tyvarseq, tycon, ty, typdesc_opt)) =
	    G.TypBind(I, tyvarseq, tycon, ty, Option.map toTypBind typdesc_opt)

    (* Functions to handle rewriting of withtype specifications *)

    fun rewriteConDesc typbind (G.ConDesc(I, op_opt, vid, ty_opt, condesc_opt))=
	    G.ConDesc(I, op_opt, vid,
			 Option.map (rewriteTy typbind) ty_opt,
			 Option.map (rewriteConDesc typbind) condesc_opt)

    fun rewriteDatDesc typbind (G.CLOSEDDatDesc(I, tyvarseq, tycon, condesc,
							      datdesc_opt)) =
	    G.CLOSEDDatDesc(I, tyvarseq, tycon, rewriteConDesc typbind condesc,
			       Option.map (rewriteDatDesc typbind) datdesc_opt)
      | rewriteDatDesc typbind (G.OPENDatDesc(I, tyvarseq, tycon,
							      datdesc_opt)) =
	    G.OPENDatDesc(I, tyvarseq, tycon,
			     Option.map (rewriteDatDesc typbind) datdesc_opt)


    (* Rewriting of abstype *)

    fun toTy tyvar = G.TYVARTy(G.infoTyVar tyvar, tyvar)

    fun redeclare ( G.CLOSEDDatBind(I, tyvarseq, tycon, _, datbind_opt)
		  | G.OPENDatBind(I, tyvarseq, tycon, datbind_opt)) =
	let
	    val G.Seq(I',tyvarseq') = tyvarseq
	    val tyseq     = G.Seq(I', List.map toTy tyvarseq')
	    val I_tycon   = G.infoTyCon tycon
	    val longtycon = G.SHORTLong(I_tycon, tycon)
	    val ty        = G.TYCONTy(I_tycon, tyseq,longtycon)
	in
	    G.TypBind(I, tyvarseq, tycon, ty, Option.map redeclare datbind_opt)
	end



    (* Patterns *)

    fun UNITAtPat(I) = G.TUPLEAtPat(I, [])

    val TUPLEAtPat   = G.TUPLEAtPat
    val WITHFUNPat   = G.WITHFUNPat

    fun LISTAtPat(I, pats) =
	let
	    fun toPatList []          = NILPat(I)
	      | toPatList(pat::pats') =
		G.APPPat(I, LONGVIDPat(I, longvid_CONS(I)),
			    TUPLEAtPat(I, [pat,toPatList pats']))
	in
	    G.PARAtPat(I, toPatList pats)
	end


    fun VIDPatRow(I, vid as G.VId(I',vid'), ty_opt, pat_opt, patrow_opt) =
	let
	    val lab    = G.Lab(I', Lab.fromString(VId.toString vid'))
	    val vidPat = VIDPat(I', vid)
	    val pat1   = case ty_opt
			   of NONE    => vidPat
			    | SOME ty => G.TYPEDPat(I, vidPat, ty)
	    val pat    = case pat_opt
			   of NONE      => pat1
			    | SOME pat' => G.ASPat(I, pat1, pat')
	in
	    G.ROWPatRow(I, lab, pat, patrow_opt)
	end



    (* Expressions *)

    fun UNITAtExp(I) = G.TUPLEAtExp(I, [])

    val TUPLEAtExp   = G.TUPLEAtExp
    val HASHAtExp    = G.HASHAtExp
    val CASEExp      = G.CASEExp
    val IFExp        = G.IFExp
    val ORELSEExp    = G.ORELSEExp
    val ANDALSOExp   = G.ANDALSOExp
    val WHILEExp     = G.WHILEExp
    val SEQAtExp     = G.SEQAtExp

    fun LETAtExp(I, dec, [exp]) = G.LETAtExp(I, dec, exp)
      | LETAtExp(I, dec,  exps) =
	    G.LETAtExp(I, dec, G.ATEXPExp(I, SEQAtExp(I, exps)))

    fun LISTAtExp(I, exps) =
	let
	    fun toExpList []          = NILExp(I)
	      | toExpList(exp::exps') =
		  G.APPExp(I, CONSExp(I), TUPLEAtExp(I, [exp, toExpList exps']))
	in
	    G.PARAtExp(I, toExpList exps)
	end

    fun RECExp(I, pat, exp) =
	let
	    val I'      = G.infoPat pat
	    val vid     = G.VId(I', VId.invent())
	    val asPat   = G.ASPat(I', VIDPat(I', vid), pat)
	    val valbind = G.RECValBind(I, G.PLAINValBind(I, asPat, exp, NONE))
	    val dec     = G.VALDec(I, G.Seq(I',[]), valbind)
	in
	    G.ATEXPExp(I, G.LETAtExp(I, dec, VIDExp(I', vid)))
	end


    fun VIDExpRow(I, vid as G.VId(I',vid'), ty_opt, exprow_opt) =
	let
	    val lab    = G.Lab(I', Lab.fromString(VId.toString vid'))
	    val vidExp = VIDExp(I', vid)
	    val exp    = case ty_opt
			   of NONE    => vidExp
			    | SOME ty => G.TYPEDExp(I, vidExp, ty)
	in
	    G.ROWExpRow(I, lab, exp, exprow_opt)
	end


    (* Types *)

    fun TUPLETy(I, [ty]) = ty
      | TUPLETy(I,  tys) =
	let
	    fun toTyRow(n, [])       = NONE
	      | toTyRow(n, ty::tys') =
		  SOME(G.ROWTyRow(I, G.Lab(I, Lab.fromInt n), ty,
				     toTyRow(n+1, tys')))
	in
	    G.RECORDTy(I, toTyRow(1, tys))
	end


    (* Declarations *)

    val FUNDec       = G.FUNDec
    val EXCEPTIONDec = G.CONSTRUCTORDec
    val FvalBind     = G.FvalBind
    val EQUALExBind  = G.EQUALDconBind
    val Fmatch       = G.Match
    val Fmrule       = G.Mrule

    fun Fpat p       = p


    fun DATATYPEDec(I, datbind, NONE)         = G.DATATYPEDec(I, datbind)
      | DATATYPEDec(I, datbind, SOME typbind) =
	let
	    val datbind' = rewriteDatBind typbind datbind
	in
	    G.SEQDec(I, G.DATATYPEDec(G.infoDatBind datbind, datbind'),
			G.TYPEDec(G.infoTypBind typbind, typbind))
	end

    fun ABSTYPEDec(I, datbind, NONE, dec) =
	let
	    val datatypeDec = G.DATATYPEDec(I, datbind)
	    val typeDec     = G.TYPEDec(I, redeclare datbind)
	in
	    G.LOCALDec(I, datatypeDec, G.SEQDec(I, typeDec, dec))
	end

      | ABSTYPEDec(I, datbind, SOME typbind, dec) =
	let
	    val I'          = G.infoTypBind typbind
	    val datbind'    = rewriteDatBind typbind datbind
	    val datatypeDec = G.DATATYPEDec(I, datbind')
	    val typeDec     = G.TYPEDec(I, redeclare datbind')
	in
	    G.SEQDec(I, G.TYPEDec(I', typbind),
			G.LOCALDec(I, datatypeDec, G.SEQDec(I, typeDec, dec)))
	end


    fun OPENMULTIDec(I, [])                    = G.EMPTYDec(I)
      | OPENMULTIDec(I, longstrid::longstrids) =
	    G.SEQDec(I, G.OPENDec(I,longstrid), OPENMULTIDec(I,longstrids))

    fun INFIXMULTIDec(I, _, [])          = G.EMPTYDec(I)
      | INFIXMULTIDec(I, NONE, longvids) = INFIXMULTIDec(I, SOME 0, longvids)
      | INFIXMULTIDec(I, SOME d, longvid::longvids) =
	    G.SEQDec(I, G.INFIXDec(I, d, longvid),
			INFIXMULTIDec(I, SOME d, longvids))

    fun INFIXRMULTIDec(I, _, [])          = G.EMPTYDec(I)
      | INFIXRMULTIDec(I, NONE, longvids) = INFIXRMULTIDec(I, SOME 0, longvids)
      | INFIXRMULTIDec(I, SOME d, longvid::longvids) =
	    G.SEQDec(I, G.INFIXRDec(I, d, longvid),
			INFIXRMULTIDec(I, SOME d, longvids))

    fun NONFIXMULTIDec(I, [])                = G.EMPTYDec(I)
      | NONFIXMULTIDec(I, longvid::longvids) =
	    G.SEQDec(I, G.NONFIXDec(I,longvid), NONFIXMULTIDec(I,longvids))


    fun NEWExBind(I, op_opt, vid, ty_opt, dconbind_opt) =
	    G.NEWDconBind(I, op_opt, vid, ty_opt,
			     G.Seq(I,[]), longtycon_EXN(I), dconbind_opt)

    (* Structure bindings *)

    fun TRANSStrBind(I, strid, NONE, strexp, strbind_opt) =
	    G.StrBind(I, strid, strexp, strbind_opt)

      | TRANSStrBind(I, strid, SOME sigexp, strexp, strbind_opt) =
	    G.StrBind(I, strid, G.TRANSStrExp(I, strexp, sigexp), strbind_opt)

    fun OPAQStrBind(I, strid, sigexp, strexp, strbind_opt) =
	    G.StrBind(I, strid, G.OPAQStrExp(I, strexp, sigexp), strbind_opt)


    (* Structure expressions *)

    fun PARStrExp(I, strexp) = strexp

    fun APPDECStrExp(I, longfunid, dec) =
	    G.APPStrExp(I, longfunid, G.STRUCTStrExp(G.infoDec dec, dec))


    (* Functor bindings *)

    fun TRANSFunBind(I, funid, strid, sigexp, NONE, strexp, funbind_opt) =
	    G.FunBind(I, funid, strid, sigexp, strexp, funbind_opt)

      | TRANSFunBind(I, funid, strid,sigexp, SOME sigexp', strexp, funbind_opt)=
	    G.FunBind(I, funid, strid, sigexp, G.TRANSStrExp(I, strexp,sigexp'),
			 funbind_opt)

    fun OPAQFunBind(I, funid, strid, sigexp, sigexp', strexp, funbind_opt) =
	    G.FunBind(I, funid, strid, sigexp, G.OPAQStrExp(I, strexp, sigexp'),
			 funbind_opt)


    fun TRANSSPECFunBind(I, funid, spec, sigexp_opt, strexp, funbind_opt) =
	let
	    val I'     = G.infoStrExp strexp
	    val strid  = G.StrId(I', StrId.invent())
	    val sigexp = G.SIGSigExp(G.infoSpec spec, spec)

	    val dec    = G.OPENDec(I',G.SHORTLong(I',strid))
	    val strexp'= case sigexp_opt
			   of NONE         => strexp
			    | SOME sigexp' => G.TRANSStrExp(I', strexp, sigexp')
	    val letexp = G.LETStrExp(I', dec, strexp')
	in
	    G.FunBind(I, funid, strid, sigexp, letexp, funbind_opt)
	end

    fun OPAQSPECFunBind(I, funid, spec, sigexp', strexp, funbind_opt) =
	let
	    val I'     = G.infoStrExp strexp
	    val strid  = G.StrId(I', StrId.invent())
	    val sigexp = G.SIGSigExp(G.infoSpec spec, spec)

	    val dec    = G.OPENDec(I',G.SHORTLong(I',strid))
	    val strexp'= G.TRANSStrExp(I', strexp, sigexp')
	    val letexp = G.LETStrExp(I', dec, strexp')
	in
	    G.FunBind(I, funid, strid, sigexp, letexp, funbind_opt)
	end


    (* Specifications *)

    val FUNSpec       = G.VALSpec
    val SHARINGSpec   = G.SHARINGSpec
    val EXCEPTIONSpec = G.CONSTRUCTORSpec
    val EQUALExDesc   = G.EQUALDconDesc

    fun DATATYPESpec(I, datdesc, NONE)         = G.DATATYPESpec(I, datdesc)
      | DATATYPESpec(I, datdesc, SOME typdesc) =
	let
	    val datdesc' = rewriteDatDesc (toTypBind typdesc) datdesc
	in
	    G.SEQSpec(I, G.DATATYPESpec(G.infoDatDesc datdesc, datdesc'),
			 G.TYPESpec(G.infoTypDesc typdesc, typdesc))
	end

    fun INCLUDEMULTISpec(I, [])             = G.EMPTYSpec(I)
      | INCLUDEMULTISpec(I, longsigid::longsigids') =
	let
	    val spec1 = G.INCLUDESpec(I, G.LONGSIGIDSigExp(I, longsigid))
	in
	    G.SEQSpec(I, spec1, INCLUDEMULTISpec(I, longsigids'))
	end

    fun INFIXMULTISpec(I, _, [])          = G.EMPTYSpec(I)
      | INFIXMULTISpec(I, NONE, longvids) = INFIXMULTISpec(I, SOME 0, longvids)
      | INFIXMULTISpec(I, SOME d, longvid::longvids) =
	    G.SEQSpec(I, G.INFIXSpec(I, d, longvid),
			 INFIXMULTISpec(I, SOME d, longvids))

    fun INFIXRMULTISpec(I, _, [])          = G.EMPTYSpec(I)
      | INFIXRMULTISpec(I, NONE, longvids) = INFIXRMULTISpec(I, SOME 0,longvids)
      | INFIXRMULTISpec(I, SOME d, longvid::longvids) =
	    G.SEQSpec(I, G.INFIXRSpec(I, d, longvid),
			 INFIXRMULTISpec(I, SOME d, longvids))

    fun NONFIXMULTISpec(I, [])                = G.EMPTYSpec(I)
      | NONFIXMULTISpec(I, longvid::longvids) =
	    G.SEQSpec(I, G.NONFIXSpec(I,longvid), NONFIXMULTISpec(I,longvids))


    val tycon_EXN = TyCon.fromString "exn"

    fun NEWExDesc(I, op_opt, vid, ty_opt, dcondesc_opt) =
	    G.NEWDconDesc(I, op_opt, vid, ty_opt, G.Seq(I,[]),
			  G.SHORTLong(I, G.TyCon(I, tycon_EXN)), dcondesc_opt)

    fun SPECFunDesc(I, funid, spec, sigexp, fundesc_opt) =
	let
	    val I'      = G.infoSigExp sigexp
	    val strid   = G.StrId(I', StrId.invent())
	    val sigexp1 = G.SIGSigExp(G.infoSpec spec, spec)
	in
	    (* UNFINISHED: to translate, I either need LETSigExp,
	       or LOCALSpec+OPENSpec. *)
	    G.FunDesc(I, funid, strid, sigexp1, sigexp, fundesc_opt)
	end


    (* Signature expressions *)

    val WHERESigExp = G.WHERESigExp

    fun PARSigExp(I, sigexp) = sigexp

    fun WHERETYPESigExp(I, sigexp, [])                                = sigexp
      | WHERETYPESigExp(I, sigexp, (I',tyvarseq,longtycon,ty)::reas') =
	let
	    val sigexp' = G.WHERETYPESigExp(I', sigexp, tyvarseq, longtycon, ty)
	in
	    WHERETYPESigExp(I, sigexp', reas')
	end


    fun TyReaDesc(I, tyvarseq, longtycon, ty, NONE) =
	    (I, tyvarseq, longtycon, ty)::[]

      | TyReaDesc(I, tyvarseq, longtycon, ty, SOME tyreadesc) =
	    (I, tyvarseq, longtycon, ty)::tyreadesc

    (* Programs *)

    fun DECProgram(I, dec, program_opt) =
	    G.Program(I, dec, program_opt)

    fun EXPProgram(I, exp, program_opt) =
	let
	    val longvid = G.SHORTLong(I, G.VId(I, VId.fromString "it"))
	    val pat     = G.ATPATPat(I, G.LONGVIDAtPat(I, G.SANSOp, longvid))
	    val valbind = G.PLAINValBind(I, pat, exp, NONE)
	    val dec     = G.VALDec(I, G.Seq(I, []), valbind)
	in
	    G.Program(I, dec, program_opt)
	end

  end
