(*
 * Stockhausen input grammar
 *
 * Extensions and modifications to core language:
 *   - unified dec and topdec (i.e. top declarations can appear in let)
 *   - vector expressions and patterns
 *   - generalized layered patterns
 *   - alternative patterns
 *   - guarded patterns
 *   - negated patterns
 *   - withval patterns
 *   - abstract type declarations
 *   - open datatypes and free construct declarations via con
 *   - removed exception declarations (made into a derived form with con)
 *   - removed abstype (made into a derived form with local)
 *   - simplified open and fixity declarations to singe id (multi ids made DF)
 *   - some hacks to build libraries: primitive value declarations,
 *     overloading declarations, special eqtype declarations and specifications
 *
 * Extensions and modifications to module language:
 *   - components
 *   - unified strdec and topdec
 *   - open datatypes and free constructor specifications via con
 *   - constructor synonym specifications
 *   - signature synonym specifications
 *   - straightified type specifications (synonyms are kept)
 *   - where for structures
 *   - sharing and where for signatures
 *   - definitional structure specifications
 *   - fixity directives in signatures
 *   - op keyword in signatures
 *
 * Notes:
 *   For easier interfacing with the back end we keep several derived forms:
 *   - tuple expressions, patterns, and types
 *   - selector functions
 *   - case, if, orelse, andalso expressions
 *   - sequential expressions
 *   - while expressions
 *   Optional semicolons are omitted.
 *   Because of delayed infix resolution we have to keep fvalbind forms and
 *   need a special app pattern form.
 *   Constructor patterns with arguments are represented as APPPats.
 *   We must also keep parentheses and stuff because of delayed infix resolving.
 *   The structure where and sharing derived forms [Definition, Appendix A]
 *   have been kept because they cannot be derived purely syntactically.
 *)

functor MakeInputGrammar(type Info) :> INPUT_GRAMMAR where type Info = Info =
  struct

    (* Import *)

    type Info = Info


    (* Identifiers and constants *)

    datatype SCon  = SCon  of Info * SCon.t
    datatype Lab   = Lab   of Info * Lab.t
    datatype VId   = VId   of Info * VId.t
    datatype TyCon = TyCon of Info * TyCon.t
    datatype TyVar = TyVar of Info * TyVar.t
    datatype StrId = StrId of Info * StrId.t
    datatype SigId = SigId of Info * SigId.t
    datatype FunId = FunId of Info * FunId.t

    datatype 'a Long =
	  SHORTLong of Info * 'a
	| DOTLong   of Info * LongStrId * 'a

    withtype LongVId   = VId Long
    and      LongTyCon = TyCon Long
    and      LongStrId = StrId Long
    and      LongSigId = SigId Long
    and      LongFunId = FunId Long


    (* Optional keyword `op' *)

    datatype Op = SANSOp | WITHOp


    (* Expressions *)

    datatype AtExp =
	  SCONAtExp      of Info * SCon
	| LONGVIDAtExp   of Info * Op * LongVId
	| RECORDAtExp    of Info * ExpRow option
	| HASHAtExp      of Info * Lab
	| TUPLEAtExp     of Info * Exp list
	| VECTORAtExp    of Info * Exp list
	| SEQAtExp       of Info * Exp list
	| LETAtExp       of Info * Dec * Exp
	| PARAtExp       of Info * Exp

    and ExpRow =
	  ROWExpRow      of Info * Lab * Exp * ExpRow option

    and Exp =
	  ATEXPExp       of Info * AtExp
	| APPExp         of Info * Exp * AtExp
	| TYPEDExp       of Info * Exp * Ty
	| ANDALSOExp     of Info * Exp * Exp
	| ORELSEExp      of Info * Exp * Exp
	| HANDLEExp      of Info * Exp * Match
	| RAISEExp       of Info * Exp
	| IFExp          of Info * Exp * Exp * Exp
	| WHILEExp       of Info * Exp * Exp
	| CASEExp        of Info * Exp * Match
	| FNExp          of Info * Match

    (* Matches *)      

    and Match =
	  Match          of Info * Mrule * Match option

    and Mrule =
	  Mrule          of Info * Pat * Exp

    (* Declarations *)

    and Dec =
	  VALDec          of Info * TyVarSeq * ValBind
	| FUNDec          of Info * TyVarSeq * FvalBind
	| PRIMITIVEDec    of Info * Op * VId * Ty * string
	| TYPEDec         of Info * TypBind
	| EQTYPEDec       of Info * TypBind
	| EQEQTYPEDec     of Info * TypBind
	| DATATYPEDec     of Info * DatBind
	| REPLICATIONDec  of Info * TyCon * LongTyCon
	| CONSTRUCTORDec  of Info * DconBind
	| PREBOUNDDec     of Info * StrId
	| STRUCTUREDec    of Info * StrBind
	| SIGNATUREDec    of Info * SigBind
	| FUNCTORDec      of Info * FunBind
	| LOCALDec        of Info * Dec * Dec
	| OPENDec         of Info * LongStrId
	| EMPTYDec        of Info
	| SEQDec          of Info * Dec * Dec
	| OVERLOADDec     of Info * Op * VId * TyVar * Ty
	| INSTANCEDec     of Info * Op * VId * LongTyCon * LongVId
	| INSTANCESCONDec of Info * SCon * LongTyCon
	| INFIXDec        of Info * int * VId
	| INFIXRDec       of Info * int * VId
	| NONFIXDec       of Info * VId

    (* Bindings *)

    and ValBind =
	  PLAINValBind   of Info * Pat * Exp * ValBind option
	| RECValBind     of Info * ValBind

    and FvalBind =
	  FvalBind       of Info * Match * FvalBind option

    and TypBind =
	  NEWTypBind     of Info * TyVarSeq * TyCon * TypBind option
	| EQUALTypBind   of Info * TyVarSeq * TyCon * Ty * TypBind option

    and DatBind =
	  CLOSEDDatBind  of Info * TyVarSeq * TyCon * ConBind * DatBind option
	| OPENDatBind    of Info * TyVarSeq * TyCon * DatBind option

    and ConBind =
	  ConBind        of Info * Op * VId * Ty option * ConBind option

    and DconBind =
	  NEWDconBind    of Info * Op * VId * Ty option * TyVarSeq * LongTyCon
							* DconBind option
	| EQUALDconBind  of Info * Op * VId * Op * LongVId * DconBind option

    and StrBind =
          StrBind        of Info * StrId * StrExp * StrBind option

    and SigBind =
          SigBind        of Info * SigId * SigExp * SigBind option

    and FunBind =
          FunBind        of Info * FunId * StrId * SigExp * StrExp
                                 * FunBind option
    (* Patterns *)

    and AtPat =
	  WILDCARDAtPat  of Info
	| SCONAtPat      of Info * SCon
	| LONGVIDAtPat   of Info * Op * LongVId
	| RECORDAtPat    of Info * PatRow option
	| TUPLEAtPat     of Info * Pat list
	| VECTORAtPat    of Info * Pat list
	| ALTAtPat       of Info * Pat list
	| PARAtPat       of Info * Pat

    and PatRow =
	  WILDCARDPatRow of Info
	| ROWPatRow      of Info * Lab * Pat * PatRow option

    and Pat =
	  ATPATPat       of Info * AtPat
	| APPPat         of Info * Pat * AtPat
	| TYPEDPat       of Info * Pat * Ty
	| NONPat         of Info * Pat
	| ASPat          of Info * Pat * Pat
	| WHENPat        of Info * Pat * AtExp
	| WITHVALPat     of Info * Pat * ValBind
	| WITHFUNPat     of Info * Pat * FvalBind

    (* Type expressions *)

    and Ty =
	  TYVARTy        of Info * TyVar
	| RECORDTy       of Info * TyRow option
	| TUPLETy        of Info * Ty list
	| TYCONTy        of Info * TySeq * LongTyCon
	| ARROWTy        of Info * Ty * Ty
	| PARTy          of Info * Ty

    and TyRow =
	  ROWTyRow       of Info * Lab * Ty * TyRow option

    (* Structures *)

    and StrExp =
	  STRUCTStrExp    of Info * Dec
	| LONGSTRIDStrExp of Info * LongStrId
	| TRANSStrExp     of Info * StrExp * SigExp
	| OPAQStrExp      of Info * StrExp * SigExp
	| APPStrExp       of Info * LongFunId * StrExp
	| LETStrExp       of Info * Dec * StrExp

    (* Signatures *)

    and SigExp =
          SIGSigExp       of Info * Spec
        | LONGSIGIDSigExp of Info * LongSigId
        | WHERETYPESigExp of Info * SigExp * TyVarSeq * LongTyCon * Ty
	| WHERESIGNATURESigExp of Info * SigExp * LongSigId * SigExp
	| WHERESigExp     of Info * SigExp * LongStrId * LongStrId

    (* Specifications *)

    and Spec =
	  VALSpec          of Info * ValDesc
	| TYPESpec         of Info * TypDesc
	| EQTYPESpec       of Info * TypDesc
	| EQEQTYPESpec     of Info * TypDesc
	| DATATYPESpec     of Info * DatDesc
	| REPLICATIONSpec  of Info * TyCon * LongTyCon
	| CONSTRUCTORSpec  of Info * DconDesc
	| PREBOUNDSpec     of Info * StrId
	| STRUCTURESpec    of Info * StrDesc
	| SIGNATURESpec    of Info * SigDesc
	| FUNCTORSpec      of Info * FunDesc
	| INCLUDESpec      of Info * SigExp
	| EMPTYSpec        of Info
	| SEQSpec          of Info * Spec * Spec
	| SHARINGTYPESpec  of Info * Spec * LongTyCon list
	| SHARINGSIGNATURESpec of Info * Spec * LongSigId list
	| SHARINGSpec      of Info * Spec * LongStrId list
	| OVERLOADSpec     of Info * Op * VId * TyVar * Ty
	| INSTANCESpec     of Info * Op * VId * LongTyCon * LongVId
	| INSTANCESCONSpec of Info * SCon * LongTyCon
	| INFIXSpec        of Info * int * VId
	| INFIXRSpec       of Info * int * VId
	| NONFIXSpec       of Info * VId

    and ValDesc =
	  ValDesc         of Info * Op * VId * Ty * ValDesc option

    and TypDesc =
	  NEWTypDesc      of Info * TyVarSeq * TyCon * TypDesc option
	| EQUALTypDesc    of Info * TyVarSeq * TyCon * Ty * TypDesc option

    and DatDesc =
	  CLOSEDDatDesc   of Info * TyVarSeq * TyCon * ConDesc * DatDesc option
	| OPENDatDesc     of Info * TyVarSeq * TyCon * DatDesc option

    and ConDesc =
	  ConDesc         of Info * Op * VId * Ty option * ConDesc option

    and DconDesc =
	  NEWDconDesc     of Info * Op * VId * Ty option * TyVarSeq * LongTyCon
			 				      * DconDesc option
	| EQUALDconDesc   of Info * Op * VId * Op * LongVId * DconDesc option

    and StrDesc =
          NEWStrDesc      of Info * StrId * SigExp * StrDesc option
	| EQUALStrDesc    of Info * StrId * SigExp option * LongStrId
							    * StrDesc option
    and SigDesc =
          NEWSigDesc      of Info * SigId * SigDesc option
	| EQUALSigDesc    of Info * SigId * SigExp * SigDesc option

    and FunDesc =
          FunDesc         of Info * FunId * StrId * SigExp * SigExp
                                  * FunDesc option
    (* Programs *)

    and Program = Program of Info * Dec * Program option

    (* Components *)

    and Component = Component of Info * Import * Program option

    and Import =
	  IMPORTImport of Info * Spec * string
	| EMPTYImport  of Info
	| SEQImport    of Info * Import * Import

    (* Sequences *)

    and 'a Seq    = Seq of Info * 'a list

    withtype TySeq    = Ty Seq
    and      TyVarSeq = TyVar Seq


    (* Extracting info fields *)

    fun infoSCon(SCon(I,_))				= I
    fun infoLab(Lab(I,_))				= I
    fun infoVId(VId(I,_))				= I
    fun infoTyCon(TyCon(I,_))				= I
    fun infoTyVar(TyVar(I,_))				= I
    fun infoStrId(StrId(I,_))				= I
    fun infoSigId(SigId(I,_))				= I
    fun infoFunId(FunId(I,_))				= I

    fun infoLong(SHORTLong(I,_))			= I
      | infoLong(DOTLong(I,_,_))			= I

    fun infoAtExp(SCONAtExp(I,_))			= I
      | infoAtExp(LONGVIDAtExp(I,_,_))			= I
      | infoAtExp(RECORDAtExp(I,_))			= I
      | infoAtExp(HASHAtExp(I,_))			= I
      | infoAtExp(TUPLEAtExp(I,_))			= I
      | infoAtExp(VECTORAtExp(I,_))			= I
      | infoAtExp(SEQAtExp(I,_))			= I
      | infoAtExp(LETAtExp(I,_,_))			= I
      | infoAtExp(PARAtExp(I,_))			= I

    fun infoExpRow(ROWExpRow(I,_,_,_))			= I

    fun infoExp(ATEXPExp(I,_))				= I
      | infoExp(APPExp(I,_,_))				= I
      | infoExp(TYPEDExp(I,_,_))			= I
      | infoExp(ANDALSOExp(I,_,_))			= I
      | infoExp(ORELSEExp(I,_,_))			= I
      | infoExp(HANDLEExp(I,_,_))			= I
      | infoExp(RAISEExp(I,_))				= I
      | infoExp(IFExp(I,_,_,_))				= I
      | infoExp(WHILEExp(I,_,_))			= I
      | infoExp(CASEExp(I,_,_))				= I
      | infoExp(FNExp(I,_))				= I

    fun infoMatch(Match(I,_,_))				= I

    fun infoMrule(Mrule(I,_,_))				= I

    fun infoDec(VALDec(I,_,_))				= I
      | infoDec(FUNDec(I,_,_))				= I
      | infoDec(PRIMITIVEDec(I,_,_,_,_))		= I
      | infoDec(TYPEDec(I,_))				= I
      | infoDec(EQTYPEDec(I,_))				= I
      | infoDec(EQEQTYPEDec(I,_))			= I
      | infoDec(DATATYPEDec(I,_))			= I
      | infoDec(REPLICATIONDec(I,_,_))			= I
      | infoDec(CONSTRUCTORDec(I,_))			= I
      | infoDec(PREBOUNDDec(I,_))			= I
      | infoDec(STRUCTUREDec(I,_))			= I
      | infoDec(SIGNATUREDec(I,_))			= I
      | infoDec(FUNCTORDec(I,_))			= I
      | infoDec(LOCALDec(I,_,_))			= I
      | infoDec(OPENDec(I,_))				= I
      | infoDec(EMPTYDec(I))				= I
      | infoDec(SEQDec(I,_,_))				= I
      | infoDec(OVERLOADDec(I,_,_,_,_))			= I
      | infoDec(INSTANCEDec(I,_,_,_,_))			= I
      | infoDec(INSTANCESCONDec(I,_,_))			= I
      | infoDec(INFIXDec(I,_,_))			= I
      | infoDec(INFIXRDec(I,_,_))			= I
      | infoDec(NONFIXDec(I,_))				= I

    fun infoValBind(PLAINValBind(I,_,_,_))		= I
      | infoValBind(RECValBind(I,_))			= I

    fun infoFvalBind(FvalBind(I,_,_))			= I

    fun infoTypBind(NEWTypBind(I,_,_,_))		= I
      | infoTypBind(EQUALTypBind(I,_,_,_,_))		= I

    fun infoDatBind(CLOSEDDatBind(I,_,_,_,_))		= I
      | infoDatBind(OPENDatBind(I,_,_,_))		= I

    fun infoConBind(ConBind(I,_,_,_,_))			= I

    fun infoDconBind(NEWDconBind(I,_,_,_,_,_,_))	= I
      | infoDconBind(EQUALDconBind(I,_,_,_,_,_))	= I

    fun infoStrBind(StrBind(I,_,_,_))			= I

    fun infoSigBind(SigBind(I,_,_,_))			= I

    fun infoFunBind(FunBind(I,_,_,_,_,_))		= I

    fun infoAtPat(WILDCARDAtPat(I))			= I
      | infoAtPat(SCONAtPat(I,_))			= I
      | infoAtPat(LONGVIDAtPat(I,_,_))			= I
      | infoAtPat(RECORDAtPat(I,_))			= I
      | infoAtPat(TUPLEAtPat(I,_))			= I
      | infoAtPat(VECTORAtPat(I,_))			= I
      | infoAtPat(ALTAtPat(I,_))			= I
      | infoAtPat(PARAtPat(I,_))			= I

    fun infoPatRow(WILDCARDPatRow(I))			= I
      | infoPatRow(ROWPatRow(I,_,_,_))			= I

    fun infoPat(ATPATPat(I,_))				= I
      | infoPat(APPPat(I,_,_))				= I
      | infoPat(TYPEDPat(I,_,_))			= I
      | infoPat(NONPat(I,_))				= I
      | infoPat(ASPat(I,_,_))				= I
      | infoPat(WHENPat(I,_,_))				= I
      | infoPat(WITHVALPat(I,_,_))			= I
      | infoPat(WITHFUNPat(I,_,_))			= I

    fun infoTy(TYVARTy(I,_))				= I
      | infoTy(RECORDTy(I,_))				= I
      | infoTy(TUPLETy(I,_))				= I
      | infoTy(TYCONTy(I,_,_))				= I
      | infoTy(ARROWTy(I,_,_))				= I
      | infoTy(PARTy(I,_))				= I

    fun infoTyRow(ROWTyRow(I,_,_,_))			= I

    fun infoStrExp(STRUCTStrExp(I,_))			= I
      | infoStrExp(LONGSTRIDStrExp(I,_))		= I
      | infoStrExp(TRANSStrExp(I,_,_))			= I
      | infoStrExp(OPAQStrExp(I,_,_))			= I
      | infoStrExp(APPStrExp(I,_,_))			= I
      | infoStrExp(LETStrExp(I,_,_))			= I

    fun infoSigExp(SIGSigExp(I,_))			= I
      | infoSigExp(LONGSIGIDSigExp(I,_))		= I
      | infoSigExp(WHERETYPESigExp(I,_,_,_,_))		= I
      | infoSigExp(WHERESIGNATURESigExp(I,_,_,_))	= I
      | infoSigExp(WHERESigExp(I,_,_,_))		= I

    fun infoSpec(VALSpec(I,_))				= I
      | infoSpec(TYPESpec(I,_))				= I
      | infoSpec(EQTYPESpec(I,_))			= I
      | infoSpec(EQEQTYPESpec(I,_))			= I
      | infoSpec(DATATYPESpec(I,_))			= I
      | infoSpec(REPLICATIONSpec(I,_,_))		= I
      | infoSpec(CONSTRUCTORSpec(I,_))			= I
      | infoSpec(PREBOUNDSpec(I,_))			= I
      | infoSpec(STRUCTURESpec(I,_))			= I
      | infoSpec(SIGNATURESpec(I,_))			= I
      | infoSpec(FUNCTORSpec(I,_))			= I
      | infoSpec(INCLUDESpec(I,_))			= I
      | infoSpec(EMPTYSpec(I))				= I
      | infoSpec(SEQSpec(I,_,_))			= I
      | infoSpec(SHARINGTYPESpec(I,_,_))		= I
      | infoSpec(SHARINGSIGNATURESpec(I,_,_))		= I
      | infoSpec(SHARINGSpec(I,_,_))			= I
      | infoSpec(OVERLOADSpec(I,_,_,_,_))		= I
      | infoSpec(INSTANCESpec(I,_,_,_,_))		= I
      | infoSpec(INSTANCESCONSpec(I,_,_))		= I
      | infoSpec(INFIXSpec(I,_,_))			= I
      | infoSpec(INFIXRSpec(I,_,_))			= I
      | infoSpec(NONFIXSpec(I,_))			= I

    fun infoValDesc(ValDesc(I,_,_,_,_))			= I

    fun infoTypDesc(NEWTypDesc(I,_,_,_))		= I
      | infoTypDesc(EQUALTypDesc(I,_,_,_,_))		= I

    fun infoDatDesc(CLOSEDDatDesc(I,_,_,_,_))		= I
      | infoDatDesc(OPENDatDesc(I,_,_,_))		= I

    fun infoConDesc(ConDesc(I,_,_,_,_))			= I

    fun infoDconDesc(NEWDconDesc(I,_,_,_,_,_,_))	= I
      | infoDconDesc(EQUALDconDesc(I,_,_,_,_,_))	= I

    fun infoStrDesc(NEWStrDesc(I,_,_,_))		= I
      | infoStrDesc(EQUALStrDesc(I,_,_,_,_))		= I

    fun infoSigDesc(NEWSigDesc(I,_,_))			= I
      | infoSigDesc(EQUALSigDesc(I,_,_,_))		= I

    fun infoFunDesc(FunDesc(I,_,_,_,_,_))		= I

    fun infoProgram(Program(I,_,_))			= I

    fun infoComponent(Component(I,_,_))			= I

    fun infoImport(IMPORTImport(I,_,_))			= I
      | infoImport(EMPTYImport(I))			= I
      | infoImport(SEQImport(I,_,_))			= I

    fun infoSeq(Seq(I,_))				= I


    fun idLab(Lab(_,id))				= id
    fun idVId(VId(_,id))				= id
    fun idTyCon(TyCon(_,id))				= id
    fun idTyVar(TyVar(_,id))				= id
    fun idStrId(StrId(_,id))				= id
    fun idSigId(SigId(_,id))				= id
    fun idFunId(FunId(_,id))				= id

  end
