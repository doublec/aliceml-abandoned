(*
 * Stockhausen input grammar
 *
 * Extensions and modifications to core language:
 *   - unified dec and topdec (i.e. top declarations can appear in let)
 *   - record update expressions
 *   - vector expressions and patterns
 *   - generalized layered patterns
 *   - alternative patterns
 *   - guarded patterns
 *   - negated patterns
 *   - withval patterns
 *   - abstract type declarations
 *   - open datatypes and free construct declarations
 *   - package types and expressions
 *   - removed exception declarations (made into a derived form)
 *   - removed abstype (made into a derived form with local)
 *   - simplified open and fixity declarations to single id (multi ids made DF)
 *   - some hacks to build libraries: primitive value declarations,
 *     overloading declarations, special eqtype declarations and specifications
 *
 * Extensions and modifications to module language:
 *   - components
 *   - unified strdec and topdec
 *   - unified strid and funid
 *   - functor expressions
 *   - removed functor declarations (made into a derived form)
 *   - package elimination
 *   - parameterized signatures
 *   - open datatypes and free constructor specifications
 *   - straightified type specifications (synonyms are kept)
 *   - signature specifications
 *   - definitional value, constructor, and structure specifications
 *   - functor signatures
 *   - top signature
 *   - generalized where
 *   - sharing for signatures
 *   - let for signature expressions
 *   - functor parameters as a separate syntactic class StrPat
 *   - parenthesized structure and signature expressions
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

signature INPUT_GRAMMAR =
  sig

    (* Import *)

    type Info


    (* Identifiers and constants *)

    datatype SCon  = SCon  of Info * SCon.t
    datatype Lab   = Lab   of Info * Lab.t
    datatype VId   = VId   of Info * VId.t
    datatype TyCon = TyCon of Info * TyCon.t
    datatype TyVar = TyVar of Info * TyVar.t
    datatype StrId = StrId of Info * StrId.t
    datatype SigId = SigId of Info * SigId.t

    datatype 'a Long =
	  SHORTLong of Info * 'a
	| DOTLong   of Info * LongStrId * 'a

    withtype LongVId   = VId Long
    and      LongTyCon = TyCon Long
    and      LongStrId = StrId Long
    and      LongSigId = SigId Long


    (* Optional keyword `op' *)

    datatype Op = SANSOp | WITHOp


    (* Expressions *)

    datatype AtExp =
	  SCONAtExp      of Info * SCon
	| LONGVIDAtExp   of Info * Op * LongVId
	| RECORDAtExp    of Info * ExpRow option
	| UPDATEAtExp    of Info * AtExp * ExpRow
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
	| PACKExp        of Info * LongStrId

    (* Matches *)      

    and Match =
	  Match          of Info * Mrule * Match option

    and Mrule =
	  Mrule          of Info * Pat * Exp

    (* Declarations *)

    and Dec =
	  VALDec          of Info * TyVarSeq * ValBind
	| FUNDec          of Info * TyVarSeq * FvalBind
	| TYPEDec         of Info * TypBind
	| EQTYPEDec       of Info * TypBind
	| EQEQTYPEDec     of Info * TypBind
	| DATATYPEDec     of Info * DatBind
	| REPLICATIONDec  of Info * TyCon * LongTyCon
	| CONSTRUCTORDec  of Info * DconBind
	| STRUCTUREDec    of Info * StrBind
	| SIGNATUREDec    of Info * SigBind
	| LOCALDec        of Info * Dec * Dec
	| OPENDec         of Info * LongStrId
	| EMPTYDec        of Info
	| SEQDec          of Info * Dec * Dec
	| PRIMITIVEVALDec         of Info * Op * VId * Ty * string
	| PRIMITIVECONSTRUCTORDec of Info * Op * VId * Ty option
					       * TyVarSeq * LongTyCon * string
	| PRIMITIVESTRUCTUREDec   of Info * StrId * SigExp * string
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
          SigBind        of Info * SigId * StrPat list * SigExp * SigBind option

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
	| PACKTy         of Info * LongSigId
	| PARTy          of Info * Ty

    and TyRow =
	  ROWTyRow       of Info * Lab * Ty * TyRow option

    (* Structures *)

    and AtStrExp =
	  STRUCTAtStrExp    of Info * Dec
	| LONGSTRIDAtStrExp of Info * LongStrId
	| LETAtStrExp       of Info * Dec * StrExp
	| PARAtStrExp       of Info * StrExp

    and StrExp =
	  ATSTREXPStrExp    of Info * AtStrExp
	| APPStrExp         of Info * StrExp * AtStrExp
	| TRANSStrExp       of Info * StrExp * SigExp
	| OPAQStrExp        of Info * StrExp * SigExp
	| FCTStrExp         of Info * StrPat * StrExp
	| UNPACKStrExp      of Info * Exp * SigExp

    and StrPat =
	  StrPat            of Info * StrId * SigExp

    (* Signatures *)

    and AtSigExp =
	  ANYAtSigExp       of Info
	| SIGAtSigExp       of Info * Spec
	| LONGSIGIDAtSigExp of Info * LongSigId
	| LETAtSigExp       of Info * Dec * SigExp
	| PARAtSigExp       of Info * SigExp

    and SigExp =
	  ATSIGEXPSigExp    of Info * AtSigExp
	| APPSigExp         of Info * SigExp * AtStrExp
	| FCTSigExp         of Info * StrPat * SigExp
	| WHERESigExp       of Info * SigExp * SigExp

    (* Specifications *)

    and Spec =
	  VALSpec          of Info * ValDesc
	| TYPESpec         of Info * TypDesc
	| EQTYPESpec       of Info * TypDesc
	| EQEQTYPESpec     of Info * TypDesc
	| DATATYPESpec     of Info * DatDesc
	| REPLICATIONSpec  of Info * TyCon * LongTyCon
	| CONSTRUCTORSpec  of Info * DconDesc
	| STRUCTURESpec    of Info * StrDesc
	| SIGNATURESpec    of Info * SigDesc
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
	  NEWValDesc      of Info * Op * VId * Ty * ValDesc option
	| EQUALValDesc    of Info * Op * VId * Op * LongVId * ValDesc option

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
          NEWSigDesc      of Info * SigId * StrPat list * SigDesc option
	| EQUALSigDesc    of Info * SigId * StrPat list * SigExp
							* SigDesc option

    (* Imports *)

    and Imp =
	  VALImp          of Info * ValItem
	| TYPEImp         of Info * TypItem
	| DATATYPEImp     of Info * DatItem
	| CONSTRUCTORImp  of Info * DconItem
	| STRUCTUREImp    of Info * StrItem
	| SIGNATUREImp    of Info * SigItem
	| EMPTYImp        of Info
	| SEQImp          of Info * Imp * Imp

    and ValItem =
	  PLAINValItem    of Info * Op * VId * ValItem option
	| DESCValItem     of Info * Op * VId * Ty * ValItem option

    and TypItem =
	  PLAINTypItem    of Info * TyCon * TypItem option
	| DESCTypItem     of Info * TyVarSeq * TyCon * TypItem option

    and DatItem =
	  PLAINDatItem    of Info * TyCon * DatItem option
	| DESCDatItem     of Info * TyVarSeq * TyCon * ConItem * DatItem option

    and ConItem =
	  ConItem         of Info * Op * VId * Ty option * ConItem option

    and DconItem =
	  PLAINDconItem   of Info * Op * VId * DconItem option
	| DESCDconItem    of Info * Op * VId * Ty option * TyVarSeq * LongTyCon
			 				      * DconItem option
    and StrItem =
	  PLAINStrItem    of Info * StrId * StrItem option
        | DESCStrItem     of Info * StrId * SigExp * StrItem option

    and SigItem =
	  PLAINSigItem    of Info * SigId * SigItem option
        | DESCSigItem     of Info * SigId * StrPat list * SigItem option

    (* Announcements *)

    and Ann =
	  IMPORTAnn   of Info * Imp * string
	| PREBOUNDAnn of Info * StrId
	| EMPTYAnn    of Info
	| SEQAnn      of Info * Ann * Ann

    (* Programs and components *)

    and Program   = Program   of Info * Dec * Program option
    and Component = Component of Info * Ann * Program option

    (* Sequences *)

    and 'a Seq    = Seq of Info * 'a list

    withtype TySeq    = Ty Seq
    and      TyVarSeq = TyVar Seq

    (* The Type(tm) *)

    type t = Component


    (* Operations *)

    val infoSCon :	SCon		-> Info
    val infoLab :	Lab		-> Info
    val infoVId :	VId		-> Info
    val infoTyCon :	TyCon		-> Info
    val infoTyVar :	TyVar		-> Info
    val infoStrId :	StrId		-> Info
    val infoSigId :	SigId		-> Info
    val infoLong :	'a Long		-> Info
    val infoAtExp :	AtExp		-> Info
    val infoExpRow :	ExpRow		-> Info
    val infoExp :	Exp		-> Info
    val infoMatch :	Match		-> Info
    val infoMrule :	Mrule		-> Info
    val infoDec :	Dec		-> Info
    val infoValBind :	ValBind		-> Info
    val infoFvalBind :	FvalBind	-> Info
    val infoTypBind :	TypBind		-> Info
    val infoDatBind :	DatBind		-> Info
    val infoConBind :	ConBind		-> Info
    val infoDconBind :	DconBind	-> Info
    val infoStrBind :	StrBind		-> Info
    val infoSigBind :	SigBind		-> Info
    val infoAtPat :	AtPat		-> Info
    val infoPatRow :	PatRow		-> Info
    val infoPat :	Pat		-> Info
    val infoTy :	Ty		-> Info
    val infoTyRow :	TyRow		-> Info
    val infoAtStrExp :	AtStrExp	-> Info
    val infoStrExp :	StrExp		-> Info
    val infoStrPat :	StrPat		-> Info
    val infoAtSigExp :	AtSigExp	-> Info
    val infoSigExp :	SigExp		-> Info
    val infoSpec :	Spec		-> Info
    val infoValDesc :	ValDesc		-> Info
    val infoTypDesc :	TypDesc		-> Info
    val infoDatDesc :	DatDesc		-> Info
    val infoConDesc :	ConDesc		-> Info
    val infoDconDesc :	DconDesc	-> Info
    val infoStrDesc :	StrDesc		-> Info
    val infoSigDesc :	SigDesc		-> Info
    val infoImp :	Imp		-> Info
    val infoValItem :	ValItem		-> Info
    val infoTypItem :	TypItem		-> Info
    val infoDatItem :	DatItem		-> Info
    val infoConItem :	ConItem		-> Info
    val infoDconItem :	DconItem	-> Info
    val infoStrItem :	StrItem		-> Info
    val infoSigItem :	SigItem		-> Info
    val infoAnn :	Ann		-> Info
    val infoProgram :	Program		-> Info
    val infoComponent :	Component	-> Info
    val infoSeq :	'a Seq		-> Info

    val idLab :		Lab		-> Lab.t
    val idVId :		VId		-> VId.t
    val idTyCon :	TyCon		-> TyCon.t
    val idTyVar :	TyVar		-> TyVar.t
    val idStrId :	StrId		-> StrId.t
    val idSigId :	SigId		-> SigId.t

    val explodeLong :	'a Long		-> StrId list * 'a

  end
