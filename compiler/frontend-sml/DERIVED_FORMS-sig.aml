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
 *	exception exbind          ==>  con exbind'
 *	vid <of ty> <and exbind>  ==>  vid <of ty> : exn <and exbind'>
 *   - abstype has been made a derived form:
 *	abstype datbind <withtype tybind> with dec end
 *	  ==>
 *	<type typbind> local datatype datbind in type typbind' dec end
 *     where typbind' contains a binding t = t for each tycon t bound in
 *     datbind. Note that this results in a different treatment of equality.
 *   - derived forms for where patterns:
 *	pat withval valbind where atexp
 *	==>
 *	pat withval valbind end where atexp
 *
 *	pat withfun fvalbind where atexp
 *	==>
 *	pat withfun fvalbind end where atexp
 *   - include takes longsigids:
 *	include longsigid_1 ... longsigid_n
 *	==>
 *	include longsigid_1 ; ... ; include longsigid_n
 *   - derived forms for primitive declarations similar to specifications:
 *   - where constraints have been made a derived form of intersection:
 *	sigexp where type tyvarseq strid_1....strid_n.tycon = ty
 *	==>
 *      sigexp where sig structure strid_1 :
 *			...
 *			   sig structure strid_n :
 *			      sig type tyvarseq tycon = ty end
 *			   end
 *			...
 *		     end
 *
 *	sigexp where strid_1....strid_n.strid = longstrid
 *	==>
 *      sigexp where sig structure strid_1 :
 *			...
 *			   sig structure strid_n :
 *			      sig structure strid = longstrid end
 *			   end
 *			...
 *		     end
 *
 * We did NOT introduce a sharing signature ... and signature ... derived form
 * similar to types, because we consider that one completely broken.
 *
 * Notes:
 * - Two phrases named Fmatch and Fmrule have been added to factorize FvalBind.
 * - A phrase named TyReaDesc has been added to factorize type
 *   realisation signature expressions.
 *)


signature DERIVED_FORMS =
  sig

    (* Import *)

    structure Grammar: INPUT_GRAMMAR = InputGrammar

    type Info      = Grammar.Info

    type Lab       = Grammar.Lab
    type VId       = Grammar.VId
    type StrId     = Grammar.StrId
    type LongVId   = Grammar.LongVId
    type LongTyCon = Grammar.LongTyCon
    type LongStrId = Grammar.LongStrId
    type LongSigId = Grammar.LongSigId

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
    type AtStrExp  = Grammar.AtStrExp
    type AppStrExp = Grammar.StrExp
    type StrExp    = Grammar.StrExp
    type StrPat    = Grammar.StrPat
    type StrBind   = Grammar.StrBind
    type AppSigExp = Grammar.SigExp
    type SigExp    = Grammar.SigExp
    type Spec      = Grammar.Spec
    type ValDesc   = Grammar.ValDesc
    type TypDesc   = Grammar.TypDesc
    type DatDesc   = Grammar.DatDesc
    type ExDesc    = Grammar.DconDesc
    type Imp       = Grammar.Imp
    type ValItem   = Grammar.ValItem
    type TypItem   = Grammar.TypItem
    type DatItem   = Grammar.DatItem
    type ExItem    = Grammar.DconItem
    type FunBind
    type FunDesc
    type FunItem
    type Rea
    type Program   = Grammar.Program

    (* Expressions *)

    val UNITAtExp:   Info                                      -> AtExp
    val TUPLEAtExp:  Info * Exp list                           -> AtExp
    val HASHAtExp:   Info * Lab                                -> AtExp
    val CASEExp:     Info * Exp * Match                        -> Exp
    val IFExp:       Info * Exp * Exp * Exp                    -> Exp
    val ANDALSOExp:  Info * Exp * Exp                          -> Exp
    val ORELSEExp:   Info * Exp * Exp                          -> Exp
    val SEQAtExp:    Info * Exp list                           -> AtExp
    val LETAtExp:    Info * Dec * Exp list                     -> AtExp
    val WHILEExp:    Info * Exp * Exp                          -> Exp
    val LISTAtExp:   Info * Exp list                           -> AtExp
    val RECExp:      Info * Pat * Exp                          -> Exp

    val VIDExpRow:   Info * VId * Ty option * ExpRow option    -> ExpRow

    (* Patterns *)

    val UNITAtPat:   Info                                      -> AtPat
    val TUPLEAtPat:  Info * Pat list                           -> AtPat
    val LISTAtPat:   Info * Pat list                           -> AtPat

    val VIDPatRow:   Info * VId * Ty option * Pat option * PatRow option
                                                               -> PatRow
    val WITHFUNPat:  Info * Pat * FvalBind                     -> Pat
    val WITHVALWHEREPat: Info * Pat * ValBind * AtExp          -> Pat
    val WITHFUNWHEREPat: Info * Pat * FvalBind * AtExp         -> Pat

    (* Types *)

    val TUPLETy:     Info * Ty list                            -> Ty

    (* Bindings *)

    val FvalBind:    Info * Fmatch * FvalBind option           -> FvalBind
    val Fmatch:      Info * Fmrule * Fmatch option             -> Fmatch
    val Fmrule:      Info * Fpat * Exp                         -> Fmrule

    (* Declarations *)

    val FUNDec:		Info * TyVarSeq * FvalBind		-> Dec
    val DATATYPEDec:	Info * DatBind * TypBind option		-> Dec
    val ABSTYPEDec:	Info * DatBind * TypBind option * Dec	-> Dec
    val EXCEPTIONDec:	Info * ExBind				-> Dec
    val FUNCTORDec:     Info * FunBind                          -> Dec
    val OPENMULTIDec:	Info * LongStrId list			-> Dec
    val INFIXMULTIDec:	Info * int option * VId list		-> Dec
    val INFIXRMULTIDec:	Info * int option * VId list		-> Dec
    val NONFIXMULTIDec:	Info * VId list				-> Dec

    val NEWExBind:    Info * Op * VId * Ty option * ExBind option    -> ExBind
    val EQUALExBind:  Info * Op * VId * Op * LongVId * ExBind option -> ExBind

    (* Structure bindings *)

    val TRANSStrBind:     Info * StrId * SigExp option * StrExp
			       * StrBind option                    -> StrBind
    val OPAQStrBind:      Info * StrId * SigExp * StrExp
			       * StrBind option                    -> StrBind
    val WILDCARDStrBind:  Info * SigExp option * StrExp
			       * StrBind option                    -> StrBind

    (* Structure expressions *)

    val DECAtStrExp:      Info * Dec -> AtStrExp
    val FCTStrExp:        Info * StrPat * StrExp -> StrExp

    val STRIDStrPat:      Info * StrId * SigExp -> StrPat
    val WILDCARDStrPat:   Info * SigExp         -> StrPat
    val SPECStrPat:       Info * Spec           -> StrPat

    (* Functor bindings *)

    val TRANSFunBind:     Info * StrId * StrPat list * SigExp option
			       * StrExp * FunBind option           -> FunBind
    val OPAQFunBind:      Info * StrId * StrPat list * SigExp
			       * StrExp * FunBind option           -> FunBind

    (* Specifications *)

    val FUNSpec:          Info * ValDesc                           -> Spec
    val DATATYPESpec:     Info * DatDesc * TypDesc option          -> Spec
    val EXCEPTIONSpec:    Info * ExDesc                            -> Spec
    val FUNCTORSpec:      Info * FunDesc                           -> Spec
    val SHARINGSpec:      Info * Spec * LongStrId list             -> Spec
    val INCLUDEMULTISpec: Info * LongSigId list                    -> Spec
    val INFIXMULTISpec:   Info * int option * VId list             -> Spec
    val INFIXRMULTISpec:  Info * int option * VId list             -> Spec
    val NONFIXMULTISpec:  Info * VId list                          -> Spec

    val NEWExDesc:        Info * Op * VId * Ty option * ExDesc option -> ExDesc
    val EQUALExDesc:      Info * Op * VId * Op * LongVId
						      * ExDesc option -> ExDesc

    val FunDesc:          Info * StrId * StrPat list * SigExp * FunDesc option
								   -> FunDesc

    (* Signature expressions *)

    val WHEREREASigExp:   Info * SigExp * Rea                      -> SigExp
    val WHERELONGSTRIDSigExp:
			  Info * SigExp * LongStrId * LongStrId    -> SigExp

    val VALRea:           Info * Op * LongVId * Op * LongVId * Rea option -> Rea
    val FUNRea:           Info * Op * LongVId * Op * LongVId * Rea option -> Rea
    val CONSTRUCTORRea:   Info * Op * LongVId * Op * LongVId * Rea option -> Rea
    val EXCEPTIONRea:     Info * Op * LongVId * Op * LongVId * Rea option -> Rea
    val TYPERea:          Info * TyVarSeq * LongTyCon * Ty * Rea option   -> Rea
    val STRUCTURERea:     Info * LongStrId * SigExp option * LongStrId
							   * Rea option   -> Rea
    val FUNCTORRea:       Info * LongStrId * SigExp option * LongStrId
							   * Rea option   -> Rea
    val SIGNATURERea:     Info * LongSigId * StrPat list * AppSigExp
							 * Rea option     -> Rea

    (* Imports *)

    val FUNImp:           Info * ValItem                           -> Imp
    val EXCEPTIONImp:     Info * ExItem                            -> Imp
    val FUNCTORImp:       Info * FunItem                           -> Imp
    val INFIXMULTIImp:    Info * int option * VId list             -> Imp
    val INFIXRMULTIImp:   Info * int option * VId list             -> Imp
    val NONFIXMULTIImp:   Info * VId list                          -> Imp

    val PLAINExItem:      Info * Op * VId * ExItem option          -> ExItem
    val DESCExItem:       Info * Op * VId * Ty * ExItem option     -> ExItem

    val PLAINFunItem:     Info * StrId * FunItem option            -> FunItem
    val DESCFunItem:      Info * StrId * StrPat list * SigExp * FunItem option
								   -> FunItem

    (* Programs *)

    val DECProgram:       Info * Dec * Program option -> Program
    val EXPProgram:       Info * Exp * Program option -> Program

  end
