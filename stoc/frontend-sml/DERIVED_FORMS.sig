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
 *)


signature DERIVED_FORMS =
  sig

    (* Import *)

    structure Grammar: INPUT_GRAMMAR = InputGrammar

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

    (* Structure expressions *)

    val PARStrExp:        Info * StrExp                            -> StrExp
    val APPDECStrExp:     Info * LongFunId * Dec                   -> StrExp

    (* Functor bindings *)

    val TRANSFunBind:     Info * FunId * StrId * SigExp * SigExp option
			       * StrExp * FunBind option           -> FunBind
    val OPAQFunBind:      Info * FunId * StrId * SigExp * SigExp
			       * StrExp * FunBind option           -> FunBind
    val TRANSSPECFunBind: Info * FunId * Spec * SigExp option
			       * StrExp * FunBind option           -> FunBind
    val OPAQSPECFunBind:  Info * FunId * Spec * SigExp
                               * StrExp * FunBind option           -> FunBind

    (* Specifications *)

    val FUNSpec:          Info * ValDesc                           -> Spec
    val DATATYPESpec:     Info * DatDesc * TypDesc option          -> Spec
    val EXCEPTIONSpec:    Info * ExDesc                            -> Spec
    val INCLUDEMULTISpec: Info * LongSigId list                    -> Spec
    val SHARINGSpec:      Info * Spec * LongStrId list             -> Spec
    val INFIXMULTISpec:   Info * int option * VId list             -> Spec
    val INFIXRMULTISpec:  Info * int option * VId list             -> Spec
    val NONFIXMULTISpec:  Info * VId list                          -> Spec

    val NEWExDesc:        Info * VId * Ty option * ExDesc option   -> ExDesc
    val EQUALExDesc:      Info * VId * LongVId * ExDesc option     -> ExDesc

    val SPECFunDesc:      Info * FunId * Spec * SigExp * FunDesc option
								   -> FunDesc

    (* Signature expressions *)

    val PARSigExp:        Info * SigExp                            -> SigExp
    val WHERETYPESigExp:  Info * SigExp * TyReaDesc                -> SigExp
    val WHERESigExp:      Info * SigExp * LongStrId * LongStrId    -> SigExp

    val TyReaDesc:        Info * TyVarSeq * LongTyCon * Ty
			       * TyReaDesc option                  -> TyReaDesc
    (* Programs *)

    val DECProgram:       Info * Dec * Program option -> Program
    val EXPProgram:       Info * Exp * Program option -> Program

  end
