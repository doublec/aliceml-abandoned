structure AbstractionError :> ABSTRACTION_ERROR =
  struct

  (* Pretty printer *)

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

    val par = paragraph

  (* Types *)

    type Lab	= Lab.t
    type VId	= VId.t
    type TyVar	= TyVar.t
    type TyCon	= TyCon.t
    type StrId	= StrId.t
    type SigId	= SigId.t
    type id	= AbstractGrammar.id

    datatype error =
	(* Identifiers *)
	  VIdUnbound		of VId
	| TyConUnbound		of TyCon
	| TyVarUnbound		of TyVar
	| StrIdUnbound		of StrId
	| SigIdUnbound		of SigId
	| PreboundFirstClass
	(* Expressions *)
	| ExpRowLabDuplicate	of Lab
	(* Patterns *)
	| PatVIdDuplicate	of VId
	| WithPatVIdDuplicate	of VId
	| PatLongVIdVar
	| PatRowLabDuplicate	of Lab
	| AppPatNonCon
	| AltPatInconsistent
	(* Types *)
	| TyRowLabDuplicate	of Lab
	| TyVarSeqDuplicate	of TyVar
	| ValTyVarSeqDuplicate	of TyVar
	(* Declarations and bindings *)
	| FvalBindDuplicate	of VId
	| FvalBindArityInconsistent
	| FvalBindArityZero
	| FvalBindNameInconsistent of VId
	| FvalBindNameMissing
	| FvalBindNameCon	of VId
	| FvalBindPatInvalid
	| TypBindDuplicate	of TyCon
	| DatBindDuplicate	of TyCon
	| DatBindConDuplicate	of VId
	| ConBindDuplicate	of VId
	| DconBindDuplicate	of VId
	| DconBindNonCon
	| StrBindDuplicate	of StrId
	| SigBindDuplicate	of SigId
	(* Specifications and descriptions *)
	| SpecFixDuplicate	of VId
	| SpecVIdDuplicate	of VId
	| SpecTyConDuplicate	of TyCon
	| SpecStrIdDuplicate	of StrId
	| SpecSigIdDuplicate	of SigId
	| ConDescDuplicate	of VId
	| DconDescNonCon
	(* Imports and items *)
	| ImpVIdDuplicate	of VId
	| ImpTyConDuplicate	of TyCon
	| ImpStrIdDuplicate	of StrId
	| ImpSigIdDuplicate	of SigId
	| ConItemDuplicate	of VId
	| ValItemUnbound	of VId
	| TypItemUnbound	of TyCon
	| DatItemUnbound	of TyCon
	| ConItemUnbound	of VId
	| DconItemUnbound	of VId
	| StrItemUnbound	of StrId
	| SigItemUnbound	of SigId
	| ConItemNonCon		of VId
	| DconItemNonCon	of VId
	(* Sharing translation *)
	| SharingExternalTy	of id
	| SharingExternalSig	of id
	| SharingExternalStr	of id

    datatype warning =
	(* Shadowing *)
	  VIdShadowed		of VId
	| TyConShadowed		of TyCon
	| TyVarShadowed		of TyVar
	| StrIdShadowed		of StrId
	| SigIdShadowed		of SigId


  (* Pretty printing *)

    fun ppQuoted s	= "`" ^ s ^ "'"

    fun ppLab lab	= ppQuoted(Lab.toString lab)
    fun ppVId vid	= ppQuoted(VId.toString vid)
    fun ppTyCon tycon	= ppQuoted(TyCon.toString tycon)
    fun ppTyVar tyvar	= ppQuoted(TyVar.toString tyvar)
    fun ppStrId strid	= ppQuoted(StrId.toString strid)
    fun ppSigId sigid	= ppQuoted(SigId.toString sigid)

    fun ppLab'(AbstractGrammar.Lab(_,l)) = Label.toString l

    fun ppId'(AbstractGrammar.Id(_,_,n)) = Name.toString n
    fun ppId x = ppQuoted(ppId' x)

    fun ppLongid'(AbstractGrammar.ShortId(_,x))  = ppId' x
      | ppLongid'(AbstractGrammar.LongId(_,y,l)) = ppLongid' y ^ "." ^ ppLab' l
    fun ppLongid y = ppQuoted(ppLongid' y)


    val classLab	= (ppLab,   ["label"])
    val classVId	= (ppVId,   ["value","or","constructor"])
    val classTyCon	= (ppTyCon, ["type"])
    val classTyVar	= (ppTyVar, ["type","variable"])
    val classStrId	= (ppStrId, ["structure","or","functor"])
    val classSigId	= (ppSigId, ["signature"])

    fun ppUnbound((ppId,class), id) =
	  par(["unknown"] @ class @ [ppId id])
    fun ppUnboundImport((ppId,class), id) =
	  par(class @ [ppId id,"is","not","exported","by","component"])

    fun ppError(VIdUnbound vid) =
	  ppUnbound(classVId, vid)
      | ppError(TyConUnbound tycon) =
	  ppUnbound(classTyCon, tycon)
      | ppError(TyVarUnbound tyvar) =
	  ppUnbound(classTyVar, tyvar)
      | ppError(StrIdUnbound strid) =
	  ppUnbound(classStrId, strid)
      | ppError(SigIdUnbound sigid) =
	  ppUnbound(classSigId, sigid)
      | ppError(PreboundFirstClass) =
	  par["invalid","use","of","pseudo","structure"]
      (* Expressions *)
      | ppError(ExpRowLabDuplicate lab) =
	  par(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      (* Patterns *)
      | ppError(PatVIdDuplicate vid) =
	  par["duplicate","variable",ppVId vid,"in","pattern",
	      "or","binding","group"]
      | ppError(WithPatVIdDuplicate vid) =
	  par["pattern","variable",ppVId vid,"redefined",
	      "inside","value","binding"]
      | ppError(PatLongVIdVar) =
	  par["non-constructor","long","identifier","in","pattern"]
      | ppError(PatRowLabDuplicate lab) =
	  par(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      | ppError(AppPatNonCon) =
	  par["application","of","non-constructor","in","pattern"]
      | ppError(AltPatInconsistent) =
	  par["inconsistent","pattern","alternative"]
      (* Types *)
      | ppError(TyRowLabDuplicate lab) =
	  par(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      | ppError(TyVarSeqDuplicate tyvar) =
	  par(["duplicate"] @ #2 classTyVar @ [ppTyVar tyvar])
      | ppError(ValTyVarSeqDuplicate tyvar) =
	  par(["duplicate","or","shadowing"] @ #2 classTyVar @ [ppTyVar tyvar])
      (* Declarations and bindings *)
      | ppError(FvalBindDuplicate vid) =
	  par["duplicate","function",ppVId vid,"in","binding","group"]
      | ppError(FvalBindArityInconsistent) =
	  par["inconistent","function","arity","in","function","clause"]
      | ppError(FvalBindArityZero) =
	  par["no","arguments","in","function","clause"]
      | ppError(FvalBindNameInconsistent vid) =
	  par["inconistent","function","name",ppVId vid,
	      "in","function","clause"]
      | ppError(FvalBindNameMissing) =
	  par["no","function","name","in","function","clause"]
      | ppError(FvalBindNameCon vid) =
	  par["redefining","constructor",ppVId vid,"as","value"]
      | ppError(FvalBindPatInvalid) =
	  par["invalid","function","clause"]
      | ppError(TypBindDuplicate tycon) =
	  par(["duplicate"] @ #2 classTyCon @
	      [ppTyCon tycon,"in","binding","group"])
      | ppError(DatBindDuplicate tycon) =
	  par(["duplicate"] @ #2 classTyCon @
	      [ppTyCon tycon,"in","binding","group"])
      | ppError(DatBindConDuplicate vid) =
	  par["duplicate","constructor",ppVId vid,"in","binding","group"]
      | ppError(ConBindDuplicate vid) =
	  par["duplicate","constructor",ppVId vid,"in","datatype"]
      | ppError(DconBindDuplicate vid) =
	  par["duplicate","constructor",ppVId vid,"in","binding","group"]
      | ppError(DconBindNonCon) =
	  par["non-constructor","on","constructor","binding",
	      "right","hand","side"]
      | ppError(StrBindDuplicate strid) =
	  par(["duplicate"] @ #2 classStrId @
	      [ppStrId strid,"in","binding","group"])
      | ppError(SigBindDuplicate sigid) =
	  par(["duplicate"] @ #2 classSigId @
	      [ppSigId sigid,"in","binding","group"])
      (* Specifications and descriptions *)
      | ppError(SpecFixDuplicate vid) =
	  par(["duplicate","fixity","specification","for"] @ #2 classVId @
	      [ppVId vid,"in","signature"])
      | ppError(SpecVIdDuplicate vid) =
	  par(["duplicate"] @ #2 classVId @ [ppVId vid,"in","signature"])
      | ppError(SpecTyConDuplicate tycon) =
	  par(["duplicate"] @ #2 classTyCon @ [ppTyCon tycon,"in","signature"])
      | ppError(SpecStrIdDuplicate strid) =
	  par(["duplicate"] @ #2 classStrId @ [ppStrId strid,"in","signature"])
      | ppError(SpecSigIdDuplicate sigid) =
	  par(["duplicate"] @ #2 classSigId @ [ppSigId sigid,"in","signature"])
      | ppError(ConDescDuplicate vid) =
	  par["duplicate","constructor",ppVId vid,"in","datatype"]
      | ppError(DconDescNonCon) =
	  par["non-constructor","on","constructor","description",
	      "right","hand","side"]
      (* Imports and items *)
      | ppError(ImpVIdDuplicate vid) =
	  par(["duplicate"] @ #2 classVId @ [ppVId vid,"in","import"])
      | ppError(ImpTyConDuplicate tycon) =
	  par(["duplicate"] @ #2 classTyCon @ [ppTyCon tycon,"in","import"])
      | ppError(ImpStrIdDuplicate strid) =
	  par(["duplicate"] @ #2 classStrId @ [ppStrId strid,"in","import"])
      | ppError(ImpSigIdDuplicate sigid) =
	  par(["duplicate"] @ #2 classSigId @ [ppSigId sigid,"in","import"])
      | ppError(ConItemDuplicate vid) =
	  par["duplicate","constructor",ppVId vid,"in","datatype"]
      | ppError(ValItemUnbound vid) =
	  ppUnboundImport(classVId, vid)
      | ppError(TypItemUnbound tycon) =
	  ppUnboundImport(classTyCon, tycon)
      | ppError(DatItemUnbound tycon) =
	  ppUnboundImport(classTyCon, tycon)
      | ppError(ConItemUnbound vid) =
	  ppUnboundImport(classVId, vid)
      | ppError(DconItemUnbound vid) =
	  ppUnboundImport(classVId, vid)
      | ppError(StrItemUnbound strid) =
	  ppUnboundImport(classStrId, strid)
      | ppError(SigItemUnbound sigid) =
	  ppUnboundImport(classSigId, sigid)
      | ppError(ConItemNonCon vid) =
	  par["value",ppVId vid,"exported","by","component","is",
	      "not","a","constructor"]
      | ppError(DconItemNonCon vid) =
	  par["value",ppVId vid,"exported","by","component","is",
	      "not","a","constructor"]
      (* Sharing translation *)
      | ppError(SharingExternalTy x) =
	  par(#2 classTyCon @ [ppId x,"is","external","to","signature"])
      | ppError(SharingExternalSig x) =
	  par(#2 classSigId @ [ppId x,"is","external","to","signature"])
      | ppError(SharingExternalStr x) =
	  par(#2 classStrId @ [ppId x,"is","external","to","signature"])


    fun ppShadowed((ppId,class), id) =
	  par(class @ [ppId id,"shadows","previous","one"])

    fun ppWarning(VIdShadowed vid) =
	  ppShadowed(classVId, vid)
      | ppWarning(TyConShadowed tycon) =
	  ppShadowed(classTyCon, tycon)
      | ppWarning(TyVarShadowed tyvar) =
	  ppShadowed(classTyVar, tyvar)
      | ppWarning(StrIdShadowed strid) =
	  ppShadowed(classStrId, strid)
      | ppWarning(SigIdShadowed sigid) =
	  ppShadowed(classSigId, sigid)


  (* Export *)

    fun errorToString e   = PrettyPrint.toString(ppError e, 75)
    fun warningToString w = PrettyPrint.toString(ppWarning w, 75)

    fun error(region, e)  = Error.error(region, errorToString e)
    fun warn(region, w)   = Error.warn(region, warningToString w)

  end
