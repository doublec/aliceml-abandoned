structure AbstractionError :> ABSTRACTION_ERROR =
  struct

  (* Pretty printer *)

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

  (* Types *)

    type Lab	= Lab.t
    type VId	= VId.t
    type TyVar	= TyVar.t
    type TyCon	= TyCon.t
    type StrId	= StrId.t
    type SigId	= SigId.t
    type typid	= AbstractGrammar.typid
    type infid	= AbstractGrammar.infid
    type modid	= AbstractGrammar.modid

    datatype error =
	(* Infix *)
	  InfixMisplaced	of VId
	| AssocConflict		of VId * VId
	(* Identifiers *)
	| VIdUnbound		of VId
	| TyConUnbound		of TyCon
	| TyVarUnbound		of TyVar
	| StrIdUnbound		of StrId
	| SigIdUnbound		of SigId
	(* Expressions *)
	| ExpConArgSuperfluous
	| ExpRowLabDuplicate	of Lab
	(* Patterns *)
	| PatConArgMissing
	| PatConArgSuperfluous
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
	| ImpFixDuplicate	of VId
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
	| SharingExternalTy	of typid
	| SharingExternalSig	of infid
	| SharingExternalStr	of modid

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
	  textpar(["unknown"] @ class @ [ppId id])
    fun ppUnboundImport((ppId,class), id) =
	  textpar(class @ [ppId id,"is","not","exported","by","component"])

    fun ppError(InfixMisplaced vid) =
	  textpar["misplaced","infix","identifier",ppVId vid]
      | ppError(AssocConflict(vid1,vid2)) =
	  textpar["conflicting","infix","associativity","between","operators",
		  ppVId vid1,"and",ppVId vid2]
      (* Unbound identifiers *)
      | ppError(VIdUnbound vid) =
	  ppUnbound(classVId, vid)
      | ppError(TyConUnbound tycon) =
	  ppUnbound(classTyCon, tycon)
      | ppError(TyVarUnbound tyvar) =
	  ppUnbound(classTyVar, tyvar)
      | ppError(StrIdUnbound strid) =
	  ppUnbound(classStrId, strid)
      | ppError(SigIdUnbound sigid) =
	  ppUnbound(classSigId, sigid)
      (* Expressions *)
      | ppError(ExpConArgSuperfluous) =
	  textpar["superfluous","constructor","argument"]
      | ppError(ExpRowLabDuplicate lab) =
	  textpar(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      (* Patterns *)
      | ppError(PatConArgMissing) =
	  textpar["missing","constructor","argument","in","pattern"]
      | ppError(PatConArgSuperfluous) =
	  textpar["superfluous","constructor","argument","in","pattern"]
      | ppError(PatVIdDuplicate vid) =
	  textpar["duplicate","variable",ppVId vid,"in","pattern",
		  "or","binding","group"]
      | ppError(WithPatVIdDuplicate vid) =
	  textpar["pattern","variable",ppVId vid,"redefined",
		  "inside","value","binding"]
      | ppError(PatLongVIdVar) =
	  textpar["non-constructor","long","identifier","in","pattern"]
      | ppError(PatRowLabDuplicate lab) =
	  textpar(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      | ppError(AppPatNonCon) =
	  textpar["application","of","non-constructor","in","pattern"]
      | ppError(AltPatInconsistent) =
	  textpar["inconsistent","pattern","alternative"]
      (* Types *)
      | ppError(TyRowLabDuplicate lab) =
	  textpar(["duplicate"] @ #2 classLab @ [ppLab lab,"in","record"])
      | ppError(TyVarSeqDuplicate tyvar) =
	  textpar(["duplicate"] @ #2 classTyVar @ [ppTyVar tyvar])
      | ppError(ValTyVarSeqDuplicate tyvar) =
	  textpar(["duplicate","or","shadowing"] @ #2 classTyVar @
		  [ppTyVar tyvar])
      (* Declarations and bindings *)
      | ppError(FvalBindDuplicate vid) =
	  textpar["duplicate","function",ppVId vid,"in","binding","group"]
      | ppError(FvalBindArityInconsistent) =
	  textpar["inconsistent","function","arity","in","function","clause"]
      | ppError(FvalBindArityZero) =
	  textpar["no","arguments","in","function","clause"]
      | ppError(FvalBindNameInconsistent vid) =
	  textpar["inconsistent","function","name",ppVId vid,
		  "in","function","clause"]
      | ppError(FvalBindNameMissing) =
	  textpar["no","function","name","in","function","clause"]
      | ppError(FvalBindNameCon vid) =
	  textpar["redefining","constructor",ppVId vid,"as","value"]
      | ppError(FvalBindPatInvalid) =
	  textpar["invalid","function","clause"]
      | ppError(TypBindDuplicate tycon) =
	  textpar(["duplicate"] @ #2 classTyCon @
		  [ppTyCon tycon,"in","binding","group"])
      | ppError(DatBindDuplicate tycon) =
	  textpar(["duplicate"] @ #2 classTyCon @
		  [ppTyCon tycon,"in","binding","group"])
      | ppError(DatBindConDuplicate vid) =
	  textpar["duplicate","constructor",ppVId vid,"in","binding","group"]
      | ppError(ConBindDuplicate vid) =
	  textpar["duplicate","constructor",ppVId vid,"in","datatype"]
      | ppError(DconBindDuplicate vid) =
	  textpar["duplicate","constructor",ppVId vid,"in","binding","group"]
      | ppError(DconBindNonCon) =
	  textpar["non-constructor","on","constructor","binding",
		  "right","hand","side"]
      | ppError(StrBindDuplicate strid) =
	  textpar(["duplicate"] @ #2 classStrId @
		  [ppStrId strid,"in","binding","group"])
      | ppError(SigBindDuplicate sigid) =
	  textpar(["duplicate"] @ #2 classSigId @
		  [ppSigId sigid,"in","binding","group"])
      (* Specifications and descriptions *)
      | ppError(SpecFixDuplicate vid) =
	  textpar(["duplicate","fixity","specification","for"] @ #2 classVId @
		  [ppVId vid,"in","signature"])
      | ppError(SpecVIdDuplicate vid) =
	  textpar(["duplicate"] @ #2 classVId @ [ppVId vid,"in","signature"])
      | ppError(SpecTyConDuplicate tycon) =
	  textpar(["duplicate"] @ #2 classTyCon @
		  [ppTyCon tycon,"in","signature"])
      | ppError(SpecStrIdDuplicate strid) =
	  textpar(["duplicate"] @ #2 classStrId @
		  [ppStrId strid,"in","signature"])
      | ppError(SpecSigIdDuplicate sigid) =
	  textpar(["duplicate"] @ #2 classSigId @
		  [ppSigId sigid,"in","signature"])
      | ppError(ConDescDuplicate vid) =
	  textpar["duplicate","constructor",ppVId vid,"in","datatype"]
      | ppError(DconDescNonCon) =
	  textpar["non-constructor","on","constructor","description",
		  "right","hand","side"]
      (* Imports and items *)
      | ppError(ImpFixDuplicate vid) =
	  textpar(["duplicate","fixity","for"] @ #2 classVId @
		  [ppVId vid,"in","import"])
      | ppError(ImpVIdDuplicate vid) =
	  textpar(["duplicate"] @ #2 classVId @ [ppVId vid,"in","import"])
      | ppError(ImpTyConDuplicate tycon) =
	  textpar(["duplicate"] @ #2 classTyCon @ [ppTyCon tycon,"in","import"])
      | ppError(ImpStrIdDuplicate strid) =
	  textpar(["duplicate"] @ #2 classStrId @ [ppStrId strid,"in","import"])
      | ppError(ImpSigIdDuplicate sigid) =
	  textpar(["duplicate"] @ #2 classSigId @ [ppSigId sigid,"in","import"])
      | ppError(ConItemDuplicate vid) =
	  textpar["duplicate","constructor",ppVId vid,"in","datatype"]
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
	  textpar["value",ppVId vid,"exported","by","component","is",
		  "not","a","proper","constructor"]
      | ppError(DconItemNonCon vid) =
	  textpar["value",ppVId vid,"exported","by","component","is",
		  "not","a","proper","constructor"]
      (* Sharing translation *)
      | ppError(SharingExternalTy x) =
	  textpar(#2 classTyCon @ [ppId x,"is","external","to","signature"])
      | ppError(SharingExternalSig x) =
	  textpar(#2 classSigId @ [ppId x,"is","external","to","signature"])
      | ppError(SharingExternalStr x) =
	  textpar(#2 classStrId @ [ppId x,"is","external","to","signature"])


    fun ppShadowed((ppId,class), id) =
	  textpar(class @ [ppId id,"shadows","previous","one"])

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
