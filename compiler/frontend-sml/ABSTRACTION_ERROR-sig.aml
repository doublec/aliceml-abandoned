signature ABSTRACTION_ERROR =
  sig

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
	(* Components *)
	| CompCorrupt		of Url.t
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

    val error :	Source.region * error -> 'a
    val warn :	Source.region * warning -> unit

  end
