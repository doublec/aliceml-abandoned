signature ABSTRACTION_ERROR =
  sig

    type Lab	= Lab.t
    type VId	= VId.t
    type TyVar	= TyVar.t
    type TyCon	= TyCon.t
    type StrId	= StrId.t
    type SigId	= SigId.t
    type FunId	= FunId.t
    type id	= AbstractGrammar.id

    datatype error =
	(* Identifiers *)
	  VIdUnbound		of VId
	| TyConUnbound		of TyCon
	| TyVarUnbound		of TyVar
	| StrIdUnbound		of StrId
	| SigIdUnbound		of SigId
	| FunIdUnbound		of FunId
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
	| FnBindDuplicate	of VId
	| FnBindArityInconsistent
	| FnBindArityZero
	| FnBindNameInconsistent of VId
	| FnBindNameMissing
	| FnBindNameCon		of VId
	| FnBindPatInvalid
	| TypBindDuplicate	of TyCon
	| DatBindDuplicate	of TyCon
	| DatBindConDuplicate	of VId
	| ConBindDuplicate	of VId
	| DconBindDuplicate	of VId
	| DconBindNonCon
	| StrBindDuplicate	of StrId
	| SigBindDuplicate	of SigId
	| FunBindDuplicate	of FunId
	(* Specifications and descriptions *)
	| SpecFixDuplicate	of VId
	| SpecVIdDuplicate	of VId
	| SpecTyConDuplicate	of TyCon
	| SpecStrIdDuplicate	of StrId
	| SpecSigIdDuplicate	of SigId
	| SpecFunIdDuplicate	of FunId
	| ConDescDuplicate	of VId
	| DconDescNonCon
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
	| FunIdShadowed		of FunId

    val error :	Source.region * error -> 'a
    val warn :	Source.region * warning -> unit

  end
