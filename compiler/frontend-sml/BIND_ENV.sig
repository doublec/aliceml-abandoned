(*******************************************************************************

The binding environment just contains the information necessary to do
binding analysis for identifiers:

	Env	= InfEnv U FldEnv U VarEnv U ValEnv U TyEnv U StrEnv U SigEnv
	InfEnv	= VId   -> InfStatus		(infix env)
	FldEnv	= Lab   -> 0			(field env)
	VarEnv	= TyVar -> Stamp		(type variable env)
	ValEnv	= VId   -> Stamp * IdStatus	(value env)
	TyEnv	= TyCon -> Stamp * Env		(type env)
	StrEnv	= StrId -> Stamp * Env		(module env)
	SigEnv	= SigId -> Stamp * Env		(signature env)

Field envs are just used to detect duplicate labels. Type envs map to
constructor environments. Module envs map to environments representing the
module's signature (th result signature for functors, as we never need to look
at the argument signature). Similarly, signature envs map to the result
environment (ignoring signature parameters).

*******************************************************************************)

signature BIND_ENV =
  sig

    type Lab   = Lab.t
    type VId   = VId.t
    type TyVar = TyVar.t
    type TyCon = TyCon.t
    type StrId = StrId.t
    type SigId = SigId.t

    type Info  = Source.region
    type stamp = Stamp.t

    datatype InfAssoc  = datatype Infix.Assoc
    type     InfStatus = Infix.InfStatus
    datatype IdStatus  = V | C of int | R

    type Env
    type Inf = Info * InfStatus
    type Fld = Info
    type Var = Info * stamp
    type Val = Info * stamp * IdStatus
    type Ty  = Info * stamp * Env
    type Str = Info * stamp * Env
    type Sig = Info * stamp * Env

    type t = Env


    exception CollisionInf of VId
    exception CollisionFld of Lab
    exception CollisionVal of VId
    exception CollisionTy  of TyCon
    exception CollisionVar of TyVar
    exception CollisionStr of StrId
    exception CollisionSig of SigId


    val new :			unit -> Env

    val size :			Env -> int
    val sizeScope :		Env -> int

    val clone :			Env -> Env
    val cloneScope :		Env -> Env

    val insertScope :		Env -> unit
    val deleteScope :		Env -> unit
    val mergeScope :		Env -> unit
    val mergeDisjointScope :	Env -> unit		(* Collision* *)
    val inheritScope :		Env * Env -> unit
    val splitScope :		Env -> Env

    val union :			Env * Env -> unit
    val unionDisjoint :		Env * Env -> unit	(* Collision* *)
    val unionCompose :		Env * Env -> unit
    val unionInf :		Env * Env -> unit

    val insertInf :		Env *  VId  * Inf -> unit
    val insertFld :		Env *  Lab  * Fld -> unit
    val insertVal :		Env *  VId  * Val -> unit
    val insertTy :		Env * TyCon * Ty  -> unit
    val insertVar :		Env * TyVar * Var -> unit
    val insertStr :		Env * StrId * Str -> unit
    val insertSig :		Env * SigId * Sig -> unit
    val insertDisjointInf :	Env *  VId  * Inf -> unit   (* CollisionInf *)
    val insertDisjointFld :	Env *  Lab  * Fld -> unit   (* CollisionFld *)
    val insertDisjointVal :	Env *  VId  * Val -> unit   (* CollisionVal *)
    val insertDisjointTy :	Env * TyCon * Ty  -> unit   (* CollisionTy *)
    val insertDisjointVar :	Env * TyVar * Var -> unit   (* CollisionVar *)
    val insertDisjointStr :	Env * StrId * Str -> unit   (* CollisionStr *)
    val insertDisjointSig :	Env * SigId * Sig -> unit   (* CollisionSig *)

    val lookupInf :		Env *  VId  -> Inf option
    val lookupFld :		Env *  Lab  -> Fld option
    val lookupVar :		Env * TyVar -> Var option
    val lookupVal :		Env *  VId  -> Val option
    val lookupTy :		Env * TyCon -> Ty  option
    val lookupStr :		Env * StrId -> Str option
    val lookupSig :		Env * SigId -> Sig option
    val lookupScopeInf :	Env *  VId  -> Inf option
    val lookupScopeFld :	Env *  Lab  -> Fld option
    val lookupScopeVar :	Env * TyVar -> Var option
    val lookupScopeVal :	Env *  VId  -> Val option
    val lookupScopeTy :		Env * TyCon -> Ty  option
    val lookupScopeStr :	Env * StrId -> Str option
    val lookupScopeSig :	Env * SigId -> Sig option

    val appiInfs :		( VId  * Inf -> unit) -> Env -> unit
    val appiFlds :		( Lab  * Fld -> unit) -> Env -> unit
    val appiVars :		(TyVar * Var -> unit) -> Env -> unit
    val appiVals :		( VId  * Val -> unit) -> Env -> unit
    val appiTys :		(TyCon * Ty  -> unit) -> Env -> unit
    val appiStrs :		(StrId * Str -> unit) -> Env -> unit
    val appiSigs :		(SigId * Sig -> unit) -> Env -> unit
    val appiScopeVals :		( VId  * Val -> unit) -> Env -> unit

    val foldiInfs :		( VId  * Inf * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiFlds :		( Lab  * Fld * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiVars :		(TyVar * Var * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiVals :		( VId  * Val * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiTys :		(TyCon * Ty  * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiStrs :		(StrId * Str * 'a -> 'a) -> 'a -> Env -> 'a
    val foldiSigs :		(SigId * Sig * 'a -> 'a) -> 'a -> Env -> 'a

    val infEnv :		Env -> VId -> InfStatus

  end
