signature BIND_ENV =
  sig

    type VId   = VId.t
    type TyVar = TyVar.t
    type TyCon = TyCon.t
    type StrId = StrId.t
    type SigId = SigId.t
    type FunId = FunId.t

    type stamp = AbstractGrammar.stamp
    type Info  = Source.position


    datatype InfAssoc  = datatype Infix.Assoc
    type     InfStatus = Infix.InfStatus
    datatype IdStatus  = V | C of int | R

    datatype Env = ENV of { IE: Inf VIdMap.t
			  , VE: Val VIdMap.t
			  , TE: Ty  TyConMap.t
			  , UE: Var TyVarMap.t
			  , SE: Str StrIdMap.t
			  , GE: Sig SigIdMap.t
			  , FE: Fun FunIdMap.t
			  }

    withtype Inf = Info * InfStatus
    and      Val = Info * stamp * IdStatus
    and      Ty  = Info * stamp * Env
    and      Var = Info * stamp
    and      Str = Info * stamp * Env
    and      Sig = Info * stamp * Env
    and      Fun = Info * stamp * Env


    exception CollisionInf of VId
    exception CollisionVal of VId   (* = VIdMap.Collision *)
    exception CollisionTy  of TyCon (* = TyConMap.Collision *)
    exception CollisionVar of TyVar (* = TyVarMap.Collision *)
    exception CollisionStr of StrId (* = StrIdMap.Collision *)
    exception CollisionSig of SigId (* = SigIdMap.Collision *)
    exception CollisionFun of FunId (* = FunIdMap.Collision *)


    val new :			unit -> Env
    val copy :			Env -> Env
    val copyScope :		Env -> Env

    val insertScope :		Env -> unit
    val deleteScope :		Env -> unit
    val delete2ndScope :	Env -> unit
    val mergeScope :		Env -> unit

    val union :			Env * Env -> unit
    val unionDisjoint :		Env * Env -> unit	(* Collision* *)

    val insertInf :		Env *  VId  * Inf -> unit
    val insertVal :		Env *  VId  * Val -> unit
    val insertTy :		Env * TyCon * Ty  -> unit
    val insertVar :		Env * TyVar * Var -> unit
    val insertStr :		Env * StrId * Str -> unit
    val insertSig :		Env * SigId * Sig -> unit
    val insertFun :		Env * FunId * Fun -> unit
    val insertDisjointInf :	Env *  VId  * Inf -> unit   (* CollisionInf *)
    val insertDisjointVal :	Env *  VId  * Val -> unit   (* CollisionVal *)
    val insertDisjointTy :	Env * TyCon * Ty  -> unit   (* CollisionTy *)
    val insertDisjointVar :	Env * TyVar * Var -> unit   (* CollisionVar *)
    val insertDisjointStr :	Env * StrId * Str -> unit   (* CollisionStr *)
    val insertDisjointSig :	Env * SigId * Sig -> unit   (* CollisionSig *)
    val insertDisjointFun :	Env * FunId * Fun -> unit   (* CollisionFun *)

    val lookupInf :		Env *  VId  -> Inf option
    val lookupVal :		Env *  VId  -> Val option
    val lookupTy :		Env * TyCon -> Ty  option
    val lookupVar :		Env * TyVar -> Var option
    val lookupStr :		Env * StrId -> Str option
    val lookupSig :		Env * SigId -> Sig option
    val lookupFun :		Env * FunId -> Fun option
    val lookupScopeInf :	Env *  VId  -> Inf option
    val lookupScopeVal :	Env *  VId  -> Val option
    val lookupScopeTy :		Env * TyCon -> Ty  option
    val lookupScopeVar :	Env * TyVar -> Var option
    val lookupScopeStr :	Env * StrId -> Str option
    val lookupScopeSig :	Env * SigId -> Sig option
    val lookupScopeFun :	Env * FunId -> Fun option

    val appVals :		( VId  * Val -> unit) -> Env -> unit
    val appTys :		(TyCon * Ty  -> unit) -> Env -> unit
    val appVars :		(TyVar * Var -> unit) -> Env -> unit
    val appStrs :		(StrId * Str -> unit) -> Env -> unit
    val appSigs :		(SigId * Sig -> unit) -> Env -> unit
    val appFuns :		(FunId * Fun -> unit) -> Env -> unit
    val foldVals :		( VId  * Val * 'a -> 'a) -> 'a -> Env -> 'a
    val foldTys :		(TyCon * Ty  * 'a -> 'a) -> 'a -> Env -> 'a
    val foldVars :		(TyVar * Var * 'a -> 'a) -> 'a -> Env -> 'a
    val foldStrs :		(StrId * Str * 'a -> 'a) -> 'a -> Env -> 'a
    val foldSigs :		(SigId * Sig * 'a -> 'a) -> 'a -> Env -> 'a
    val foldFuns :		(FunId * Fun * 'a -> 'a) -> 'a -> Env -> 'a

  end
