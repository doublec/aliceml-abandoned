structure BindEnv :> BIND_ENV =
  struct

    type Info  = Source.region
    type stamp = Stamp.t


    (* The environment's domain *)

    type Lab   = Lab.t
    type VId   = VId.t
    type TyVar = TyVar.t
    type TyCon = TyCon.t
    type StrId = StrId.t
    type SigId = SigId.t

    datatype Dom = INFIX of VId
		 | LAB   of Lab
		 | TYVAR of TyVar
		 | VID   of VId
		 | TYCON of TyCon
		 | STRID of StrId
		 | SIGID of SigId

    fun hashDom(INFIX id) = StringHashKey.hash(  VId.toString id)
      | hashDom(LAB   id) = StringHashKey.hash(  Lab.toString id)
      | hashDom(TYVAR id) = StringHashKey.hash(TyVar.toString id)
      | hashDom(VID   id) = StringHashKey.hash(  VId.toString id)
      | hashDom(TYCON id) = StringHashKey.hash(TyCon.toString id)
      | hashDom(STRID id) = StringHashKey.hash(StrId.toString id)
      | hashDom(SIGID id) = StringHashKey.hash(SigId.toString id)


    (* The map implementing the environment *)

    structure Map = MakeHashScopedImpMap(type t = Dom val hash = hashDom)


    (* The environment's range *)

    datatype InfAssoc  = datatype Infix.Assoc
    type     InfStatus = Infix.InfStatus
    datatype IdStatus  = V | C of int | R

    datatype Env = ENV of Ran Map.t
    and      Ran = INF of Inf
		 | FLD of Fld
		 | VAR of Var
		 | VAL of Val
		 | TY  of Ty
		 | STR of Str
		 | SIG of Sig

    withtype Inf = Info * InfStatus
    and      Fld = Info
    and      Var = Info * stamp
    and      Val = Info * stamp * IdStatus
    and      Ty  = Info * stamp * Env
    and      Str = Info * stamp * Env
    and      Sig = Info * stamp * Env

    fun asInfo(SOME(INF x)) = SOME x | asInfo _ = NONE
    fun asFldo(SOME(FLD x)) = SOME x | asFldo _ = NONE
    fun asVaro(SOME(VAR x)) = SOME x | asVaro _ = NONE
    fun asValo(SOME(VAL x)) = SOME x | asValo _ = NONE
    fun asTyo (SOME(TY  x)) = SOME x | asTyo  _ = NONE
    fun asStro(SOME(STR x)) = SOME x | asStro _ = NONE
    fun asSigo(SOME(SIG x)) = SOME x | asSigo _ = NONE

    fun appInf f (INFIX id, INF x) = f(id,x) | appInf f _ = ()
    fun appFld f (LAB   id, FLD x) = f(id,x) | appFld f _ = ()
    fun appVar f (TYVAR id, VAR x) = f(id,x) | appVar f _ = ()
    fun appVal f (VID   id, VAL x) = f(id,x) | appVal f _ = ()
    fun appTy  f (TYCON id, TY  x) = f(id,x) | appTy  f _ = ()
    fun appStr f (STRID id, STR x) = f(id,x) | appStr f _ = ()
    fun appSig f (SIGID id, SIG x) = f(id,x) | appSig f _ = ()

    fun foldInf f (INFIX id, INF x, a) = f(id,x,a) | foldInf f (_,_,a) = a
    fun foldFld f (LAB   id, FLD x, a) = f(id,x,a) | foldFld f (_,_,a) = a
    fun foldVar f (TYVAR id, VAR x, a) = f(id,x,a) | foldVar f (_,_,a) = a
    fun foldVal f (VID   id, VAL x, a) = f(id,x,a) | foldVal f (_,_,a) = a
    fun foldTy  f (TYCON id, TY  x, a) = f(id,x,a) | foldTy  f (_,_,a) = a
    fun foldStr f (STRID id, STR x, a) = f(id,x,a) | foldStr f (_,_,a) = a
    fun foldSig f (SIGID id, SIG x, a) = f(id,x,a) | foldSig f (_,_,a) = a


    (* Collision exceptions *)

    exception CollisionInf of VId
    exception CollisionFld of Lab
    exception CollisionVal of VId
    exception CollisionTy  of TyCon
    exception CollisionVar of TyVar
    exception CollisionStr of StrId
    exception CollisionSig of SigId

    fun transformCollision(INFIX id)	= raise CollisionInf id
      | transformCollision(LAB   id)	= raise CollisionFld id
      | transformCollision(TYVAR id)	= raise CollisionVar id
      | transformCollision(VID   id)	= raise CollisionVal id
      | transformCollision(TYCON id)	= raise CollisionTy  id
      | transformCollision(STRID id)	= raise CollisionStr id
      | transformCollision(SIGID id)	= raise CollisionSig id


    (* Actual operations *)

    fun new()				= ENV(Map.new())
    fun copy(ENV E)			= ENV(Map.copy E)
    fun copyScope(ENV E)		= ENV(Map.copyScope E)
    fun splitScope(ENV E)		= ENV(Map.splitScope E)

    fun insertScope(ENV E)		= Map.insertScope E
    fun inheritScope(ENV E1, ENV E2)	= Map.inheritScope(E1,E2)
    fun deleteScope(ENV E)		= Map.deleteScope E
    fun mergeScope(ENV E)		= Map.mergeScope E
    fun mergeDisjointScope(ENV E)	= Map.mergeDisjointScope E
					  handle Map.Collision coll =>
						 transformCollision coll

    fun union(ENV E1, ENV E2)		= Map.union(E1,E2)
    fun unionDisjoint(ENV E1, ENV E2)	= Map.unionDisjoint(E1,E2)
					  handle Map.Collision coll =>
						 transformCollision coll

    fun size(ENV E)			= Map.size E
    fun sizeScope(ENV E)		= Map.sizeScope E

    fun insertInf(ENV E, id, x)		= Map.insert(E, INFIX id, INF x)
    fun insertFld(ENV E, id, x)		= Map.insert(E, LAB   id, FLD x)
    fun insertVar(ENV E, id, x)		= Map.insert(E, TYVAR id, VAR x)
    fun insertVal(ENV E, id, x)		= Map.insert(E, VID   id, VAL x)
    fun insertTy (ENV E, id, x)		= Map.insert(E, TYCON id, TY  x)
    fun insertStr(ENV E, id, x)		= Map.insert(E, STRID id, STR x)
    fun insertSig(ENV E, id, x)		= Map.insert(E, SIGID id, SIG x)

    fun insertDisjointInf(ENV E, id, x)	= Map.insertDisjoint(E, INFIX id, INF x)
					  handle Map.Collision(INFIX id) =>
						 raise CollisionInf id
    fun insertDisjointFld(ENV E, id, x)	= Map.insertDisjoint(E, LAB id, FLD x)
					  handle Map.Collision(LAB id) =>
						 raise CollisionFld id
    fun insertDisjointVar(ENV E, id, x)	= Map.insertDisjoint(E, TYVAR id, VAR x)
					  handle Map.Collision(TYVAR id) =>
						 raise CollisionVar id
    fun insertDisjointVal(ENV E, id, x)	= Map.insertDisjoint(E, VID id, VAL x)
					  handle Map.Collision(VID id) =>
						 raise CollisionVal id
    fun insertDisjointTy(ENV E, id, x)	= Map.insertDisjoint(E, TYCON id, TY x)
					  handle Map.Collision(TYCON id) =>
						 raise CollisionTy id
    fun insertDisjointStr(ENV E, id, x)	= Map.insertDisjoint(E, STRID id, STR x)
					  handle Map.Collision(STRID id) =>
						 raise CollisionStr id
    fun insertDisjointSig(ENV E, id, x)	= Map.insertDisjoint(E, SIGID id, SIG x)
					  handle Map.Collision(SIGID id) =>
						 raise CollisionSig id

    fun lookupInf(ENV E, id)		= asInfo(Map.lookup(E, INFIX id))
    fun lookupFld(ENV E, id)		= asFldo(Map.lookup(E, LAB   id))
    fun lookupVar(ENV E, id)		= asVaro(Map.lookup(E, TYVAR id))
    fun lookupVal(ENV E, id)		= asValo(Map.lookup(E, VID   id))
    fun lookupTy (ENV E, id)		= asTyo (Map.lookup(E, TYCON id))
    fun lookupStr(ENV E, id)		= asStro(Map.lookup(E, STRID id))
    fun lookupSig(ENV E, id)		= asSigo(Map.lookup(E, SIGID id))

    fun lookupScopeInf(ENV E, id)	= asInfo(Map.lookupScope(E, INFIX id))
    fun lookupScopeFld(ENV E, id)	= asFldo(Map.lookupScope(E, LAB   id))
    fun lookupScopeVar(ENV E, id)	= asVaro(Map.lookupScope(E, TYVAR id))
    fun lookupScopeVal(ENV E, id)	= asValo(Map.lookupScope(E, VID   id))
    fun lookupScopeTy (ENV E, id)	= asTyo (Map.lookupScope(E, TYCON id))
    fun lookupScopeStr(ENV E, id)	= asStro(Map.lookupScope(E, STRID id))
    fun lookupScopeSig(ENV E, id)	= asSigo(Map.lookupScope(E, SIGID id))

    fun appiInfs f (ENV E)		= Map.appi (appInf f) E
    fun appiFlds f (ENV E)		= Map.appi (appFld f) E
    fun appiVars f (ENV E)		= Map.appi (appVar f) E
    fun appiVals f (ENV E)		= Map.appi (appVal f) E
    fun appiTys  f (ENV E)		= Map.appi (appTy  f) E
    fun appiStrs f (ENV E)		= Map.appi (appStr f) E
    fun appiSigs f (ENV E)		= Map.appi (appSig f) E
    fun appiScopeVals f (ENV E)		= Map.appiScope (appVal f) E

    fun foldiInfs f a (ENV E)		= Map.foldi (foldInf f) a E
    fun foldiFlds f a (ENV E)		= Map.foldi (foldFld f) a E
    fun foldiVars f a (ENV E)		= Map.foldi (foldVar f) a E
    fun foldiVals f a (ENV E)		= Map.foldi (foldVal f) a E
    fun foldiTys  f a (ENV E)		= Map.foldi (foldTy  f) a E
    fun foldiStrs f a (ENV E)		= Map.foldi (foldStr f) a E
    fun foldiSigs f a (ENV E)		= Map.foldi (foldSig f) a E


    fun unionInf(E1,E2)			= appiInfs (fn(id,x) =>
						     insertInf(E1,id,x)) E2

    fun unionCompose(ENV E1, ENV E2)		= Map.unionWith compose (E1,E2)

    and compose(INF(I1,fix1),   INF(I2,fix2))	= INF(I1,fix2)
      | compose(FLD(I1),        FLD(I2))	= FLD(I1)
      | compose(VAR(I1,z1),     VAR(I2,z2))	= VAR(I1,z1)
      | compose(VAL(I1,z1,is1), VAL(I2,z2,is2))	= VAL(I1,z1,
						    composeIdStatus(is1,is2))
      | compose(TY (I1,z1,E1),  TY (I2,z2,E2))	= ( unionCompose(E1,E2)
						  ; TY(I1,z1,E1) )
      | compose(STR(I1,z1,E1),  STR(I2,z2,E2))	= ( unionCompose(E1,E2)
						  ; STR(I1,z1,E1) )
      | compose(SIG(I1,z1,E1),  SIG(I2,z2,E2))	= ( unionCompose(E1,E2)
						  ; SIG(I1,z1,E1) )
      | compose _				= raise Crash.Crash
							"BindEnv.compose"
    and composeIdStatus(V, is)			= is
      | composeIdStatus(is, V)			= is
      | composeIdStatus(is1, is2)		= is2


    fun infEnv E vid				= case lookupInf(E, vid)
						    of NONE        => NONE
						     | SOME(_,inf) => inf

  end
