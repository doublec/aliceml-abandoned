functor IdHashKey(Id: sig  eqtype t  val toString: t -> string  end): HASH_KEY =
  struct
    type t = Id.t
    fun hash id = StringHashKey.hash(Id.toString id)
  end


structure VIdMap   = MakeHashScopedImpMap(IdHashKey(VId))
structure TyConMap = MakeHashScopedImpMap(IdHashKey(TyCon))
structure TyVarMap = MakeHashScopedImpMap(IdHashKey(TyVar))
structure StrIdMap = MakeHashScopedImpMap(IdHashKey(StrId))
structure SigIdMap = MakeHashScopedImpMap(IdHashKey(SigId))
structure FunIdMap = MakeHashScopedImpMap(IdHashKey(FunId))
