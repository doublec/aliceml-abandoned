structure BindEnv :> BIND_ENV =
  struct

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
    exception CollisionVal = VIdMap.Collision
    exception CollisionTy  = TyConMap.Collision
    exception CollisionVar = TyVarMap.Collision
    exception CollisionStr = StrIdMap.Collision
    exception CollisionSig = SigIdMap.Collision
    exception CollisionFun = FunIdMap.Collision


    fun new()				= ENV { IE = VIdMap.new()
					      , VE = VIdMap.new()
					      , TE = TyConMap.new()
					      , UE = TyVarMap.new()
					      , SE = StrIdMap.new()
					      , GE = SigIdMap.new()
					      , FE = FunIdMap.new()
					      }

    fun copy(ENV{UE,IE,VE,TE,SE,GE,FE})	= ENV { IE = VIdMap.copy IE
					      , VE = VIdMap.copy VE
					      , TE = TyConMap.copy TE
					      , UE = TyVarMap.copy UE
					      , SE = StrIdMap.copy SE
					      , GE = SigIdMap.copy GE
					      , FE = FunIdMap.copy FE
					      }

    fun copyScope(ENV{UE,IE,VE,TE,SE,GE,FE}) =
					  ENV { IE = VIdMap.copyScope IE
					      , VE = VIdMap.copyScope VE
					      , TE = TyConMap.copyScope TE
					      , UE = TyVarMap.copyScope UE
					      , SE = StrIdMap.copyScope SE
					      , GE = SigIdMap.copyScope GE
					      , FE = FunIdMap.copyScope FE
					      }

    fun insertScope(ENV{UE,IE,VE,TE,SE,GE,FE}) =
					  ( VIdMap.insertScope IE
					  ; VIdMap.insertScope VE
					  ; TyConMap.insertScope TE
					  ; TyVarMap.insertScope UE
					  ; StrIdMap.insertScope SE
					  ; SigIdMap.insertScope GE
					  ; FunIdMap.insertScope FE
					  )

    fun deleteScope(ENV{UE,IE,VE,TE,SE,GE,FE}) =
					  ( VIdMap.deleteScope IE
					  ; VIdMap.deleteScope VE
					  ; TyConMap.deleteScope TE
					  ; TyVarMap.deleteScope UE
					  ; StrIdMap.deleteScope SE
					  ; SigIdMap.deleteScope GE
					  ; FunIdMap.deleteScope FE
					  )

    fun delete2ndScope(ENV{UE,IE,VE,TE,SE,GE,FE}) =
					  ( VIdMap.delete2ndScope IE
					  ; VIdMap.delete2ndScope VE
					  ; TyConMap.delete2ndScope TE
					  ; TyVarMap.delete2ndScope UE
					  ; StrIdMap.delete2ndScope SE
					  ; SigIdMap.delete2ndScope GE
					  ; FunIdMap.delete2ndScope FE
					  )

    fun mergeScope(ENV{UE,IE,VE,TE,SE,GE,FE}) =
					  ( VIdMap.mergeScope IE
					  ; VIdMap.mergeScope VE
					  ; TyConMap.mergeScope TE
					  ; TyVarMap.mergeScope UE
					  ; StrIdMap.mergeScope SE
					  ; SigIdMap.mergeScope GE
					  ; FunIdMap.mergeScope FE
					  )


    fun union(ENV{UE,IE,VE,TE,SE,GE,FE},
	      ENV{UE=UE',IE=IE',VE=VE',TE=TE',SE=SE',GE=GE',FE=FE'}) =
					  ( VIdMap.union(IE,IE')
					  ; VIdMap.union(VE,VE')
					  ; TyConMap.union(TE,TE')
					  ; TyVarMap.union(UE,UE')
					  ; StrIdMap.union(SE,SE')
					  ; SigIdMap.union(GE,GE')
					  ; FunIdMap.union(FE,FE')
					  )

    fun unionDisjoint(ENV{UE,IE,VE,TE,SE,GE,FE},
		      ENV{UE=UE',IE=IE',VE=VE',TE=TE',SE=SE',GE=GE',FE=FE'}) =
	( FunIdMap.unionDisjoint(FE,FE')
	; SigIdMap.unionDisjoint(GE,GE')
	; StrIdMap.unionDisjoint(SE,SE')
	; TyConMap.unionDisjoint(TE,TE')
	; TyVarMap.unionDisjoint(UE,UE')
	; VIdMap.unionDisjoint(VE,VE')
	; VIdMap.unionDisjoint(IE,IE')
	  handle VIdMap.Collision vid => raise CollisionInf vid
	)


    fun insertInf(ENV{IE,...}, id, x)	= VIdMap.insert(IE, id, x)
    fun insertVal(ENV{VE,...}, id, x)	= VIdMap.insert(VE, id, x)
    fun insertTy (ENV{TE,...}, id, x)	= TyConMap.insert(TE, id, x)
    fun insertVar(ENV{UE,...}, id, x)	= TyVarMap.insert(UE, id, x)
    fun insertStr(ENV{SE,...}, id, x)	= StrIdMap.insert(SE, id, x)
    fun insertSig(ENV{GE,...}, id, x)	= SigIdMap.insert(GE, id, x)
    fun insertFun(ENV{FE,...}, id, x)	= FunIdMap.insert(FE, id, x)

    fun insertDisjointInf(ENV{IE,...}, id, x) =
	VIdMap.insertDisjoint(IE, id, x)
	handle VIdMap.Collision vid => raise CollisionInf vid
    fun insertDisjointVal(ENV{VE,...}, id, x) =
	VIdMap.insertDisjoint(VE, id, x)
    fun insertDisjointTy (ENV{TE,...}, id, x) =
	TyConMap.insertDisjoint(TE, id, x)
    fun insertDisjointVar(ENV{UE,...}, id, x) =
	TyVarMap.insertDisjoint(UE, id, x)
    fun insertDisjointStr(ENV{SE,...}, id, x) =
	StrIdMap.insertDisjoint(SE, id, x)
    fun insertDisjointSig(ENV{GE,...}, id, x) =
	SigIdMap.insertDisjoint(GE, id, x)
    fun insertDisjointFun(ENV{FE,...}, id, x) =
	FunIdMap.insertDisjoint(FE, id, x)

    fun lookupInf(ENV{IE,...}, id)	= VIdMap.lookup(IE, id)
    fun lookupVal(ENV{VE,...}, id)	= VIdMap.lookup(VE, id)
    fun lookupTy (ENV{TE,...}, id)	= TyConMap.lookup(TE, id)
    fun lookupVar(ENV{UE,...}, id)	= TyVarMap.lookup(UE, id)
    fun lookupStr(ENV{SE,...}, id)	= StrIdMap.lookup(SE, id)
    fun lookupSig(ENV{GE,...}, id)	= SigIdMap.lookup(GE, id)
    fun lookupFun(ENV{FE,...}, id)	= FunIdMap.lookup(FE, id)

    fun lookupScopeInf(ENV{IE,...}, id)	= VIdMap.lookupScope(IE, id)
    fun lookupScopeVal(ENV{VE,...}, id)	= VIdMap.lookupScope(VE, id)
    fun lookupScopeTy (ENV{TE,...}, id)	= TyConMap.lookupScope(TE, id)
    fun lookupScopeVar(ENV{UE,...}, id) = TyVarMap.lookupScope(UE, id)
    fun lookupScopeStr(ENV{SE,...}, id)	= StrIdMap.lookupScope(SE, id)
    fun lookupScopeSig(ENV{GE,...}, id)	= SigIdMap.lookupScope(GE, id)
    fun lookupScopeFun(ENV{FE,...}, id)	= FunIdMap.lookupScope(FE, id)


    fun appVals f (ENV{VE,...})		= VIdMap.appi   f VE
    fun appTys  f (ENV{TE,...})		= TyConMap.appi f TE
    fun appVars f (ENV{UE,...})		= TyVarMap.appi f UE
    fun appStrs f (ENV{SE,...})		= StrIdMap.appi f SE
    fun appSigs f (ENV{GE,...})		= SigIdMap.appi f GE
    fun appFuns f (ENV{FE,...})		= FunIdMap.appi f FE

    fun foldF f ((x,a),b)		= f(x,a,b)

    fun foldVals f a (ENV{VE,...})	= VIdMap.foldi   (foldF f) a VE
    fun foldTys  f a (ENV{TE,...})	= TyConMap.foldi (foldF f) a TE
    fun foldVars f a (ENV{UE,...})	= TyVarMap.foldi (foldF f) a UE
    fun foldStrs f a (ENV{SE,...})	= StrIdMap.foldi (foldF f) a SE
    fun foldSigs f a (ENV{GE,...})	= SigIdMap.foldi (foldF f) a GE
    fun foldFuns f a (ENV{FE,...})	= FunIdMap.foldi (foldF f) a FE

  end
