structure TranslationPhase :> TRANSLATION_PHASE =
  struct

    structure C = EmptyContext
    structure I = TypedGrammar
    structure O = IntermediateGrammar

    open TypedInfo


  (* Recognize sum type constructors (tags) *)

    fun isTagType t =
	Type.isArrow t andalso isTagType(#2(Type.asArrow t)) orelse
	Type.isAll t   andalso isTagType(#2(Type.asAll t))   orelse
	Type.isExist t andalso isTagType(#2(Type.asExist t)) orelse
	Type.isMu t    andalso isTagType(Type.asMu t)        orelse
	Type.isSum t


  (* Names and labels *)

    fun trName  n		= n
    fun trName'(n as Name.InId)	= n
      | trName'(Name.ExId s)	= Name.ExId("$" ^ s)

    fun trLabel l		= Label.fromName(trName(Label.toName l))
    fun trLabel' l		= Label.fromName(trName'(Label.toName l))


  (* Transformation of type info *)

    fun kindToKind k =
	if Inf.isGround k then
	    Type.STAR
	else if Inf.isGround k then
	    let
		val (p,j,k1) = Inf.asDependent k
	    in
		Type.ARROW(Type.STAR, kindToKind k1)
	    end
	else
	    raise Crash.Crash "TranslationPhase.kindToKind: unknown kind"

    fun infToTyp j =
	if Inf.isTop j then
	    (*UNFINISHED: is this right? *)
	    PreboundType.typ_unit
	else if Inf.isCon j then
	    let
		val (k,p) = Inf.asCon j
	    in
		Type.inCon(kindToKind k, Type.CLOSED, p)
	    end
	else if Inf.isSig j then
	    let
		val s = Inf.asSig j
	    in
		Type.inProd(sigToRow s)
	    end
	else if Inf.isArrow j then
	    let
		val (p,j1,j2) = Inf.asArrow j
	    in
		Type.inArrow(infToTyp j1, infToTyp j2)
	    end
	else if Inf.isLambda j then
	    let
		val (p,j1,j2) = Inf.asLambda j
	    in
		Type.inLambda(Type.var(Type.STAR), infToTyp j2)
	    end
	else if Inf.isApply j then
	    let
		val (j1,p,j2) = Inf.asApply j
	    in
		Type.inApply(infToTyp j1, infToTyp j2)
	    end
	else
	    raise Crash.Crash "TranslationPhase.infToTyp: unknown inf"

    and sigToRow s			= itemsToRow(Inf.items s)

    and itemsToRow  []			= Type.emptyRow()
      | itemsToRow(i::is)		=
	let
	    val r = itemsToRow is
	in
	    if Inf.isValItem i then
		let
		    val (l,t,s,d) = Inf.asValItem i
		in
		    Type.extendRow(trLabel l, [t], r)
		end
	    else if Inf.isModItem i then
		let
		    val (l,j,d) = Inf.asModItem i
		in
		    Type.extendRow(trLabel' l, [infToTyp j], r)
		end
	    else
		r
	end

    fun trInfo {region,inf} = { region = region, typ = infToTyp inf }
    (*UNFINISHED: use punning*)


  (* Create fields for all structures and values in an environment *)

    fun idToField(x' as O.Id(i,_,n), t) =
	    O.Field(i, O.Lab(i,Label.fromName n),
		       O.VarExp(typInfo(#region i,t), O.ShortId(i, x')))

    fun idToDec(O.Id(i as {region=r,...}, z, n), y, t) =
	let
	    val l    = Label.fromName n
	    val tMod = Type.inProd(Type.extendRow(l, [t], Type.unknownRow()))
	    val tSel = Type.inArrow(tMod, t)
	in
	    O.ValDec(i, O.VarPat(typInfo(r,t), O.Id(i,z,n)),
			O.AppExp(typInfo(r,t),
				 O.SelExp(typInfo(r,tSel), O.Lab(i,l)),
				 O.VarExp(typInfo(r,tMod), y)))
	end

    fun idsToRow'(     [],    r) = r
      | idsToRow'((x,t)::ids, r) =
	Type.extendRow(Label.fromName(O.name x), [t], r)

    fun idsToRow ids = idsToRow'(ids, Type.emptyRow())



  (* Literals *)

    fun trLit(I.WordLit w)		= O.WordLit w
      | trLit(I.IntLit n)		= O.IntLit n
      | trLit(I.CharLit c)		= O.CharLit c
      | trLit(I.StringLit s)		= O.StringLit s
(*    | trLit(I.RealLit x)		= O.RealLit x
UNFINISHED: obsolete after bootstrapping:
*)    | trLit(I.RealLit x)		= O.RealLit(LargeReal.toString x)


  (* Identifiers *)

    fun trLab(I.Lab(i,l))		= O.Lab(i, trLabel  l)
    fun trLab'(I.Lab(i,l))		= O.Lab(i, trLabel' l)

    fun trId(I.Id(i,z,n))		= O.Id(i, z, trName n)
    fun trId'(I.Id(i,z,n))		= O.Id(i, z, trName' n)

    fun trLongid'(I.ShortId(i,x))	= O.ShortId(i, trId' x)
      | trLongid'(I.LongId(i,y,a))	= O.LongId(i, trLongid' y, trLab' a)

    fun trLongid(I.ShortId(i,x))	= O.ShortId(i, trId x)
      | trLongid(I.LongId(i,y,a))	= O.LongId(i, trLongid' y, trLab a)

    fun trLabLongid(I.ShortId(i,x))	= O.Lab(i,
					       Label.fromName(trName(I.name x)))
      | trLabLongid(I.LongId(i,y,a))	= trLabLongid y


  (* Extract bound ids from declarations. *)

    fun idsId trId xs' x t =
	case trId x
	  of x' as O.Id(_,_,Name.ExId s') => StringMap.insert(xs', s', (x',t))
	   | _                            => ()

    fun idsDec xs' (I.ValDec(i,p,e))	= idsPat xs' p
      | idsDec xs' (I.ConDec(i,x,t,k))	= idsId trId xs' x (#typ(I.infoTyp t))
      | idsDec xs' (I.TypDec(i,x,t))	= ()
      | idsDec xs' (I.ModDec(i,x,m))	= idsId trId' xs' x
						(infToTyp(#inf(I.infoMod m)))
      | idsDec xs' (I.InfDec(i,x,j))	= ()
      | idsDec xs' (I.FixDec(i,x,q))	= ()
      | idsDec xs' (I.VarDec(i,x,d))	= idsDec xs' d
      | idsDec xs' (I.RecDec(i,ds))	= idsDecs xs' ds
      | idsDec xs' (I.LocalDec(i,ds))	= ()
    and idsDecs xs'			= List.app(idsDec xs')

    and idsPat xs' (I.JokPat(i))	= ()
      | idsPat xs' (I.LitPat(i,l))	= ()
      | idsPat xs' (I.VarPat(i,x))	= idsId trId xs' x (#typ i)
      | idsPat xs' (I.TagPat(i,l,k))	= ()
      | idsPat xs' (I.ConPat(i,y,k))	= ()
      | idsPat xs' (I.RefPat(i))	= ()
      | idsPat xs' (I.TupPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (I.ProdPat(i,r))	= idsRow xs' r
      | idsPat xs' (I.VecPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (I.AppPat(i,p1,p2))	= ( idsPat xs' p1 ; idsPat xs' p2 )
      | idsPat xs' (I.AsPat(i,p1,p2))	= ( idsPat xs' p1 ; idsPat xs' p2 )
      | idsPat xs' (I.AltPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (I.NegPat(i,p))	= idsPat xs' p
      | idsPat xs' (I.GuardPat(i,p,e))	= idsPat xs' p
      | idsPat xs' (I.AnnPat(i,p,t))	= idsPat xs' p
      | idsPat xs' (I.WithPat(i,p,ds))	= ( idsPat xs' p ; idsDecs xs' ds )
    and idsPats xs'			= List.app(idsPat xs')

    and idsRow    xs' (I.Row(i,fs,_))   = idsFields xs' fs
    and idsField  xs' (I.Field(i,a,z))  = idsPats xs' z
    and idsFields xs'			= List.app(idsField xs')

    fun ids ds				= let val xs' = StringMap.new() in
					      idsDecs xs' ds ;
					      StringMap.fold op:: [] xs'
					  end


  (* Expressions *)

    fun trExp(I.LitExp(i,l))		= O.LitExp(i, trLit l)
      | trExp(I.PrimExp(i,s,t))		= O.PrimExp(i, s)
      | trExp(I.VarExp(i,y))		= O.VarExp(i, trLongid y)
      | trExp(I.TagExp(i,a,k))		= O.TagExp(i, trLab a, k>1)
      | trExp(I.ConExp(i,y,k))		= if isTagType(#typ i) then
					      O.TagExp(i, trLabLongid y, k>1)
					  else
					      O.ConExp(i, trLongid y, k>1)
      | trExp(I.RefExp(i))		= O.RefExp(i)
      | trExp(I.TupExp(i,es))		= O.TupExp(i, trExps es)
      | trExp(I.ProdExp(i,r))		= O.RowExp(i, trExpRow r)
      | trExp(I.SelExp(i,a))		= O.SelExp(i, trLab a)
      | trExp(I.VecExp(i,es))		= O.VecExp(i, trExps es)
      | trExp(I.FunExp(i,ms))		= O.FunExp(i, trMatchs ms)
      | trExp(I.AppExp(i,e1,e2))	= O.AppExp(i, trExp e1, trExp e2)
      | trExp(I.CompExp(i,e1,e2))	= O.AdjExp(i, trExp e1, trExp e2)
      | trExp(I.AndExp(i,e1,e2))	= O.AndExp(i, trExp e1, trExp e2)
      | trExp(I.OrExp(i,e1,e2))		= O.OrExp(i, trExp e1, trExp e2)
      | trExp(I.IfExp(i,e1,e2,e3))	= O.IfExp(i, trExp e1, trExp e2,
							       trExp e3)
      | trExp(I.WhileExp(i,e1,e2))	= O.WhileExp(i, trExp e1, trExp e2)
      | trExp(I.SeqExp(i,es))		= O.SeqExp(i, trExps es)
      | trExp(I.CaseExp(i,e,ms))	= O.CaseExp(i, trExp e, trMatchs ms)
      | trExp(I.RaiseExp(i,e))		= O.RaiseExp(i, trExp e)
      | trExp(I.HandleExp(i,e,ms))	= O.HandleExp(i, trExp e, trMatchs ms)
      | trExp(I.AnnExp(i,e,t))		= trExp e
      | trExp(I.LetExp(i,ds,e))		= O.LetExp(i, trDecs ds, trExp e)
      | trExp(I.PackExp(i,m))		= trMod m

    and trExps es			= List.map trExp es

    and trExpRow(I.Row(i,fs,_))		= trExpFields fs
    and trExpField(I.Field(i,a,e))	= O.Field(i, trLab a, List.hd(trExps e))
    and trExpFields fs			= List.map trExpField fs


  (* Matches and Patterns *)

    and trMatch(I.Match(i,p,e))		= O.Match(i, trPat p, trExp e)
    and trMatchs ms			= List.map trMatch ms

    and trPat(I.JokPat(i))		= O.WildPat(i)
      | trPat(I.LitPat(i,l))		= O.LitPat(i, trLit l)
      | trPat(I.VarPat(i,x))		= O.VarPat(i, trId x)
      | trPat(I.TagPat(i,a,k))		= O.TagPat(i, trLab a, k>1)
      | trPat(I.ConPat(i,y,k))		= O.ConPat(i, trLongid y, k>1)
      | trPat(I.RefPat(i))		= O.RefPat(i)
      | trPat(I.TupPat(i,ps))		= O.TupPat(i, trPats ps)
      | trPat(I.ProdPat(i,r))		= O.RowPat(i, trPatRow r)
      | trPat(I.VecPat(i,ps))		= O.VecPat(i, trPats ps)
      | trPat(I.AppPat(i,p1,p2))	= O.AppPat(i, trPat p1, trPat p2)
      | trPat(I.AsPat(i,p1,p2))		= O.AsPat(i, trPat p1, trPat p2)
      | trPat(I.AltPat(i,ps))		= O.AltPat(i, trPats ps)
      | trPat(I.NegPat(i,p))		= O.NegPat(i, trPat p)
      | trPat(I.GuardPat(i,p,e))	= O.GuardPat(i, trPat p, trExp e)
      | trPat(I.AnnPat(i,p,t))		= trPat p
      | trPat(I.WithPat(i,p,ds))	= O.WithPat(i, trPat p,trDecs ds)

    and trPats ps			= List.map trPat ps

    and trPatRow(I.Row(i,fs,b))		= trPatFields fs
    and trPatField(I.Field(i,a,p))	= O.Field(i, trLab a, List.hd(trPats p))
    and trPatFields fs			= List.map trPatField fs


  (* Modules *)

    and trMod(I.PrimMod(i,s,j))		= O.PrimExp(trInfo i, s)
      | trMod(I.VarMod(i,x))		= let val x' as O.Id(i',_,_)= trId' x in
					      O.VarExp(trInfo i,
						       O.ShortId(i', x'))
					  end
      | trMod(I.StrMod(i,ds))		= let val i'   = trInfo i
					      val ids' = ids ds
					      val fs'  = List.map idToField ids'
					      val ds'  = trDecs ds in
					      O.LetExp(i',ds', O.RowExp(i',fs'))
					  end
      | trMod(I.SelMod(i,m,a))		= let val i' = trInfo i
					      val r  = #region i'
					      val e' = trMod m
					      val t1 = #typ(O.infoExp e')
					      val t2 = #typ i'
					      val t  = Type.inArrow(t1,t2) in
					      O.AppExp(i',O.SelExp(typInfo(r,t),
							           trLab' a),e')
					  end
      | trMod(I.FunMod(i,x,j,m))	= let val i' = trInfo i
					      val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e')
					      val p' = O.VarPat(typInfo(r,t),
								trId' x)
					      val m' = O.Match(nonInfo r,p',e')
					      in O.FunExp(i', [m'])
					  end
      | trMod(I.AppMod(i,m1,m2))	= O.AppExp(trInfo i, trMod m1, trMod m2)
      | trMod(I.AnnMod(i,m,j))		= O.UpExp(trInfo i, trMod m)
      | trMod(I.UpMod(i,m,j))		= O.UpExp(trInfo i, trMod m)
      | trMod(I.LetMod(i,ds,m))		= O.LetExp(trInfo i, trDecs ds, trMod m)
      | trMod(I.UnpackMod(i,e,j))	= trExp e


  (* Declarations *)

    and trDec(I.ValDec(i,p,e), ds')	= O.ValDec(i, trPat p, trExp e) :: ds'
      | trDec(I.ConDec(i,x,t,k), ds')	= let val i' = I.infoTyp t in
					      case t of I.SingTyp(_,y) =>
						trEqCon(i',x,trLongid y,ds')
					      | _ => if isTagType(#typ i') then
						trTagCon(i',x,k,ds')
					      else
						trNewCon(i',x,k,ds')
					  end
      | trDec(I.TypDec(i,x,t), ds')	= ds'
      | trDec(I.ModDec(i,x,m), ds')	= let val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e') in
					      O.ValDec(i, O.VarPat(typInfo(r,t),
							trId' x), e') :: ds'
					  end
      | trDec(I.InfDec(i,x,j), ds')	= ds'
      | trDec(I.FixDec(i,x,q), ds')	= ds'
      | trDec(I.VarDec(i,x,d), ds')	= trDec(d, ds')
      | trDec(I.RecDec(i,ds), ds')	= O.RecDec(i, trDecs ds) :: ds'
      | trDec(I.LocalDec(i,ds), ds')	= trDecs'(ds, ds')

    and trDecs ds			= trDecs'(ds, [])
    and trDecs'(ds, ds')		= List.foldr trDec ds' ds

    and trEqCon(i,x,y',ds')		= O.ValDec(nonInfo(#region i),
						   O.VarPat(i,trId x),
						   O.VarExp(i,y')) :: ds'
    and trNewCon(i,x,k,ds')		= O.ValDec(nonInfo(#region i),
						   O.VarPat(i,trId x),
						   O.NewExp(i,k>1)) :: ds'
    and trTagCon(i,x,k,ds')		= O.ValDec(nonInfo(#region i),
						   O.VarPat(i,trId x),
						   O.TagExp(i,trLab(I.idToLab x)
							     ,k>1)) :: ds'


  (* Imports and annotations *)

    fun trAnns'(a_s, ds') = List.foldr trAnn ([],ds') a_s

    and trAnn(I.ImpAnn(i,is,u),(xsus',ds')) =
	let
	    val i'   = nonInfo(#region i)
	    val x'   = O.Id(i', Stamp.new(), Name.InId)
	    val y'   = O.ShortId(i', x')
	    val ds'' = trImps(is, y', ds')
	in
	    ( (x',#sign i,u)::xsus', ds'' )
	end

    and trImps(is, y, ds')		= List.foldr (trImp y) ds' is

    and trImp y (I.ValImp(i,x,d),ds')	= idToDec(trId x, y, #typ(I.infoDesc d))
					  :: ds'
      | trImp y (I.ConImp(i,x,d,k),ds')	= idToDec(trId x, y, #typ(I.infoDesc d))
					  :: ds'
      | trImp y (I.TypImp(i,x,d),ds')	= ds'
      | trImp y (I.ModImp(i,x,d),ds')	= idToDec(trId' x, y,
						  infToTyp(#inf(I.infoDesc d)))
					  :: ds'
      | trImp y (I.InfImp(i,x,d),ds')	= ds'
      | trImp y (I.FixImp(i,x,d),ds')	= ds'
      | trImp y (I.RecImp(i,is), ds')	= trImps(is, y, ds')


  (* Components *)

    fun trComp(I.Comp(i,a_s,ds)) =
	let
	    val  ids'       = ids ds
	    val (xsus',ds') = trAnns'(a_s, trDecs ds)
	    val  fs'        = List.map idToField ids'
	    val  t          = Type.inProd(idsToRow ids')
	    val  i'         = typInfo(#region i,t)
	    val  exp'       = O.LetExp(i', ds', O.RowExp(i', fs'))
	in
	    ( xsus', (exp', #sign i) )
	end

    fun translate() = trComp

  end
