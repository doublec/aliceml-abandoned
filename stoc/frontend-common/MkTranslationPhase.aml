(*
 * UNFINISHED: maintain sharing on transformed interfaces.
 *)

functor MakeTranslationPhase(structure Switches: SWITCHES):> TRANSLATION_PHASE =
  struct

    structure C = EmptyContext
    structure I = TypedGrammar
    structure O = IntermediateGrammar

    open TypedInfo


  (* Recognize sum type constructors (tags) *)

    fun isTagType t =
	Type.isArrow t  andalso isTagType(#2(Type.asArrow t))  orelse
	Type.isAll t    andalso isTagType(#2(Type.asAll t))    orelse
	Type.isExist t  andalso isTagType(#2(Type.asExist t))  orelse
	Type.isLambda t andalso isTagType(#2(Type.asLambda t)) orelse
	Type.isApply t  andalso isTagType(#1(Type.asApply t))  orelse
	Type.isMu t     andalso isTagType(Type.asMu t)         orelse
	Type.isSum t


  (* Auxiliaries for reflection *)

    open PervasiveType
    open LabelReflection
    open PathReflection
    open TypeReflection

    val typ_typtyp		= Type.inTuple #[typ_typ, typ_typ]
    val typ_vartyp		= Type.inTuple #[typ_var, typ_typ]
    val typ_typVec		= Type.inApply(typ_vec, typ_typ)
    val typ_kindkind		= Type.inTuple #[typ_kind, typ_kind]
    val typ_kindToVar		= Type.inArrow(typ_kind, typ_var)
    val typ_kindkindToKind	= Type.inArrow(typ_kindkind, typ_kind)

    (*UNFINISHED: how exactly access pervasives? *)
    val info_nowhere		= nonInfo(Source.nowhere)
    val info_nowhere'		= typInfo(Source.nowhere, NONE : typ option)
    val info_pervasive		= info_nowhere' (*UNFINISHED*)
    val id_pervasive		= O.Id(info_nowhere, stamp_pervasive, Name.InId)
    val longid_pervasive	= O.ShortId(info_pervasive, id_pervasive)
    fun longid_pervasiveDot i n	= O.LongId(i, longid_pervasive,
    					  O.Lab(info_nowhere, Label.fromName n))

    val longid_ref		= longid_pervasiveDot info_nowhere' name_ref
    val info_label		= info_nowhere' (*UNFINISHED*)
    val longid_label		= longid_pervasiveDot info_label modname_label
    val info_path		= info_nowhere' (*UNFINISHED*)
    val longid_path		= longid_pervasiveDot info_path modname_path
    val info_type		= info_nowhere' (*UNFINISHED*)
    val longid_type		= longid_pervasiveDot info_type modname_type
    fun longid_labelDot l'	= O.LongId(info_nowhere', longid_label, l')
    fun longid_pathDot l'	= O.LongId(info_nowhere', longid_path, l')
    fun longid_typeDot l'	= O.LongId(info_nowhere', longid_type, l')

    fun labOp(a,e')		= let val r   = #region(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, a)
				      val t   = #typ(O.infoExp e')
				      val t1  = Type.inArrow(t, typ_lab)
				      val e1' = O.VarExp(typInfo(r,t1),
							 longid_labelDot l')
				  in
				      O.AppExp(typInfo(r, typ_lab), e1', e')
				  end
    fun pathOp(a,e')		= let val r   = #region(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, a)
				      val t   = #typ(O.infoExp e')
				      val t1  = Type.inArrow(t, typ_path)
				      val e1' = O.VarExp(typInfo(r,t1),
							 longid_pathDot l')
				  in
				      O.AppExp(typInfo(r, typ_path), e1', e')
				  end
    fun typOp(a,e')		= let val r   = #region(O.infoExp e')
				      val t   = #typ(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, a)
				      val t1  = Type.inArrow(t, typ_typ)
				      val e1' = O.VarExp(typInfo(r,t1),
							 longid_typeDot l')
				  in
				      O.AppExp(typInfo(r, typ_typ), e1', e')
				  end
    fun varOp e'		= let val r   = #region(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, lab_var)
				      val t1  = typ_kindToVar
				      val e1' = O.VarExp(typInfo(r,t1),
							 longid_typeDot l')
				  in
				      O.AppExp(typInfo(r, typ_var), e1', e')
				  end
    fun rowOp(a,e')		= let val r   = #region(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, a)
				      val t   = #typ(O.infoExp e')
				      val t1  = Type.inArrow(t, typ_row)
				      val e1' = O.VarExp(typInfo(r,t1),
							 longid_typeDot l')
				  in
				      O.AppExp(typInfo(r, typ_row), e1', e')
				  end

    fun trKind r (Type.STAR) =
	    O.TagExp(typInfo(r,typ_kind),
		     O.Lab(nonInfo r, lab_star), false)
      | trKind r (Type.ARROW(k1,k2)) =
	    O.AppExp(typInfo(r,typ_kind),
		     O.TagExp(typInfo(r,typ_kindkindToKind),
			      O.Lab(nonInfo r, lab_arrow), true),
		     O.TupExp(typInfo(r,typ_kindkind),
			      [trKind r k1, trKind r k2]))



  (* Names and labels *)

    fun trName' f (Name.ExId s)	= Name.ExId(f s)
      | trName' f  n		= n

    fun trName  n		= n
    val trTypName		= trName'(fn s => "$" ^ s)
    val trModName		= trName'(fn s => s ^ "$")
    val trInfName		= trName'(fn s => "$" ^ s ^ "$")

    fun trLabel a		= Label.fromName(trName(Label.toName a))
    fun trTypLabel a		= Label.fromName(trTypName(Label.toName a))
    fun trModLabel a		= Label.fromName(trModName(Label.toName a))
    fun trInfLabel a		= Label.fromName(trInfName(Label.toName a))


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
	    Type.inVar(Type.var Type.STAR)
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
		    Type.extendRow(trLabel l, #[t], r)
		end
	    else if Inf.isTypItem i then
		let
		    val (l,k,s,d) = Inf.asTypItem i
		in
		    Type.extendRow(trTypLabel l, #[typ_typ], r)
		end
	    else if Inf.isModItem i then
		let
		    val (l,j,d) = Inf.asModItem i
		in
		    Type.extendRow(trModLabel l, #[infToTyp j], r)
		end
	    else (*UNFINISHED: interfaces*)
		r
	end

    (*UNFINISHED: use punning*)
    fun trLongidInfo {region}        = {region=region, typ=NONE}
    fun trModlongidInfo {region,inf} = {region=region, typ=SOME(infToTyp inf)}
    fun trInfInfo {region,inf}       = {region=region, typ=infToTyp inf}


  (* Signature coercions *)

    (*
     * We use the following transformation rules:
     *
     *	[x : sig item1* end :> sig item2* end] =
     *	   struct [x . item1 :> item2]* end
     *	[x : fct(x1:j11)->j12 :> fct(x2:j21)->j22] =
     *	   fct(y:j21) => let z = x([y : j21 :> j11]) in [z : j12 :> j22] end
     *	[x : j1 :> j2] = x
     *
     *	[x . val y:t1 :> val y:t2] = val y = lazy x.y
     *	[x . constructor y:t1 :> val y:t2] = val y = lazy (x.y)
     *	[x . structure y:j1 :> structure y:j2] =
     *	   structure y = lazy [x.y : j1 :> j2]
     *
     * Moreover, we apply the optimization that - if the transformation is the
     * identity function - the coercion is a no-op.
     *)

    fun upInf(abs, x,j1,j2, r,t1,t2) =
	if Inf.isSig j2 andalso Inf.isSig j1 then
	    let
		val s1 = Inf.asSig j1
		val s2 = Inf.asSig j2
		val b  = Inf.size s1 <> Inf.size s2
	    in
		case upItems(abs, r, x, Inf.items s2, s1, b, [])
		  of NONE        => NONE
		   | SOME fields => SOME(O.ProdExp(typInfo(r,t2), fields))
	    end
	else if Inf.isArrow j2 andalso Inf.isArrow j1 then
	    let
		val (p1,j11,j12) = Inf.asArrow j1
		val (p2,j21,j22) = Inf.asArrow j2
		val    (t11,t12) = Type.asArrow t1
		val    (t21,t22) = Type.asArrow t2

		val i' = nonInfo r
		val y' = O.Id(i', Stamp.new(), Name.InId)
		val z' = O.Id(i', Stamp.new(), Name.InId)
		val y  = O.ShortId(typInfo(r, SOME t21), y')
		val z  = O.ShortId(typInfo(r, SOME t12), z')
	    in
		case (upInf(false, y,j21,j11, r,t21,t11),
		      upInf(abs, z,j12,j22, r,t12,t22))
		  of (SOME exp1, SOME exp2) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val zpat   = O.VarPat(typInfo(r,t12), z')
			val appexp = O.AppExp(typInfo(r,t12), xexp, exp1)
			val dec    = O.ValDec(i', zpat, appexp)
			val letexp = O.LetExp(typInfo(r,t22), [dec], exp2)
		     in
			SOME(O.FunExp(typInfo(r,t2), [O.Match(i',ypat,letexp)]))
		     end
		   | (NONE, SOME exp2) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val yexp   = O.VarExp(typInfo(r,t21), y)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val zpat   = O.VarPat(typInfo(r,t12), z')
			val appexp = O.AppExp(typInfo(r,t12), xexp, yexp)
			val dec    = O.ValDec(i', zpat, appexp)
			val letexp = O.LetExp(typInfo(r,t22), [dec], exp2)
		     in
			SOME(O.FunExp(typInfo(r,t2), [O.Match(i',ypat,letexp)]))
		     end
		   | (SOME exp1, NONE) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val appexp = O.AppExp(typInfo(r,t12), xexp, exp1)
		     in
			SOME(O.FunExp(typInfo(r,t2), [O.Match(i',ypat,appexp)]))
		     end
		   | (NONE, NONE) => NONE
	    end
	else
	    NONE

    and upItems(abs, r, x, [], s1, false, fields) = NONE
      | upItems(abs, r, x, [], s1, true,  fields) = SOME fields
      | upItems(abs, r, x, item::items, s1, b, fields) =
	if Inf.isValItem item then
	    let
		val (a,t,w2,_) = Inf.asValItem item
		val      w1    = Inf.lookupValSort(s1,a)
		val      i     = typInfo(r,t)
		val      i'    = nonInfo r
		val      l'    = O.Lab(i', trLabel a)
		val      y     = O.LongId(typInfo(r,NONE), x, l')
		val  (exp,b')  = if w1 = w2 then
				     (O.VarExp(i,y), b)
				 else let
				     val n = case w1
					       of Inf.CONSTRUCTOR k => k > 0
						| Inf.VALUE =>
						  raise Crash.Crash
						    "TranslationPhase.upItems: \
						    \funny arity (hoho)"
				 in
				     (if isTagType t then
					 O.TagExp(i,l',n)
				      else
					 O.ConExp(i,y,n)
				     , true)
				 end
		val  exp' = O.LazyExp(i,exp)
	    in
		upItems(abs, r, x, items, s1, b', O.Field(i',l',exp')::fields)
	    end
	else if Inf.isTypItem item then
	    let
		val (a,k,w,d) = Inf.asTypItem item
		val    i      = typInfo(r, typ_typ)
		val    i'     = nonInfo r
		val    l'     = O.Lab(i', trTypLabel a)
		val    y      = O.LongId(typInfo(r,NONE), x, l')
		val (exp,b')  =
		    if not abs orelse Option.isSome d then
			(O.VarExp(i,y), b)
		    else let
			val l'  = O.Lab(i', case w of Type.CLOSED => lab_closed
						    | Type.OPEN   => lab_open)
			val e1' = trKind r k
			val e2' = O.TagExp(typInfo(r,typ_sort), l', false)
			val lit = O.StringLit(String.toWide(Label.toString a))
			val e3' = pathOp(lab_fromLab,
				   labOp(lab_fromString,
					 O.LitExp(typInfo(r,typ_string), lit)))
			val e'  = O.TupExp(typInfo(r,typ_con),[e1',e2',e2'])
		    in
			(typOp(lab_inCon, e'), true)
		    end
		val exp' = O.LazyExp(i,exp)
	    in
		upItems(abs, r, x, items, s1, b', O.Field(i',l',exp')::fields)
	    end
	else if Inf.isModItem item then
	    let
		val (a,j2,_) = Inf.asModItem item
		val    j1    = Inf.lookupMod(s1,a)
		val    t1    = infToTyp j1
		val    t2    = infToTyp j2
		val    i     = typInfo(r,t2)
		val    i'    = nonInfo r
		val    l'    = O.Lab(i', trModLabel a)
		val    y     = O.LongId(typInfo(r, SOME t2), x, l')
		val (exp,b') = case upInf(abs, y,j1,j2, r,t1,t2)
				 of NONE     => (O.VarExp(i,y), b)
				  | SOME exp => (exp, true)
		val  exp'    = O.LazyExp(i,exp)
	    in
		upItems(abs, r, x, items, s1, b', O.Field(i',l',exp')::fields)
	    end
	else (*UNFINISHED: interfaces*)
	    upItems(abs, r, x, items, s1, b, fields)



  (* Create fields for all structures and values in an environment *)

    fun idToField(x' as O.Id(i,_,n), t) =
	let
	    val r = #region i
	in
	    O.Field(i, O.Lab(i, Label.fromName n),
		       O.VarExp(typInfo(r,t), O.ShortId(typInfo(r,SOME t), x')))
	end

    fun idToDec(O.Id(i as {region=r,...}, z, n), y, tMod, tField) =
	let
	    val tSel = Type.inArrow(tMod, tField)
	in
	    O.ValDec(i, O.VarPat(typInfo(r,tField), O.Id(i,z,n)),
			O.AppExp(typInfo(r,tField),
				 O.SelExp(typInfo(r,tSel),
					  O.Lab(i, Label.fromName n)),
				 O.VarExp(typInfo(r,tMod), y)))
	end

    fun idsToRow'(     [],    r) = r
      | idsToRow'((x,t)::ids, r) =
	Type.extendRow(Label.fromName(O.name x), #[t], r)

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

    fun trLab(I.Lab(i,a))		= O.Lab(i, trLabel  a)
    fun trTyplab(I.Lab(i,a))		= O.Lab(i, trTypLabel a)
    fun trModlab(I.Lab(i,a))		= O.Lab(i, trModLabel a)
    fun trInflab(I.Lab(i,a))		= O.Lab(i, trInfLabel a)

    fun trId(I.Id(i,z,n))		= O.Id(i, z, trName n)
    fun trTypid(I.Id(i,z,n))		= O.Id(i, z, trTypName n)
    fun trModid(I.Id(i,z,n))		= O.Id(i, z, trModName n)
    fun trInfid(I.Id(i,z,n))		= O.Id(i, z, trInfName n)

    fun trModlongid(I.ShortId(i,x))	= O.ShortId(trModlongidInfo i,trModid x)
      | trModlongid(I.LongId(i,y,l))	= O.LongId(trModlongidInfo i,
						   trModlongid y, trModlab l)

    fun trLongid(I.ShortId(i,x))	= O.ShortId(trLongidInfo i, trId x)
      | trLongid(I.LongId(i,y,l))	= O.LongId(trLongidInfo i,
						   trModlongid y, trLab l)

    fun trTyplongid(I.ShortId(i,x))	= O.ShortId(trLongidInfo i, trTypid x)
      | trTyplongid(I.LongId(i,y,l))	= O.LongId(trLongidInfo i,
						   trModlongid y, trTyplab l)

    fun trInflongid(I.ShortId(i,x))	= O.ShortId(trLongidInfo i, trInfid x)
      | trInflongid(I.LongId(i,y,l))	= O.LongId(trLongidInfo i,
						   trModlongid y, trInflab l)

    fun trLabLongid(I.ShortId(i,x))	= O.Lab(i,
					       Label.fromName(trName(I.name x)))
      | trLabLongid(I.LongId(i,y,l))	= trLab l


  (* Extract bound ids from declarations. *)

    fun idsId trId xs' x t =
	case trId x
	  of x' as O.Id(_,_,Name.ExId s') => StringMap.insert(xs', s', (x',t))
	   | _                            => ()

    fun idsDec xs' (I.ValDec(i,p,e))	= idsPat xs' p
      | idsDec xs' (I.ConDec(i,x,t,k))	= idsId trId xs' x (#typ(I.infoTyp t))
      | idsDec xs' (I.TypDec(i,x,t))	= idsId trTypid xs' x (typ_typ)
      | idsDec xs' (I.ModDec(i,x,m))	= idsId trModid xs' x
						(infToTyp(#inf(I.infoMod m)))
      | idsDec xs' (I.InfDec(i,x,j))	= () (*UNFINISHED*)
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
    and idsField  xs' (I.Field(i,l,z))  = idsPats xs' z
    and idsFields xs'			= List.app(idsField xs')

    fun ids ds				= let val xs' = StringMap.new() in
					      idsDecs xs' ds ;
					      StringMap.fold op:: [] xs'
					  end


  (* Expressions *)

    fun trExp(I.LitExp(i,l))		= O.LitExp(i, trLit l)
      | trExp(I.PrimExp(i,s,t))		= O.PrimExp(i, s)
      | trExp(I.VarExp(i,y))		= O.VarExp(i, trLongid y)
      | trExp(I.TagExp(i,l,k))		= O.TagExp(i, trLab l, k>1)
      | trExp(I.ConExp(i,y,k))		= if isTagType(#typ i) then
					      O.TagExp(i, trLabLongid y, k>1)
					  else
					      O.ConExp(i, trLongid y, k>1)
      | trExp(I.RefExp(i))		= O.RefExp(i)
      | trExp(I.TupExp(i,es))		= O.TupExp(i, trExps es)
      | trExp(I.ProdExp(i,r))		= O.ProdExp(i, trExpRow r)
      | trExp(I.SelExp(i,l))		= O.SelExp(i, trLab l)
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
    and trExpField(I.Field(i,l,e))	= O.Field(i, trLab l, List.hd(trExps e))
    and trExpFields fs			= List.map trExpField fs


  (* Matches and Patterns *)

    and trMatch(I.Match(i,p,e))		= O.Match(i, trPat p, trExp e)
    and trMatchs ms			= List.map trMatch ms

    and trPat(I.JokPat(i))		= O.JokPat(i)
      | trPat(I.LitPat(i,l))		= O.LitPat(i, trLit l)
      | trPat(I.VarPat(i,x))		= O.VarPat(i, trId x)
      | trPat(I.TagPat(i,l,k))		= O.TagPat(i, trLab l, k>1)
      | trPat(I.ConPat(i,y,k))		= if isTagType(#typ i) then
					      O.TagPat(i, trLabLongid y, k>1)
					  else
					      O.ConPat(i, trLongid y, k>1)
      | trPat(I.RefPat(i))		= O.RefPat(i)
      | trPat(I.TupPat(i,ps))		= O.TupPat(i, trPats ps)
      | trPat(I.ProdPat(i,r))		= O.ProdPat(i, trPatRow r)
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
    and trPatField(I.Field(i,l,p))	= O.Field(i, trLab l, List.hd(trPats p))
    and trPatFields fs			= List.map trPatField fs


  (* Modules *)

    and trMod(I.PrimMod(i,s,j))		= O.PrimExp(trInfInfo i, s)
      | trMod(I.VarMod(i,x))		= let val x' as O.Id(i',_,_) =
								trModid x in
					      O.VarExp(trInfInfo i,
						       O.ShortId(
							   trLongidInfo i', x'))
					  end
      | trMod(I.StrMod(i,ds))		= let val i'   = trInfInfo i
					      val ids' = ids ds
					      val fs'  = List.map idToField ids'
					      val ds'  = trDecs ds in
					      O.LetExp(i',ds',O.ProdExp(i',fs'))
					  end
      | trMod(I.SelMod(i,m,l))		= let val i' = trInfInfo i
					      val r  = #region i'
					      val e' = trMod m
					      val t1 = #typ(O.infoExp e')
					      val t2 = #typ i'
					      val t  = Type.inArrow(t1,t2) in
					      O.AppExp(i',O.SelExp(typInfo(r,t),
							         trModlab l),e')
					  end
      | trMod(I.FunMod(i,x,j,m))	= let val i' = trInfInfo i
					      val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e')
					      val p' = O.VarPat(typInfo(r,t),
								trModid x)
					      val m' = O.Match(nonInfo r,p',e')
					      in O.FunExp(i', [m'])
					  end
      | trMod(I.AppMod(i,m1,m2))	= let val i1  = I.infoMod m1
					      val i2  = I.infoMod m2
					      val j1  = #inf i1
					      val j12 = #3(Inf.asArrow j1)
					  in
					      O.AppExp(trInfInfo i, trMod m1,
						       trUpMod(false,i2,m2,j12))
					  end
      | trMod(I.AnnMod(i,m,j))		= trUpMod(false,i, m, #inf(I.infoInf j))
      | trMod(I.UpMod(i,m,j))		= trUpMod(true, i, m, #inf(I.infoInf j))
      | trMod(I.LetMod(i,ds,m))		= O.LetExp(trInfInfo i, trDecs ds,
								trMod m)
      | trMod(I.UnpackMod(i,e,j))	= trExp e

    and trUpMod(abs,i,m,j) =
	let
	    val j1 = #inf(I.infoMod m)
	    val j2 = #inf i

	    val i' = trInfInfo i
	    val r  = #region i

	    val e1 = trMod m
	    val t1 = #typ(O.infoExp e1)
	    val t2 = #typ i'

	    val i2 = nonInfo r
	    val x' = O.Id(i2, Stamp.new(), Name.InId)
	    val x  = O.ShortId(typInfo(r, SOME t1), x')
	in
	    case upInf(abs, x,j1,j2, r,t1,t2)
	      of NONE    => e1
	       | SOME e2 =>
		 let
		     val p = O.VarPat(typInfo(r,t1), x')
		     val d = O.ValDec(i2,p,e1)
		 in
		     O.LetExp(typInfo(r,t2), [d], e2)
		 end
	end


  (* Types *)

    and trVarTyp r x =
	(* [a] = {[a]} *)
	let
	    val y' = O.ShortId(typInfo(r,NONE), trTypid x)
	in
	    O.VarExp(typInfo(r, typ_var), y')
        end

    and trTyp t =
	if !Switches.rttLevel = Switches.NO_RTT then
	    O.FailExp(typInfo(#region(I.infoTyp t), typ_typ))
	else
	    trTyp' t

    and trTyp'(I.VarTyp(i,x)) =
	(* [a] = Type.inVar[a] *)
	typOp(lab_inVar, trVarTyp (#region i) x)

      | trTyp'(I.ConTyp(i,y)) =
	(* [y] = {[y]} *)
	O.VarExp(typInfo(#region i, typ_typ), trTyplongid y)

      | trTyp'(I.FunTyp(i,x,t)) =
	(* [fn x => t] = let val {[x]} = Type.var <<kind x>>
	 *               in Type.inLambda([x],[t]) end
	 *)
	trBindTyp lab_inLambda Type.asLambda (i,x,t)

      | trTyp'(I.AppTyp(i,t1,t2)) =
	(* [t1 t2] = Type.inApply([t1],[t2]) *)
	let
	    val e1' = trTyp' t1
	    val e2' = trTyp' t2
	    val e'  = O.TupExp(typInfo(#region i, typ_typtyp), [e1',e2'])
	in
	    typOp(lab_inApply, e')
	end

      | trTyp'(I.RefTyp(i,t)) =
	(* [Ref t2] = Type.inApply(pervasiveTyp_ref, [t2]) *)
	(* pervasiveTyp_ref = Type.inCon(ARROW(STAR,STAR), CLOSED,
	 *                               pervasivePath_ref)
	 * pervasivePath_ref = Path.pervasive(Name.ExId ??) *)
	let
	    val r   = #region i
	    val l'  = O.Lab(nonInfo r, lab_closed)
	    val e1' = trKind r (Type.ARROW(Type.STAR,Type.STAR))
	    val e2' = O.TagExp(typInfo(r, typ_sort), l', false)
	    val e3' = pathOp(lab_pervasive, O.LitExp(typInfo(r,typ_string),
			O.StringLit(Name.toString(PervasiveType.name_ref))))
	    val e'  = O.TupExp(typInfo(r, typ_con), [e1',e2',e2'])
	    val e1' = typOp(lab_inCon, e')
	    val e2' = trTyp' t
	    val e'  = O.TupExp(typInfo(#region i, typ_typtyp), [e1',e2'])
	in
	    typOp(lab_inApply, e')
	end

      | trTyp'(I.TupTyp(i,ts)) =
	(* [(t1,...,tn)] = Type.inTuple #[t1,...,tn] *)
	let
	    val es' = List.map trTyp' ts
	    val t   = Type.inApply(typ_vec, typ_typ)
	    val e'  = O.VecExp(typInfo(#region i, t), es')
	in
	    typOp(lab_inTuple, e')
	end

      | trTyp'(I.ProdTyp(i,r)) =
	(* [{r}] = Type.inProd[r] *)
	let
	    val e' = trTypRow r
	in
	    typOp(lab_inProd, e')
	end

      | trTyp'(I.SumTyp(i,r)) =
	(* [<r>] = Type.inSum[r] *)
	let
	    val e' = trTypRow r
	in
	    typOp(lab_inSum, e')
	end

      | trTyp'(I.ArrTyp(i,t1,t2)) =
	(* [t1 -> t2] = Type.inArrow([t1],[t2]) *)
	let
	    val e1' = trTyp' t1
	    val e2' = trTyp' t2
	    val e'  = O.TupExp(typInfo(#region i, typ_typtyp), [e1',e2'])
	in
	    typOp(lab_inArrow, e')
	end

      | trTyp'(I.AllTyp(i,x,t)) =
	(* [forall x => t] = let val {[x]} = Type.var <<kind x>>
	 *                   in Type.inAll([x],[t]) end
	 *)
	trBindTyp lab_inAll Type.asAll (i,x,t)

      | trTyp'(I.ExTyp(i,x,t)) =
	(* [exists x => t] = let val {[x]} = Type.var <<kind x>>
	 *                   in Type.inExists([x],[t]) end
	 *)
	trBindTyp lab_inExist Type.asExist (i,x,t)

      | trTyp'(I.PackTyp(i,j)) =
	(* [pack j] = Type.inPack[j] *)
	(*UNFINISHED*)
	raise Crash.Crash "TranslationPhase.trTyp: PackTyp"

      | trTyp'(I.SingTyp(i,y)) =
	(* [sing y] = Type.inSing[y] *)
	(*UNFINISHED*)
	raise Crash.Crash "TranslationPhase.trTyp: SingTyp"

      | trTyp'(I.AbsTyp(i,so)) =
	(* [abstract] = Type.inCon(<<kind>>, CLOSED, Path.invent()) *)
	let
	    val r   = #region i
	    val l'  = O.Lab(nonInfo r, lab_closed)
	    val e1' = trKind r (Type.kind(#typ i))
	    val e2' = O.TagExp(typInfo(r, typ_sort), l', false)
	    val a   = Path.toLab(#3(Type.asCon(#typ i)))
	    val lit = O.StringLit(String.toWide(Label.toString a))
	    val e3' = pathOp(lab_fromLab, labOp(lab_fromString,
					O.LitExp(typInfo(r,typ_string), lit)))
	    val e'  = O.TupExp(typInfo(r, typ_con), [e1',e2',e2'])
	in
	    typOp(lab_inCon, e')
	end

      | trTyp'(I.ExtTyp(i,so)) =
	(* [abstract] = Type.inCon(<<kind>>, OPEN, Path.invent()) *)
	let
	    val r   = #region i
	    val l'  = O.Lab(nonInfo r, lab_open)
	    val e1' = trKind r (Type.kind(#typ i))
	    val e2' = O.TagExp(typInfo(r, typ_sort), l', false)
	    val a   = Path.toLab(#3(Type.asCon(#typ i)))
	    val lit = O.StringLit(String.toWide(Label.toString a))
	    val e3' = pathOp(lab_fromLab, labOp(lab_fromString,
					O.LitExp(typInfo(r,typ_string), lit)))
	    val e'  = O.TupExp(typInfo(r, typ_con), [e1',e2',e2'])
	in
	    typOp(lab_inCon, e')
	end

    and trTypRow(I.Row(i,fs,b)) =
	(* [f1,...,fn]     = [f1](...[fn](Type.emptyRow())...)
	 * [f1,...,fn,...] = [f1](...[fn](Type.unknownRow())...) *)
	let
	    val a   = if b then lab_unknownRow else lab_emptyRow
	    val e'  = rowOp(a, O.TupExp(typInfo(#region i, typ_unit), []))
	in
	    List.foldl trTypField e' fs
	end

    and trTypField(I.Field(i1, I.Lab(i2,a), ts), e3') =
	(* [a:t1,...,tn](e3) = Type.extendRow(Lab.fromString "a", #[t1,...,tn],
	 *				      e3) *)
	let
	    val r   = #region i1
	    val l'  = O.StringLit(String.toWide(Label.toString a))
	    val e1' = labOp(lab_fromString,
			    O.LitExp(typInfo(#region i2, typ_string), l'))
	    val t2  = Type.inApply(typ_vec, typ_typ)
	    val e2' = O.VecExp(typInfo(r,t2), List.map trTyp' ts)
	    val t   = Type.inTuple #[typ_lab, t2, typ_row]
	    val e'  = O.TupExp(typInfo(r,t), [e1',e2',e3'])
	in
	    rowOp(lab_extendRow, e')
	end

    and trBindTyp a asT (i,x,t) =
	let
	    val r  = #region(I.infoId x)
	    val k' = trKind r (Type.kindVar(#1(asT(#typ i))))
	    val p' = O.VarPat(typInfo(#region(I.infoId x), typ_var), trTypid x)
	    val d' = O.ValDec(nonInfo(#region i), p', varOp k')
	    val e' = typOp(a, O.TupExp(typInfo(#region i, typ_vartyp),
				       [trVarTyp r x, trTyp' t]))
	in
	    O.LetExp(O.infoExp e', [d'], e')
	end


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
      | trDec(I.TypDec(i,x,t), ds')	= let val r  = #region(I.infoId x)
					      val e' = trTyp t
					      val t  = #typ(O.infoExp e') in
					      O.ValDec(i, O.VarPat(typInfo(r,t),
							trTypid x), e') :: ds'
					  end
      | trDec(I.ModDec(i,x,m), ds')	= let val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e') in
					      O.ValDec(i, O.VarPat(typInfo(r,t),
							trModid x), e') :: ds'
					  end
      | trDec(I.InfDec(i,x,j), ds')	= ds' (*UNFINISHED*)
      | trDec(I.FixDec(i,x,q), ds')	= ds'
      | trDec(I.VarDec(i,x,d), ds')	= trDec(d, ds')
      | trDec(I.RecDec(i,ds), ds')	= O.RecDec(i, trDecs ds) :: ds'
      | trDec(I.LocalDec(i,ds), ds')	= trDecs'(ds, ds')

    and trDecs ds			= trDecs'(ds, [])
    and trDecs'(ds, ds')		= List.foldr trDec ds' ds

    and trEqCon(i,x,y',ds')		= O.ValDec(nonInfo(#region i),
						   O.VarPat(i,trId x),
						   O.VarExp(i,y')) :: ds'
    and trNewCon(i,x,k,ds')		= let val r  = #region i
					      val _  = Type.enterLevel()
					      val t  = Type.instance(#typ i)
					      val _  = Type.exitLevel()
					      val _  = Type.close t
					      val i' = typInfo(r,t)
					  in O.ValDec(nonInfo r,
						      O.VarPat(i',trId x),
						      O.NewExp(i',k>1)) :: ds'
					  end
    and trTagCon(i,x,k,ds')		= let val r  = #region i
					      val _  = Type.enterLevel()
					      val t  = Type.instance(#typ i)
					      val _  = Type.exitLevel()
					      val _  = Type.close t
					      val i' = typInfo(r,t)
					      val a  = Label.fromName(I.name x)
					  in O.ValDec(nonInfo r,
						O.VarPat(i',trId x),
						O.TagExp(i',O.Lab(nonInfo r,a)
							   ,k>1)) :: ds'
					  end


  (* Imports and annotations *)

    fun trAnns'(a_s, ds') = List.foldr trAnn ([],ds') a_s

    and trAnn(I.ImpAnn(i,is,u),(xsus',ds')) =
	let
	    val r    = #region i
	    val s    = #sign i
	    val t    = infToTyp(Inf.inSig s)
	    val x'   = O.Id(nonInfo r, Stamp.new(), Name.InId)
	    val y'   = O.ShortId(typInfo(r, SOME t), x')
	    val ds'' = trImps(is, y', t, ds')
	    val _    = Inf.stripSig s
	in
	    ( (x',s,u)::xsus', ds'' )
	end

    and trImps(is, y, t, ds')		= List.foldr (trImp y t) ds' is

    and trImp y t (I.ValImp(i,x,d),ds')	= idToDec(trId x, y, t,
						  #typ(I.infoDesc d)) :: ds'
      | trImp y t (I.ConImp(i,x,d,k),ds')
					= idToDec(trId x, y, t,
						  #typ(I.infoDesc d)) :: ds'
      | trImp y t (I.TypImp(i,x,d),ds')	= idToDec(trTypid x, y, t, typ_typ)::ds'
      | trImp y t (I.ModImp(i,x,d),ds')	= idToDec(trModid x, y, t,
						  infToTyp(#inf(I.infoDesc d)))
					  :: ds'
      | trImp y t (I.InfImp(i,x,d),ds')	= ds' (*UNFINISHED*)
      | trImp y t (I.FixImp(i,x,d),ds')	= ds'
      | trImp y t (I.RecImp(i,is), ds')	= trImps(is, y, t, ds')


  (* Components *)

    fun trComp(I.Comp(i,a_s,ds)) =
	let
	    val  ids'       = ids ds
	    val (xsus',ds') = trAnns'(a_s, trDecs ds)
	    val  fs'        = List.map idToField ids'
	    val  t          = Type.inProd(idsToRow ids')
	    val  i'         = typInfo(#region i,t)
	    val  exp'       = O.LetExp(i', ds', O.ProdExp(i', fs'))
	    val  s          = #sign i
	    val  _          = Inf.stripSig s
	in
	    ( xsus', (exp',s) )
	end

    fun translate () (desc, component) = trComp component

  end
