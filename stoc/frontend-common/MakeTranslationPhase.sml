(*
import structure Source			from "Source"
import structure TypedInfo		from "TypedGrammar"
import structure TypedGrammar		from "TypedGrammar"
import structure IntermediateGrammar	from "IntermediateGrammar"
import structure EmptyContext		from "EmptyContext"
import structure StringMap		from "StringMap"

import structure Stamp			from "Stamp"
import structure Name			from "Name"
import structure Label			from "Label"
import structure Type			from "Type"
import structure Inf			from "Inf"

import structure PerasiveType		from "PervasiveType"
import structure LabelReflection	from "LabelReflection"
import structure PathReflection		from "PathReflection"
import structure TypeReflection		from "TypeReflection"
import structure InfReflection		from "InfReflection"

import signature SWITCHES		from "SWITCHES"
import signature TRANSLATION_PHASE	from "TRANSLATION_PHASE"

import structure Crash			from "Crash"
*)

functor MakeTranslationPhase(Switches: SWITCHES) :> TRANSLATION_PHASE =
struct

    structure C = EmptyContext
    structure I = TypedGrammar
    structure O = IntermediateGrammar

    open TypedInfo


  (* UNFINISHED... *)

    fun unfinished i funname casename =
	Error.error(i, "Translation." ^ funname ^ ": " ^ casename ^
		       " not implementeed yet")

  (* Recognize sum type constructors (tags) *)

    fun decomposeConarrow t =
	if Type.isAll t then decomposeConarrow(#2(Type.asAll t)) else
	let
	    val (t',tArity)  = Type.asApply t
	    val (t'',tArrow) = Type.asApply t'
	in
(*ASSERT    assert Type.isCon t''
	    andalso Path.equals(#3(Type.asCon t''), PervasiveType.path_conarrow)
	    => *)
	    if Type.isCon t''
	    andalso Path.equals(#3(Type.asCon t''), PervasiveType.path_conarrow)
	    then () else raise Assert.failure;
	    (tArrow, tArity)
	end

    fun decodeArity t  = decodeArity'(#2(decomposeConarrow t))
    and decodeArity' t = if Type.isApply t
			 then 1 + decodeArity'(#1(Type.asApply t))
			 else 0


  (* Names and labels *)

    fun trName' f (Name.ExId s)	= Name.ExId(f s)
      | trName' f  n		= n

    fun trValName  n		= n
    val trTypName		= trName'(fn s => "$" ^ s)
    val trModName		= trName'(fn s => s ^ "$")
    val trInfName		= trName'(fn s => "$" ^ s ^ "$")

    fun trValLabel a		= Label.fromName(trValName(Label.toName a))
    fun trTypLabel a		= Label.fromName(trTypName(Label.toName a))
    fun trModLabel a		= Label.fromName(trModName(Label.toName a))
    fun trInfLabel a		= Label.fromName(trInfName(Label.toName a))


  (* Auxiliaries for reflection *)

    open PervasiveType
    open InfReflection
    open TypeReflection
    open PervasiveTypeReflection
    open PathReflection
    open LabelReflection

    val typ_icon		= InfReflection.typ_con
    val typ_ikind		= InfReflection.typ_kind

    val modname_label'		= trModName modname_label
    val modname_path'		= trModName modname_path
    val modname_type'		= trModName modname_type
    val modname_pervType'	= trModName modname_pervasiveType
    val modname_inf'		= trModName modname_inf
    val lab_label'		= Label.fromName modname_label'
    val lab_path'		= Label.fromName modname_path'
    val lab_type'		= Label.fromName modname_type'
    val lab_pervType'		= Label.fromName modname_pervType'
    val lab_inf'		= Label.fromName modname_inf'

    val longidr_pervasive	= ref(NONE : O.longid option)
    fun longid_pervasive()	= case !longidr_pervasive
				    of SOME y => y
				     | NONE => raise Crash.Crash
					"TranslationPhase.longid_pervasive"

    fun info_pervasiveDot a	= let val i = O.infoLongid(longid_pervasive())
				      val r = Type.asProd(#typ i)
				      val t = Type.lookupRow(r,a)
				  in typInfo(#region i, t) end
    fun info_label()		= info_pervasiveDot lab_label'
    fun info_path()		= info_pervasiveDot lab_path'
    fun info_type()		= info_pervasiveDot lab_type'
    fun info_inf()		= info_pervasiveDot lab_inf'
    fun info_pervType()		= info_pervasiveDot lab_pervType'

    val typ_typtyp		= Type.inTuple #[typ_typ, typ_typ]
    val typ_vartyp		= Type.inTuple #[typ_var, typ_typ]
    val typ_typVec		= Type.inApply(typ_vec, typ_typ)
    val typ_kindkind		= Type.inTuple #[typ_kind, typ_kind]
    val typ_kindToVar		= Type.inArrow(typ_kind, typ_var)
    val typ_kindkindToKind	= Type.inArrow(typ_kindkind, typ_kind)
    val typ_infinf		= Type.inTuple #[typ_inf, typ_inf]
    val typ_pathinfinf		= Type.inTuple #[typ_path, typ_inf, typ_inf]
    val typ_infpathinf		= Type.inTuple #[typ_inf, typ_path, typ_inf]
    val typ_labtypVecrow	= Type.inTuple #[typ_lab, typ_typVec, typ_row]

    val info_nowhere		= nonInfo(Source.nowhere)
    val info_nowhere'		= typInfo(Source.nowhere, typ_typ(*UNFINISHED*))
    fun longid_pervasiveDot i n	= O.LongId(i, longid_pervasive(),
    					  O.Lab(info_nowhere, Label.fromName n))

    fun longid_ref()		= longid_pervasiveDot info_nowhere' name_ref
    fun longid_label()		= longid_pervasiveDot(info_label())
						      modname_label'
    fun longid_path()		= longid_pervasiveDot(info_path()) modname_path'
    fun longid_type()		= longid_pervasiveDot(info_type()) modname_type'
    fun longid_inf()		= longid_pervasiveDot(info_inf())  modname_inf'
    fun longid_pervType()	= longid_pervasiveDot(info_pervType())
						      modname_pervType'
    fun longid_pervInf()	= (*UNFINISHED*)
				  unfinished (Source.nowhere)"longid_pervInf" ""
    fun longid_labelDot l'	= O.LongId(info_nowhere', longid_label(), l')
    fun longid_pathDot l'	= O.LongId(info_nowhere', longid_path(), l')
    fun longid_typeDot l'	= O.LongId(info_nowhere', longid_type(), l')
    fun longid_infDot l'	= O.LongId(info_nowhere', longid_inf(), l')
    fun longid_pervTypeDot l'	= O.LongId(info_nowhere', longid_pervType(),l')
    fun longid_pervInfDot l'	= O.LongId(info_nowhere', longid_pervInf(), l')

    fun yVal (mkY,typ) a	= let val r  = Source.nowhere
				      val l' = O.Lab(nonInfo r, a)
				  in
				      O.VarExp(typInfo(r,typ), mkY l')
				  end

    fun yOp (mkY,typ) (a,e')	= let val r   = #region(O.infoExp e')
				      val l'  = O.Lab(nonInfo r, a)
				      val t   = #typ(O.infoExp e')
				      val t1  = Type.inArrow(t, typ)
				      val e1' = O.VarExp(typInfo(r,t1), mkY l')
				  in
				      O.AppExp(typInfo(r, typ), e1', e')
				  end

    val labOp			= yOp(longid_labelDot,     typ_lab)
    val pathOp			= yOp(longid_pathDot,      typ_path)
    val typOp			= yOp(longid_typeDot,      typ_typ)
    val varOp			= yOp(longid_typeDot,      typ_var)
    val rowOp			= yOp(longid_typeDot,      typ_row)
    val unitTypOp		= yOp(longid_typeDot,      typ_unit)
    val pervTypeOp		= yOp(longid_pervTypeDot,  typ_con)
    val pervTypeVal		= yVal(longid_pervTypeDot, typ_typ)
    val sigOp			= yOp(longid_infDot,       typ_sign)
    val infOp			= yOp(longid_infDot,       typ_inf)
    val kindOp			= yOp(longid_infDot,       typ_ikind)
    val unitInfOp		= yOp(longid_infDot,       typ_unit)
    val pathInfOp		= yOp(longid_infDot,       typ_path)
    val pervInfOp		= yOp(longid_pervInfDot,   typ_icon)


  (* Runtime kinds and sorts *)

    fun trKind r (Type.STAR) =
	    O.TagExp(typInfo(r,typ_kind),
		     O.Lab(nonInfo r, lab_star),
		     O.FailExp(typInfo(r,typ_zero)), false)
      | trKind r (Type.ARROW(k1,k2)) =
	    O.TagExp(typInfo(r,typ_kindkindToKind),
		     O.Lab(nonInfo r, lab_arrow),
		     O.TupExp(typInfo(r,typ_kindkind),
			      (*UNFINISHED: this should be a vector constant,
			        but NJ has a bug... *)
			      vector[trKind r k1, trKind r k2]), true)

    fun trSort r (Type.OPEN) =
	    O.TagExp(typInfo(r,typ_sort),
		     O.Lab(nonInfo r, lab_open),
		     O.FailExp(typInfo(r,typ_zero)), false)
      | trSort r (Type.CLOSED) =
	    O.TagExp(typInfo(r,typ_sort),
		     O.Lab(nonInfo r, lab_closed),
		     O.FailExp(typInfo(r,typ_zero)), false)


  (* Transformation of type info *)

    structure InfHash = MakeHashImpMap(open Inf val equals = Inf.same)

    val infHash = InfHash.new() : Type.t InfHash.map
		(*UNFINISHED: remove this global variable! *)

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
	case InfHash.lookup(infHash, j) of
	  SOME t => t
	| NONE   =>
	    let
		val t = infToTyp' j
	    in
		InfHash.insertDisjoint(infHash, j, t);
		t
	    end

    and infToTyp' j =
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
		    val (l,t,d) = Inf.asValItem i
		in
		    Type.extendRow(trValLabel l, t, r)
		end
	    else if Inf.isTypItem i then
		let
		    val (l,k,d) = Inf.asTypItem i
		in
		    Type.extendRow(trTypLabel l, typ_typ, r)
		end
	    else if Inf.isModItem i then
		let
		    val (l,j,d) = Inf.asModItem i
		in
		    Type.extendRow(trModLabel l, infToTyp j, r)
		end
	    else if Inf.isInfItem i then
		let
		    val (l,k,d) = Inf.asInfItem i
		in
		    Type.extendRow(trInfLabel l, typ_inf, r)
		end
	    else (* fixity *)
		r
	end

    fun trValInfo i		= i
    fun trTypInfo {region}	= {region=region, typ=typ_typ}
    fun trModInfo {region,inf}	= {region=region, typ=infToTyp inf}
    fun trInfInfo {region}	= {region=region, typ=typ_inf}

    fun updatePervasive(I.Id(i,z,n) : I.modid, j) =
	if n <> name_pervasive then () else
	let
	    val i' = typInfo(#region i, infToTyp j)
	in
	    longidr_pervasive := SOME(O.ShortId(i', O.Id(i', z, trModName n)))
	end


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
     *  [x . type y:k1 :> type y:k2 = t2] = type y = lazy (x.y)
     *  [x . type y:k1 :> type y:k2] = type y = lazy (Type.inCon(...))
     *	[x . structure y:j1 :> structure y:j2] =
     *	   structure y = lazy [x.y : j1 :> j2]
     *
     * Moreover, we apply the optimization that - if the transformation is the
     * identity function - the coercion is a no-op.
     *)

    fun upInf(withAbs, x,j1,j2, r,t1,t2) =
	if Inf.isSig j2 andalso Inf.isSig j1 then
	    let
		val s1 = Inf.asSig j1
		val s2 = Inf.asSig j2
		val isntIdentity = Inf.size s1 <> Inf.size s2
	    in
		case upItems(withAbs, r, x, Inf.items s2, s1, isntIdentity, [])
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
		val i1 = typInfo(r, t21)
		val i2 = typInfo(r, t21)
		val y' = O.Id(i1, Stamp.new(), Name.InId)
		val z' = O.Id(i2, Stamp.new(), Name.InId)
		val y  = O.ShortId(i1, y')
		val z  = O.ShortId(i2, z')
	    in
		case (upInf(false, y,j21,j11, r,t21,t11),
		      upInf(withAbs, z,j12,j22, r,t12,t22))
		  of (SOME exp1, SOME exp2) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val zpat   = O.VarPat(typInfo(r,t12), z')
			val appexp = O.AppExp(typInfo(r,t12), xexp, exp1)
			val dec    = O.ValDec(i', zpat, appexp)
			val letexp = O.LetExp(typInfo(r,t22), #[dec], exp2)
		     in
			SOME(O.FunExp(typInfo(r,t2),#[O.Match(i',ypat,letexp)]))
		     end
		   | (NONE, SOME exp2) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val yexp   = O.VarExp(typInfo(r,t21), y)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val zpat   = O.VarPat(typInfo(r,t12), z')
			val appexp = O.AppExp(typInfo(r,t12), xexp, yexp)
			val dec    = O.ValDec(i', zpat, appexp)
			val letexp = O.LetExp(typInfo(r,t22), #[dec], exp2)
		     in
			SOME(O.FunExp(typInfo(r,t2),#[O.Match(i',ypat,letexp)]))
		     end
		   | (SOME exp1, NONE) =>
		     let
			val xexp   = O.VarExp(typInfo(r,t1), x)
			val ypat   = O.VarPat(typInfo(r,t21), y')
			val appexp = O.AppExp(typInfo(r,t12), xexp, exp1)
		     in
			SOME(O.FunExp(typInfo(r,t2),#[O.Match(i',ypat,appexp)]))
		     end
		   | (NONE, NONE) => NONE
	    end
	else
	    NONE

    and upItems(withAbs, r, x, [], s1, false, fields) = NONE
      | upItems(withAbs, r, x, [], s1, true,  fields) =
	    SOME(Vector.fromList fields)
      | upItems(withAbs, r, x, item::items, s1, isntIdentity, fields) =
	if Inf.isValItem item then
	    let
		val (a,t2,_) = Inf.asValItem item
		val    t1    = Inf.lookupVal(s1,a)
		val    i1    = typInfo(r,t1)
		val    i2    = typInfo(r,t2)
		val    i'    = nonInfo r
		val    l     = O.Lab(i', trValLabel a)
		val    y     = O.LongId(i1,x,l)
		val    exp   = O.LazyExp(i2, O.UpExp(i2, O.VarExp(i1,y)))
	    in
		upItems(withAbs, r, x, items, s1, isntIdentity,
			O.Field(i',l,exp)::fields)
	    end
	else if Inf.isTypItem item then
	    let
		val (a,k,d) = Inf.asTypItem item
		val    i    = typInfo(r, typ_typ)
		val    i'   = nonInfo r
		val    l'   = O.Lab(i', trTypLabel a)
		val    y    = O.LongId(i,x,l')
		val (exp,isntIdentity') =
		    if not withAbs orelse Option.isSome d
		    orelse !Switches.Bootstrap.rttLevel =
			    Switches.Bootstrap.NO_RTT then
			(O.VarExp(i,y), isntIdentity)
		    else let
		(* UNFINISHED: Is this bullshit? We have to do realisations,
		 * don't we?
		 *)
			val e1' = trKind r k
			val e2' = trSort r (Type.CLOSED)
			val lit = O.StringLit(String.toWide(Label.toString a))
			val e3' = pathOp(lab_fromLab,
				   labOp(lab_fromString,
					 O.LitExp(typInfo(r,typ_string), lit)))
			val e'  = O.TupExp(typInfo(r,typ_con), #[e1',e2',e3'])
		    in
			(typOp(lab_inCon, e'), true)
		    end
		val exp' = O.LazyExp(i,exp)
	    in
		upItems(withAbs, r, x, items, s1, isntIdentity',
			O.Field(i',l',exp')::fields)
	    end
	else if Inf.isModItem item then
	    let
		val (a,j2,_) = Inf.asModItem item
		val    j1    = Inf.lookupMod(s1,a)
		val    t1    = infToTyp j1
		val    t2    = infToTyp j2
		val    i1    = typInfo(r,t1)
		val    i2    = typInfo(r,t2)
		val    i'    = nonInfo r
		val    l'    = O.Lab(i', trModLabel a)
		val    y     = O.LongId(i2,x,l')
		val (exp,isntIdentity') =
		    case upInf(withAbs, y,j1,j2, r,t1,t2)
		      of NONE => (if withAbs then O.UpExp(i2, O.VarExp(i1,y))
		 			     else O.VarExp(i1,y), isntIdentity)
		       | SOME exp => (exp, true)
		val  exp'    = O.LazyExp(i2,exp)
	    in
		upItems(withAbs, r, x, items, s1, isntIdentity',
			O.Field(i',l',exp')::fields)
	    end
	else if Inf.isInfItem item then
	    let
		val (a,k,d) = Inf.asInfItem item
		val    i    = typInfo(r, typ_inf)
		val    i'   = nonInfo r
		val    l'   = O.Lab(i', trInfLabel a)
		val    y    = O.LongId(i,x,l')
		val (exp,isntIdentity') =
		    if not withAbs orelse Option.isSome d
		    orelse !Switches.Bootstrap.rttLevel <>
			    Switches.Bootstrap.FULL_RTT then
			(O.VarExp(i,y), isntIdentity)
		    else let
		(* UNFINISHED: No idea how to do it yet. *)
		    in
			unfinished r "upItems" "runtime interface abstraction"
		    end
		val exp' = O.LazyExp(i,exp)
	    in
		upItems(withAbs, r, x, items, s1, isntIdentity',
			O.Field(i',l',exp')::fields)
	    end
	else (* fixity *)
	    upItems(withAbs, r, x, items, s1, isntIdentity, fields)



  (* Create fields for all structures and values in an environment *)

    fun idToField(x' as O.Id(i,_,n)) =
	let
	    val i' = nonInfo(#region i)
	in
	    O.Field(i', O.Lab(i', Label.fromName n),
			O.VarExp(i, O.ShortId(i,x')))
	end

    fun idToDec(x' as O.Id(i,_,n), y, tMod, tField) =
	let
	    val r    = #region i
	    val i'   = nonInfo r
	    val tSel = Type.inArrow(tMod, tField)
	in
	    O.ValDec(i', O.VarPat(typInfo(r,tField), x'),
			 O.LazyExp(typInfo(r,tSel),
				   O.SelExp(typInfo(r,tSel),
					    O.Lab(i', Label.fromName n),
					    O.VarExp(typInfo(r,tMod), y))))
	end

    fun idToRow(O.Id(i,_,n), r) = Type.extendRow(Label.fromName n, #typ i, r)



  (* Literals *)

    fun trLit(I.IntLit n)		= O.IntLit n
      | trLit(I.WordLit w)		= O.WordLit w
      | trLit(I.CharLit c)		= O.CharLit c
      | trLit(I.StringLit s)		= O.StringLit s
      | trLit(I.RealLit x)		= O.RealLit x


  (* Identifiers *)

    fun trVallab(I.Lab(i,a))		= O.Lab(i, trValLabel  a)
    fun trTyplab(I.Lab(i,a))		= O.Lab(i, trTypLabel a)
    fun trModlab(I.Lab(i,a))		= O.Lab(i, trModLabel a)
    fun trInflab(I.Lab(i,a))		= O.Lab(i, trInfLabel a)

    fun trValid(I.Id(i,z,n))		= O.Id(trValInfo i, z, trValName n)
    fun trTypid(I.Id(i,z,n))		= O.Id(trTypInfo i, z, trTypName n)
    fun trModid(I.Id(i,z,n))		= O.Id(trModInfo i, z, trModName n)
    fun trInfid(I.Id(i,z,n))		= O.Id(trInfInfo i, z, trInfName n)

    fun trModlongid(I.ShortId(i,x))	= O.ShortId(trModInfo i, trModid x)
      | trModlongid(I.LongId(i,y,l))	= O.LongId(trModInfo i,
						   trModlongid y, trModlab l)

    fun trVallongid(I.ShortId(i,x))	= O.ShortId(trValInfo i, trValid x)
      | trVallongid(I.LongId(i,y,l))	= O.LongId(trValInfo i,
						   trModlongid y, trVallab l)

    fun trTyplongid(I.ShortId(i,x))	= O.ShortId(trTypInfo i, trTypid x)
      | trTyplongid(I.LongId(i,y,l))	= O.LongId(trTypInfo i,
						   trModlongid y, trTyplab l)

    fun trInflongid(I.ShortId(i,x))	= O.ShortId(trInfInfo i, trInfid x)
      | trInflongid(I.LongId(i,y,l))	= O.LongId(trInfInfo i,
						   trModlongid y, trInflab l)


  (* Extract bound ids from declarations. *)

    fun idsId xs' (x as O.Id(_,_,Name.ExId s))	= StringMap.insert(xs',s,x)
      | idsId xs'  _				= ()

    fun idsDec xs' (O.ValDec(i,p,e))	= idsPat xs' p
      | idsDec xs' (O.RecDec(i,ds))	= idsDecs xs' ds
    and idsDecs xs'			= Vector.app(idsDec xs')

    and idsPat xs' (O.JokPat(i))	= ()
      | idsPat xs' (O.VarPat(i,x))	= idsId xs' x
      | idsPat xs' (O.LitPat(i,l))	= ()
      | idsPat xs' (O.TagPat(i,l,p,b))	= idsPat xs' p
      | idsPat xs' (O.ConPat(i,y,p,b))	= idsPat xs' p
      | idsPat xs' (O.RefPat(i,p))	= idsPat xs' p
      | idsPat xs' (O.TupPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (O.ProdPat(i,fs))	= idsFields xs' fs
      | idsPat xs' (O.VecPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (O.AsPat(i,p1,p2))	= ( idsPat xs' p1 ; idsPat xs' p2 )
      | idsPat xs' (O.AltPat(i,ps))	= idsPats xs' ps
      | idsPat xs' (O.NegPat(i,p))	= idsPat xs' p
      | idsPat xs' (O.GuardPat(i,p,e))	= idsPat xs' p
      | idsPat xs' (O.WithPat(i,p,ds))	= ( idsPat xs' p ; idsDecs xs' ds )
    and idsPats xs'			= Vector.app(idsPat xs')

    and idsField  xs' (O.Field(i,l,p))  = idsPat xs' p
    and idsFields xs'			= Vector.app(idsField xs')

    fun ids ds				= let val xs' = StringMap.new() in
					      idsDecs xs' ds ;
					      Vector.fromList
					  	  (StringMap.fold op:: [] xs')
					  end


  (* Expressions *)

    fun trExps es			= Vector.map trExp es
    and trExp(I.LitExp(i,l))		= O.LitExp(i, trLit l)
      | trExp(I.VarExp(i,y))		= O.VarExp(i, trVallongid y)
      | trExp(I.PrimExp(i,s,t))		= O.PrimExp(i, s)
      | trExp(I.LabExp(i,l,t))		= O.FailExp(i)
      | trExp(I.NewExp(i,t))		= O.NewExp(i, decodeArity(#typ i) > 1)
      | trExp(I.TagExp(i,l,NONE,e))	= O.TagExp(i, trVallab l, trExp e,false)
      | trExp(I.TagExp(i,l,SOME y,e))	= O.TagExp(i, trVallab l, trExp e,
					  decodeArity(#typ(I.infoLongid y)) > 1)
      | trExp(I.ConExp(i,y,e))		= O.ConExp(i, trVallongid y, trExp e,
					  decodeArity(#typ(I.infoLongid y)) > 1)
      | trExp(I.RefExp(i,e))		= O.RefExp(i, trExp e)
      | trExp(I.TupExp(i,es))		= O.TupExp(i, trExps es)
      | trExp(I.ProdExp(i,r))		= O.ProdExp(i, trExpRow r)
      | trExp(I.UpdExp(i,e,r))		= trUpdExp(i, trExp e, trExpRow r)
      | trExp(I.SelExp(i,l,e))		= O.SelExp(i, trVallab l, trExp e)
      | trExp(I.VecExp(i,es))		= O.VecExp(i, trExps es)
      | trExp(I.FunExp(i,ms))		= O.FunExp(i, trMatchs ms)
      | trExp(I.AppExp(i,e1,e2))	= O.AppExp(i, trExp e1, trExp e2)
      | trExp(I.AndExp(i,e1,e2))	= O.AndExp(i, trExp e1, trExp e2)
      | trExp(I.OrExp(i,e1,e2))		= O.OrExp(i, trExp e1, trExp e2)
      | trExp(I.IfExp(i,e1,e2,e3))	= O.IfExp(i, trExp e1, trExp e2,
							       trExp e3)
      | trExp(I.SeqExp(i,es))		= O.SeqExp(i, trExps es)
      | trExp(I.CaseExp(i,e,ms))	= O.CaseExp(i, trExp e, trMatchs ms)
      | trExp(I.FailExp(i))		= O.FailExp(i)
      | trExp(I.RaiseExp(i,e))		= O.RaiseExp(i, trExp e)
      | trExp(I.HandleExp(i,e,ms))	= O.HandleExp(i, trExp e, trMatchs ms)
      | trExp(I.AnnExp(i,e,t))		= trExp e
      | trExp(I.LetExp(i,ds,e))		= O.LetExp(i, trDecs ds, trExp e)
      | trExp(I.PackExp(i,m))		= trMod m

    and trExpRow(I.Row(i,fs,_))		= trExpFields fs
    and trExpFields fs			= Vector.map trExpField fs
    and trExpField(I.Field(i,l,e))	= O.Field(i, trVallab l, trExp e)

    and trUpdExp(i,e',r') =
	let
	    val i'  = O.infoExp e'
	    val i'' = nonInfo(#region i')
	    val x'  = O.Id(i', Stamp.new(), Name.InId)
	    val d'  = O.ValDec(i'', O.VarPat(i',x'), e')
	    val e'' = O.VarExp(i', O.ShortId(i', x'))
	    val r'' = trUpdExp'(Type.asSum(#typ i'),i'',e'',r',Vector.toList r')
	in
	    O.LetExp(i, #[d'], O.ProdExp(i, r''))
	end
	
    and trUpdExp'(tr,i,e',r',fs) =
	if Type.isEmptyRow tr then	(*UNFINISHED: check for unknown *)
	    Vector.fromList fs
	else
	let
	    val (a,t) = Type.headRow tr
	    val  tr'  = Type.tailRow tr
	    val  fs'  = if Vector.exists (fn O.Field(_,O.Lab(_,b),_) => a=b) r'
			then fs else
			let val l' = O.Lab(i,a)
			    val i' = typInfo(#region i, t)
			in O.Field(i, l', O.SelExp(i',l',e')) :: fs end
	in
	    trUpdExp'(tr',i,e',r',fs')
	end


  (* Matches and Patterns *)

    and trMatchs ms			= Vector.map trMatch ms
    and trMatch(I.Match(i,p,e))		= O.Match(i, trPat p, trExp e)

    and trPats ps			= Vector.map trPat ps
    and trPat(I.JokPat(i))		= O.JokPat(i)
      | trPat(I.LitPat(i,l))		= O.LitPat(i, trLit l)
      | trPat(I.VarPat(i,x))		= O.VarPat(i, trValid x)
      | trPat(I.TagPat(i,l,NONE,p))	= O.TagPat(i, trVallab l, trPat p,false)
      | trPat(I.TagPat(i,l,SOME y,p))	= O.TagPat(i, trVallab l, trPat p,
					  decodeArity(#typ(I.infoLongid y)) > 1)
      | trPat(I.ConPat(i,y,p))		= O.ConPat(i, trVallongid y, trPat p,
					  decodeArity(#typ(I.infoLongid y)) > 1)
      | trPat(I.RefPat(i,p))		= O.RefPat(i, trPat p)
      | trPat(I.TupPat(i,ps))		= O.TupPat(i, trPats ps)
      | trPat(I.ProdPat(i,r))		= O.ProdPat(i, trPatRow r)
      | trPat(I.VecPat(i,ps))		= O.VecPat(i, trPats ps)
      | trPat(I.AsPat(i,p1,p2))		= O.AsPat(i, trPat p1, trPat p2)
      | trPat(I.AltPat(i,ps))		= O.AltPat(i, trPats ps)
      | trPat(I.NegPat(i,p))		= O.NegPat(i, trPat p)
      | trPat(I.GuardPat(i,p,e))	= O.GuardPat(i, trPat p, trExp e)
      | trPat(I.AnnPat(i,p,t))		= trPat p
      | trPat(I.WithPat(i,p,ds))	= O.WithPat(i, trPat p, trDecs ds)

    and trPatRow(I.Row(i,fs,b))		= trPatFields fs
    and trPatFields fs			= Vector.map trPatField fs
    and trPatField(I.Field(i,l,p))	= O.Field(i, trVallab l, trPat p)


  (* Modules *)

    and trMod(I.PrimMod(i,s,j))		= O.PrimExp(trModInfo i, s)
      | trMod(I.VarMod(i,x))		= let val x' as O.Id(i',_,_) = trModid x
					  in O.VarExp(trModInfo i,
						    O.ShortId(trValInfo i', x'))
					  end
      | trMod(I.StrMod(i,ds))		= let val i'   = trModInfo i
					      val ds'  = trDecs ds
					      val ids' = ids ds'
					      val fs'  = Vector.map idToField
								    ids'
					  in O.LetExp(i',ds',O.ProdExp(i',fs'))
					  end
      | trMod(I.SelMod(i,l,m))		= O.SelExp(trModInfo i, trModlab l,
								trMod m)
      | trMod(I.FunMod(i,x,j,m))	= let val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e')
					      val p' = O.VarPat(typInfo(r,t),
								trModid x)
					      val m' = O.Match(nonInfo r,p',e')
					  in O.FunExp(trModInfo i, #[m'])
					  end
      | trMod(I.AppMod(i,m1,m2))	= let val i1  = I.infoMod m1
					      val i2  = I.infoMod m2
					      val j1  = #inf i1
					      val j12 = #3(Inf.asArrow j1)
					  in O.AppExp(trModInfo i, trMod m1,
						      trUpMod(false,i2,m2,j12))
					  end
      | trMod(I.AnnMod(i,m,j))		= trUpMod(false,i, m, #inf(I.infoInf j))
      | trMod(I.UpMod(i,m,j))		= trUpMod(true, i, m, #inf(I.infoInf j))
      | trMod(I.LetMod(i,ds,m))		= O.LetExp(trModInfo i, trDecs ds,
								trMod m)
      | trMod(I.UnpackMod(i,e,j))	= trExp e

    and trUpMod(withAbs,i,m,j) =
	let
	    val j1 = #inf(I.infoMod m)
	    val j2 = #inf i

	    val i2 = trModInfo i
	    val r  = #region i

	    val e1 = trMod m
	    val i1 = O.infoExp e1
	    val t1 = #typ i1
	    val t2 = #typ i2

	    val i' = nonInfo r
	    val x' = O.Id(typInfo(r,t1), Stamp.new(), Name.InId)
	    val x  = O.ShortId(typInfo(r,t1), x')
	in
	    case upInf(withAbs, x,j1,j2, r,t1,t2)
	      of NONE    => if withAbs then O.UpExp(i2, e1) else e1
	       | SOME e2 => O.LetExp(i2,#[O.ValDec(i',O.VarPat(i1,x'), e1)], e2)
	end


  (* Types *)

    (* The code generated to produce runtime types and interfaces very much
     * mirrors what happens in the elaborator.
     *)

    and trTyp t =
	if !Switches.Bootstrap.rttLevel = Switches.Bootstrap.NO_RTT then
	    O.FailExp(typInfo(#region(I.infoTyp t), typ_typ))
	else
	    trTyp' t

    and trTyp'(I.VarTyp(i,x)) =
	(* [a] = Type.inVar{[a]} *)
	let
	    val i' = typInfo(#region(I.infoId x), typ_var)
	in
	    typOp(lab_inVar, O.VarExp(i', O.ShortId(i', trTypid x)))
	end

      | trTyp'(I.PrimTyp(i,s)) =
	(* [prim s] = Type.inCon(PervasiveType.lookup s) *)
	let
	    val e1' = O.LitExp(typInfo(#region i, typ_string),
			       O.StringLit(String.toWide s))
	in
	    typOp(lab_inCon, pervTypeOp(lab_lookup, e1'))
	end

      | trTyp'(I.ConTyp(i,y)) =
	(* [y] = {[y]} *)
	O.VarExp(typInfo(#region i, typ_typ), trTyplongid y)

      | trTyp'(I.FunTyp(i,x,t)) =
	(* [fn x => t] = let val {[x]} = Type.var <<kind x>>
	 *               in Type.inLambda([x],[t]) end
	 *)
	trBindTyp lab_inLambda (i, x, t, Type.domKind(Type.kind(#typ i)))

      | trTyp'(I.AppTyp(i,t1,t2)) =
	(* [t1 t2] = Type.inApply([t1],[t2]) *)
	let
	    val e1' = trTyp' t1
	    val e2' = trTyp' t2
	in
	    typOp(lab_inApply,
		  O.TupExp(typInfo(#region i, typ_typtyp), #[e1',e2']))
	end

      | trTyp'(I.RefTyp(i,t)) =
	(* [Ref t2] = Type.inApply(PervasiveType.typ_ref, [t2]) *)
	let
	    val e1' = pervTypeVal lab_typ_ref
	    val e2' = trTyp' t
	in
	    typOp(lab_inApply,
		  O.TupExp(typInfo(#region i, typ_typtyp), #[e1',e2']))
	end

      | trTyp'(I.TupTyp(i,ts)) =
	(* [(t1,...,tn)] = Type.inTuple #[t1,...,tn] *)
	let
	    val es' = Vector.map trTyp' ts
	in
	    typOp(lab_inTuple, O.VecExp(typInfo(#region i, typ_typVec), es'))
	end

      | trTyp'(I.ProdTyp(i,r)) =
	(* [{r}] = Type.inProd[r] *)
	typOp(lab_inProd, trTypRow r)

      | trTyp'(I.SumTyp(i,r)) =
	(* [<r>] = Type.inSum[r] *)
	typOp(lab_inSum, trTypRow r)

      | trTyp'(I.ArrTyp(i,t1,t2)) =
	(* [t1 -> t2] = Type.inArrow([t1],[t2]) *)
	let
	    val e1' = trTyp' t1
	    val e2' = trTyp' t2
	in
	    typOp(lab_inArrow,
		  O.TupExp(typInfo(#region i, typ_typtyp), #[e1',e2']))
	end

      | trTyp'(I.AllTyp(i,x,t)) =
	(* [forall x => t] = let val {[x]} = Type.var <<kind x>>
	 *                   in Type.inAll([x],[t]) end
	 *)
	trBindTyp lab_inAll (i,x,t, Type.kindVar(#1(Type.asAll'(#typ i))))

      | trTyp'(I.ExTyp(i,x,t)) =
	(* [exists x => t] = let val {[x]} = Type.var <<kind x>>
	 *                   in Type.inExists([x],[t]) end
	 *)
	trBindTyp lab_inExist (i,x,t, Type.kindVar(#1(Type.asExist'(#typ i))))

      | trTyp'(I.PackTyp(i,j)) =
	(* [pack j] = Type.inPack[j] *)
	(*UNFINISHED*)
	unfinished (#region i) "trTyp" "runtime package types"

      | trTyp'(I.SingTyp(i,y)) =
	(* [sing y] = Type.inSing[y] *)
	(*UNFINISHED*)
	unfinished (#region i) "trTyp" "runtime singleton types"

      | trTyp'(I.AbsTyp(i, isExtensible)) =
	raise Crash.Crash "TranslationPhase.trTyp: AbsTyp"

    and trTypRep t =
	if !Switches.Bootstrap.rttLevel = Switches.Bootstrap.NO_RTT then
	    O.FailExp(typInfo(#region(I.infoTyp t), typ_typ))
	else
	    case trTypRep' t
	      of NONE    => trTyp' t
	       | SOME e' => e'

    and trTypRep'(I.FunTyp(i,x,t)) = trTypRep' t

      | trTypRep'(I.AbsTyp(i, isExtensible)) =
	(* [abstract] = Type.inCon(<<kind>>, <<isExtensible>>,
	 *                         Path.fromLab(Label.fromString <<name>>)) *)
	let
	    val r   = #region i
	    val e1' = trKind r (Type.kind(#typ i))
	    val e2' = trSort r (if isExtensible then Type.OPEN else Type.CLOSED)
	    val a   = Path.toLab(#3(Type.asCon'(#typ i)))
	    val lit = O.StringLit(String.toWide(Label.toString a))
	    val e3' = pathOp(lab_fromLab, labOp(lab_fromString,
					O.LitExp(typInfo(r,typ_string), lit)))
	in
	    SOME(typOp(lab_inCon, O.TupExp(typInfo(r,typ_con), #[e1',e2',e3'])))
	end

      | trTypRep' t = NONE


    and trTypRow(I.Row(i,fs,b)) =
	(* [f1,...,fn]     = [f1](...[fn](Type.emptyRow())...)
	 * [f1,...,fn,...] = [f1](...[fn](Type.unknownRow())...) *)
	let
	    val a   = if b then lab_unknownRow else lab_emptyRow
	    val e'  = rowOp(a, O.TupExp(typInfo(#region i, typ_unit), #[]))
	in
	    Vector.foldr trTypField e' fs
	end

    and trTypField(I.Field(i1, I.Lab(i2,a), t), e3') =
	(* [a:t](e3) = Type.extendRow(Lab.fromString "a", [t], e3) *)
	let
	    val r   = #region i1
	    val l'  = O.StringLit(String.toWide(Label.toString a))
	    val e1' = labOp(lab_fromString,
			    O.LitExp(typInfo(#region i2, typ_string), l'))
	    val e2' = trTyp' t
	in
	    rowOp(lab_extendRow,
		  O.TupExp(typInfo(r,typ_labtypVecrow), #[e1',e2',e3']))
	end

    and trBindTyp a (i,x,t,k) =
	let
	    val r  = #region i
	    val x' = trTypid x
	    val d' = O.ValDec(nonInfo r,
			      O.VarPat(typInfo(#region(I.infoId x),typ_var),x'),
			      varOp(lab_var, trKind r k))
	    val i' = O.infoId x'
	    val e1'= O.VarExp(i', O.ShortId(i',x'))
	    val e' = typOp(a, O.TupExp(typInfo(r,typ_vartyp), #[e1', trTyp' t]))
	in
	    O.LetExp(O.infoExp e', #[d'], e')
	end


  (* Interfaces *)

    and trInf j =
	if !Switches.Bootstrap.rttLevel <> Switches.Bootstrap.FULL_RTT then
	    O.FailExp(typInfo(#region(I.infoInf j), typ_inf))
	else
	    trInf' j

    and trInf'(I.TopInf(i)) =
	(* [any] = Inf.inTop() *)
	infOp(lab_inTop, O.TupExp(typInfo(#region i, typ_unit), #[]))

      | trInf'(I.PrimInf(i,s)) =
	(* [prim s] = Inf.inCon(PervasiveInf.lookup s) *)
	let
	    val e1' = O.LitExp(typInfo(#region i, typ_string),
			       O.StringLit(String.toWide s))
	in
	    infOp(lab_inCon, pervInfOp(lab_lookup, e1'))
	end

      | trInf'(I.ConInf(i,y)) =
	(* [y] = Inf.instance{[y]} *)
	infOp(lab_instance, O.VarExp(typInfo(#region i,typ_inf), trInflongid y))

      | trInf'(I.SigInf(i,ss)) =
	(* [sig ss end] = let val s = Inf.empty() in [ss](s) end *)
	let
	    val i'  = typInfo(#region i, typ_sign)
	    val s'  = O.Id(i', Stamp.new(), Name.InId)
	    val e1' = O.TupExp(typInfo(#region i, typ_unit), #[])
	    val d'  = O.ValDec(nonInfo(#region i),
			       O.VarPat(i', s'),
			       sigOp(lab_empty, e1'))
	    val e2' = infOp(lab_inSig, O.VarExp(i', O.ShortId(i', s')))
	    val e'  = trSpecs(O.ShortId(i',s'), e2', ss)
	in
	    O.LetExp(i', #[d'], e')
	end

      | trInf'(I.FunInf(i,x,j1,j2)) =
(*UNFINISHED: have to bind structure trModid(x) *)
	(* [fn(x:j1) => j2] = let val p = Path.invent()
	 *                        val {[x]} = ???
	 *                    in Inf.inLambda(p,[j1],[j2]) end
	 *)
	trBindInf lab_inLambda (i,x,j1,j2)

      | trInf'(I.AppInf(i,j,m)) =
	(* [j m] = Inf.inApply([j],[m],???) *)
	(*UNFINISHED*)
	let
	    val r   = #region i
	    val e1' = trInf' j
	    val e2' = trMod m			(* V UNFINISHED! V *)
	    val e3' = infOp(lab_inTop, O.TupExp(typInfo(r, typ_unit), #[]))
	    val e'  = O.TupExp(typInfo(r, typ_infpathinf), #[e1',e2',e3'])
	in
	    infOp(lab_inApply, e')
	end

      | trInf'(I.CompInf(i,j1,j2)) =
	(* [j1 where j2] = Inf.intersect([j1],[j2]) *)
	let
	    val e' = O.TupExp(typInfo(#region i, typ_infinf),
			      #[trInf j1, trInf j2])
	in
	    infOp(lab_intersect, e')
	end

      | trInf'(I.ArrInf(i,x,j1,j2)) =
(*UNFINISHED: have to bind structure trModid(x) *)
	(* [fct(x:j1) -> j2] = let val p = Path.invent()
	 *                         val {[x]} = ???
	 *                     in Inf.inArrow(p,[j1],[j2]) end
	 *)
	trBindInf lab_inArrow (i,x,j1,j2)

      | trInf'(I.LetInf(i,ds,j)) =
	(* [let ds in j end] = let [ds] in [j] end *)
	O.LetExp(typInfo(#region i, typ_inf), trDecs ds, trInf' j)

      | trInf'(I.SingInf(i,m)) =
	(* [sing m] = Inf.inSing[m] *)
	(*UNFINISHED*)
	unfinished (#region i) "trTyp" "runtime singleton interfaces"

      | trInf'(I.AbsInf(i)) =
	raise Crash.Crash "TranslationPhase.trInf: AbsInf"

    and trInfRep j =
	if !Switches.Bootstrap.rttLevel <> Switches.Bootstrap.FULL_RTT then
	    O.FailExp(typInfo(#region(I.infoInf j), typ_inf))
	else
	    case trInfRep'(fn k' => k', j)
	      of NONE    => trInf' j
	       | SOME e' => e'

    and trInfRep'(mkKind, I.FunInf(i,x,j1,j2)) =
(*UNFINISHED: have to bind structure trModid(x) *)
	(* [fn(x:j1) => j2](MkKind) = [j2](fn k => MkKind
	 *                                 let val p = Path.invent()
	 *                                     val {[x]} = ????
	 *                                 in Inf.inDependent(p,[j1],<k>) end)
	 *)
	let
	    fun mkKind' k' =
		let
		    val r   = #region(I.infoId x)
		    val i'  = nonInfo(#region i)
		    val i1' = typInfo(r, typ_path)
		    val p'  = O.Id(i1', Stamp.new(), Name.InId)
		    val e1' = pathOp(lab_invent,
				     O.TupExp(typInfo(r,typ_unit), #[]))
		    val d1' = O.ValDec(i', O.VarPat(i1',p'), e1')
		    val x'  = trModid x
		    (*val i2' = typInfo(r, ???)
		    val d2' = O.ValDec(i', O.VarPat(i2',x'), ??)*)
		    val e3' = O.VarExp(i1', O.ShortId(i1', p'))
		    val e'  = kindOp(lab_inDependent,
				     O.TupExp(typInfo(#region i,typ_pathinfinf),
					      #[e3', trInf' j1, k']))
		in
		    mkKind(O.LetExp(O.infoExp e', #[d1'(*,d2'*)], e'))
		end
	in
	    trInfRep'(mkKind', j2)
	end

      | trInfRep'(mkKind, I.AbsInf(i)) =
	(* [abstract]_MkKind = Inf.inCon(MkKind<Inf.inGround()>,
	 *                               Path.invent())
	 *)
	let
	    val r   = #region i
	    val a   = Path.toLab(#2(Inf.asCon(#inf i)))
	    val lit = O.StringLit(String.toWide(Label.toString a))
	    val e1' = mkKind(kindOp(lab_inGround,
				    O.TupExp(typInfo(r,typ_unit), #[])))
	    val e2' = pathOp(lab_fromLab, labOp(lab_fromString,
					O.LitExp(typInfo(r,typ_string), lit)))
	in
	    SOME(typOp(lab_inCon, O.TupExp(typInfo(r, typ_icon), #[e1',e2'])))
	end

      | trInfRep'(mkKind, j) = NONE

    and trBindInf a (i,x,j1,j2) =
(*UNFINISHED: have to bind structure trModid(x) *)
	let
	    val r   = #region(I.infoId x)
	    val i'  = nonInfo(#region i)
	    val i1' = typInfo(r, typ_path)
	    val p'  = O.Id(i1', Stamp.new(), Name.InId)
	    val e1' = pathOp(lab_invent, O.TupExp(typInfo(r, typ_unit), #[]))
	    val d1' = O.ValDec(i', O.VarPat(i1',p'), e1')
	    val x'  = trModid x
	    (*val i2' = typInfo(r, ???)
	    val d2' = O.ValDec(i', O.VarPat(i2',x'), ??)*)
	    val e3' = O.VarExp(i1', O.ShortId(i1', p'))
	    val e'  = infOp(a, O.TupExp(typInfo(#region i, typ_pathinfinf),
					#[e3', trInf' j1, trInf j2]))
	in
	    O.LetExp(O.infoExp e', #[d1'(*,d2'*)], e')
	end


  (* Declarations *)

    and trDecs ds			= Vector.rev
					      (Vector.fromList(trDecs'(ds, [])))
    and trDecs'(ds, ds')		= Vector.foldl trDec ds' ds
    and trDec(I.ValDec(i,p,e), ds')	= O.ValDec(i, trPat p, trExp e) :: ds'
      | trDec(I.TypDec(i,x,t), ds')	= let val r  = #region(I.infoId x)
					      val e' = trTypRep t
					      val t  = typ_typ
					  in O.ValDec(i, O.VarPat(typInfo(r,t),
							trTypid x), e')
					  end :: ds'
      | trDec(I.ModDec(i,x,m), ds')	= let val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e')
					  in O.ValDec(i, O.VarPat(typInfo(r,t),
							 trModid x), e')
					  end :: ds'
					  before
					  updatePervasive(x, #inf(I.infoMod m))
      | trDec(I.InfDec(i,x,j), ds')	= let val r  = #region(I.infoId x)
					      val e' = trInfRep j
					      val t  = typ_inf
					  in O.ValDec(i, O.VarPat(typInfo(r,t),
							trInfid x), e')
					  end :: ds'
      | trDec(I.FixDec(i,x,q), ds')	= ds'
      | trDec(I.VarDec(i,x,d), ds')	= trDec(d, ds')
      | trDec(I.RecDec(i,ds), ds')	= O.RecDec(i, trRecDecs ds) ::
					      trRHSRecDecs ds @
					      trLHSRecDecs ds @ ds'
      | trDec(I.LocalDec(i,ds), ds')	= trDecs'(ds, ds')


    and trRecDecs ds			= Vector.rev
					   (Vector.fromList(trRecDecs'(ds, [])))
    and trRecDecs'(ds, ds')		= Vector.foldl trRecDec ds' ds
    and trRecDec(I.TypDec(i,x,t), ds')	= ds'
      | trRecDec(d,ds')			= trDec(d,ds')


    and trLHSRecDecs ds			= List.rev(trLHSRecDecs'(ds, []))
    and trLHSRecDecs'(ds, ds')		= Vector.foldl trLHSRecDec ds' ds
    and trLHSRecDec(I.TypDec(i,x,t), ds')
					= let val r  = #region i
					      val i' = typInfo(r, typ_typ)
					  in O.ValDec(nonInfo r,
						O.VarPat(i', trTypid x),
						if !Switches.Bootstrap.rttLevel=
						    Switches.Bootstrap.NO_RTT
						then O.FailExp(i')
						else typOp(lab_unknown,
						   trKind r (Type.kind
							 (#typ(I.infoTyp t)))))
					  end :: ds'
      | trLHSRecDec(_, ds')		= ds'

    and trRHSRecDecs ds			= List.rev(trRHSRecDecs'(ds, []))
    and trRHSRecDecs'(ds, ds')		= Vector.foldl trRHSRecDec ds' ds
    and trRHSRecDec(I.TypDec(i,x,t), ds')
					= if !Switches.Bootstrap.rttLevel =
					      Switches.Bootstrap.NO_RTT
					  then ds' else
					  let val r   = #region i
					      val i'  = typInfo(r, typ_typ)
					      val y'  = O.ShortId(i', trTypid x)
					      val e1' = O.VarExp(i', y')
					      val e2' = trTypRep t
					      val i'' = typInfo(r, typ_typtyp)
					      val e'  = O.TupExp(i'',#[e1',e2'])
					  in O.ValDec(nonInfo r,
						 O.JokPat(typInfo(r,typ_unit)),
						 unitTypOp(lab_fill, e'))
					  end :: ds'
      | trRHSRecDec(_, ds')		= ds'


  (* Specifications *)

    and trSpecs(x', e', ss) = e'	(*UNFINISHED*)
(*(**)

    and trSpecs (x', e', ss)		= trSpecs'(x',e',ss, Vector.length ss-1)
    and trSpecs'(x', e', ss, ~1)	= e'
      | trSpecs'(x', e', ss, n)		= let val s   = Vector.sub(ss,n)
					      val e2' = trSpec(x', s, e')
					  in trSpecs'(x', e2', ss, n-1) end
	    
    and trSpec(s', I.ValSpec(i,x,t), e') =
	(* [val x : t](s,e) = (Inf.extendVal(s,
	 *                           Inf.newVal(s,Label.fromString[{x}]),
	 *                           [t], NONE)
	 *                    ; e)
	 *)
	let
	    val r   = #region i
	    val n   = Name.toString(name x)
	    val a'  = labOp(lab_fromString, O.StringLit(String.toWide n))
	    val e1' = pathInfOp(lab_newVal,
				O.TupExp(typInfo(r, typ_signlab), #[s',a']))
	    val e2' = unitInfOp(lab_extendVal,
				O.TupExp(typInfo(r, typ_signlab), #[s',a']))
	in
	
	end

    O.ValSpec(i, trValid x, trTyp t)::ds'
      | trSpec(I.TypSpec(i,x,t), ds')	= let val r  = #region(I.infoId x)
					      val e' = trTypRep t
					      val t  = typ_typ
					  in O.ValSpec(i, O.VarPat(typInfo(r,t),
							trTypid x), e')
					  end :: ds'
      | trSpec(I.ModSpec(i,x,m), ds')	= let val r  = #region(I.infoId x)
					      val e' = trMod m
					      val t  = #typ(O.infoExp e')
					  in O.ValSpec(i, O.VarPat(typInfo(r,t),
							 trModid x), e')
					  end :: ds'
					  before
					  updatePervasive(x, #inf(I.infoMod m))
      | trSpec(I.InfSpec(i,x,j), ds')	= let val r  = #region(I.infoId x)
					      val e' = trInfRep j
					      val t  = typ_inf
					  in O.ValSpec(i, O.VarPat(typInfo(r,t),
							trInfid x), e')
					  end :: ds'
      | trSpec(I.FixSpec(i,x,q), ds')	= ds'
      | trSpec(I.VarSpec(i,x,d), ds')	= trSpec(d, ds')
      | trSpec(I.RecSpec(i,ds), ds')	= O.RecSpec(i, trRecSpecs ds) ::
					      trRHSRecSpecs ds @
					      trLHSRecSpecs ds @ ds'
      | trSpec(I.LocalSpec(i,ds), ds')	= trSpecs'(ds, ds')


    and trRecSpecs ds			= Vector.rev
					   (Vector.fromList(trRecSpecs'(ds, [])))
    and trRecSpecs'(ds, ds')		= Vector.foldl trRecSpec ds' ds
    and trRecSpec(I.TypSpec(i,x,t), ds')	= ds'
      | trRecSpec(d,ds')			= trSpec(d,ds')


    and trLHSRecSpecs ds			= List.rev(trLHSRecSpecs'(ds, []))
    and trLHSRecSpecs'(ds, ds')		= Vector.foldl trLHSRecSpec ds' ds
    and trLHSRecSpec(I.TypSpec(i,x,t), ds')
					= let val r  = #region i
					      val i' = typInfo(r, typ_typ)
					  in O.ValSpec(nonInfo r,
						O.VarPat(i', trTypid x),
						if !Switches.Bootstrap.rttLevel=
						    Switches.Bootstrap.NO_RTT
						then O.FailExp(i')
						else typOp(lab_unknown,
						   trKind r (Type.kind
							 (#typ(I.infoTyp t)))))
					  end :: ds'
      | trLHSRecSpec(_, ds')		= ds'

    and trRHSRecSpecs ds			= List.rev(trRHSRecSpecs'(ds, []))
    and trRHSRecSpecs'(ds, ds')		= Vector.foldl trRHSRecSpec ds' ds
    and trRHSRecSpec(I.TypSpec(i,x,t), ds')
					= if !Switches.Bootstrap.rttLevel =
					      Switches.Bootstrap.NO_RTT
					  then ds' else
					  let val r   = #region i
					      val i'  = typInfo(r, typ_typ)
					      val y'  = O.ShortId(i', trTypid x)
					      val e1' = O.VarExp(i', y')
					      val e2' = trTypRep t
					      val i'' = typInfo(r, typ_typtyp)
					      val e'  = O.TupExp(i'',#[e1',e2'])
					  in O.ValSpec(nonInfo r,
						 O.JokPat(typInfo(r,typ_unit)),
						 unitTypOp(lab_fill, e'))
					  end :: ds'
      | trRHSRecSpec(_, ds')		= ds'
*)


  (* Imports and annotations *)

    fun trAnns a_s =
	let
	    val (rxsus',ds') = Vector.foldl trAnn ([],[]) a_s
	    val  xsus'       = Vector.rev(Vector.fromList rxsus')
	    val  ds''        = Vector.rev(Vector.fromList ds')
	in
	    ( xsus', ds'' )
	end

    and trAnn(I.ImpAnn(i,is,u),(xsus',ds')) =
	let
	    val r    = #region i
	    val s    = #sign i
	    val t    = infToTyp(Inf.inSig s)
	    val x'   = O.Id(typInfo(r,t), Stamp.new(), Name.InId)
	    val y'   = O.ShortId(typInfo(r,t), x')
	    val ds'' = trImps(is, y', t, ds')
	    val _    = Inf.stripSig s
	in
	    ( (x',s,u)::xsus', ds'' )
	end

    and trImps(is, y, t, ds')		= Vector.foldl(trImp y t) ds' is
    and trImp y t (I.ValImp(i,x,d),ds')	= idToDec(trValid x, y, t,
						  #typ(I.infoDesc d)) :: ds'
      | trImp y t (I.TypImp(i,x,d),ds')	= idToDec(trTypid x, y, t, typ_typ)::ds'
      | trImp y t (I.ModImp(i,x,d),ds')	= idToDec(trModid x, y, t,
						  infToTyp(#inf(I.infoDesc d)))
					  :: ds'
					  before
					  updatePervasive(x, #inf(I.infoDesc d))
      | trImp y t (I.InfImp(i,x,d),ds')	= idToDec(trInfid x, y, t, typ_inf)::ds'
      | trImp y t (I.FixImp(i,x,d),ds')	= ds'
      | trImp y t (I.RecImp(i,is), ds')	= trImps(is, y, t, ds')


  (* Components *)

    fun trComp(I.Comp(i,a_s,ds)) =
	let
	    val (xsus',ds1') = trAnns a_s
	    val  ds2'        = trDecs ds
	    val  ds'         = Vector.append(ds1',ds2')
	    val  ids'        = ids ds2'
	    val  fs'         = Vector.map idToField ids'
	    val  r           = Vector.foldl idToRow (Type.emptyRow()) ids'
	    val  i'          = typInfo(#region i, Type.inProd r)
	    val  exp'        = O.LetExp(i', ds', O.ProdExp(i', fs'))
	    val  s           = #sign i
	    val  _           = Inf.stripSig s
	in
	    ( xsus', (exp',s) )
	end

    fun translate () (desc, component) =
	let
	    val _     = InfHash.deleteAll infHash
	    val comp' = trComp component
	in
	    if not(!Switches.Debug.checkIntermediate) then () else
		CheckIntermediate.check comp';
	    comp'
	end
end
