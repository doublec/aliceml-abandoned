(* UNFINISHED:
   - packages
   - appropriate treatment of value paths
*)

(*
 * Note: We assume complete binding analysis and alpha conversion has been
 * performed on the input program. So we would not need to bother with scoping.
 * Nevertheless, we sometimes use scopes to reduce the size of the symbol
 * table.
 *)

functor MakeElaborationPhase(Composer: COMPOSER where type Sig.t = Inf.sign)
    :> ELABORATION_PHASE =
struct

    structure C = Env
    structure I = AbstractGrammar
    structure O = TypedGrammar
    structure E = ElaborationError

    open Env
    open TypedInfo

    nonfix mod

  (* Error *)

    val error = E.error

  (* Under construction... *)

    fun unfinished i funname casename =
	Error.warn(i, "Elab." ^ funname ^ ": " ^ casename ^ " not checked yet")


  (* Predefined types *)

    fun unitTyp E	= PervasiveType.typ_unit
    fun boolTyp E	= PervasiveType.typ_bool
    fun exnTyp E	= PervasiveType.typ_exn

    (* UNFINISHED: overloading *)
    fun intTyp E	= PervasiveType.typ_int
    fun wordTyp E	= PervasiveType.typ_word
    fun charTyp E	= PervasiveType.typ_char
    fun stringTyp E	= PervasiveType.typ_string
    fun realTyp E	= PervasiveType.typ_real

    fun refTyp(E,t)	= Type.inApply(PervasiveType.typ_ref, t)
    fun vecTyp(E,t)	= Type.inApply(PervasiveType.typ_vec, t)
    fun listTyp(E,t)	= Type.inApply(PervasiveType.typ_list, t)

    fun conarrowTyp(E,t1,t2) =
	Type.inApply(Type.inApply(PervasiveType.typ_conarrow,
		Type.inArrow(t1,t2)), Type.unknown(Type.STAR))


  (* Unification of vectors *)

    exception UnifyVec of int * typ * typ

    fun unifyVec ts =
	if Vector.length ts = 0 then
	    ()
	else let
	    val t0 = Vector.sub(ts,0)
	in
	    Vector.appi (fn(i,t) => Type.unify(t0,t) handle Type.Unify(t1,t2) =>
				    raise UnifyVec(i,t1,t2)) (ts,1,NONE)
	end


  (* Check value restriction *)

    fun isValue( I.LitExp _
	       | I.VarExp _
	       | I.PrimExp _
	       | I.LabExp _
	       | I.NewExp _
	       | I.FunExp _
	       | I.FailExp _ )			= true
      | isValue( I.TagExp(_, _, _, exp)
	       | I.ConExp(_, _, exp)
	       | I.RefExp(_, exp)
	       | I.SelExp(_, _, exp)
	       | I.AnnExp(_, exp, _) )		= isValue exp
      | isValue( I.TupExp(_, exps)
	       | I.VecExp(_, exps) )		= Vector.all isValue exps
      | isValue( I.ProdExp(_, exprow) )		= isValueRow exprow
      | isValue( I.UpdExp(_, exp, exprow) )	= isValue exp andalso
						  isValueRow exprow
      | isValue( I.AndExp(_, exp1, exp2)
	       | I.OrExp(_, exp1, exp2) )	= isValue exp1 andalso
						  isValue exp2
      | isValue( I.IfExp (_, exp1, exp2, exp3))	= isValue exp1 andalso
						  isValue exp2 andalso
						  isValue exp3
      | isValue( I.AppExp _
	       | I.SeqExp _
	       | I.CaseExp _
	       | I.RaiseExp _
	       | I.HandleExp _
	       | I.LetExp _
	       | I.PackExp _ )			= false

    and isValueRow(I.Row(_, fields, _))		= Vector.all isValueField fields
    and isValueField(I.Field(_, _, exp))	= isValue exp


  (* Literals *)

    fun elabLit(E, I.IntLit n)		= ( intTyp E, O.IntLit n )
      | elabLit(E, I.WordLit w)		= ( wordTyp E, O.WordLit w )
      | elabLit(E, I.CharLit c)		= ( charTyp E, O.CharLit c )
      | elabLit(E, I.StringLit s)	= ( stringTyp E, O.StringLit s )
      | elabLit(E, I.RealLit x)		= ( realTyp E, O.RealLit x )

  (* Fixity *)

    fun elabFix(E, I.Fix(i,f))		= (f, O.Fix(fixInfo(i,f), f))

  (* Rows (polymorphic, thus put here) *)

    fun elabLab(E, I.Lab(i, a)) = ( a, O.Lab(nonInfo(i), a) )

    fun elabRow(elabX, E, I.Row(i, fields, b)) =
	let
	    val  r0         = (if b then Type.unknownRow else Type.emptyRow)()
	    val (r,fields') = elabFields(elabX, E, r0, fields)
	in
	    ( r, O.Row(nonInfo(i), fields', b) )
	end

    and elabField(elabX, E, I.Field(i, vallab, x)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val (t,x')      = elabX(E, x)
	in
	    ( a, t, O.Field(nonInfo(i), vallab', x') )
	end

    and elabFields(elabX, E, r, fields) =
	let
	    val atfields' = Vector.map (fn fld => elabField(elabX,E,fld)) fields
	    val fields'   = Vector.map #3 atfields'
	    val r         = Vector.foldl
				(fn((a,t,_), r) => Type.extendRow(a,t,r))
				r atfields'
	in
	    ( r, fields' )
	end


  (* Value identifiers *)

    fun elabValid_bind(E, s, valid as I.Id(i, stamp, name)) =
	let
	    val  p      = Inf.newVal(s, Label.fromName name
					handle Domain => Label.fromInt 0)
	    val  t      = Type.unknown(Type.STAR)
	    (*UNFINISHED: use punning: *)
	    val (p',t') = ( insertVal(E, stamp, {id=valid, path=p, typ=t})
			  ; (p,t) )
			  handle Collision _ =>	(* val rec or alt pat *)
			    let val {path=p', typ=t', ...} = lookupVal(E, stamp)
			    in (p',t') end
	in
	    ( t', p', O.Id(typInfo(i,t'), stamp, name) )
	end

    fun elabValid(E, I.Id(i, stamp, name)) =
	let
	    val t = #typ(lookupVal(E, stamp))
	in
	    ( t, O.Id(typInfo(i,t), stamp, name) )
	end

    and elabVallongid(E, I.ShortId(i, id)) =
	let
	    val (t,valid') = elabValid(E, id)
	in
	    ( t, O.ShortId(typInfo(i,t), valid') )
	end

      | elabVallongid(E, I.LongId(i, modlongid, vallab)) =
	let
	    val (s,modlongid') = elabModlongid_path(E, modlongid)
	    val (l,vallab')    = elabLab(E, vallab)
	    val  t             = Inf.lookupVal(s, l)
	in
	    ( t, O.LongId(typInfo(i,t), modlongid', vallab') )
	end

    and elabVallongid_path(E, I.ShortId(_, I.Id(_, stamp, _))) =
	    #path(lookupVal(E, stamp))

      | elabVallongid_path(E, I.LongId(_, modlongid, I.Lab(_, l))) =
	let
	    val (s,_) = elabModlongid_path(E, modlongid)
	in
	    Inf.lookupValPath(s, l)
	end


  (* Expressions *)

    and elabExp(E, I.LitExp(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitExp(typInfo(i,t), lit') )
	end

      | elabExp(E, I.VarExp(i, vallongid)) =
	let
	    val (t,vallongid') = elabVallongid(E, vallongid)
	    val  t'            = Type.instance t
	in
	    ( t', O.VarExp(typInfo(i,t'), vallongid') )
	end

      | elabExp(E, I.PrimExp(i, s, typ)) =
	let
	    val (t,typ') = elabStarTyp(E, typ)
	in
	    ( t, O.PrimExp(typInfo(i,t), s, typ') )
	end

      | elabExp(E, I.LabExp(i, lab, typ)) =
	(*UNFINISHED: check that label is in domain of record type*)
	let
	    val (a,lab') = elabLab(E, lab)
	    val (t,typ') = elabStarTyp(E, typ)
	in
	    ( t, O.LabExp(typInfo(i,t), lab', typ') )
	end

      | elabExp(E, I.NewExp(i, typ)) =
	(*UNFINISHED: check that type is open*)
	let
	    val (t,typ') = elabStarTyp(E, typ)
	in
	    ( t, O.NewExp(typInfo(i,t), typ') )
	end

      | elabExp(E, I.TagExp(i, vallab, NONE, exp)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val (t1,exp')   = elabExp(E, exp)
	    val  r          = Type.extendRow(a, t1, Type.unknownRow())
	    val  t          = Type.inSum r
	in
	    ( t, O.TagExp(typInfo(i,t), vallab', NONE, exp') )
	end

      | elabExp(E, I.TagExp(i, vallab, SOME vallongid, exp)) =
	let
	    (*UNFINISHED: check consistency of label*)
	    val (a,vallab')     = elabLab(E, vallab)
	    val (t1,vallongid') = elabVallongid(E, vallongid)
	    val  t1'            = Type.instance t1
	    val (t2,exp')       = elabExp(E, exp)
	    val  t11            = Type.unknown(Type.STAR)
	    val  t12            = Type.unknown(Type.STAR)
	    val  t3             = conarrowTyp(E, t11, t12)
	    val  _              = Type.unify(t3,t1') handle Type.Unify(t4,t5) =>
				    error(I.infoLongid vallongid,
					  E.TagExpConUnify(t3, t1', t4, t5))
	    val  _              = Type.unify(t11,t2) handle Type.Unify(t4,t5) =>
				    error(i, E.TagExpArgUnify(t11, t2, t4, t5))
	in
	    ( t12, O.TagExp(typInfo(i,t12), vallab', SOME vallongid', exp') )
	end

      | elabExp(E, I.ConExp(i, vallongid, exp)) =
	let
	    val (t1,vallongid') = elabVallongid(E, vallongid)
	    val  t1'            = Type.instance t1
	    val (t2,exp')       = elabExp(E, exp)
	    val  t11            = Type.unknown(Type.STAR)
	    val  t12            = Type.unknown(Type.STAR)
	    val  t3             = conarrowTyp(E, t11, t12)
	    val  _              = Type.unify(t3,t1') handle Type.Unify(t4,t5) =>
				    error(I.infoLongid vallongid,
					  E.ConExpConUnify(t3, t1', t4, t5))
	    val  _              = Type.unify(t11,t2) handle Type.Unify(t4,t5) =>
				    error(i, E.ConExpArgUnify(t11, t2, t4, t5))
	in
	    ( t12, O.ConExp(typInfo(i,t12), vallongid', exp') )
	end

      | elabExp(E, I.RefExp(i, exp)) =
	let
	    val (t1,exp') = elabExp(E, exp)
	    val  t        = refTyp(E, t1)
	in
	    ( t, O.RefExp(typInfo(i,t), exp') )
	end

      | elabExp(E, I.TupExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.ProdExp(i, exprow)) =
	let
	    val (r,exprow') = elabRow(elabExp, E, exprow)
	    val  t          = Type.inProd r
	in
	    ( t, O.ProdExp(typInfo(i,t), exprow') )
	end

      | elabExp(E, I.UpdExp(i, exp, exprow)) =
	let
	    val (t1,exp')    = elabExp(E, exp)
	    val (r,exprow')  = elabRow(elabExp, E, exprow)
	    val  t2          = Type.inProd(Type.openRow r)
	    val  _           = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.UpdExpUnify(t2, t1, t4, t3))
	in
	    ( t1, O.UpdExp(typInfo(i,t1), exp', exprow') )
	end

      | elabExp(E, I.SelExp(i, vallab, exp')) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val (t1,exp')   = elabExp(E, exp')
	    val  t          = Type.unknown(Type.STAR)
	    val  r          = Type.extendRow(a, t, Type.unknownRow())
	    val  t2         = Type.inProd r
	    val  _          = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.SelExpUnify(t1, t2, t3, t4))
	in
	    ( t, O.SelExp(typInfo(i,t), vallab', exp') )
	end

      | elabExp(E, I.VecExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t0        = case ts of #[] => Type.unknown(Type.STAR)
				      |  _  => Vector.sub(ts,0)
	    val  t         = vecTyp(E, t0)
	    val  _         = unifyVec ts handle UnifyVec(n,t1,t2) =>
				error(I.infoExp(Vector.sub(exps,n)),
				      E.VecExpUnify(t, Vector.sub(ts,n), t1,t2))
	in
	    ( t, O.VecExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.FunExp(i, matchs)) =
	let
	    val  t1          = Type.unknown(Type.STAR)
	    val (t2,matchs') = elabMatchs(E, t1, matchs)
	    val  t           = Type.inArrow(t1,t2)
	in
	    ( t, O.FunExp(typInfo(i,t), matchs') )
	end

      | elabExp(E, I.AppExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t11       = Type.unknown(Type.STAR)
	    val  t12       = Type.unknown(Type.STAR)
	    val  t1'       = Type.inArrow(t11,t12)
	    val  _         = Type.unify(t1,t1') handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.AppExpFunUnify(t1, t1', t3, t4))
	    val  _         = Type.unify(t11,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AppExpArgUnify(t11, t2, t3, t4))
	in
	    ( t12, O.AppExp(typInfo(i,t12), exp1', exp2') )
	end

      | elabExp(E, I.AndExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t         = boolTyp E
	    val  _         = Type.unify(t1,t) handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1, E.AndExpUnify(t1,t,t3,t4))
	    val  _         = Type.unify(t2,t) handle Type.Unify(t3,t4) =>
				error(I.infoExp exp2, E.AndExpUnify(t2,t,t3,t4))
	in
	    ( t, O.AndExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.OrExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t         = boolTyp E
	    val  _         = Type.unify(t1,t) handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1, E.OrExpUnify(t1,t,t3,t4))
	    val  _         = Type.unify(t2,t) handle Type.Unify(t3,t4) =>
				error(I.infoExp exp2, E.OrExpUnify(t2,t,t3,t4))
	in
	    ( t, O.OrExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.IfExp(i, exp1, exp2, exp3)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val (t3,exp3') = elabExp(E, exp3)
	    val  tb        = boolTyp E
	    val  _         = Type.unify(t1,tb) handle Type.Unify(t4,t5) =>
				error(I.infoExp exp1,
				      E.IfExpCondUnify(t1, tb, t4, t5))
	    val  _         = Type.unify(t2,t3) handle Type.Unify(t4,t5) =>
				error(i, E.IfExpBranchUnify(t2, t3, t4, t5))
	in
	    ( t2, O.IfExp(typInfo(i,t2), exp1', exp2', exp3') )
	end

      | elabExp(E, I.SeqExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = Vector.sub(ts, Vector.length ts - 1)
	in
	    ( t, O.SeqExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.CaseExp(i, exp, matchs)) =
	(* UNFINISHED: check for exhaustiveness and redundancy *)
	let
	    val (t1,exp')    = elabExp(E, exp)
	    val (t2,matchs') = elabMatchs(E, t1, matchs)
	in
	    ( t2, O.CaseExp(typInfo(i,t2), exp', matchs') )
	end

      | elabExp(E, I.FailExp(i)) =
	let
	    val  t = Type.unknown(Type.STAR)
	in
	    ( t, O.FailExp(typInfo(i,t)) )
	end

      | elabExp(E, I.RaiseExp(i, exp)) =
	let
	    val (t1,exp') = elabExp(E, exp)
	    val  te       = exnTyp E
	    val  t        = Type.unknown(Type.STAR)
	    val  _        = Type.unify(t1,te) handle Type.Unify(t2,t3) =>
				error(I.infoExp exp,
				      E.RaiseExpUnify(t1, te, t2, t3))
	in
	    ( t, O.RaiseExp(typInfo(i,t), exp') )
	end

      | elabExp(E, I.HandleExp(i, exp, matchs)) =
	(* UNFINISHED: check for redundancy *)
	let
	    val (t1,exp')    = elabExp(E, exp)
	    val (t2,matchs') = elabMatchs(E, exnTyp E, matchs)
	    val  _           = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.HandleExpUnify(t1, t2, t3, t4))
	in
	    ( t1, O.HandleExp(typInfo(i,t1), exp', matchs') )
	end

      | elabExp(E, I.AnnExp(i, exp, typ)) =
	let
	    val (t1,exp') = elabExp(E, exp)
	    val (t2,typ') = elabStarTyp(E, typ)
	    val  _        = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AnnExpUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AnnExp(typInfo(i,t2), exp', typ') )
	end

      | elabExp(E, I.LetExp(i, decs, exp)) =
	let
	    val  _       = insertScope E
	    val  s       = Inf.empty()
	    val  decs'   = elabDecs(E, s, decs)
(*DEBUG*)
(*val _ = Inf.strengthenSig(Path.fromLab(Label.fromString "?let"), s)*)
	    val  _       = Inf.strengthenSig(Path.invent(), s)
	    val (t,exp') = elabExp(E, exp)
	    val  _       = deleteScope E
	in
	    ( t, O.LetExp(typInfo(i,t), decs', exp') )
	end

      | elabExp(E, I.PackExp(i, mod)) =
	let
	    val (j,mod') = elabMod(E, mod)
	    (*UNFINISHED*)
	    val  t       = Type.unknown(Type.STAR)
	in
	    unfinished i "elabExp" "packages";
	    ( t, O.PackExp(typInfo(i,t), mod') )
	end

    and elabExps(E, exps) =
	VectorPair.unzip(Vector.map (fn exp => elabExp(E,exp)) exps)


  (* Matches *)

    and elabMatch(E, t1, t2, I.Match(i, pat, exp)) =
	let
	    val  _        = insertScope E
	    val (t3,pat') = elabPat(E, Inf.empty(), pat)
	    val  _        = Type.unify(t1,t3) handle Type.Unify(t5,t6) =>
				error(I.infoPat pat,
				      E.MatchPatUnify(t1, t3, t5, t6))
	    val (t4,exp') = elabExp(E, exp)
	    val  _        = Type.unify(t2,t4) handle Type.Unify(t5,t6) =>
				error(I.infoExp exp,
				      E.MatchExpUnify(t2, t4, t5, t6))
	    val  _        = deleteScope E
	in
	    O.Match(nonInfo(i), pat', exp')
	end

    and elabMatchs(E, t1, matchs) =
	let
	    val t2 = Type.unknown(Type.STAR)

	    fun elabMatch1 match = elabMatch(E, t1, t2, match)
	in
	    ( t2, Vector.map elabMatch1 matchs )
	end


  (* Patterns *)

    and elabPat(E, s, I.JokPat(i)) =
	let
	    val t = Type.unknown(Type.STAR)
	in
	    ( t, O.JokPat(typInfo(i,t)) )
	end

      | elabPat(E, s, I.LitPat(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitPat(typInfo(i,t), lit') )
	end

      | elabPat(E, s, I.VarPat(i, valid)) =
	let
	    val (t,p,valid') = elabValid_bind(E, s, valid)
	in
	    ( t, O.VarPat(typInfo(i,t), valid') )
	end

      | elabPat(E, s, I.TagPat(i, vallab, NONE, pat)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val (t1,pat')   = elabPat(E, s, pat)
	    val  r          = Type.extendRow(a, t1, Type.unknownRow())
	    val  t          = Type.inSum r
	in
	    ( t, O.TagPat(typInfo(i,t), vallab', NONE, pat') )
	end

      | elabPat(E, s, I.TagPat(i, vallab, SOME vallongid, pat)) =
	let
	    (*UNFINISHED: check consistency of label*)
	    val (a,vallab')     = elabLab(E, vallab)
	    val (t1,vallongid') = elabVallongid(E, vallongid)
	    val  t1'            = Type.instance t1
	    val (t2,pat')       = elabPat(E, s, pat)
	    val  t11            = Type.unknown(Type.STAR)
	    val  t12            = Type.unknown(Type.STAR)
	    val  t3             = conarrowTyp(E, t11, t12)
	    val  _              = Type.unify(t3,t1') handle Type.Unify(t4,t5) =>
				    error(I.infoLongid vallongid,
					  E.TagPatConUnify(t3, t1', t4, t5))
	    val  _              = Type.unify(t11,t2) handle Type.Unify(t4,t5) =>
				    error(i, E.TagPatArgUnify(t11, t2, t4, t5))
	in
	    ( t12, O.TagPat(typInfo(i,t12), vallab', SOME vallongid', pat') )
	end

      | elabPat(E, s, I.ConPat(i, vallongid, pat)) =
	let
	    val (t1,vallongid') = elabVallongid(E, vallongid)
	    val  t1'            = Type.instance t1
	    val (t2,pat')       = elabPat(E, s, pat)
	    val  t11            = Type.unknown(Type.STAR)
	    val  t12            = Type.unknown(Type.STAR)
	    val  t3             = conarrowTyp(E, t11, t12)
	    val  _              = Type.unify(t3,t1') handle Type.Unify(t4,t5) =>
				    error(I.infoLongid vallongid,
					  E.ConPatConUnify(t3, t1', t4, t5))
	    val  _              = Type.unify(t11,t2) handle Type.Unify(t4,t5) =>
				    error(i, E.ConPatArgUnify(t11, t2, t4, t5))
	in
	    ( t12, O.ConPat(typInfo(i,t12), vallongid', pat') )
	end

      | elabPat(E, s, I.RefPat(i, pat)) =
	let
	    val (t1,pat') = elabPat(E, s, pat)
	    val  t        = refTyp(E, t1)
	in
	    ( t, O.RefPat(typInfo(i,t), pat') )
	end

      | elabPat(E, s, I.TupPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, s, pats)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupPat(typInfo(i,t), pats') )
	end

      | elabPat(E, s, I.ProdPat(i, patrow)) =
	let
	    val (r,patrow') = elabRow(fn(E,pat) => elabPat(E,s,pat), E, patrow)
	    val  t          = Type.inProd r
	in
	    ( t, O.ProdPat(typInfo(i,t), patrow') )
	end

      | elabPat(E, s, I.VecPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, s, pats)
	    val  t0        = case ts of #[] => Type.unknown(Type.STAR)
				      |  _  => Vector.sub(ts,0)
	    val  t         = vecTyp(E, t0)
	    val  _         = unifyVec ts handle UnifyVec(n,t1,t2) =>
				error(I.infoPat(Vector.sub(pats,n)),
				      E.VecPatUnify(t, Vector.sub(ts,n), t1,t2))
	in
	    ( t, O.VecPat(typInfo(i,t), pats') )
	end

      | elabPat(E, s, I.AsPat(i, pat1, pat2)) =
	let
	    val (t1,pat1') = elabPat(E, s, pat1)
	    val (t2,pat2') = elabPat(E, s, pat2)
	    val  _         = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AsPatUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AsPat(typInfo(i,t2), pat1', pat2') )
	end

      | elabPat(E, s, I.AltPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, s, pats)
	    val  t         = case ts of #[] => Type.unknown(Type.STAR)
				      |  _  => Vector.sub(ts,0)
	    val  _         = unifyVec ts handle UnifyVec(n,t1,t2) =>
				error(I.infoPat(Vector.sub(pats,n)),
				      E.AltPatUnify(t, Vector.sub(ts,n), t1,t2))
	in
	    ( t, O.AltPat(typInfo(i,t), pats') )
	end

      | elabPat(E, s, I.NegPat(i, pat)) =
	let
	    val (t,pat') = elabPat(E, s, pat)
	in
	    ( t, O.NegPat(typInfo(i,t), pat') )
	end

      | elabPat(E, s, I.GuardPat(i, pat, exp)) =
	let
	    val (t1,pat') = elabPat(E, s, pat)
	    val (t2,exp') = elabExp(E, exp)
	    val  tb       = boolTyp E
	    val  _        = Type.unify(t2,tb) handle Type.Unify(t3,t4) =>
				error(i, E.GuardPatUnify(t2, tb, t3, t4))
	in
	    ( t1, O.GuardPat(typInfo(i,t1), pat', exp') )
	end

      | elabPat(E, s, I.AnnPat(i, pat, typ)) =
	let
	    val (t1,pat') = elabPat(E, s, pat)
	    val (t2,typ') = elabStarTyp(E, typ)
	    val  _        = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AnnPatUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AnnPat(typInfo(i,t2), pat', typ') )
	end

      | elabPat(E, s, I.WithPat(i, pat, decs)) =
	let
	    val (t,pat') = elabPat(E, s, pat)
	    val  decs'   = elabDecs(E, s, decs)
	in
	    ( t, O.WithPat(typInfo(i,t), pat', decs') )
	end


    and elabPats(E, s, pats) =
	VectorPair.unzip(Vector.map (fn pat => elabPat(E,s,pat)) pats)



  (* Type identifiers *)

    and elabVarid_bind'(E, I.Id(i, stamp, name)) =
	    O.Id(nonInfo(i), stamp, name)

    and elabVarid_bind(E, k, varid as I.Id(i, stamp, name)) =
	let
	    val a = Type.var k
	    val _ = insertVar(E, stamp, {id=varid, var=a})
	in
	    ( a, O.Id(nonInfo(i), stamp, name) )
	end

    and elabVarid(E, I.Id(i, stamp, name)) =
	let
	    val a = #var(lookupVar(E, stamp))
	in
	    ( a, O.Id(nonInfo(i), stamp, name) )
	end


    and elabTypid_bind(E, p, t, typid as I.Id(i, stamp, name)) =
	let
	    (*UNFINISHED: sort should be calculated from t *)
	    val t' = Type.inAbbrev(Type.inCon(Type.kind t, Type.CLOSED, p), t)
	    val _  = insertTyp(E, stamp, {id=typid, path=p, typ=t'})
	in
	    O.Id(nonInfo(i), stamp, name)
	end

    and elabTypid(E, I.Id(i, stamp, name)) =
	let
	    val {typ=t, path=p, ...} = lookupTyp(E, stamp)
	in
	    ( t, p, O.Id(nonInfo(i), stamp, name) )
	end

    and elabTyplongid(E, I.ShortId(i, typid)) =
	let
	    val (t,_,typid') = elabTypid(E, typid)
	in
	    ( t, O.ShortId(nonInfo(i), typid') )
	end

      | elabTyplongid(E, I.LongId(i, modlongid, typlab)) =
	let
	    val (s,modlongid') = elabModlongid_path(E, modlongid)
	    val (l,typlab')    = elabLab(E, typlab)
	    val  t             = Inf.lookupTyp(s, l)
	    val  k             = Type.kind t
	    val  p             = Inf.pathTyp(s, l)
	    (*UNFINISHED: sort should be calculated from t *)
	    val  t'            = Type.inAbbrev(Type.inCon(k, Type.CLOSED, p), t)
	in
	    ( t', O.LongId(nonInfo(i), modlongid', typlab') )
	end


  (* Kinds of types (without elaborating the full type) *)

    (* These are needed to elaborate recursive type bindings.
     * ASSUMPTION: under recursion we do not have higher-order bindings.
     * ASSUMPTION: type lambdas are first order.
     *)

    and elabTypKind(E, I.FunTyp(i, typid, typ)) =
	let
	    val k = elabTypKind(E, typ)
	in
	    Type.ARROW(Type.STAR,k)
	end

      | elabTypKind(E, I.ConTyp(i, typlongid)) =
	let
	    val (t,_) = elabTyplongid(E, typlongid)
	in
	    Type.kind t
	end

      | elabTypKind(E, I.VarTyp(i, typid)) =
	let
	    val (a,_) = elabVarid(E, typid)
	in
	    Type.kindVar a
	end

      | elabTypKind(E, I.AppTyp(i, typ1, typ2)) =
	let
	    val k = elabTypKind(E, typ1)
	in
	    case k
	      of Type.ARROW(k1,k2) => k2
	       | _                 => error(i, E.AppTypFunKind(k))
	end

      | elabTypKind(E, _) =
	    Type.STAR


  (* Types *)

    and elabTyp(E, I.VarTyp(i, typid)) =
	let
	    val (a,typid') = elabVarid(E, typid)
	    val  t         = Type.inVar a
	in
	    ( t, O.VarTyp(typInfo(i,t), typid') )
	end

      | elabTyp(E, I.ConTyp(i, typlongid)) =
	let
	    val (t,typlongid') = elabTyplongid(E, typlongid)
	in
	    ( t, O.ConTyp(typInfo(i,t), typlongid') )
	end

      | elabTyp(E, I.FunTyp(i, typid, typ)) =
	let
	    val (a,typid') = elabVarid_bind(E, Type.STAR, typid)
	    val (t1,typ')  = elabTyp(E, typ)
	    val  t         = Type.inLambda(a,t1)
	in
	    ( t, O.FunTyp(typInfo(i,t), typid', typ') )
	end

      | elabTyp(E, I.AppTyp(i, typ1, typ2)) =
	let
	    val (t1,typ1') = elabTyp(E, typ1)
	    val (t2,typ2') = elabTyp(E, typ2)
	    val  k1        = Type.kind t1
	    val  k2        = Type.kind t2
	    val  _         = case k1
			       of Type.STAR =>
					error(i, E.AppTypFunKind(k1))
				| Type.ARROW(k11,k12) =>
				    if k11 = k2 then () else
					error(i, E.AppTypArgKind(k11, k2))
	    val  t         = Type.inApply(t1,t2)
	in
	    ( t, O.AppTyp(typInfo(i,t), typ1', typ2') )
	end

      | elabTyp(E, I.RefTyp(i, typ)) =
	let
	    val (t1,typ') = elabTyp(E, typ)
	    val  _        = case Type.kind t1 of Type.STAR => () | k =>
				error(I.infoTyp typ, E.RefTypKind(k))
	    val  t        = refTyp(E, t1)
	in
	    ( t, O.RefTyp(typInfo(i,t), typ') )
	end

      | elabTyp(E, I.TupTyp(i, typs)) =
	let
	    val (ts,typs') = elabStarTyps(E, typs)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupTyp(typInfo(i,t), typs') )
	end

      | elabTyp(E, I.ProdTyp(i, typrow)) =
	let
	    val (r,typrow') = elabRow(elabStarTyp, E, typrow)
	    val  t          = Type.inProd r
	in
	    ( t, O.ProdTyp(typInfo(i,t), typrow') )
	end

      | elabTyp(E, I.SumTyp(i, typrow)) =
	let
	    val (r,typrow') = elabRow(elabStarTyp, E, typrow)
	    val  t          = Type.inSum r
	in
	    ( t, O.SumTyp(typInfo(i,t), typrow') )
	end

      | elabTyp(E, I.ArrTyp(i, typ1, typ2)) =
	let
	    val (t1,typ1') = elabStarTyp(E, typ1)
	    val (t2,typ2') = elabStarTyp(E, typ2)
	    val  t         = Type.inArrow(t1,t2)
	in
	    ( t, O.ArrTyp(typInfo(i,t), typ1', typ2') )
	end

      | elabTyp(E, I.AllTyp(i, typid, typ)) =
	let
	    val (a,typid') = elabVarid_bind(E, Type.STAR, typid)
	    val (t1,typ')  = elabTyp(E, typ)
	    val  t         = Type.inAll(a,t1)
	in
	    ( t, O.AllTyp(typInfo(i,t), typid', typ') )
	end

      | elabTyp(E, I.ExTyp(i, typid, typ)) =
	let
	    val (a,typid') = elabVarid_bind(E, Type.STAR, typid)
	    val (t1,typ')  = elabTyp(E, typ)
	    val  t         = Type.inExist(a,t1)
	in
	    ( t, O.ExTyp(typInfo(i,t), typid', typ') )
	end

      | elabTyp(E, I.PackTyp(i, inf)) =
	let
	    val (j,inf') = elabInf(E, inf)
	    (*UNFINISHED*)
	    val  t       = Type.unknown(Type.STAR)
	in
	    unfinished i "elabTyp" "packages";
	    ( t, O.PackTyp(typInfo(i,t), inf') )
	end

      | elabTyp(E, I.SingTyp(i, vallongid)) =
	let
	    val (t,vallongid') = elabVallongid(E, vallongid)
	in
	    ( t, O.SingTyp(typInfo(i,t), vallongid') )
	end

      | elabTyp(E, I.AbsTyp _) =
	raise Crash.Crash "Elab.elabTyp: AbsTyp"

      | elabTyp(E, I.ExtTyp _) =
	raise Crash.Crash "Elab.elabTyp: ExtTyp"


  (* Types in positions where they may not be higher order *)

    and elabStarTyp(E, typ) =
	let
	    val ttyp' as (t,typ') = elabTyp(E, typ)
	in
	    case Type.kind t
	      of Type.STAR => ttyp'
	       | k         => error(I.infoTyp typ, E.StarTypKind(k))
	end

    and elabStarTyps(E, typs) =
	VectorPair.unzip(Vector.map (fn typ => elabStarTyp(E, typ)) typs)



  (* Type representations (RHSs of type bindings) *)

    and elabTypRep(E, p, buildKind, I.ConTyp(i, typlongid)) =
	let
	    val (t,typlongid') = elabTyplongid(E, typlongid)
	in
	    ( t, false, O.ConTyp(typInfo(i,t), typlongid'), p )
	end

      | elabTypRep(E, p, buildKind, I.FunTyp(i, typid, typ)) =
	let
	    val  k               = Type.STAR
	    val (a,typid')       = elabVarid_bind(E, k, typid)
	    val (t1,gen,typ',p') = elabTypRep(E, p,
				      fn k' => Type.ARROW(k, buildKind k'), typ)
            val  t               = if gen then t1 else Type.inLambda(a,t1)
				   (* If the type is generative then we
				    * get a constructor with appropriate kind
				    * and do not need to insert lambdas.
				    *)
	in
	    ( t, gen, O.FunTyp(typInfo(i,t), typid', typ'), p' )
	end

      | elabTypRep(E, p, buildKind, I.AbsTyp(i,so))=
	let
	    val (t,p') =
		case so
		  of NONE =>
			( Type.inCon(buildKind Type.STAR, Type.CLOSED, p), p )
		   | SOME s =>
			let val con = PervasiveType.lookup s in
			    ( Type.inCon con, #3 con )
			end handle PervasiveType.Lookup =>
				   error(i, E.PervasiveTypUnknown s)
	in
	    ( t, true, O.AbsTyp(typInfo(i,t), so), p' )
	end

      | elabTypRep(E, p, buildKind, I.ExtTyp(i,so))=
	let
	    val (t,p') =
		case so
		  of NONE =>
			( Type.inCon(buildKind Type.STAR, Type.OPEN, p), p )
		   | SOME s =>
			let val con = PervasiveType.lookup s in
			    ( Type.inCon con, #3 con )
			end handle PervasiveType.Lookup =>
				   error(i, E.PervasiveTypUnknown s)
	in
	    ( t, true, O.ExtTyp(typInfo(i,t), so), p' )
	end

      | elabTypRep(E, p, buildKind, typ) =
	let
	    val (t,typ') = elabTyp(E, typ)
	in
	    ( t, false, typ', p )
	end


  (* Module identifiers *)

    and elabModid_bind(E, p, j, modid as I.Id(i, stamp, name)) =
	let
	    val _ = insertMod(E, stamp, {id=modid, path=p, inf=j})
	in
	    O.Id(infInfo(i,j), stamp, name)
	end

    and elabModid(E, I.Id(i, stamp, name)) =
	let
	    val j = #inf(lookupMod(E, stamp))
	in
	    ( j, O.Id(infInfo(i,j), stamp, name) )
	end

    and elabModlongid(E, I.ShortId(i, modid)) =
	let
	    val (j,modid') = elabModid(E, modid)
	in
	    ( j, O.ShortId(infInfo(i,j), modid') )
	end

      | elabModlongid(E, I.LongId(i, modlongid, modlab)) =
	let
	    val (s,modlongid') = elabModlongid_path(E, modlongid)
	    val (l,modlab')    = elabLab(E, modlab)
	    val  j             = Inf.lookupMod(s, l)
	in
	    ( j, O.LongId(infInfo(i,j), modlongid', modlab') )
	end

    and elabModlongid_path(E, modlongid) =
	let
	    val (j,modlongid') = elabModlongid(E, modlongid)
	    val  s             = Inf.asSig j handle Inf.Interface =>
					error(I.infoLongid modlongid,
					      E.ModlongidInf(modlongid, j))
	in
	    ( s, modlongid' )
	end


  (* Modules *)

    and elabMod(E, I.PrimMod(i, s, inf)) =
	let
	    val (j,inf') = elabGroundInf(E, inf)
	in
	    ( j, O.PrimMod(infInfo(i,j), s, inf') )
	end

      | elabMod(E, I.VarMod(i, modid)) =
	let
	    val (j,modid') = elabModid(E, modid)
	in
	    ( j, O.VarMod(infInfo(i,j), modid') )
	end

      | elabMod(E, I.StrMod(i, decs)) =
	let
	    val s     = Inf.empty()
	    val decs' = elabDecs(E, s, decs)
	    val _     = Inf.close s handle Inf.Unclosed lnt =>
			    error(i, E.StrModUnclosed lnt)
	    val j     = Inf.inSig s
	in
	    ( j, O.StrMod(infInfo(i,j), decs') )
	end

      | elabMod(E, I.SelMod(i, modlab, mod)) =
	let
	    val (l,modlab') = elabLab(E, modlab)
	    val (j1,mod')   = elabMod(E, mod)
	    val  s          = Inf.asSig j1 handle Inf.Interface =>
				error(I.infoMod mod, E.SelModInf j1)
	    val  j          = Inf.lookupMod(s, l)
	in
	    ( j, O.SelMod(infInfo(i,j), modlab', mod') )
	end

      | elabMod(E, I.FunMod(i, modid, inf, mod)) =
	let
	    val  _        = insertScope E
	    val (j1,inf') = elabGroundInf(E, inf)
	    val  j1'      = Inf.clone j1
	    val  p        = Path.fromLab(Label.fromName(I.name modid))
	    val  _        = Inf.strengthen(p, j1')
	    val  modid'   = elabModid_bind(E, p, j1', modid)
	    val (j2,mod') = elabMod(E, mod)
	    val  _        = deleteScope E
	    val  j        = Inf.inArrow(p, j1, j2)
	in
	    ( j, O.FunMod(infInfo(i,j), modid', inf', mod') )
	end

      | elabMod(E, I.AppMod(i, mod1, mod2)) =
	let
	    val (j1,mod1')  = elabMod(E, mod1)
	    val (j2,mod2')  = elabMod(E, mod2)
	    val (p,j11,j12) = if Inf.isArrow j1 then
				  Inf.asArrow(Inf.instance j1)
			      else
				  error(I.infoMod mod1, E.AppModFunMismatch j1)
	    val  p2         = case elabMod_path(E, mod2)
				of SOME(p2,_) => p2
				 | NONE       => Path.invent()
	    val  _          = Inf.strengthen(p2, j2)
	    val  rea        = Inf.match(j2,j11) handle Inf.Mismatch mismatch =>
				  error(i, E.AppModArgMismatch mismatch)
	    val  _          = PathMap.insert(#mod_rea rea, p, p2)
	    val  _          = Inf.realise(rea, j12)
	    val  j          = j12
	in
	    ( j, O.AppMod(infInfo(i,j), mod1', mod2') )
	end

      | elabMod(E, I.AnnMod(i, mod, inf)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	    val (j2,inf') = elabGroundInf(E, inf)
	    val  _        = Inf.match(j1, j2) handle Inf.Mismatch mismatch =>
				error(i, E.AnnModMismatch mismatch)
	    val  j        = j2
	in
	    ( j, O.AnnMod(infInfo(i,j), mod', inf') )
	end

      | elabMod(E, I.UpMod(i, mod, inf)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	    val (j2,inf') = elabGroundInf(E, inf)
	    val  j2'      = Inf.instance j2	(* opaque *)
	    val  _        = Inf.match(j1, j2') handle Inf.Mismatch mismatch =>
				error(i, E.AnnModMismatch mismatch)
	    val  j        = j2
	in
	    ( j, O.UpMod(infInfo(i,j), mod', inf') )
	end

      | elabMod(E, I.LetMod(i, decs, mod)) =
	let
	    val  _       = insertScope E
	    val  s       = Inf.empty()
	    val  decs'   = elabDecs(E, s, decs)
	    val  p       = Path.invent()
(*DEBUG*)
(*val p = Path.fromLab(Label.fromString "?let")*)
	    val  _       = Inf.strengthenSig(Path.invent(), s)
	    val (j,mod') = elabMod(E, mod)
	    val  _       = deleteScope E
	in
	    ( j, O.LetMod(infInfo(i,j), decs', mod') )
	end

      | elabMod(E, I.UnpackMod(i, exp, inf)) =
	let
	    val (t,exp') = elabExp(E, exp)
	    val (j,inf') = elabInf(E, inf)
	    (*UNFINISHED*)
	in
	    unfinished i "elabMod" "packages";
	    ( j, O.UnpackMod(infInfo(i,j), exp', inf') )
	end


  (* Modules as paths *)

    and elabMod_path(E, I.VarMod(i, I.Id(_, stamp, _))) =
	let
	    val {path=p, inf=j, ...} = lookupMod(E, stamp)
	in
	    SOME (p,j)
	end

      | elabMod_path(E, I.SelMod(_, I.Lab(_, a), mod)) =
	(case elabMod_path(E, mod)
	   of NONE      => NONE
	    | SOME(_,j) =>
	      let
		  val s = Inf.asSig j
		  val j = Inf.lookupMod(s, a)
		  val p = Inf.lookupModPath(s, a)
	      in
		  SOME (p,j)
	      end
	)

      | elabMod_path(E, I.AnnMod(_, mod, inf))=
	    elabMod_path(E, mod)

      | elabMod_path _ = NONE


  (* Interface identifiers *)

    and elabInfid_bind(E, p, j, infid as I.Id(i, stamp, name)) =
	let
	    val j' = Inf.inAbbrev(Inf.inCon(Inf.kind j, p), j)
	    val _  = insertInf(E, stamp, {id=infid, path=p, inf=j'})
	in
	    O.Id(nonInfo(i), stamp, name)
	end

    and elabInfid(E, I.Id(i, stamp, name)) =
	let
	    val j = #inf(lookupInf(E, stamp))
	in
	    ( j, O.Id(nonInfo(i), stamp, name) )
	end

    and elabInflongid(E, I.ShortId(i, infid)) =
	let
	    val (j,infid') = elabInfid(E, infid)
	in
	    ( j, O.ShortId(nonInfo(i), infid') )
	end

      | elabInflongid(E, I.LongId(i, modlongid, inflab)) =
	let
	    val (s,modlongid') = elabModlongid_path(E, modlongid)
	    val (a,inflab')    = elabLab(E, inflab)
	    val  j             = Inf.lookupInf(s, a)
	in
	    ( j, O.LongId(nonInfo(i), modlongid', inflab') )
	end


  (* Interfaces *)

    and elabInf(E, I.TopInf(i)) =
	let
	    val j = Inf.inTop()
	in
	    ( j, O.TopInf(infInfo(i,j)) )
	end

      | elabInf(E, I.ConInf(i, inflongid)) =
	let
	    val (j,inflongid') = elabInflongid(E, inflongid)
	    val  j'            = Inf.instance j
	in
	    ( j', O.ConInf(infInfo(i,j'), inflongid') )
	end

      | elabInf(E, I.SigInf(i, specs)) =
	let
	    val s      = Inf.empty()
	    val specs' = elabSpecs(E, s, specs)
	    val j      = Inf.inSig s
	in
	    ( j, O.SigInf(infInfo(i,j), specs') )
	end

      | elabInf(E, I.FunInf(i, modid, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabGroundInf(E, inf1)
	    val  j1'       = Inf.clone j1
	    val  p         = Path.fromLab(Label.fromName(I.name modid))
	    val  _         = Inf.strengthen(p, j1')
	    (* UNFINISHED: revert renaming of paths somehow *)
	    val  modid'    = elabModid_bind(E, p, j1', modid)
	    val (j2,inf2') = elabInf(E, inf2)
	    val  _         = deleteScope E
	    val  j         = Inf.inLambda(p, j1, j2)
	in
	    ( j, O.FunInf(infInfo(i,j), modid', inf1', inf2') )
	end

      | elabInf(E, I.AppInf(i, inf, mod)) =
	let
	    val (j1,inf') = elabInf(E, inf)
	    val (j2,mod') = elabMod(E, mod)
	(*UNFINISHED*)
	    val j = j1
	in
	    unfinished i "elabMod" "interface application";
	    ( j, O.AppInf(infInfo(i,j), inf', mod') )
	end

      | elabInf(E, I.CompInf(i, inf1, inf2)) =
	let
	    val (j1,inf1') = elabGroundInf(E, inf1)
	    val (j2,inf2') = elabGroundInf(E, inf2)
	    val  j         = Inf.intersect(j1,j2) handle Inf.Mismatch mismatch=>
				error(i, E.CompInfMismatch mismatch)
	in
	    ( j, O.CompInf(infInfo(i,j), inf1', inf2') )
	end

      | elabInf(E, I.ArrInf(i, modid, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabGroundInf(E, inf1)
	    val  j1'       = Inf.clone j1
	    val  p         = Path.fromLab(Label.fromName(I.name modid))
	    val  _         = Inf.strengthen(p, j1')
	    val  modid'    = elabModid_bind(E, p, j1', modid)
	    val (j2,inf2') = elabGroundInf(E, inf2)
	    val  _         = deleteScope E
	    val  j         = Inf.inArrow(p, j1, j2)
	in
	    ( j, O.ArrInf(infInfo(i,j), modid', inf1', inf2') )
	end

      | elabInf(E, I.LetInf(i, decs, inf)) =
	let
	    val  _       = insertScope E
	    val  s       = Inf.empty()
	    val  decs'   = elabDecs(E, s, decs)
	    val  p       = Path.invent()
(*DEBUG*)
(*val p = Path.fromLab(Label.fromString "?let")*)
	    val  _       = Inf.strengthenSig(Path.invent(), s)
	    val (j,inf') = elabInf(E, inf)
	    val  _       = deleteScope E
	in
	    ( j, O.LetInf(infInfo(i,j), decs', inf') )
	end

      | elabInf(E, I.SingInf(i, mod)) =
	let
	    val (j,mod') = elabMod(E, mod)
(*DEBUG*)
(*val _ = Inf.strengthen(Path.fromLab(Label.fromString "?singleton"), j)*)
	    val  _       = Inf.strengthen(Path.invent(), j)
	    val  j'      = Inf.singleton j
	in
	    ( j', O.SingInf(infInfo(i,j'), mod') )
	end

      | elabInf(E, I.AbsInf _) =
	    raise Crash.Crash "Elab.elabInf: AbsInf"


  (* Interfaces in positions where they not may be higher order *)

    and elabGroundInf(E, inf) =
	let
	    val jinf' as (j,_) = elabInf(E, inf)
	    val k              = Inf.kind j
	in
	    if Inf.isGround k then
		jinf'
	    else
		error(I.infoInf inf, E.GroundInfKind(k))
	end


  (* Interfaces representations (RHSs of bindings) *)

    and elabInfRep(E, p', buildKind, I.FunInf(i, modid, inf1, inf2)) =
	let
	    val  _             = insertScope E
	    val (j1,inf1')     = elabGroundInf(E, inf1)
	    val  j1'           = Inf.clone j1
	    val  p             = Path.fromLab(Label.fromName(I.name modid))
	    val  _             = Inf.strengthen(p, j1')
	    val  modid'        = elabModid_bind(E, p, j1', modid)
	    val (j2,gen,inf2') = elabInfRep(E, p',
				     fn k => Inf.inDependent(p,j1,buildKind k),
				     inf2)
	    val  _             = deleteScope E
	    val  j             = Inf.inLambda(p, j1, j2)
	in
	    ( j, gen, O.FunInf(infInfo(i,j), modid', inf1', inf2') )
	end

      | elabInfRep(E, p, buildKind, I.AbsInf(i,so)) =
	let
	    (*UNFINISHED: pervasive interfaces *)
	    val j = case so
		      of NONE => Inf.inCon(buildKind(Inf.inGround()), p)
		       | SOME s => error(i, E.PervasiveInfUnknown s)
	in
	    ( j, true, O.AbsInf(infInfo(i,j), so) )
	end

      | elabInfRep(E, p, buildKind, inf) =
	let
	    val (j,inf') = elabInf(E, inf)
	in
	    ( j, false, inf' )
	end


  (* Declarations *)

    and elabDec(E, s, vars, I.ValDec(i, pat, exp)) =
	let
	    val  _        = insertScope E
	    val  _        = Type.enterLevel()
	    val  _        = enterVars(E, vars)
	    val  _        = insertScope E
	    val (t2,exp') = elabExp(E, exp)
	    val  _        = deleteScope E
	    val (t1,pat') = elabPat(E, s, pat)
	    val  _        = Type.exitLevel()
	    val  E'       = splitScope E
	    val  _        = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.ValDecUnify(t1, t2, t3, t4))
	    (* UNFINISHED: if pat = x and exp = y then equate x to y *)
	    val  _        = appVals (generaliseVal
					(E, s, SOME NONE, isValue exp)) E'
	in
	    O.ValDec(nonInfo(i), pat', exp')
	end

      | elabDec(E, s, vars, I.TypDec(i, typid, typ)) =
	let
	    val  p              = Inf.newTyp(s, Label.fromName(I.name typid))
	    val (t,gen,typ',p') = elabTypRep(E, p, fn k'=>k', typ)
	    val  typid'         = elabTypid_bind(E, p', t, typid)
	    val  _              = Inf.extendTyp(s, p', Type.kind t, SOME t)
	in
	    O.TypDec(nonInfo(i), typid', typ')
	end

      | elabDec(E, s, vars, I.ModDec(i, modid, mod)) =
	let
	    val  p       = Inf.newMod(s, Label.fromName(I.name modid))
	    val (j,mod') = elabMod(E, mod)
	    val  _       = Inf.strengthen(p, j)
	    val  p'      = case elabMod_path(E, mod)
			     of SOME (p',_) => p'
			      | NONE        => p
	    val  modid'  = elabModid_bind(E, p', j, modid)
	    val  _       = Inf.extendMod(s, p, j, SOME p')
	in
	    O.ModDec(nonInfo(i), modid', mod')
	end

      | elabDec(E, s, vars, I.InfDec(i, infid, inf)) =
	let
	    val  p         = Inf.newInf(s, Label.fromName(I.name infid))
	    val (j,_,inf') = elabInfRep(E, p, fn k'=>k', inf)
	    val  k         = Inf.kind j
	    val  infid'    = elabInfid_bind(E, p, j, infid)
	    val  _         = Inf.extendInf(s, p, k, SOME j)
	in
	    O.InfDec(nonInfo(i), infid', inf')
	end

      | elabDec(E, s, vars, I.FixDec(i, vallab, fix)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val  p          = Inf.newFix(s, a)
	    val (f,fix')    = elabFix(E, fix)
	    val  _          = Inf.extendFix(s, p, f)
	in
	    O.FixDec(nonInfo(i), vallab', fix')
	end

      | elabDec(E, s, vars, I.VarDec(i, typid, dec)) =
	let
	    val typid'  = elabVarid_bind'(E, typid)
	    val dec'    = elabDec(E, s, typid::vars, dec)
	in
	    O.VarDec(nonInfo(i), typid', dec')
	end

      | elabDec(E, s, vars, I.RecDec(i, decs)) =
	let
	    val _      = insertScope E
	    val _      = Type.enterLevel()
	    val _      = enterVars(E, vars)
	    val tpats' = elabLHSRecDecs(E, s, decs)
	    val decs'  = elabRHSRecDecs(E, s, ref tpats', decs)
	    val _      = Type.exitLevel()
	    val E'     = splitScope E
	    (* ASSUME that only ValDec or TypDec are under RecDec *)
	    (* ASSUME that recursive ValDecs are never expansive *)
	    (* ASSUME that recursive ValDecs are never equatable *)
	    val _      = appTyps (fn(x,entry) => insertTyp(E,x,entry)) E'
	    val _      = appVals (generaliseVal (E, s, SOME NONE, true)) E'
	in
	    O.RecDec(nonInfo(i), decs')
	end

      | elabDec(E, s, vars, I.LocalDec(i, decs)) =
	let
	    val s'    = Inf.empty()
	    val decs' = elabDecs(E, s', decs)
	    val p     = Path.invent()
(*DEBUG*)
(*val p = Path.fromLab(Label.fromString "?local")*)
	    val _     = Inf.strengthenSig(p, s')
	in
	    O.LocalDec(nonInfo(i), decs')
	end


    and enterVars(E, vars) =
	List.app (fn typid as I.Id(_, stamp, name) =>
		  (*UNFINISHED: use punning: *)
		  insertVar(E, stamp, {id=typid, var=Type.var(Type.STAR)})) vars

    and generaliseVal (E, s, poo, isPoly) (x, {id=valid, path=p, typ=t}) =
	let
	    val t' = if isPoly then Type.close t
			       else (Type.lift t ; t) handle Type.Lift a =>
				   error(I.infoId valid, E.ValDecLift(valid, a))
	    val d  = Option.map (fn po => Option.getOpt(po, p)) poo
	in
	    (*UNFINISHED: use record update: *)
	    (*insertVal(E, x, {entry where typ=t'}));*)
	    insertVal(E, x, {id=valid, path=p, typ=t'});
	    Inf.extendVal(s, p, t', d)
	end


      and elabDecs(E, s, decs)        = elabDecs'(E, s, [], decs)
      and elabDecs'(E, s, vars, decs) =
	    Vector.map (fn dec => elabDec(E, s, vars, dec)) decs


  (* Recursive declarations *)

    and elabLHSRecDecs(E, s, decs) =
	    Vector.foldr (fn(dec,xs) => elabLHSRecDec(E,s,dec) @ xs) [] decs

    and elabLHSRecDec(E, s, I.ValDec(i, pat, exp)) =
	    [elabPat(E, s, pat)]

      | elabLHSRecDec(E, s, I.TypDec(i, typid, typ)) =
	let
	    val p = Inf.newTyp(s, Label.fromName(I.name typid))
	    val k = elabTypKind(E, typ)
	    val t = Type.unknown k
	    val _ = elabTypid_bind(E, p, t, typid)
	in
	    []
	end

      | elabLHSRecDec(E, s, I.RecDec(i, decs)) =
	    elabLHSRecDecs(E, s, decs)

      | elabLHSRecDec(E, s, _) = raise Crash.Crash "elabLHSRecDec"


    and elabRHSRecDecs(E, s, rtpats', decs) =
	    Vector.map (fn dec => elabRHSRecDec(E, s, rtpats', dec)) decs

    and elabRHSRecDec(E, s, r as ref((t1,pat')::tpats'), I.ValDec(i, pat, exp))=
	let
	    val  _        = insertScope E
	    val (t2,exp') = elabExp(E, exp)
	    val  _        = deleteScope E
	    val  _        = r := tpats'
	    val  _        = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.ValDecUnify(t1, t2, t3, t4))
	in
	    O.ValDec(nonInfo(i), pat', exp')
	end

      | elabRHSRecDec(E, s, rtpats', I.TypDec(i, typid, typ)) =
	let
	    val (t0,p,typid') = elabTypid(E, typid)
	    val (t,_,typ',p') = elabTypRep(E, p, fn k'=>k', typ)
	    val  t1           = #2(Type.asAbbrev t0)
	    (* ASSUME typ is not a non-recursive type lambda *)
	    val  _            = Type.fill(t1, Type.inMu t)
	    val  _            = Inf.extendTyp(s, p', Type.kind t1, SOME t1)
	in
	    O.TypDec(nonInfo(i), typid', typ')
	end

      | elabRHSRecDec(E, s, rtpats', I.RecDec(i, decs)) =
	let
	    val dec' = elabRHSRecDecs(E, s, rtpats', decs)
	in
	    O.RecDec(nonInfo(i), dec')
	end

      | elabRHSRecDec(E, s, rtpats', dec) =
	    raise Crash.Crash "elabRHSRecDec"



  (* Specifications *)

    and elabSpec(E, s, I.ValSpec(i, valid, typ)) =
	let
	    val (t0,p,valid') = elabValid_bind(E, s, valid)
	    val (t,typ')      = elabStarTyp(E, typ)
	    val  d            = case typ
				  of I.SingTyp(_, vallongid) =>
					SOME(elabVallongid_path(E,vallongid))
				   | _ => NONE
	    val  _            = Type.fill(t0,t)
	    val  _            = Inf.extendVal(s, p, t, d)
	in
	    O.ValSpec(nonInfo(i), valid', typ')
	end

      | elabSpec(E, s, I.TypSpec(i, typid, typ)) =
	let
	    val  p              = Inf.newTyp(s, Label.fromName(I.name typid))
	    val (t,gen,typ',p') = elabTypRep(E, p, fn k'=>k', typ)
	    val  typid'         = elabTypid_bind(E, p', t, typid)
	    val  _              = Inf.extendTyp(s, p', Type.kind t,
						if gen then NONE else SOME t)
	in
	    O.TypSpec(nonInfo(i), typid', typ')
	end

      | elabSpec(E, s, I.ModSpec(i, modid, inf)) =
	let
	    val  p       = Inf.newMod(s, Label.fromName(I.name modid))
	    val (j,inf') = elabGroundInf(E, inf)
	    val  j'      = Inf.clone j
	    val  _       = Inf.strengthen(p, j')
	    (* UNFINISHED: revert renaming of paths somehow *)
	    val (p',d)   = case inf
			     of I.SingInf(i', mod) =>
				(case elabMod_path(E, mod)
				   of NONE        => error(i', E.SingInfPath)
				    | SOME (p',_) => (p', SOME p')
				)
			      | _ => (p, NONE)
	    val  modid'  = elabModid_bind(E, p', j', modid)
	    val  _       = Inf.extendMod(s, p, j, d)
	in
	    O.ModSpec(nonInfo(i), modid', inf')
	end

      | elabSpec(E, s, I.InfSpec(i, infid, inf)) =
	let
	    val  p           = Inf.newInf(s, Label.fromName(I.name infid))
	    val (j,gen,inf') = elabInfRep(E, p, fn k'=>k', inf)
	    val  k           = Inf.kind j
	    val  infid'      = elabInfid_bind(E, p, j, infid)
	    val  _           = Inf.extendInf(s, p, k,
					     if gen then NONE else SOME j)
	in
	    O.InfSpec(nonInfo(i), infid', inf')
	end

      | elabSpec(E, s, I.FixSpec(i, vallab, fix)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val  p          = Inf.newFix(s, a)
	    val (f,fix')    = elabFix(E, fix)
	    val  _          = Inf.extendFix(s, p, f)
	in
	    O.FixSpec(nonInfo(i), vallab', fix')
	end

      | elabSpec(E, s, I.RecSpec(i, specs)) =
	let
	    val _      = elabLHSRecSpecs(E, s, specs)
	    val specs' = elabRHSRecSpecs(E, s, specs)
	    (* ASSUME that only TypSpec is under RecSpec *)
	in
	    O.RecSpec(nonInfo(i), specs')
	end

      | elabSpec(E, s, I.ExtSpec(i, inf)) =
	let
	    val (j,inf') = elabGroundInf(E, inf)
	(*UNFINISHED: insert stuff*)
	in
	    unfinished i "elabSpec" "signature extension";
	    O.ExtSpec(nonInfo(i), inf')
	end


    and elabSpecs(E, s, specs) =
	    Vector.map (fn spec => elabSpec(E, s, spec)) specs


  (* Recursive specifications *)

    and elabLHSRecSpecs(E, s, specs) =
	    Vector.app (fn spec => elabLHSRecSpec(E,s,spec)) specs

    and elabLHSRecSpec(E, s, I.TypSpec(i, typid, typ)) =
	let
	    val p = Inf.newTyp(s, Label.fromName(I.name typid))
	    val k = elabTypKind(E, typ)
	    val t = Type.unknown k
	    val _ = elabTypid_bind(E, p, t, typid)
	in
	    ()
	end

      | elabLHSRecSpec(E, s, I.RecSpec(i, specs)) =
	    elabLHSRecSpecs(E, s, specs)

      | elabLHSRecSpec(E, s, _) = ()


    and elabRHSRecSpecs(E, s, specs) =
	    Vector.map (fn spec => elabRHSRecSpec(E, s, spec)) specs

    and elabRHSRecSpec(E, s, I.RecSpec(i, specs)) =
	let
	    val spec' = elabRHSRecSpecs(E, s, specs)
	in
	    O.RecSpec(nonInfo(i), spec')
	end

      | elabRHSRecSpec(E, s, I.TypSpec(i, typid, typ)) =
	let
	    val (t0,p,typid')   = elabTypid(E, typid)
	    val (t,gen,typ',p') = elabTypRep(E, p, fn k'=>k', typ)
	    val  t1             = #2(Type.asAbbrev t0)
	    (* ASSUME typ is not a non-recursive type lambda *)
	    val  _              = Type.fill(t1, Type.inMu t)
	    val  _              = Inf.extendTyp(s, p', Type.kind t1,
						if gen then NONE else SOME t1)
	in
	    O.TypSpec(nonInfo(i), typid', typ')
	end

      | elabRHSRecSpec(E, s, spec) =
	    elabSpec(E, s, spec)


  (* Announcements *)

    fun elabAnn(E, I.ImpAnn(i, imps, url)) =
	let
	    val s     = Composer.sign url
			(*UNFINISHED: Handling of IO failure? *)
	    val imps' = elabImps(E, s, imps)
	in
	    O.ImpAnn(sigInfo(i,s), imps', url)
	end

    and elabAnns(E, anns) = Vector.map (fn ann => elabAnn(E, ann)) anns


  (* Imports *)

    and elabImp(E, s, I.ValImp(i, valid, desc)) =
	let
	    val (t0,p,valid') = elabValid_bind(E, s, valid)
	    val  a            = Label.fromName(O.name valid')
	    val  t1           = Inf.lookupVal(s, a) handle Inf.Lookup =>
				    error(I.infoId valid, E.ValImpUnbound a)
	    val (t2,desc')    = case desc
				of I.NoDesc(i') =>
				   (t1, O.NoDesc(typInfo(i',t1)))
				 | I.SomeDesc(i',typ) =>
				   let				
				      val (t2,typ')  = elabStarTyp(E, typ)
				   in
				      if Type.matches(t2,t1) then
					  (t2, O.SomeDesc(typInfo(i',t2), typ'))
				      else
					  error(i, E.ValImpMismatch(a,t2,t1))
				   end
	    val  _            = Type.fill(t0,t2)
	in
	    O.ValImp(nonInfo(i), valid', desc')
	end

      | elabImp(E, s, I.TypImp(i, typid, desc)) =
	let
	    (*UNFINISHED: have to check (or disallow) manifest type *)
	    val  a         = Label.fromName(I.name typid)
	    val  p         = Inf.newTyp(s, a)
	    val  t1        = Inf.lookupTyp(s, a) handle Inf.Lookup =>
				    error(I.infoId typid, E.TypImpUnbound a)
	    val (t2,desc') = case desc
			       of I.NoDesc(i') =>
				      (t1, O.NoDesc(typInfo(i', t1)))
			        | I.SomeDesc(i',typ) =>
				  let
				      val (t2,_,typ',_) =
						elabTypRep(E, p, fn k'=>k', typ)
				      val  k1 = Type.kind t1
				      val  k2 = Type.kind t2
				  in
				      if k2 = k1 then
					  (t1, O.SomeDesc(typInfo(i',t1),typ'))
				      else
					  error(i, E.TypImpMismatch(a,k2,k1))
				  end
	    val  typid'       = elabTypid_bind(E, p, t2, typid)
	in
	    O.TypImp(nonInfo(i), typid', desc')
	end

      | elabImp(E, s, I.ModImp(i, modid, desc)) =
	let
	    val  a         = Label.fromName(I.name modid)
	    val  p         = Inf.newMod(s, a)
	    val  j1        = Inf.lookupMod(s, a) handle Inf.Lookup =>
				error(I.infoId modid, E.ModImpUnbound a)
	    val (j2,desc') = case desc
				of I.NoDesc(i') =>
				   (j1, O.NoDesc(infInfo(i',j1)))
				 | I.SomeDesc(i',inf) =>
				   let				
				       val (j2,inf') = elabGroundInf(E, inf)
				       val  j2'      = Inf.clone j2
				       val  _        = Inf.strengthen(p, j2')
				   in
				       Inf.match(j2,j1) handle Inf.Mismatch m =>
					   error(i, E.ModImpMismatch(a, m)) ;
				       (j2', O.SomeDesc(infInfo(i',j2'), inf'))
				   end
	    val modid'     = elabModid_bind(E, p, j2, modid)
	in
	    O.ModImp(nonInfo(i), modid', desc')
	end

      | elabImp(E, s, I.InfImp(i, infid, desc)) =
	let
	    val  a         = Label.fromName(I.name infid)
	    val  p         = Inf.newInf(s, a)
	    val  j1        = Inf.lookupInf(s, a) handle Inf.Lookup =>
				error(I.infoId infid, E.InfImpUnbound a)
	    val (j2,desc') = case desc
				of I.NoDesc(i') =>
				   (j1, O.NoDesc(infInfo(i',j1)))
				 | I.SomeDesc(i',inf) =>
				   let				
					val (j2,gen,inf') =
						elabInfRep(E, p, fn k'=>k', inf)
					val  k2 = Inf.kind j2
				   in
					Inf.equaliseKind(k2, Inf.kind j1)
					handle Inf.Mismatch m =>
					    error(i, E.InfImpMismatch(a,m)) ;
					(j1, O.SomeDesc(infInfo(i',j1), inf'))
				   end
	    val  infid'    = elabInfid_bind(E, p, j2, infid)
	in
	    O.InfImp(nonInfo(i), infid', desc')
	end

      | elabImp(E, s, I.FixImp(i, vallab, desc)) =
	let
	    val (a,vallab') = elabLab(E, vallab)
	    val f1          = Inf.lookupFix(s, a) handle Inf.Lookup =>
				error(I.infoLab vallab, E.FixImpUnbound a)
	    val desc'       = case desc
			       of I.NoDesc(i')       => O.NoDesc(fixInfo(i',f1))
				| I.SomeDesc(i',fix) =>
				  let
				    val (f2,fix') = elabFix(E, fix)
				  in
				    if f2 = f1 then
					O.SomeDesc(fixInfo(i',f2), fix')
				    else
					error(i, E.FixImpMismatch(a,f2,f1))
				  end
	in
	    O.FixImp(nonInfo(i), vallab', desc')
	end

      | elabImp(E, s, I.RecImp(i, imps)) =
	let
	    val _     = elabLHSRecImps(E, s, imps)
	    val imps' = elabRHSRecImps(E, s, imps)
	    (* ASSUME that only TypImp is under RecImp *)
	in
	    O.RecImp(nonInfo(i), imps')
	end


    and elabImps(E, s, imps) = Vector.map (fn imp => elabImp(E, s, imp)) imps


  (* Recursive specifications *)

    and elabLHSRecImps(E, s, imps) =
	    Vector.app (fn imp => elabLHSRecImp(E,s,imp)) imps

    and elabLHSRecImp(E, s, I.TypImp(i, typid, desc)) =
	let
	    val a  = Label.fromName(I.name typid)
	    val p  = Inf.newTyp(s, a)
	    val t1 = Inf.lookupTyp(s, a) handle Inf.Lookup =>
			error(I.infoId typid, E.TypImpUnbound a)
	    val k1 = Type.kind t1
	    val _  = case desc
			of I.NoDesc(i')       => ()
			 | I.SomeDesc(i',typ) =>
			   let
				val k2 = elabTypKind(E, typ)
			   in
				if k2 = k1 then () else
				    error(i, E.TypImpMismatch(a,k2,k1))
			   end
	    val _  = elabTypid_bind(E, p, t1, typid)
	in
	    ()
	end

      | elabLHSRecImp(E, s, I.RecImp(i, imps)) =
	    elabLHSRecImps(E, s, imps)

      | elabLHSRecImp(E, s, _) = ()


    and elabRHSRecImps(E, s, imps) =
	    Vector.map (fn imp => elabRHSRecImp(E, s, imp)) imps

    and elabRHSRecImp(E, s, I.RecImp(i, imps)) =
	let
	    val imps' = elabRHSRecImps(E, s, imps)
	in
	    O.RecImp(nonInfo(i), imps')
	end

      | elabRHSRecImp(E, s, I.TypImp(i, typid, desc)) =
	let
	    (*UNFINISHED: have to check (or disallow) manifest type *)
	    val (t1,p,typid') = elabTypid(E, typid)
	    val (t2,desc')    = case desc
				  of I.NoDesc(i') =>
					(t1, O.NoDesc(typInfo(i',t1)))
				   | I.SomeDesc(i',typ) =>
				     let
					 val (t2,_,typ',_) =
						elabTypRep(E, p, fn k'=>k', typ)
				     in
					 (t1, O.SomeDesc(typInfo(i',t1), typ'))
				     end
	in
	    O.TypImp(nonInfo(i), typid', desc')
	end

      | elabRHSRecImp(E, s, imp) =
	    elabImp(E, s, imp)


  (* Components *)

    fun elabComp(E, I.Comp(i, anns, decs)) =
	let
	    val anns' = elabAnns(E, anns)
	    val s     = Inf.empty()
	    val decs' = elabDecs(E, s, decs)
	    val _     = Inf.close s handle Inf.Unclosed lnt =>
			    error(i, E.CompUnclosed lnt)
	    (*UNFINISHED: do we need this?
	    val _     = Inf.strengthenSig(Path.fromLab(Label.fromString "?"), s)
	    *)
	in
	    O.Comp(sigInfo(i,s), anns', decs')
	end


    fun translate E (desc, component) =
	let
	    val _         = insertScope E
	    val impsdecs' = elabComp(E, component)
	    val _         = mergeScope E
	in
	    impsdecs'
	end
	handle Error.Error x =>
	    ( deleteScope E
	    ; Type.resetLevel()
	    ; raise Error.Error x
	    )

end
