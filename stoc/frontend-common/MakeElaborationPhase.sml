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

structure ElaborationPhase :> ELABORATION_PHASE =
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

    fun boolTyp E    = #typ(lookupTyp(E, Prebound.typstamp_bool))
    fun exnTyp E     = #typ(lookupTyp(E, Prebound.typstamp_exn))

    (* UNFINISHED: overloading *)
    fun wordTyp E    = #typ(lookupTyp(E, Prebound.typstamp_word))
    fun intTyp E     = #typ(lookupTyp(E, Prebound.typstamp_int))
    fun charTyp E    = #typ(lookupTyp(E, Prebound.typstamp_char))
    fun stringTyp E  = #typ(lookupTyp(E, Prebound.typstamp_string))
    fun realTyp E    = #typ(lookupTyp(E, Prebound.typstamp_real))

    fun refTyp(E,t)  = Type.inApp(#typ(lookupTyp(E, Prebound.typstamp_ref)), t)
    fun vecTyp(E,t)  = Type.inApp(#typ(lookupTyp(E, Prebound.typstamp_vec)), t)
    fun listTyp(E,t) = Type.inApp(#typ(lookupTyp(E, Prebound.typstamp_list)), t)


  (* Check value restriction *)

    fun isValue( I.LitExp _
	       | I.PrimExp _
	       | I.VarExp _
	       | I.ConExp _
	       | I.SelExp _
	       | I.CompExp _
	       | I.FunExp _ )			= true
      | isValue( I.TupExp(_, exps)
	       | I.VecExp(_, exps) )		= List.all isValue exps
      | isValue( I.RowExp(_, exprow))		= isValueRow exprow
      | isValue( I.AppExp(_, exp1, exp2))	= isConstr exp1 andalso
						  isValue exp2
      | isValue( I.IfExp (_, exp1, exp2, exp3))	= isValue exp1 andalso
						  isValue exp2 andalso
						  isValue exp3
      | isValue( I.AnnExp(_, exp, _))		= isValue exp
      | isValue  _				= false

    and isValueRow(I.Row(_, fields, _))		= List.all isValueField fields
    and isValueField(I.Field(_, _, exp))	= isValue exp

    and isConstr( I.VarExp _
		| I.FunExp _ )			= false
      | isConstr  exp				= isValue exp


  (* Literals *)

    fun elabLit(E, I.WordLit w)		= ( wordTyp E, O.WordLit w )
      | elabLit(E, I.IntLit n)		= ( intTyp E, O.IntLit n )
      | elabLit(E, I.CharLit c)		= ( charTyp E, O.CharLit c )
      | elabLit(E, I.StringLit s)	= ( stringTyp E, O.StringLit s )
      | elabLit(E, I.RealLit x)		= ( realTyp E, O.RealLit x )


  (* Rows (polymorphic, thus put here) *)

    fun elabLab(E, I.Lab(i, l)) = ( l, O.Lab(nonInfo(i), l) )

    fun elabRow(elabX, E, I.Row(i, fields, b)) =
	let
	    val  r0         = (if b then Type.unknownRow else Type.emptyRow)()
	    val (r,fields') = elabFields(elabX, E, r0, fields)
	    val  t          = Type.inRow r
	in
	    ( t, O.Row(nonInfo(i), fields', b) )
	end

    and elabField(elabX, E, I.Field(i, lab, x)) =
	let
	    val (l,lab') = elabLab(E, lab)
	    val (t,x')   = elabX(E, x)
	in
	    ( l, [t], O.Field(nonInfo(i), lab', x') )
	end

    and elabFields(elabX, E, r, fields) =
	let
	    fun elabField1(field, (r,fields')) =
		let
		    val (l,ts,field') = elabField(elabX, E, field)
		in
		    ( Type.extendRow(l, ts, r), field'::fields' )
		end
	in
	    List.foldr elabField1 (r,[]) fields
	end


  (* Expressions *)

    fun elabValId_bind'(E, id as I.Id(i, stamp, name)) =
	    O.Id(nonInfo(i), stamp, name)

    fun elabValId_bind(E, s, w, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- insert val " ^ x ^ "(" ^ Stamp.toString stamp ^ ")")
*)
	    val  p      = Inf.newVal(s, Label.fromName name)
	    val  t      = Type.unknown(Type.STAR)
	    (*UNFINISHED: use punning: *)
	    val (p',t') = ( insertVal(E, stamp, {id=id, path=p, typ=t, sort=w})
			  ; (p,t) )
			  handle Collision _ =>	(* val rec or alt pat *)
			    let val {path=p', typ=t', ...} = lookupVal(E, stamp)
			    in (p',t') end
(*
before (print" (* found : ";
PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t', 60);
print" *)")
val _=print "\n"
*)
	in
	    ( t', p', O.Id(nonInfo(i), stamp, name) )
	end

    fun elabValId(E, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup val " ^ x ^ "(" ^ Stamp.toString stamp ^ ") : ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp(#typ(lookupVal(E, stamp))), 60)
val _=print "\n"
"*)
	    val t  = #typ(lookupVal(E, stamp))
	in
	    ( t, O.Id(nonInfo(i), stamp, name) )
	end

    and elabValLongid(E, I.ShortId(i, id)) =
	let
	    val (t,id') = elabValId(E, id)
	in
	    ( t, O.ShortId(nonInfo(i), id') )
	end

      | elabValLongid(E, I.LongId(i, longid, lab)) =
	let
	    val (s,longid') = elabModLongid_path(E, longid)
	    val (l,lab')    = elabLab(E, lab)
	    val  t          = Inf.lookupVal(s, l)
	in
	    ( t, O.LongId(nonInfo(i), longid', lab') )
	end

    and elabValLongid_path(E, I.ShortId(_, I.Id(_, stamp, _))) =
	    #path(lookupVal(E, stamp))

      | elabValLongid_path(E, I.LongId(_, longid, I.Lab(_, l))) =
	let
	    val (s,_) = elabModLongid_path(E, longid)
	in
	    Inf.lookupValPath(s, l)
	end


    and elabExp(E, I.LitExp(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitExp(typInfo(i,t), lit') )
	end

      | elabExp(E, I.PrimExp(i, s, typ)) =
	let
	    val (t,typ') = elabStarTyp(E, typ)
	in
	    ( t, O.PrimExp(typInfo(i,t), s, typ') )
	end

      | elabExp(E, I.VarExp(i, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	    val  t'         = Type.instance t
	in
	    ( t', O.VarExp(typInfo(i,t'), longid') )
	end

      | elabExp(E, I.ConExp(i, k, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	    val  t'         = Type.instance t
	in
	    ( t', O.ConExp(typInfo(i,t'), k, longid') )
	end

      | elabExp(E, I.RefExp(i)) =
	let
	    val ta = Type.unknown(Type.STAR)
	    val t  = Type.inArrow(ta, refTyp(E, ta))
	in
	    ( t, O.RefExp(typInfo(i,t)) )
	end

      | elabExp(E, I.TupExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.RowExp(i, exprow)) =
	let
	    val (t,exprow') = elabRow(elabExp, E, exprow)
	in
	    ( t, O.RowExp(typInfo(i,t), exprow') )
	end

      | elabExp(E, I.SelExp(i, lab)) =
	let
	    val (l,lab') = elabLab(E, lab)
	    val  t1      = Type.unknown Type.STAR
	    val  r       = Type.extendRow(l, [t1], Type.unknownRow())
	    val  t       = Type.inArrow(Type.inRow r, t1)
	in
	    ( t, O.SelExp(typInfo(i,t), lab') )
	end

      | elabExp(E, I.VecExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = vecTyp(E, List.hd ts)
	    val  _         = Type.unifyList ts handle Type.UnifyList(n,t1,t2) =>
				error(I.infoExp(List.nth(exps,n)),
				      E.VecExpUnify(t, List.nth(ts,n), t1, t2))
	in
	    ( t, O.VecExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.FunExp(i, matchs)) =
	let
	    val  t1          = Type.unknown Type.STAR
	    val (t2,matchs') = elabMatchs(E, t1, matchs)
	    val  t           = Type.inArrow(t1,t2)
	in
	    ( t, O.FunExp(typInfo(i,t), matchs') )
	end

      | elabExp(E, I.AppExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t11       = Type.unknown Type.STAR
	    val  t12       = Type.unknown Type.STAR
	    val  t1'       = Type.inArrow(t11,t12)
	    val  _         = Type.unify(t1,t1') handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.AppExpFunUnify(t1, t1', t3, t4))
	    val  _         = Type.unify(t11,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AppExpArgUnify(t11, t2, t3, t4))
	in
	    ( t12, O.AppExp(typInfo(i,t12), exp1', exp2') )
	end

      | elabExp(E, I.CompExp(i, exp1, exp2)) =
	(* UNFINISHED: more polymorphic treatment *)
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  _         = Type.openRowType t2 handle Type.Row =>
				error(I.infoExp exp2, E.CompExpNoRow t2)
	    val  _         = Type.unify(t1,t2) handle Type.Unify(t3,t4) =>
				error(i, E.CompExpUnify(t2, t1, t4, t3))
	in
	    ( t1, O.CompExp(typInfo(i,t1), exp1', exp2') )
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

      | elabExp(E, I.WhileExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  tb        = boolTyp E
	    val  t         = Type.inTuple[]
	    val  _         = Type.unify(t1,tb) handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.WhileExpCondUnify(t1, tb, t3, t4))
	in
	    ( t, O.WhileExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.SeqExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = List.last ts
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

      | elabExp(E, I.RaiseExp(i, exp)) =
	let
	    val (t1,exp') = elabExp(E, exp)
	    val  te       = exnTyp E
	    val  t        = Type.unknown Type.STAR
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
val _ = Inf.strengthenSig(Path.fromLab(Label.fromString "?let"), s)
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
	ListPair.unzip(List.map (fn exp => elabExp(E,exp)) exps)


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
	    val t2 = Type.unknown Type.STAR

	    fun elabMatch1 match = elabMatch(E, t1, t2, match)
	in
	    ( t2, List.map elabMatch1 matchs )
	end


  (* Patterns *)

    and elabPat(E, s, I.JokPat(i)) =
	let
	    val t = Type.unknown Type.STAR
	in
	    ( t, O.JokPat(typInfo(i,t)) )
	end

      | elabPat(E, s, I.LitPat(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitPat(typInfo(i,t), lit') )
	end

      | elabPat(E, s, I.VarPat(i, id)) =
	let
	    val (t,p,id') = elabValId_bind(E, s, Inf.VALUE, id)
	in
	    ( t, O.VarPat(typInfo(i,t), id') )
	end

      | elabPat(E, s, I.TupPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, s, pats)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupPat(typInfo(i,t), pats') )
	end

      | elabPat(E, s, I.RowPat(i, patrow)) =
	let
	    val (t,patrow') = elabRow(fn(E,pat) => elabPat(E,s,pat), E, patrow)
	in
	    ( t, O.RowPat(typInfo(i,t), patrow') )
	end

      | elabPat(E, s, I.VecPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, s, pats)
	    val  t         = vecTyp(E, List.hd ts)
	    val  _         = Type.unifyList ts handle Type.UnifyList(n,t1,t2) =>
				error(I.infoPat(List.nth(pats,n)),
				      E.VecPatUnify(t, List.nth(ts,n), t1, t2))
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
	    val  t         = List.hd ts
	    val  _         = Type.unifyList ts handle Type.UnifyList(n,t1,t2) =>
				error(I.infoPat(List.nth(pats,n)),
				      E.AltPatUnify(t, List.nth(ts,n), t1, t2))
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

      | elabPat(E, s, pat as ( I.ConPat _ | I.RefPat _ | I.AppPat _ )) =
	let
	    val tpat' as (t,pat') = elabAppPat(E, s, pat)
	in
	    if Type.isArrow t then
		error(I.infoPat pat, E.AppPatArrTyp(t))
	    else
		tpat'
	end


    and elabAppPat(E, s, I.ConPat(i, k, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	    val  t'         = Type.instance t
	in
	    ( t', O.ConPat(typInfo(i,t'), k, longid') )
	end

      | elabAppPat(E, s, I.RefPat(i)) =
	let
	    val ta = Type.unknown(Type.STAR)
	    val t  = Type.inArrow(ta, refTyp(E, ta))
	in
	    ( t, O.RefPat(typInfo(i,t)) )
	end

      | elabAppPat(E, s, I.AppPat(i, pat1, pat2)) =
	let
	    val (t1,pat1') = elabAppPat(E, s, pat1)
	    val (t2,pat2') = elabPat(E, s, pat2)
	    val  t11       = Type.unknown Type.STAR
	    val  t12       = Type.unknown Type.STAR
	    val  t1'       = Type.inArrow(t11,t12)
	    val  _         = Type.unify(t1',t1) handle Type.Unify(t3,t4) =>
				error(i, E.AppPatFunUnify(t1', t1, t3, t4))
	    val  _         = Type.unify(t11,t2) handle Type.Unify(t3,t4) =>
				error(i, E.AppPatUnify(t11, t2, t3, t4))
	in
	    ( t12, O.AppPat(typInfo(i,t12), pat1', pat2') )
	end

      | elabAppPat(E, s, pat) = raise Crash.Crash "Elab.elabAppPat: invalid con"


    and elabPats(E, s, pats) =
	ListPair.unzip(List.map (fn pat => elabPat(E,s,pat)) pats)



  (* Types *)

    and elabVarId_bind'(E, id as I.Id(i, stamp, name)) =
	    O.Id(nonInfo(i), stamp, name)

    and elabVarId_bind(E, k, id as I.Id(i, stamp, name)) =
	let
	    val a = Type.var k
	    (*UNFINISHED: use punning: *)
	    val _ = insertVar(E, stamp, {id=id, var=a})
	in
	    ( a, O.Id(nonInfo(i), stamp, name) )
	end

    and elabVarId(E, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup type variable " ^ x ^ "(" ^ Stamp.toString stamp ^ ") = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp(Type.inVar(#var(lookupVar(E, stamp)))), 60)
val _=print "\n"
*)
	    val a = #var(lookupVar(E, stamp))
	in
	    ( a, O.Id(nonInfo(i), stamp, name) )
	end


    and elabTypId_bind(E, p, t, w, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- insert type " ^ x ^ "(" ^ Stamp.toString stamp ^ ") = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t, 60)
val _=print "\n"
*)
	    (*UNFINISHED: use punning: *)
	    val _ = insertTyp(E, stamp, {id=id, path=p, typ=t, sort=w})
	in
	    O.Id(nonInfo(i), stamp, name)
	end

    and elabTypId(E, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup type " ^ x ^ "(" ^ Stamp.toString stamp ^ ") = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp(#typ(lookupTyp(E, stamp))), 60)
val _=print "\n"
*)
	    val {typ=t, path=p, ...} = lookupTyp(E, stamp)
	in
	    ( t, p, O.Id(nonInfo(i), stamp, name) )
	end

    and elabTypLongid(E, I.ShortId(i, id)) =
	let
	    val (t,_,id') = elabTypId(E, id)
	in
	    ( t, O.ShortId(nonInfo(i), id') )
	end

      | elabTypLongid(E, I.LongId(i, longid, lab)) =
	let
	    val (s,longid') = elabModLongid_path(E, longid)
	    val (l,lab')    = elabLab(E, lab)
	    val  t          = Inf.lookupTyp(s, l)
	in
	    ( t, O.LongId(nonInfo(i), longid', lab') )
	end


    and elabTypKind(E, I.FunTyp(i, id, typ)) =
	    Type.ARROW(Type.STAR, elabTypKind(E, typ))

      | elabTypKind(E, I.ConTyp(i, longid)) =
	let
	    val (t,_) = elabTypLongid(E, longid)
	in
	    Type.kind t
	end

      | elabTypKind(E, I.VarTyp(i, id)) =
	let
	    val (a,_) = elabVarId(E, id)
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


    and elabStarTyp(E, typ) =
	let
	    val ttyp' as (t,typ') = elabTyp(E, typ)
	in
	    case Type.kind t
	      of Type.STAR => ttyp'
	       | k         => error(I.infoTyp typ, E.StarTypKind(k))
	end

    and elabStarTyps(E, typs) =
	ListPair.unzip(List.map (fn typ => elabStarTyp(E, typ)) typs)


    and elabTyp(E, I.VarTyp(i, id)) =
	let
	    val (a,id') = elabVarId(E, id)
	    val  t      = Type.inVar a
	in
	    ( t, O.VarTyp(typInfo(i,t), id') )
	end

      | elabTyp(E, I.ConTyp(i, longid)) =
	let
	    val (t,longid') = elabTypLongid(E, longid)
	in
	    ( t, O.ConTyp(typInfo(i,t), longid') )
	end

      | elabTyp(E, I.FunTyp(i, id, typ)) =
	let
	    val (a,id')   = elabVarId_bind(E, Type.STAR, id)
	    val (t1,typ') = elabTyp(E, typ)
	    val  t        = Type.inLambda(a,t1)
	in
	    ( t, O.FunTyp(typInfo(i,t), id', typ') )
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
	    val  t         = Type.inApp(t1,t2)
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

      | elabTyp(E, I.RowTyp(i, typrow)) =
	let
	    val (t,typrow') = elabRow(elabStarTyp, E, typrow)
	in
	    ( t, O.RowTyp(typInfo(i,t), typrow') )
	end

      | elabTyp(E, I.ArrTyp(i, typ1, typ2)) =
	let
	    val (t1,typ1') = elabStarTyp(E, typ1)
	    val (t2,typ2') = elabStarTyp(E, typ2)
	    val  t         = Type.inArrow(t1,t2)
	in
	    ( t, O.ArrTyp(typInfo(i,t), typ1', typ2') )
	end

      | elabTyp(E, I.SumTyp(i, cons)) =
	let
	    (*UNFINISHED: t0 is not quite right*)
	    val  t0       = Type.unknown(Type.STAR)
	    val (t,cons') = elabCons(E, t0, cons)
	in
	    ( t, O.SumTyp(typInfo(i,t), cons') )
	end

      | elabTyp(E, I.AllTyp(i, id, typ)) =
	let
	    val (a,id')   = elabVarId_bind(E, Type.STAR, id)
	    val (t1,typ') = elabTyp(E, typ)
	    val  t        = Type.inAll(a,t1)
	in
	    ( t, O.AllTyp(typInfo(i,t), id', typ') )
	end

      | elabTyp(E, I.ExTyp(i, id, typ)) =
	let
	    val (a,id')   = elabVarId_bind(E, Type.STAR, id)
	    val (t1,typ') = elabTyp(E, typ)
	    val  t        = Type.inExist(a,t1)
	in
	    ( t, O.ExTyp(typInfo(i,t), id', typ') )
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

      | elabTyp(E, I.SingTyp(i, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	in
	    ( t, O.SingTyp(typInfo(i,t), longid') )
	end

      | elabTyp(E, I.AbsTyp(i)) =
	raise Crash.Crash "Elab.elabTyp: AbsTyp"

      | elabTyp(E, I.ExtTyp(i)) =
	raise Crash.Crash "Elab.elabTyp: ExtTyp"


    and elabCon(E, t0, I.Con(i, id, typs)) =
	let
	    val  id'       = elabValId_bind'(E, id)
	    val  l         = Label.fromName(I.name id)
	    val (ts,typs') = elabStarTyps(E, typs)
	    val  t         = List.foldr Type.inArrow t0 ts
	in
	    ( l, ts, O.Con(typInfo(i,t), id', typs') )
	end

    and elabCons(E, t0, cons) =
	let
	    fun elabCon1(con, (r,cons')) =
		let
		    val (l,ts,con') = elabCon(E, t0, con)
		in
		    ( Type.extendRow(l,ts,r), con'::cons' )
		end

	    val (r,cons') = List.foldr elabCon1 (Type.emptyRow(), []) cons
	    val  t        = Type.inSum r
	in
	    ( t, cons' )
	end


  (* Type representations *)

    (*UNFINISHED: do full traversal to enter all nested constructors
    		  (not needed for SML frontend though...)  *)

    and elabTypRep(E, s, p, t0, buildTyp, buildKind, I.AbsTyp(i))=
	let
	    val t = Type.inCon(buildKind Type.STAR, Type.CLOSED, p)
	in
	    ( t, true, Type.CLOSED, O.AbsTyp(typInfo(i,t)) )
	end

      | elabTypRep(E, s, p, t0, buildTyp, buildKind, I.ExtTyp(i))=
	let
	    val t = Type.inCon(buildKind Type.STAR, Type.OPEN, p)
	in
	    ( t, true, Type.OPEN, O.ExtTyp(typInfo(i,t)) )
	end

      | elabTypRep(E, s, p, t0, buildTyp, buildKind, I.FunTyp(i, id, typ)) =
	let
	    val  k             = Type.STAR
	    val (a,id')        = elabVarId_bind(E, k, id)
	    val (t,gen,w,typ') = elabTypRep(E, s, p,
				      Type.inApp(t0, Type.inVar a),
				      fn t' => Type.inLambda(a,t'),
				      fn k' => Type.ARROW(k, buildKind k'), typ)
	in
	    ( t, gen, w, O.FunTyp(typInfo(i,t), id', typ') )
	end

      | elabTypRep(E, s, p, t0, buildTyp, buildKind, I.SumTyp(i, cons)) =
	let
	    val  t        = Type.inCon (buildKind Type.STAR, Type.CLOSED, p)
	    val (_,cons') = elabConReps(E, s, t0, cons)
	in
	    ( t, true, Type.CLOSED, O.SumTyp(typInfo(i,t), cons') )
	end
	(* Note: structural datatypes would work this way:
	let
	    val (t,cons') = elabConReps(E, s, t0, cons)
	in
	    ( buildTyp t, false, O.SumTyp(typInfo(i,t), cons') )
	end
	*)

      | elabTypRep(E, s, p, t0, buildTyp, buildKind, typ) =
	let
	    val (t,typ') = elabTyp(E, typ)
	in
	    ( t, false, Type.CLOSED, typ' )
	end


    and elabConRep(E, s, t0, I.Con(i, id, typs)) =
	let
	    val  l         = Label.fromName(I.name id)
	    val (ts,typs') = elabStarTyps(E, typs)
	    val (t,p,id')  = elabValId_bind(E, s, Inf.CONSTRUCTOR, id)
	    val  _         = Type.unify(t, List.foldr Type.inArrow t0 ts)
	in
	    ( l, ts, O.Con(typInfo(i,t), id', typs') )
	end

    and elabConReps(E, s, t0, cons) =
	let
	    fun elabCon1(con, (r,cons')) =
		let
		    val (l,ts,con') = elabConRep(E, s, t0, con)
		in
		    ( Type.extendRow(l,ts,r), con'::cons' )
		end

	    val (r,cons') = List.foldr elabCon1 (Type.emptyRow(), []) cons
	    val  t        = Type.inSum r
	in
	    ( t, cons' )
	end


  (* Modules *)

    and elabModId_bind(E, p, j, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val _ = if false then "" else let
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- insert module " ^ x ^ "(" ^ Stamp.toString stamp ^ ") :\n")
val _=PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75)
val _=print "\n"
in ""(*TextIO.inputLine TextIO.stdIn*) end
*)
	    (*UNFINISHED: use punning: *)
	    val _ = insertMod(E, stamp, {id=id, path=p, inf=j})
	in
	    O.Id(nonInfo(i), stamp, name)
	end

    and elabModId(E, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup module " ^ x ^ "(" ^ Stamp.toString stamp ^ ") :\n")
val _=PrettyPrint.output(TextIO.stdOut, PPInf.ppInf(#inf(lookupMod(E, stamp))), 75)
val _=print "\n"
*)
	    val j = #inf(lookupMod(E, stamp))
	in
	    ( j, O.Id(nonInfo(i), stamp, name) )
	end

    and elabModLongid(E, I.ShortId(i, id)) =
	let
	    val (j,id') = elabModId(E, id)
	in
	    ( j, O.ShortId(nonInfo(i), id') )
	end

      | elabModLongid(E, I.LongId(i, longid, lab)) =
	let
	    val (s,longid') = elabModLongid_path(E, longid)
	    val (l,lab')    = elabLab(E, lab)
	    val  j          = Inf.lookupMod(s, l)
	in
	    ( j, O.LongId(nonInfo(i), longid', lab') )
	end

    and elabModLongid_path(E, longid) =
	let
	    val (j,longid') = elabModLongid(E, longid)
	    val  s          = Inf.asSig j handle Inf.Interface =>
				error(I.infoLongid longid,
				      E.ModLongidInf(longid, j))
	in
	    ( s, longid' )
	end


    and elabMod(E, I.PrimMod(i, s, inf)) =
	let
	    val (j,inf') = elabGroundInf(E, inf)
	in
	    ( j, O.PrimMod(infInfo(i,j), s, inf') )
	end

      | elabMod(E, I.VarMod(i, id)) =
	let
	    val (j,id') = elabModId(E, id)
	in
	    ( j, O.VarMod(infInfo(i,j), id') )
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

      | elabMod(E, I.SelMod(i, mod, lab)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	    val (l,lab')  = elabLab(E, lab)
	    val  s        = Inf.asSig j1 handle Inf.Interface =>
				error(I.infoMod mod, E.SelModInf j1)
	    val  j        = Inf.lookupMod(s, l)
	in
	    ( j, O.SelMod(infInfo(i,j), mod', lab') )
	end

      | elabMod(E, I.FunMod(i, id, inf, mod)) =
	let
	    val  _        = insertScope E
	    val (j1,inf') = elabGroundInf(E, inf)
	    val  j1'      = Inf.clone j1
	    val  p        = Path.fromLab(Label.fromName(I.name id))
	    val  _        = Inf.strengthen(p, j1')
	    val  id'      = elabModId_bind(E, p, j1', id)
	    val (j2,mod') = elabMod(E, mod)
	    val  _        = deleteScope E
	    val  j        = Inf.inArrow(p, j1, j2)
	in
	    ( j, O.FunMod(infInfo(i,j), id', inf', mod') )
	end

      | elabMod(E, I.AppMod(i, mod1, mod2)) =
	let
	    val (j1,mod1')  = elabMod(E, mod1)
	    val (j2,mod2')  = elabMod(E, mod2)
	    val (p,j11,j12) = if Inf.isArrow j1 then
				 Inf.asArrow(Inf.instance j1)
			      else
				 error(I.infoMod mod1, E.AppModFunMismatch j1)
(*DEBUG
val _ = (
print "#### Application ####\n\
\#### j_param =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j11, 75);
print "\n\
\#### j_arg =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j2, 75);
print "\n"
)*)
	    val  p2         = case elabMod_path(E, mod2)
				of SOME(p2,_) => p2
(*DEBUG*)
| NONE => Path.fromLab(Label.fromString "?arg")(*
				 | NONE       => Path.invent()
*)
	    val  _          = Inf.strengthen(p2, j2)
(*val _ = (
print "#### p_arg = ";
PrettyPrint.output(TextIO.stdOut, PPPath.ppPath p2, 75);
print "\n\
\#### j_arg' =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j2, 75);
print "\n"
)*)
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
	    val  j        = Inf.instance j2	(* opaque *)
	    val  _        = Inf.match(j1, j2) handle Inf.Mismatch mismatch =>
				error(i, E.AnnModMismatch mismatch)
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
val p = Path.fromLab(Label.fromString "?let")
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
	    val  j'      = Inf.instance j
	    (*UNFINISHED*)
	in
	    unfinished i "elabMod" "packages";
	    ( j', O.UnpackMod(infInfo(i,j), exp', inf') )
	end


    and elabMod_path(E, I.VarMod(i, I.Id(_, stamp, _))) =
	let
	    val {path=p, inf=j, ...} = lookupMod(E, stamp)
	in
	    SOME (p,j)
	end

      | elabMod_path(E, I.SelMod(_, mod, I.Lab(_, l))) =
	(case elabMod_path(E, mod)
	   of NONE      => NONE
	    | SOME(_,j) =>
	      let
		  val s = Inf.asSig j
		  val j = Inf.lookupMod(s, l)
		  val p = Inf.lookupModPath(s, l)
	      in
		  SOME (p,j)
	      end
	)

      | elabMod_path(E, I.AnnMod(_, mod, inf))=
	    elabMod_path(E, mod)

      | elabMod_path _ = NONE


  (* Interfaces *)

    and elabInfId_bind(E, p, j, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- insert interface " ^ x ^ "(" ^ Stamp.toString stamp ^ ") =\n")
val _=PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75)
val _=print "\n"
*)
	    (*UNFINISHED: use punning: *)
	    val _ = insertInf(E, stamp, {id=id, path=p, inf=j})
	in
	    O.Id(nonInfo(i), stamp, name)
	end

    and elabInfId(E, id as I.Id(i, stamp, name)) =
	let
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup interface " ^ x ^ "(" ^ Stamp.toString stamp ^ ") =\n")
val _=PrettyPrint.output(TextIO.stdOut, PPInf.ppInf(#inf(lookupInf(E, stamp))), 75)
val _=print "\n"
*)
	    val j = #inf(lookupInf(E, stamp))
	in
	    ( j, O.Id(nonInfo(i), stamp, name) )
	end

    and elabInfLongid(E, I.ShortId(i, id)) =
	let
	    val (j,id') = elabInfId(E, id)
	in
	    ( j, O.ShortId(nonInfo(i), id') )
	end

      | elabInfLongid(E, I.LongId(i, longid, lab)) =
	let
	    val (s,longid') = elabModLongid_path(E, longid)
	    val (l,lab')    = elabLab(E, lab)
	    val  j          = Inf.lookupInf(s, l)
	in
	    ( j, O.LongId(nonInfo(i), longid', lab') )
	end


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


    and elabInf(E, I.TopInf(i)) =
	let
	    val j = Inf.inTop()
	in
	    ( j, O.TopInf(infInfo(i,j)) )
	end

      | elabInf(E, I.ConInf(i, longid)) =
	let
	    val (j,longid') = elabInfLongid(E, longid)
	    val  j'         = Inf.instance j
	in
	    ( j', O.ConInf(infInfo(i,j'), longid') )
	end

      | elabInf(E, I.SigInf(i, specs)) =
	let
	    val s      = Inf.empty()
	    val specs' = elabSpecs(E, s, specs)
	    val j      = Inf.inSig s
	in
	    ( j, O.SigInf(infInfo(i,j), specs') )
	end

      | elabInf(E, I.FunInf(i, id, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabGroundInf(E, inf1)
	    val  j1'       = Inf.clone j1
	    val  p         = Path.fromLab(Label.fromName(I.name id))
	    val  _         = Inf.strengthen(p, j1')
	    (* UNFINISHED: revert renaming of paths somehow *)
	    val  id'       = elabModId_bind(E, p, j1', id)
	    val (j2,inf2') = elabInf(E, inf2)
	    val  _         = deleteScope E
	    val  j         = Inf.inLambda(p, j1, j2)
	in
	    ( j, O.FunInf(infInfo(i,j), id', inf1', inf2') )
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
(*DEBUG
val _ = (
print "#### Intersection ####\n\
\#### j1 =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j1, 75);
print "\n\
\#### j2 =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j2, 75);
print "\n"
)*)
	    val  j         = Inf.intersect(j1,j2) handle Inf.Mismatch mismatch=>
				error(i, E.CompInfMismatch mismatch)
(*val _ = (
print "#### j =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75);
print "\n\
\#### End Intersection ####\n"
)*)
	in
	    ( j, O.CompInf(infInfo(i,j), inf1', inf2') )
	end

      | elabInf(E, I.ArrInf(i, id, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabGroundInf(E, inf1)
	    val  j1'       = Inf.clone j1
	    val  p         = Path.fromLab(Label.fromName(I.name id))
	    val  _         = Inf.strengthen(p, j1')
	    val  id'       = elabModId_bind(E, p, j1', id)
	    val (j2,inf2') = elabGroundInf(E, inf2)
	    val  _         = deleteScope E
	    val  j         = Inf.inArrow(p, j1, j2)
	in
	    ( j, O.ArrInf(infInfo(i,j), id', inf1', inf2') )
	end

      | elabInf(E, I.SingInf(i, mod)) =
	let
	    val (j,mod') = elabMod(E, mod)
(*DEBUG*)
val _ = Inf.strengthen(Path.fromLab(Label.fromString "?singleton"), j)
	    val  _       = Inf.strengthen(Path.invent(), j)
(*DEBUG
val _ = (
print "#### Singleton ####\n\
\#### j =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75);
print "\n"
)*)
	    val  j'      = Inf.singleton j
(*val _ = (
print "#### j' =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j', 75);
print "\n\
\#### End Singleton ####\n"
)*)
	in
	    ( j', O.SingInf(infInfo(i,j'), mod') )
	end

      | elabInf(E, I.AbsInf(i)) =
	    raise Crash.Crash "Elab.elabInf: AbsInf"


    and elabInfRep(E, p, buildKind, I.AbsInf(i)) =
	let
	    val c = (buildKind(Inf.inGround()), p)
	    val j = Inf.inCon c
	in
	    ( j, true, O.AbsInf(infInfo(i,j)) )
	end

      | elabInfRep(E, p', buildKind, I.FunInf(i, id, inf1, inf2)) =
	let
	    val  _             = insertScope E
	    val (j1,inf1')     = elabGroundInf(E, inf1)
	    val  j1'           = Inf.clone j1
	    val  p             = Path.fromLab(Label.fromName(I.name id))
	    val  _             = Inf.strengthen(p, j1')
	    val  id'           = elabModId_bind(E, p, j1', id)
	    val (j2,gen,inf2') = elabInfRep(E, p',
				     fn k => Inf.inDependent(p,j1,buildKind k),
				     inf2)
	    val  _             = deleteScope E
	    val  j             = Inf.inLambda(p, j1, j2)
	in
	    ( j, gen, O.FunInf(infInfo(i,j), id', inf1', inf2') )
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

      | elabDec(E, s, vars, I.ConDec(i, con, typ)) =
	let
	    val  _          = insertScope E
	    val  _          = Type.enterLevel()
	    val  _          = enterVars(E, vars)
	    val (t,typ')    = elabStarTyp(E, typ)
	    val (l,ts,con') = elabConRep(E, s, t, con)
	    val  _          = Type.exitLevel()
	    val  E'         = splitScope E
	    val  d          = case typ
				of I.SingTyp(_, longid) =>
					SOME(elabValLongid_path(E, longid))
				 | _ => NONE
	    val  _          = appVals (generaliseVal (E, s, SOME d, true)) E'
	in
	    O.ConDec(nonInfo(i), con', typ')
	end

      | elabDec(E, s, vars, I.TypDec(i, id, typ)) =
	let
	    val  p       = Inf.newTyp(s, Label.fromName(I.name id))
	    val (t,typ') = elabTyp(E, typ)
	    val  id'     = elabTypId_bind(E, p, t, Type.CLOSED, id)
	    val  _       = Inf.extendTyp(s, p, Type.kind t, Type.CLOSED, SOME t)
(*DEBUG
val x=case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString(I.stamp id) | x => x
val _=print("type " ^ x ^ " = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t, 60)
val _=print "\n"
*)
	in
	    O.TypDec(nonInfo(i), id', typ')
	end

      | elabDec(E, s, vars, I.DatDec(i, id, typ)) =
	let
	    val  p           = Inf.newTyp(s, Label.fromName(I.name id))
	    val  _           = insertScope E
	    val  _           = Type.enterLevel()
	    val  k           = elabTypKind(E, typ)
	    val  t0          = Type.unknown k
	    val (t,_,w,typ') = elabTypRep(E,s, p, t0, fn t'=>t', fn k'=>k', typ)
	    val  _           = Type.unify(t, t0)
	    val  _           = Type.exitLevel()
	    val  E'          = splitScope E
(*DEBUG
val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString(I.stamp id) | x => x
val _= print("datatype " ^ x ^ " = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t, 60)
val _=print "\n"
*)
	    val  id'         = elabTypId_bind(E, p, t, w, id)
	    val  _           = Inf.extendTyp(s, p, k, w, SOME t)
	    val  _           = appVals (generaliseVal (E,s, SOME NONE, true)) E'
	in
	    O.DatDec(nonInfo(i), id', typ')
	end

      | elabDec(E, s, vars, I.ModDec(i, id, mod)) =
	let
	    val  p       = Inf.newMod(s, Label.fromName(I.name id))
	    val (j,mod') = elabMod(E, mod)
	    val  _       = Inf.strengthen(p, j)
	    val  p'      = case elabMod_path(E, mod)
			     of SOME (p',_) => p'
			      | NONE        => p
	    val  id'     = elabModId_bind(E, p', j, id)
	    val  _       = Inf.extendMod(s, p, j, SOME p')
	in
	    O.ModDec(nonInfo(i), id', mod')
	end

      | elabDec(E, s, vars, I.InfDec(i, id, inf)) =
	let
	    val  p         = Inf.newInf(s, Label.fromName(I.name id))
	    val (j,_,inf') = elabInfRep(E, p, fn k'=>k', inf)
	    val  k         = Inf.kind j
	    val  id'       = elabInfId_bind(E, p, j, id)
	    val  _         = Inf.extendInf(s, p, k, SOME j)
	in
	    O.InfDec(nonInfo(i), id', inf')
	end

      | elabDec(E, s, vars, I.FixDec(i, id, fix)) =
	let
	    val id' = elabValId_bind'(E, id)
	in
	    O.FixDec(nonInfo(i), id', fix)
	end

      | elabDec(E, s, vars, I.VarDec(i, id, dec)) =
	let
	    val id'  = elabVarId_bind'(E, id)
	    val dec' = elabDec(E, s, id::vars, dec)
	in
	    O.VarDec(nonInfo(i), id', dec')
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
	    (* ASSUME that only ValDec and DatDec are under RecDec *)
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
val p = Path.fromLab(Label.fromString "?local")
	    val _     = Inf.strengthenSig(p, s')
	in
	    O.LocalDec(nonInfo(i), decs')
	end


    and enterVars(E, vars) =
	List.app (fn id as I.Id(_, stamp, name) =>
		  (*UNFINISHED: use punning: *)
		  insertVar(E, stamp, {id=id, var = Type.var(Type.STAR)})) vars

    and generaliseVal (E, s, poo, isPoly) (x, {id, path=p, typ=t, sort=w}) =
	let
	    val t' = if isPoly then Type.close t
			       else (Type.lift t ; t) handle Type.Lift a =>
				   error(I.infoId id, E.ValDecLift(id, a))
	    val d  = Option.map (fn po => Option.getOpt(po, p)) poo
	in
	    (*UNFINISHED: use record update: *)
	    (*insertVal(E, x, {entry where typ=t'}));*)
	    insertVal(E, x, {id=id, path=p, typ=t', sort=w});
	    Inf.extendVal(s, p, t', w, d)
(*DEBUG
;let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t', 60);
print(if w = Inf.CONSTRUCTOR then " (* constructor *)\n" else if isPoly then "\n" else " (* not generalised *)\n")
"*)
	end


    and elabDecs(E, s, decs) = List.map (fn dec => elabDec(E, s, [], dec)) decs


  (* Recursive declarations *)

    and elabLHSRecDecs(E, s, decs) =
	List.foldr (fn(dec,xs) => elabLHSRecDec(E,s,dec) @ xs) [] decs

    and elabLHSRecDec(E, s, I.ValDec(i, pat, exp)) =
	    [elabPat(E, s, pat)]

      | elabLHSRecDec(E, s, I.DatDec(i, id, typ)) =
	let
	    val p = Inf.newTyp(s, Label.fromName(I.name id))
	    val k = elabTypKind(E, typ)
	    val t = Type.inRec(Type.unknown k)
	    (* ASSUME that typ does not contain ExtTyp *)
	    val _ = elabTypId_bind(E, p, t, Type.CLOSED, id)
	in
	    []
	end

      | elabLHSRecDec(E, s, I.RecDec(i, decs)) =
	    elabLHSRecDecs(E, s, decs)

      | elabLHSRecDec(E, s, _) = raise Crash.Crash "elabLHSRecDec"


    and elabRHSRecDecs(E, s, rtpats', decs) =
	    List.map (fn dec => elabRHSRecDec(E, s, rtpats', dec)) decs

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

      | elabRHSRecDec(E, s, rtpats', I.DatDec(i, id, typ)) =
	let
	    val (t0,p,id')   = elabTypId(E, id)
	    val (t,_,w,typ') = elabTypRep(E,s, p, t0, fn t'=>t', fn k'=>k', typ)
	    val  _           = Type.unify(t, t0)
	    val  _           = Inf.extendTyp(s, p, Type.kind t, w, SOME t)
(*DEBUG
val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString(I.stamp id) | x => x
val _= print("datatype " ^ x ^ " = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppTyp t, 60)
val _=print "\n"
*)
	in
	    O.DatDec(nonInfo(i), id', typ')
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

    and elabSpec(E, s, vars, I.ValSpec(i, id, typ)) =
	let
	    val (t0,p,id') = elabValId_bind(E, s, Inf.VALUE, id)
	    val (t,typ')   = elabStarTyp(E, typ)
	    val  _         = Type.unify(t,t0)
	    val  _         = Inf.extendVal(s, p, t, Inf.VALUE, NONE)
	in
	    O.ValSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, s, vars, I.ConSpec(i, con, typ)) =
	let
	    val  _          = insertScope E
	    val  _          = Type.enterLevel()
	    val  _          = enterVars(E, vars)
	    val (t,typ')    = elabStarTyp(E, typ)
	    val (l,ts,con') = elabConRep(E, s, t, con)
	    val  _          = Type.exitLevel()
	    val  E'         = splitScope E
	    val  od         = case typ
				of I.SingTyp(_, longid) =>
					SOME(SOME(elabValLongid_path(E,longid)))
				 | _ => NONE
	    val  _          = appVals (generaliseVal (E, s, od, true)) E'
	in
	    O.ConSpec(nonInfo(i), con', typ')
	end

      | elabSpec(E, s, vars, I.TypSpec(i, id, typ)) =
	let
	    val  p       = Inf.newTyp(s, Label.fromName(I.name id))
	    val (t,typ') = elabTyp(E, typ)
	    val  id'     = elabTypId_bind(E, p, t, Type.CLOSED, id)
	    val  _       = Inf.extendTyp(s, p, Type.kind t, Type.CLOSED, SOME t)
	in
	    O.TypSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, s, vars, I.DatSpec(i, id, typ)) =
	let
	    val  p             = Inf.newTyp(s, Label.fromName(I.name id))
	    val  _             = insertScope E
	    val  _             = Type.enterLevel()
	    val  k             = elabTypKind(E, typ)
	    val  t0            = Type.unknown k
	    val (t,gen,w,typ') = elabTypRep(E, s, p, t0,
					    fn t'=>t', fn k'=>k', typ)
	    val  _             = Type.unify(t, t0)
	    val  _             = Type.exitLevel()
	    val  E'            = splitScope E
	    val  id'           = elabTypId_bind(E, p, t, w, id)
	    val  _             = Inf.extendTyp(s, p, k, w,
					       if gen then NONE else SOME t)
	    val  _             = appVals (generaliseVal (E, s, NONE, true)) E'
	in
	    O.DatSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, s, vars, I.ModSpec(i, id, inf)) =
	let
	    val  p       = Inf.newMod(s, Label.fromName(I.name id))
	    val (j,inf') = elabGroundInf(E, inf)
	    val  j'      = Inf.clone j
(*DEBUG
val _ = (
print "#### Mod Spec ####\n\
\#### j =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75);
print "\n\
\#### j' =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j', 75);
print "\n"
)*)
	    val  _       = Inf.strengthen(p, j')
(*val _ = (
print "#### j =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j, 75);
print "\n\
\#### j' =\n";
PrettyPrint.output(TextIO.stdOut, PPInf.ppInf j', 75);
print "\n\
\#### End Mod Spec ####\n"
)*)
	    (* UNFINISHED: revert renaming of paths somehow *)
	    val (p',d)   = case inf
			     of I.SingInf(i', mod) =>
				(case elabMod_path(E, mod)
				   of NONE        => error(i', E.SingInfPath)
				    | SOME (p',_) => (p', SOME p')
				)
			      | _ => (p, NONE)
	    val  id'     = elabModId_bind(E, p', j', id)
	    val  _       = Inf.extendMod(s, p, j, d)
	in
	    O.ModSpec(nonInfo(i), id', inf')
	end

      | elabSpec(E, s, vars, I.InfSpec(i, id, inf)) =
	let
	    val  p           = Inf.newInf(s, Label.fromName(I.name id))
	    val (j,gen,inf') = elabInfRep(E, p, fn k'=>k', inf)
	    val  k           = Inf.kind j
	    val  id'         = elabInfId_bind(E, p, j, id)
	    val  _           = Inf.extendInf(s, p, k,
					     if gen then NONE else SOME j)
	in
	    O.InfSpec(nonInfo(i), id', inf')
	end

      | elabSpec(E, s, vars, I.FixSpec(i, id, fix)) =
	let
	    val id' = elabValId_bind'(E, id)
	in
	    O.FixSpec(nonInfo(i), id', fix)
	end

      | elabSpec(E, s, vars, I.VarSpec(i, id, spec)) =
	let
	    val id'   = elabVarId_bind'(E, id)
	    val spec' = elabSpec(E, s, id::vars, spec)
	in
	    O.VarSpec(nonInfo(i), id', spec')
	end

      | elabSpec(E, s, vars, I.RecSpec(i, specs)) =
	let
	    val _      = insertScope E
	    val _      = Type.enterLevel()
	    val _      = elabLHSRecSpecs(E, s, specs)
	    val specs' = elabRHSRecSpecs(E, s, specs)
	    val _      = Type.exitLevel()
	    val E'     = splitScope E
	    (* ASSUME that only DatSpec is under RecSpec *)
	    val _      = appTyps (fn(x,entry) => insertTyp(E,x,entry)) E'
	    val _      = appVals (generaliseVal (E, s, NONE, true)) E'
	in
	    O.RecSpec(nonInfo(i), specs')
	end

      | elabSpec(E, s, vars, I.LocalSpec(i, specs)) =
	let
	    val s'     = Inf.empty()
	    val specs' = elabSpecs(E, s, specs)
	    val p      = Path.invent()
(*DEBUG*)
val p = Path.fromLab(Label.fromString "?localSpec")
	    val _      = Inf.strengthenSig(p, s')
	in
	    O.LocalSpec(nonInfo(i), specs')
	end

      | elabSpec(E, s, vars, I.ExtSpec(i, inf)) =
	let
	    val (j,inf') = elabGroundInf(E, inf)
	(*UNFINISHED: insert stuff*)
	in
	    unfinished i "elabSpec" "signature extension";
	    O.ExtSpec(nonInfo(i), inf')
	end


    and elabSpecs(E, s, specs) =
	List.map (fn spec => elabSpec(E, s, [], spec)) specs


  (* Recursive specifications *)

    and elabLHSRecSpecs(E, s, specs) =
	List.app (fn spec => elabLHSRecSpec(E,s,spec)) specs

    and elabLHSRecSpec(E, s, I.DatSpec(i, id, typ)) =
	let
	    val p = Inf.newTyp(s, Label.fromName(I.name id))
	    val k = elabTypKind(E, typ)
	    val t = Type.inRec(Type.unknown k)
	    (* ASSUME that typ does not contain ExtTyp *)
	    val _ = elabTypId_bind(E, p, t, Type.CLOSED, id)
	in
	    ()
	end

      | elabLHSRecSpec(E, s, I.RecSpec(i, specs)) =
	    elabLHSRecSpecs(E, s, specs)

      | elabLHSRecSpec(E, s, _) = ()


    and elabRHSRecSpecs(E, s, specs) =
	List.map (fn spec => elabRHSRecSpec(E, s, spec)) specs

    and elabRHSRecSpec(E, s, I.RecSpec(i, specs)) =
	let
	    val spec' = elabRHSRecSpecs(E, s, specs)
	in
	    O.RecSpec(nonInfo(i), spec')
	end

      | elabRHSRecSpec(E, s, I.DatSpec(i, id, typ)) =
	let
	    val (t0,p,id')     = elabTypId(E, id)
	    val (t,gen,w,typ') = elabTypRep(E, s, p, t0,
					    fn t'=>t', fn k'=>k', typ)
	    val  _             = Type.unify(t, t0)
	    val  _             = Inf.extendTyp(s, p, Type.kind t, w,
					       if gen then NONE else SOME t)
	in
	    O.DatSpec(nonInfo(i), id', typ')
	end

      | elabRHSRecSpec(E, s, spec) =
	    elabSpec(E, s, [], spec)



  (* Components *)

    fun elabComp(E, I.Comp(i, imps, decs)) =
	let
	    val imps' = elabImps(E, imps)
	    val s     = Inf.empty()
	    val decs' = elabDecs(E, s, decs)
	    val _     = Inf.close s handle Inf.Unclosed lnt =>
			    error(i, E.CompUnclosed lnt)
(*DEBUG*)
val _ = print "Component signature:\n"
val _ = PrettyPrint.output(TextIO.stdOut, PPInf.ppSig s, 78)
val _ = print "\n"
	in
	    O.Comp(nonInfo(i), imps', decs')
	end

    and elabImp(E, I.Imp(i, specs, url)) =
	let
	    val specs' = elabSpecs(E, Inf.empty(), specs)
	in
	    O.Imp(nonInfo(i), specs', url)
	end

    and elabImps(E, imps) = List.map (fn imp => elabImp(E, imp)) imps



    fun translate E component =
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
