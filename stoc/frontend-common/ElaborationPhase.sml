(*
 * Note: We assume complete binding analysis and alpha conversion has been
 * performed on the input program. So we would not need to bother with scoping.
 * Nevertheless, we sometimes use scopes to reduce the size of the symbol
 * table.
 *)

structure ElaborationPhase :> ELABORATION_PHASE =
  struct

    structure I = AbstractGrammar
    structure O = TypedGrammar
    structure E = ElaborationError

    open Env

    nonfix mod

  (* Error *)

    val error = E.error

  (* Until we are finished... *)

    fun unfinished i funname casename =
	Error.warn(i, "Elab." ^ funname ^ ": " ^ casename ^ " not checked yet")


  (* Predefined types *)

    fun boolTyp E   = #2(lookupTyp(E, Prebound.stamp_bool))
    fun exnTyp E    = #2(lookupTyp(E, Prebound.stamp_exn))

    (* UNFINISHED: overloading *)
    fun wordTyp E   = #2(lookupTyp(E, Prebound.stamp_exn))
    fun intTyp E    = #2(lookupTyp(E, Prebound.stamp_int))
    fun charTyp E   = #2(lookupTyp(E, Prebound.stamp_char))
    fun stringTyp E = #2(lookupTyp(E, Prebound.stamp_string))
    fun realTyp E   = #2(lookupTyp(E, Prebound.stamp_real))

    fun refTyp(E,t) = Type.inApp(#2(lookupTyp(E, Prebound.stamp_tref)), t)
    fun vecTyp(E,t) = Type.inApp(#2(lookupTyp(E, Prebound.stamp_vec)), t)
    fun listTyp(E,t)= Type.inApp(#2(lookupTyp(E, Prebound.stamp_list)), t)


  (* Output info field *)

    fun nonInfo(i)   = (i, TypedInfo.NON)
    fun typInfo(i,t) = (i, TypedInfo.TYP t)
    fun infInfo(i,j) = (i, TypedInfo.INF j)


  (* Check value restriction *)

    fun isValue( I.LitExp _
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


  (* Elaborate kind of type (no higher kinds yet) *)

    fun elabTypKind(E, I.FunTyp(i, id, typ)) =
	    Type.ARROW(Type.STAR, elabTypKind(E, typ))

      | elabTypKind(E, _) =
	    Type.STAR


  (* Rows (polymorphic, thus put here) *)

    fun elabLab(E, I.Lab(i, s)) = ( Lab.fromString s, O.Lab(nonInfo(i), s) )

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

    fun elabValId(E, id as I.Id(i, stamp, name)) =
	(* May be binding occurance *)
	let
(*DEBUG*)
val x= case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup val " ^ x ^ "(" ^ Stamp.toString stamp ^ ") : ")
val _=(PrettyPrint.output(TextIO.stdOut, PPType.ppType(#2(lookupVal(E, stamp))), 600)
;print "\n")
handle Lookup _ => ()

	    val t = Type.instance(#2(lookupVal(E, stamp)))
		    handle Lookup _ =>
			let val t = Type.unknown Type.STAR in
(*DEBUG*)
print "'_? (* not found *)\n";
			    insertVal(E, stamp, (id,t)) ; t
			end
(*(*DEBUG*)
val x= case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- instantiated " ^ x ^ "(" ^ Stamp.toString stamp ^ ") : ")
val _=(PrettyPrint.output(TextIO.stdOut, PPType.ppType t, 600)
;print "\n")
*)
	in
	    ( t, O.Id(typInfo(i,t), stamp, name) )
	end

    and elabValLongid(E, I.ShortId(i, id)) =
	let
	    val (t,id') = elabValId(E, id)
	in
	    ( t, O.ShortId(typInfo(i,t), id') )
	end

      | elabValLongid(E, I.LongId(i, longid, lab)) =
	(*UNFINISHED*)
	let
	    val (E',longid') = elabModLongid(E, longid)
	    val (l,lab')     = elabLab(E, lab)
	    val  t           = Type.unknown Type.STAR 
	in
	    unfinished i "elabValLongid" "long ids";
	    ( t, O.LongId(typInfo(i,t), longid', lab') )
	end


    and elabExp(E, I.LitExp(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitExp(typInfo(i,t), lit') )
	end

      | elabExp(E, I.PrimExp(i, s, typ)) =
	let
	    val (t,typ') = elabTyp(E, typ)
	in
	    ( t, O.PrimExp(typInfo(i,t), s, typ') )
	end

      | elabExp(E, I.VarExp(i, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	in
	    ( t, O.VarExp(typInfo(i,t), longid') )
	end

      | elabExp(E, I.ConExp(i, n, longid)) =
	let
	    val (t,longid') = elabValLongid(E, longid)
	in
	    ( t, O.ConExp(typInfo(i,t), n, longid') )
	end

      | elabExp(E, I.RefExp(i)) =
	let
	    val t = refTyp(E, Type.unknown Type.STAR)
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
	    val  r       = Type.extendRow(l, [Type.unknown Type.STAR],
					     Type.unknownRow())
	    val  t       = Type.inRow r
	in
	    ( t, O.SelExp(typInfo(i,t), lab') )
	end

      | elabExp(E, I.VecExp(i, exps)) =
	let
	    val (ts,exps') = elabExps(E, exps)
	    val  t         = vecTyp(E, List.hd ts)
	    val  _         = Type.unifyList ts
			     handle Type.UnifyList(n,t1,t2) =>
				error(I.infoExp(List.nth(exps,n)),
				      E.VecExpUnify(t, List.nth(ts,n), t1, t2))
	in
	    ( t, O.VecExp(typInfo(i,t), exps') )
	end

      | elabExp(E, I.FunExp(i, id, exp)) =
	let
	    val (t1,id')  = elabValId(E, id)
	    val (t2,exp') = elabExp(E, exp)
	    val  t        = Type.inArrow(t1,t2)
	in
	    ( t, O.FunExp(typInfo(i,t), id', exp') )
	end

      | elabExp(E, I.AppExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t11       = Type.unknown Type.STAR
	    val  t12       = Type.unknown Type.STAR
	    val  t1'       = Type.inArrow(t11,t12)
	    val  _         = Type.unify(t1',t1)
			     handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.AppExpFunUnify(t1', t1, t3, t4))
	    val  _         = Type.unify(t11,t2)
			     handle Type.Unify(t3,t4) =>
((*DEBUG*)
print "function domain:  ";
PrettyPrint.output(TextIO.stdOut, PPType.ppType t11, 600);
print "\n";
print "argument type:    ";
PrettyPrint.output(TextIO.stdOut, PPType.ppType t2, 600);
print "\n";
print "mismatch between: ";
PrettyPrint.output(TextIO.stdOut, PPType.ppType t3, 600);
print "\n";
print "and:              ";
PrettyPrint.output(TextIO.stdOut, PPType.ppType t4, 600);
print "\n";
				error(i, E.AppExpArgUnify(t11, t2, t3, t4))
)
	in
	    ( t12, O.AppExp(typInfo(i,t12), exp1', exp2') )
	end

      | elabExp(E, I.CompExp(i, exp1, exp2)) =
	(* UNFINISHED *)
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t         = Type.unknown Type.STAR
	in
	    unfinished i "elabExp" "record composition";
	    ( t, O.CompExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.AndExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t         = boolTyp E
	    val  _         = Type.unify(t1,t)
			     handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.AndExpUnify(t1, t, t3, t4))
	    val  _         = Type.unify(t2,t)
			     handle Type.Unify(t3,t4) =>
				error(I.infoExp exp2,
				      E.AndExpUnify(t2, t, t3, t4))
	in
	    ( t, O.AndExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.OrExp(i, exp1, exp2)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val  t         = boolTyp E
	    val  _         = Type.unify(t1,t)
			     handle Type.Unify(t3,t4) =>
				error(I.infoExp exp1,
				      E.OrExpUnify(t1, t, t3, t4))
	    val  _         = Type.unify(t2,t)
			     handle Type.Unify(t3,t4) =>
				error(I.infoExp exp2,
				      E.OrExpUnify(t2, t, t3, t4))
	in
	    ( t, O.OrExp(typInfo(i,t), exp1', exp2') )
	end

      | elabExp(E, I.IfExp(i, exp1, exp2, exp3)) =
	let
	    val (t1,exp1') = elabExp(E, exp1)
	    val (t2,exp2') = elabExp(E, exp2)
	    val (t3,exp3') = elabExp(E, exp3)
	    val  tb        = boolTyp E
	    val  _         = Type.unify(t1,tb)
			     handle Type.Unify(t4,t5) =>
				error(I.infoExp exp1,
				      E.IfExpCondUnify(t1, tb, t4, t5))
	    val  _         = Type.unify(t2,t3)
			     handle Type.Unify(t4,t5) =>
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
	    val  _         = Type.unify(t1,tb)
			     handle Type.Unify(t3,t4) =>
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
	    val  _        = Type.unify(t1,te)
			    handle Type.Unify(t2,t3) =>
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
	    val  _           = Type.unify(t1,t2)
			       handle Type.Unify(t3,t4) =>
				error(i, E.HandleExpUnify(t1, t2, t3, t4))
	in
	    ( t1, O.HandleExp(typInfo(i,t1), exp', matchs') )
	end

      | elabExp(E, I.AnnExp(i, exp, typ)) =
	let
	    val (t1,exp') = elabExp(E, exp)
	    val (t2,typ') = elabStarTyp(E, typ)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(i, E.AnnExpUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AnnExp(typInfo(i,t2), exp', typ') )
	end

      | elabExp(E, I.LetExp(i, decs, exp)) =
	let
	    val  _          = insertScope E
	    val  decs'      = elabDecs(E, decs)
	    val (t,exp')    = elabExp(E, exp)
	    val  _          = deleteScope E
	in
	    ( t, O.LetExp(typInfo(i,t), decs', exp') )
	end


    and elabExps(E, exps) =
	ListPair.unzip(List.map (fn exp => elabExp(E,exp)) exps)


  (* Matches *)

    and elabMatch(E, t1, t2, I.Match(i, pat, exp)) =
	let
	    val  _        = insertScope E
	    val (t3,pat') = elabPat(E, pat)
	    val  _        = Type.unify(t1,t3)
			    handle Type.Unify(t5,t6) =>
				error(I.infoPat pat,
				      E.MatchPatUnify(t1, t3, t5, t6))
	    val (t4,exp') = elabExp(E, exp)
	    val  _        = Type.unify(t2,t4)
			    handle Type.Unify(t5,t6) =>
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

    and elabPat(E, I.JokPat(i)) =
	let
	    val t = Type.unknown Type.STAR
	in
	    ( t, O.JokPat(typInfo(i,t)) )
	end

      | elabPat(E, I.LitPat(i, lit)) =
	let
	    val (t,lit') = elabLit(E, lit)
	in
	    ( t, O.LitPat(typInfo(i,t), lit') )
	end

      | elabPat(E, I.VarPat(i, id)) =
	let
	    val (t,id') = elabValId(E, id)
	in
	    ( t, O.VarPat(typInfo(i,t), id') )
	end

      | elabPat(E, I.ConPat(i, longid, pats)) =
	let
	    fun elabArgs(t1, []) =
		if Type.isArrow t1 then
		    error(i, E.ConPatFewArgs(longid))
		else
		    t1

	      | elabArgs(t1, t2::ts) =
		let
		    val  t11  = Type.unknown Type.STAR
		    val  t12  = Type.unknown Type.STAR
		    val  t1'  = Type.inArrow(t11,t12)
		    val  _    = Type.unify(t1',t1)
				handle Type.Unify(t3,t4) =>
				    error(i, E.ConPatManyArgs(longid))
		    val  _    = Type.unify(t11,t2)
				handle Type.Unify(t3,t4) =>
				    error(i, E.ConPatUnify(t11, t2, t3, t4))
		in
		    elabArgs(t12, ts)
		end

	    val (t1,longid') = elabValLongid(E, longid)
	    val (ts,pats')   = elabPats(E, pats)
	    val  t           = elabArgs(t1,ts)
	in
	    ( t, O.ConPat(typInfo(i,t), longid', pats') )
	end

      | elabPat(E, I.RefPat(i, pat)) =
	let
	    val (t1,pat') = elabPat(E, pat)
	    val  t        = refTyp(E, t1)
	in
	    ( t, O.RefPat(typInfo(i,t), pat') )
	end

      | elabPat(E, I.TupPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, pats)
	    val  t         = Type.inTuple ts
	in
	    ( t, O.TupPat(typInfo(i,t), pats') )
	end

      | elabPat(E, I.RowPat(i, patrow)) =
	let
	    val (t,patrow') = elabRow(elabPat, E, patrow)
	in
	    ( t, O.RowPat(typInfo(i,t), patrow') )
	end

      | elabPat(E, I.VecPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, pats)
	    val  t         = vecTyp(E, List.hd ts)
	    val  _         = Type.unifyList ts
			     handle Type.UnifyList(n,t1,t2) =>
				error(I.infoPat(List.nth(pats,n)),
				      E.VecPatUnify(t, List.nth(ts,n), t1, t2))
	in
	    ( t, O.VecPat(typInfo(i,t), pats') )
	end

      | elabPat(E, I.AsPat(i, pat1, pat2)) =
	let
	    val (t1,pat1') = elabPat(E, pat1)
	    val (t2,pat2') = elabPat(E, pat2)
	    val  _         = Type.unify(t1,t2)
			     handle Type.Unify(t3,t4) =>
				error(i, E.AsPatUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AsPat(typInfo(i,t2), pat1', pat2') )
	end

      | elabPat(E, I.AltPat(i, pats)) =
	let
	    val (ts,pats') = elabPats(E, pats)
	    val  t         = List.hd ts
	    val  _         = Type.unifyList ts
			     handle Type.UnifyList(n,t1,t2) =>
				error(I.infoPat(List.nth(pats,n)),
				      E.AltPatUnify(t, List.nth(ts,n), t1, t2))
	in
	    ( t, O.AltPat(typInfo(i,t), pats') )
	end

      | elabPat(E, I.NegPat(i, pat)) =
	let
	    val (t,pat') = elabPat(E, pat)
	in
	    ( t, O.NegPat(typInfo(i,t), pat') )
	end

      | elabPat(E, I.GuardPat(i, pat, exp)) =
	let
	    val (t1,pat') = elabPat(E, pat)
	    val (t2,exp') = elabExp(E, exp)
	    val  tb       = boolTyp E
	    val  _        = Type.unify(t2,tb)
			    handle Type.Unify(t3,t4) =>
				error(i, E.GuardPatUnify(t2, tb, t3, t4))
	in
	    ( t1, O.GuardPat(typInfo(i,t1), pat', exp') )
	end

      | elabPat(E, I.AnnPat(i, pat, typ)) =
	let
	    val (t1,pat') = elabPat(E, pat)
	    val (t2,typ') = elabStarTyp(E, typ)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(i, E.AnnPatUnify(t1, t2, t3, t4))
	in
	    ( t2, O.AnnPat(typInfo(i,t2), pat', typ') )
	end

      | elabPat(E, I.WithPat(i, pat, decs)) =
	let
	    val (t,pat') = elabPat(E, pat)
	    val  decs'   = elabDecs(E, decs)
	in
	    ( t, O.WithPat(typInfo(i,t), pat', decs') )
	end


    and elabPats(E, pats) =
	ListPair.unzip(List.map (fn pat => elabPat(E,pat)) pats)


  (* Types *)

    and elabVarId(E, k, id as I.Id(i, stamp, name)) =
	(* May be binding occurance *)
	let
	    val a = #2(lookupVar(E, stamp))
		    handle Lookup _ =>
			let val a = Type.var k in
			    insertVar(E, stamp, (id,a)) ; a
			end
	in
	    ( a, O.Id(nonInfo(i), stamp, name) )
	end


    and elabTypId(E, k, id as I.Id(i, stamp, name)) =
	(* May be binding occurance *)
	let
(*DEBUG*)
val x= case Name.toString(I.name id) of "?" => "?" | x => x
val _=print("-- lookup type " ^ x ^ "(" ^ Stamp.toString stamp ^ ") = ")
val _=(PrettyPrint.output(TextIO.stdOut, PPType.ppType(#2(lookupTyp(E, stamp))), 600)
;print "\n")
handle Lookup _ => ()
	    val t = #2(lookupTyp(E, stamp))
		    handle Lookup _ =>
			let val t = Type.unknown k in
(*DEBUG*)
print"'_? (* not found *)\n";
			    insertTyp(E, stamp, (id,t)) ; t
			end
	in
	    ( t, O.Id(typInfo(i,t), stamp, name) )
	end

    and elabTypLongid(E, I.ShortId(i, id)) =
	let
	    val (t,id') = elabTypId(E, Type.STAR, id)
	in
	    ( t, O.ShortId(typInfo(i,t), id') )
	end

      | elabTypLongid(E, I.LongId(i, longid, lab)) =
	(*UNFINISHED*)
	let
	    val (E',longid') = elabModLongid(E, longid)
	    val (l,lab')     = elabLab(E, lab)
	    val  t           = Type.unknown Type.STAR 
	in
	    unfinished i "elabTypLongid" "long ids";
	    ( t, O.LongId(typInfo(i,t), longid', lab') )
	end


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
	    val (a,id') = elabVarId(E, Type.STAR, id)
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
	    val (a,id')   = elabVarId(E, Type.STAR, id)
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
	    val (t,cons') = elabCons(E, cons)
	in
	    ( t, O.SumTyp(typInfo(i,t), cons') )
	end

      | elabTyp(E, I.AllTyp(i, id, typ)) =
	let
	    val (a,id')   = elabVarId(E, Type.STAR, id)
	    val (t1,typ') = elabTyp(E, typ)
	    val  t        = Type.inAll(a,t1)
	in
	    ( t, O.AllTyp(typInfo(i,t), id', typ') )
	end

      | elabTyp(E, I.ExTyp(i, id, typ)) =
	let
	    val (a,id')   = elabVarId(E, Type.STAR, id)
	    val (t1,typ') = elabTyp(E, typ)
	    val  t        = Type.inExist(a,t1)
	in
	    ( t, O.ExTyp(typInfo(i,t), id', typ') )
	end

      | elabTyp(E, I.AbsTyp(i)) =
	Crash.crash "Elab.elabTyp: AbsTyp"

      | elabTyp(E, I.ExtTyp(i)) =
	Crash.crash "Elab.elabTyp: ExtTyp"

      | elabTyp(E, I.SingTyp(i, longid)) =
	Crash.crash "Elab.elabTyp: SingTyp"


    and elabCon(E, I.Con(i, id, typs)) =
	let
	    val  l         = Lab.fromString(I.lab(I.idToLab id))
	    val (t,id')    = elabValId(E, id)
	    val (ts,typs') = elabStarTyps(E, typs)
	in
	    ( l, ts, O.Con(nonInfo(i), id', typs') )
	end

    and elabCons(E, cons) =
	let
	    fun elabCon1(con, (r,cons')) =
		let
		    val (l,ts,con') = elabCon(E, con)
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

    and elabTypRep(E, id', t0, buildTyp, buildKind, I.ConTyp(i, longid)) =
	let
	    val (t,longid') = elabTypLongid(E, longid)
	in
	    ( buildTyp t, O.ConTyp(typInfo(i,t), longid') )
	end

      | elabTypRep(E, O.Id(_,stamp,name), t0, buildTyp, buildKind, I.AbsTyp(i))=
	let
	    val p = Path.PLAIN(stamp, name)
	    val t = Type.inCon (buildKind Type.STAR, Type.CLOSED, p)
	in
	    ( t, O.AbsTyp(typInfo(i,t)) )
	end

      | elabTypRep(E, O.Id(_,stamp,name), t0, buildTyp, buildKind, I.ExtTyp(i))=
	let
	    val p = Path.PLAIN(stamp, name)
	    val t = Type.inCon (buildKind Type.STAR, Type.OPEN, p)
	in
	    ( t, O.AbsTyp(typInfo(i,t)) )
	end

      | elabTypRep(E, id', t0, buildTyp, buildKind, I.FunTyp(i, id, typ)) =
	let
	    val (a,id1') = elabVarId(E, Type.STAR, id)
	    val  k1      = Type.kindVar a
	    val (t,typ') = elabTypRep(E, id', Type.inApp(t0, Type.inVar a),
				      fn t => Type.inLambda(a,t),
				      fn k => Type.ARROW(k1, buildKind k), typ)
	in
	    ( t, O.FunTyp(typInfo(i,t), id', typ') )
	end

      | elabTypRep(E, id', t0, buildTyp, buildKind, I.SumTyp(i, cons)) =
	let
	    val (t,cons') = elabConReps(E, t0, cons)
	in
	    ( buildTyp t, O.SumTyp(typInfo(i,t), cons') )
	end

      | elabTypRep(E, id', t0, buildTyp, buildKind, typ) =
	    elabTyp(E, typ)


    and elabConRep(E, t0, I.Con(i, id, typs)) =
	let
	    val  l         = Lab.fromString(I.lab(I.idToLab id))
	    val (t,id')    = elabValId(E, id)
	    val (ts,typs') = elabStarTyps(E, typs)
	    val  _         = Type.unify(t, List.foldr Type.inArrow t0 ts)
			     handle Type.Unify _ => Crash.crash"Elab.elabConRep"
	in
	    ( l, ts, O.Con(nonInfo(i), id', typs') )
	end

    and elabConReps(E, t0, cons) =
	let
	    fun elabCon1(con, (r,cons')) =
		let
		    val (l,ts,con') = elabConRep(E, t0, con)
		in
		    ( Type.extendRow(l,ts,r), con'::cons' )
		end

	    val (r,cons') = List.foldr elabCon1 (Type.emptyRow(), []) cons
	    val  t        = Type.inSum r
	in
	    ( t, cons' )
	end


  (* Modules *)

    and elabModId(E, id as I.Id(i, stamp, name)) =
	(* May be binding occurance *)
	let
	    val j = #2(lookupMod(E, stamp))
		    handle Lookup _ =>
			(*UNFINISHED*)
			let val j = () in
			    insertMod(E, stamp, (id,j,new())) ; j
			end
	in
	    ( j, O.Id(infInfo(i,j), stamp, name) )
	end

    and elabModLongid(E, I.ShortId(i, id)) =
	let
	    val (j,id') = elabModId(E, id)
	in
	    ( j, O.ShortId(infInfo(i,j), id') )
	end

      | elabModLongid(E, I.LongId(i, longid, lab)) =
	(*UNFINISHED*)
	let
	    val (E',longid') = elabModLongid(E, longid)
	    val (l,lab')     = elabLab(E, lab)
	    val  j           = ()
	in
	    unfinished i "elabModLongid" "long ids";
	    ( j, O.LongId(infInfo(i,j), longid', lab') )
	end


    and elabMod(E, I.VarMod(i, id)) =
	let
	    val (j,id') = elabModId(E, id)
	in
	    ( j, O.VarMod(infInfo(i,j), id') )
	end

      | elabMod(E, I.StrMod(i, decs)) =
	let
	    val decs' = elabDecs(E, decs)
	(*UNFINISHED*)
	    val j     = ()
	in
	    ( j, O.StrMod(infInfo(i,j), decs') )
	end

      | elabMod(E, I.SelMod(i, mod, lab)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	    val (l,lab')  = elabLab(E, lab)
	(*UNFINISHED*)
	    val  j        = ()
	in
	    ( j, O.SelMod(infInfo(i,j), mod', lab') )
	end

      | elabMod(E, I.FunMod(i, id, inf, mod)) =
	let
	    val  _        = insertScope E
	    val (j1,inf') = elabInf(E, inf)
	(*UNFINISHED: copy j1 and strengthen it *)
	    val (j3,id')  = elabModId(E, id)
	    val (j2,mod') = elabMod(E, mod)
	    val  _        = deleteScope E
	(*UNFINISHED*)
	    val  j        = ()
	in
	    ( j, O.FunMod(infInfo(i,j), id', inf', mod') )
	end

      | elabMod(E, I.AppMod(i, mod1, mod2)) =
	let
	    val (j1,mod1') = elabMod(E, mod1)
	    val (j2,mod2') = elabMod(E, mod2)
	(*UNFINISHED*)
	    val  j         = ()
	in
	    ( j, O.AppMod(infInfo(i,j), mod1', mod2') )
	end

      | elabMod(E, I.AnnMod(i, mod, inf)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	    val (j2,inf') = elabInf(E, inf)
	(*UNFINISHED*)
	    val  j        = ()
	in
	    unfinished i "elabMod" "annotated modules";
	    ( j, O.AnnMod(infInfo(i,j), mod', inf') )
	end

      | elabMod(E, I.LetMod(i, decs, mod)) =
	let
	    val  _          = insertScope E
	    val  decs'      = elabDecs(E, decs)
	    val (j,mod')    = elabMod(E, mod)
	    val  _          = deleteScope E
	in
	    ( j, O.LetMod(infInfo(i,j), decs', mod') )
	end


  (* Interfaces *)

    and elabInfId(E, id as I.Id(i, stamp, name)) =
	(* May be binding occurance *)
	let
	    val j = #2(lookupInf(E, stamp))
		    handle Lookup _ =>
			(*UNFINISHED*)
			let val j = () in
			    insertInf(E, stamp, (id,j,new())) ; j
			end
	in
	    ( j, O.Id(infInfo(i,j), stamp, name) )
	end

    and elabInfLongid(E, I.ShortId(i, id)) =
	let
	    val (j,id') = elabInfId(E, id)
	in
	    ( j, O.ShortId(infInfo(i,j), id') )
	end

      | elabInfLongid(E, I.LongId(i, longid, lab)) =
	(*UNFINISHED*)
	let
	    val (E',longid') = elabInfLongid(E, longid)
	    val (l,lab')     = elabLab(E, lab)
	    val  j           = ()
	in
	    unfinished i "elabModLongid" "long ids";
	    ( j, O.LongId(infInfo(i,j), longid', lab') )
	end


    and elabInf(E, I.AnyInf(i)) =
	let
	(*UNFINISHED*)
	    val j = ()
	in
	    ( j, O.AnyInf(infInfo(i,j)) )
	end

      | elabInf(E, I.ConInf(i, longid)) =
	let
	    val (j,longid') = elabInfLongid(E, longid)
	in
	    ( j, O.ConInf(infInfo(i,j), longid') )
	end

      | elabInf(E, I.SigInf(i, specs)) =
	let
	    val specs' = elabSpecs(E, specs)
	(*UNFINISHED*)
	    val j      = ()
	in
	    ( j, O.SigInf(infInfo(i,j), specs') )
	end

      | elabInf(E, I.FunInf(i, id, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabInf(E, inf1)
	(*UNFINISHED: copy j1 and strengthen it *)
	    val (j3,id')   = elabModId(E, id)
	    val (j2,inf2') = elabInf(E, inf2)
	    val  _         = deleteScope E
	(*UNFINISHED*)
	    val  j        = ()
	in
	    ( j, O.FunInf(infInfo(i,j), id', inf1', inf2') )
	end

      | elabInf(E, I.AppInf(i, inf, mod)) =
	let
	    val (j1,inf') = elabInf(E, inf)
	    val (j2,mod') = elabMod(E, mod)
	(*UNFINISHED*)
	    val j = ()
	in
	    ( j, O.AppInf(infInfo(i,j), inf', mod') )
	end

      | elabInf(E, I.CompInf(i, inf1, inf2)) =
	let
	    val (j1,inf1') = elabInf(E, inf1)
	    val (j2,inf2') = elabInf(E, inf2)
	(*UNFINISHED*)
	    val j = ()
	in
	    ( j, O.CompInf(infInfo(i,j), inf1', inf2') )
	end

      | elabInf(E, I.ArrInf(i, id, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabInf(E, inf1)
	(*UNFINISHED: copy j1 and strengthen it *)
	    val (j3,id')   = elabModId(E, id)
	    val (j2,inf2') = elabInf(E, inf2)
	    val  _         = deleteScope E
	(*UNFINISHED*)
	    val  j         = ()
	in
	    ( j, O.ArrInf(infInfo(i,j), id', inf1', inf2') )
	end

      | elabInf(E, I.SingInf(i, mod)) =
	let
	    val (j1,mod') = elabMod(E, mod)
	(*UNFINISHED*)
	    val j = ()
	in
	    ( j, O.SingInf(infInfo(i,j), mod') )
	end

      | elabInf(E, I.AbsInf(i)) =
	Crash.crash "Elab.elabInf: AbsInf"


    and elabInfRep(E, O.Id(_,stamp,name), I.AbsInf(i)) =
	let
	    val p = Path.PLAIN(stamp, name)
	(*UNFINISHED*)
	    val j = ()
	in
	    ( j, O.AbsInf(infInfo(i,j)) )
	end

      | elabInfRep(E, id', I.FunInf(i, id, inf1, inf2)) =
	let
	    val  _         = insertScope E
	    val (j1,inf1') = elabInf(E, inf1)
	(*UNFINISHED: copy j1 and strengthen it *)
	    val (j3,id1')  = elabModId(E, id)
	    val (j2,inf2') = elabInfRep(E, id', inf2)
	    val  _         = deleteScope E
	(*UNFINISHED*)
	    val  j        = ()
	in
	    ( j, O.FunInf(infInfo(i,j), id1', inf1', inf2') )
	end

      | elabInfRep(E, id', inf) =
	    elabInf(E, inf)


  (* Declarations *)

    and elabDec(E, I.ValDec(i, pat, exp)) =
	let
	    val  _        = insertScope E
	    val  _        = Type.enterLevel()
	    val  _        = insertScope E
	    val (t2,exp') = elabExp(E, exp)
	    val  _        = deleteScope E
	    val (t1,pat') = elabPat(E, pat)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(i, E.ValDecUnify(t1, t2, t3, t4))
	    val  _        = Type.exitLevel()
(*DEBUG*)
infix andthen
fun a andthen b = b
	    val  _        = if isValue exp then
				appVals (fn(x,(id,t)) =>
(*DEBUG*)
(let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppType(Type.close t), 600);
print "\n") andthen
					 insertVal(E, x, (id, Type.close t))
					) (splitScope E)
			    else
(*DEBUG*)
appVals (fn(x,(id,t)) =>
(let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppType t, 600);
print " (* not generalised *)\n")) (copyScope E) andthen
				mergeScope E
	in
	    O.ValDec(nonInfo(i), pat', exp')
	end

      | elabDec(E, I.ConDec(i, con, typ)) =
	let
	    val  _          = insertScope E
	    val  _          = Type.enterLevel()
	    val (t,typ')    = elabTyp(E, typ)
	    val (l,ts,con') = elabConRep(E, t, con)
	    val  _          = Type.exitLevel()
	    val  E'         = splitScope E
	    val  _          = appVals (fn(x,(id,t)) =>
(*DEBUG*)
((let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppType(Type.close t), 600);
print " (* constructor *)\n");
					 insertVal(E, x, (id, Type.close t))
)				      ) E'
	in
	    O.ConDec(nonInfo(i), con', typ')
	end

      | elabDec(E, I.TypDec(i, id, typ)) =
	let
	    val  k        = elabTypKind(E, typ)
	    val (t1,id')  = elabTypId(E, k, id)
	    val (t2,typ') = elabTyp(E, typ)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(I.infoTyp typ,
				      E.TypDecUnify(t1, t2, t3, t4))
(*DEBUG*)
val x=case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString(I.stamp id) | x => x
val _=print("type " ^ x ^ " = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppType t1, 600)
val _=print "\n"
	in
	    O.TypDec(nonInfo(i), id', typ')
	end

      | elabDec(E, I.DatDec(i, id, typ)) =
	let
	    val  _        = insertScope E
	    val  _        = Type.enterLevel()
	    val  k        = elabTypKind(E, typ)
	    val (t1,id')  = elabTypId(E, k, id)
	    val (t2,typ') = elabTypRep(E, id', t1, fn t => t, fn k => k, typ)
	    val  _        = Type.exitLevel()
	    val  E'       = splitScope E
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
((*DEBUG*)
print"DatDec unification failure:\n";
print("t1 = ");
PrettyPrint.output(TextIO.stdOut, PPType.ppType t1, 600);
print "\n";
print("t2 = ");
PrettyPrint.output(TextIO.stdOut, PPType.ppType t2, 600);
print "\n";
print("t3 = ");
PrettyPrint.output(TextIO.stdOut, PPType.ppType t3, 600);
print "\n";
print("t4 = ");
PrettyPrint.output(TextIO.stdOut, PPType.ppType t4, 600);
print "\n";
				error(I.infoTyp typ,
				      E.DatDecUnify(t1, t2, t3, t4))
)
(*DEBUG*)
val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString(I.stamp id) | x => x
val _= print("datatype " ^ x ^ " = ")
val _=PrettyPrint.output(TextIO.stdOut, PPType.ppType t1, 600)
val _=print "\n"
	    val  _        = appVals (fn(x,(id,t)) =>
(*DEBUG*)
((let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppType(Type.close t), 600);
print " (* constructor *)\n");
					 insertVal(E, x, (id, Type.close t))
)					) E'
	in
	    O.DatDec(nonInfo(i), id', typ')
	end

      | elabDec(E, I.ModDec(i, id, mod)) =
	let
	    val (j1,id')  = elabModId(E, id)
	    val (j2,mod') = elabMod(E, mod)
	(*UNFINISHED: strengthen j2, assign it to j1 *)
	in
	    unfinished i "elabDec" "module declarations";
	    O.ModDec(nonInfo(i), id', mod')
	end

      | elabDec(E, I.InfDec(i, id, inf)) =
	let
	    val (j1,id')  = elabInfId(E, id)
	    val (j2,inf') = elabInfRep(E, id', inf)
	(*UNFINISHED: assign j2 to j1 *)
	in
	    unfinished i "elabDec" "interface declarations";
	    O.InfDec(nonInfo(i), id', inf')
	end

      | elabDec(E, I.RecDec(i, decs)) =
	let
	    val _      = insertScope E
	    val _      = Type.enterLevel()
	    val tpats' = elabLHSRecDecs(E, decs)
	    val decs'  = elabRHSRecDecs(E, ref tpats', decs)
	    val _      = Type.exitLevel()
	    val E'     = splitScope E
infix andthen
fun a andthen b = b
	    val _      = appTyps (fn(x,entry) => insertTyp(E, x, entry)) E'
	    val _      = appVals (fn(x,(id,t)) =>
(*DEBUG*)
(let val x= case Name.toString(I.name id) of "?" => "?" ^ Stamp.toString x | x => x
in print("val " ^ x ^ " : ") end;
PrettyPrint.output(TextIO.stdOut, PPType.ppType(Type.close t), 600);
print "\n") andthen
					insertVal(E, x, (id, Type.close t))) E'

	in
	    O.RecDec(nonInfo(i), decs')
	end

      | elabDec(E, I.TypvarDec(i, id, decs)) =
	let
	    val (a,id') = elabVarId(E, Type.STAR, id)
	    val  decs'  = elabDecs(E, decs)
	(*UNFINISHED: check that a does not appear in top scope of E*)
	in
	    unfinished i "elabDec" "scoped type variables";
	    O.TypvarDec(nonInfo(i), id', decs')
	end

      | elabDec(E, I.LocalDec(i, decs)) =
	let
	    val decs' = elabDecs(E, decs)
	(*UNFINISHED: effects the building of the structure env*)
	in
	    unfinished i "elabDec" "local and hiding";
	    O.LocalDec(nonInfo(i), decs')
	end


    and elabDecs(E, decs) = List.map (fn dec => elabDec(E, dec)) decs


  (* Recursive declarations *)

    and elabLHSRecDecs(E, decs) =
	List.foldr (fn(dec,xs) => elabLHSRecDec(E,dec) @ xs) [] decs

    and elabLHSRecDec(E, I.ValDec(i, pat, exp)) =
	    [elabPat(E, pat)]

      | elabLHSRecDec(E, I.ConDec(i, con, typ)) =
	    ( elabLHSRecCon(E, con) ; [] )

      | elabLHSRecDec(E, I.TypDec(i, id, typ)) =
	let
	    val  k    = elabTypKind(E, typ)
	    val (t,_) = elabTypId(E, k, id)
	    val  _    = Type.unify(t, Type.inRec(Type.unknown k))
			handle Type.Unify _ => Crash.crash "Elab.elabLHSRecDec"
	in
	    []
	end

      | elabLHSRecDec(E, I.DatDec(i, id, typ)) =
	let
	    val  k    = elabTypKind(E, typ)
	    val (t,_) = elabTypId(E, k, id)
	    val  _    = Type.unify(t, Type.inRec(Type.unknown k))
			handle Type.Unify _ => Crash.crash "Elab.elabLHSRecDec"
	in
	    []
	end

      | elabLHSRecDec(E, I.RecDec(i, decs)) =
	    elabLHSRecDecs(E, decs)

      | elabLHSRecDec(E, _) = []

    and elabLHSRecCon(E, I.Con(i, id, typs)) =
	    ( elabValId(E, id) ; [] )


    and elabRHSRecDecs(E, rtpats', decs) =
	    List.map (fn dec => elabRHSRecDec(E, rtpats', dec)) decs

    and elabRHSRecDec(E, r as ref((t1,pat')::tpats'), I.ValDec(i, pat, exp)) =
	let
	    val (t2,exp') = elabExp(E, exp)
	    val  _        = r := tpats'
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(i, E.ValDecUnify(t1, t2, t3, t4))
	in
	    O.ValDec(nonInfo(i), pat', exp')
	end

      | elabRHSRecDec(E, rtpats', I.RecDec(i, decs)) =
	let
	    val dec' = elabRHSRecDecs(E, rtpats', decs)
	in
	    O.RecDec(nonInfo(i), dec')
	end

      | elabRHSRecDec(E, rtpats', dec) =
	    elabDec(E, dec)



  (* Specifications *)

    and elabSpec(E, I.ValSpec(i, id, typ)) =
	let
	    val (t1,id')  = elabValId(E, id)
	    val (t2,typ') = elabTyp(E, typ)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				Crash.crash "Elab.elabSpec: rebound value stamp"
	in
	    O.ValSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, I.ConSpec(i, con, typ)) =
	(*UNFINISHED*)
	let
	    val  _          = insertScope E
	    val (t1,typ')   = elabTyp(E, typ)
	    val (l,t3,con') = elabCon(E, con)
	    val  _          = deleteScope E
	in
	    unfinished i "elabSpec" "constructor specifications";
	    O.ConSpec(nonInfo(i), con', typ')
	end

      | elabSpec(E, I.TypSpec(i, id, typ)) =
	let
	    val  k        = elabTypKind(E, typ)
	    val (t1,id')  = elabTypId(E, k, id)
	    val (t2,typ') = elabTyp(E, typ)
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(I.infoTyp typ,
				      E.TypSpecUnify(t1, t2, t3, t4))
	in
	    O.TypSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, I.DatSpec(i, id, typ)) =
	let
	    val  _        = insertScope E
	    val  _        = Type.enterLevel()
	    val  k        = elabTypKind(E, typ)
	    val (t1,id')  = elabTypId(E, k, id)
	    val (t2,typ') = elabTypRep(E, id', t1, fn t => t, fn k => k, typ)
	    val  _        = Type.exitLevel()
	    val  E'       = splitScope E
	    val  _        = Type.unify(t1,t2)
			    handle Type.Unify(t3,t4) =>
				error(I.infoTyp typ,
				      E.DatSpecUnify(t1, t2, t3, t4))
	    val  _        = appVals (fn(x,(id,t)) =>
					 insertVal(E, x, (id, Type.close t))
				    ) E'
	in
	    O.DatSpec(nonInfo(i), id', typ')
	end

      | elabSpec(E, I.ModSpec(i, id, inf)) =
	let
	    val (j1,id')  = elabModId(E, id)
	    val (j2,inf') = elabInf(E, inf)
	(*UNFINISHED: strengthen j2, assign it to j1 *)
	in
	    unfinished i "elabSpec" "module specifications";
	    O.ModSpec(nonInfo(i), id', inf')
	end

      | elabSpec(E, I.InfSpec(i, id, inf)) =
	let
	    val (j1,id')  = elabInfId(E, id)
	    val (j2,inf') = elabInfRep(E, id', inf)
	(*UNFINISHED: assign j2 to j1 *)
	in
	    unfinished i "elabSpec" "interface specifications";
	    O.InfSpec(nonInfo(i), id', inf')
	end

      | elabSpec(E, I.RecSpec(i, specs)) =
	let
	    val _      = elabLHSRecSpecs(E, specs)
	    val specs' = elabRHSRecSpecs(E, specs)
	in
	    O.RecSpec(nonInfo(i), specs')
	end

      | elabSpec(E, I.LocalSpec(i, specs)) =
	let
	    val specs' = elabSpecs(E, specs)
	(*UNFINISHED: effects the building of the structure env*)
	in
	    unfinished i "elabSpec" "local and hiding";
	    O.LocalSpec(nonInfo(i), specs')
	end

      | elabSpec(E, I.ExtSpec(i, inf)) =
	let
	    val (j,inf') = elabInf(E, inf)
	(*UNFINISHED: insert stuff*)
	in
	    unfinished i "elabSpec" "signature extension";
	    O.ExtSpec(nonInfo(i), inf')
	end


    and elabSpecs(E, specs) = List.map (fn spec => elabSpec(E, spec)) specs


  (* Recursive specifications *)

    and elabLHSRecSpecs(E, specs) =
	List.app (fn spec => elabLHSRecSpec(E,spec)) specs

    and elabLHSRecSpec(E, I.TypSpec(i, id, typ)) =
	let
	    val  k    = elabTypKind(E, typ)
	    val (t,_) = elabTypId(E, k, id)
	    val  _    = Type.unify(t, Type.inRec(Type.unknown k))
			handle Type.Unify _ => Crash.crash "Elab.elabLHSRecSpec"
	in
	    insertTyp(E, I.stamp id, (id,t))
	end

      | elabLHSRecSpec(E, I.DatSpec(i, id, typ)) =
	let
	    val  k    = elabTypKind(E, typ)
	    val (t,_) = elabTypId(E, k, id)
	    val  _    = Type.unify(t, Type.inRec(Type.unknown k))
			handle Type.Unify _ => Crash.crash "Elab.elabLHSRecSpec"
	in
	    insertTyp(E, I.stamp id, (id,t))
	end

      | elabLHSRecSpec(E, I.RecSpec(i, specs)) =
	    elabLHSRecSpecs(E, specs)

      | elabLHSRecSpec(E, _) = ()


    and elabRHSRecSpecs(E, specs) =
	List.map (fn spec => elabRHSRecSpec(E, spec)) specs

    and elabRHSRecSpec(E, I.RecSpec(i, specs)) =
	let
	    val spec' = elabRHSRecSpecs(E, specs)
	in
	    O.RecSpec(nonInfo(i), spec')
	end

      | elabRHSRecSpec(E, spec) =
	    elabSpec(E, spec)



  (* Programs *)

    fun elabProgram E program =
	let
	    val _     = insertScope E
	    val decs' = elabDecs(E,program)
	    val _     = mergeScope E
	in
	    decs'
	end


  (* Components *)

    fun elabComp(E, I.Comp(i, imps, decs)) =
	let
	    val imps' = elabImps(E, imps)
	    val decs' = elabDecs(E, decs)
	in
	    O.Comp(nonInfo(i), imps', decs')
	end

    and elabImp(E, I.Imp(i, specs, s)) =
	let
	    val specs' = elabSpecs(E, specs)
	in
	    O.Imp(nonInfo(i), specs', s)
	end

    and elabImps(E, imps) = List.map (fn imp => elabImp(E, imp)) imps



    fun elab E component =
	let
	    val _         = insertScope E
	    val impsdecs' = elabComp(E,component)
	    val _         = mergeScope E
	in
	    impsdecs'
	end
	handle Error.Error x => ( deleteScope E ; raise Error.Error x )

  end
