(* Q&D Untyped translation *)

(* What does not work yet:
   - structures
   - open
   - datatype replication
*)

structure TranslationPhase :> TRANSLATION_PHASE =
  struct

    structure I = AbstractGrammar
    structure O = IntermediateGrammar


    (* Literals *)

    fun trLit(I.WordLit w)		= O.WordLit w
      | trLit(I.IntLit n)		= O.IntLit n
      | trLit(I.CharLit c)		= O.CharLit c
      | trLit(I.StringLit s)		= O.StringLit s
      | trLit(I.RealLit x)		= O.RealLit x


    (* Identifiers *)

    fun trLab(I.Lab(i,s))		= O.Lab(i,s)

    fun trId(I.Id(i,z,I.InId))		= O.Id(i, z, O.InId)
      | trId(I.Id(i,z,I.ExId s))	= O.Id(i, z, O.ExId s)

    fun trLongid(I.ShortId(i,x))	= O.ShortId(i, trId x)
      | trLongid(I.LongId(i,y,a))	= O.LongId(i, trLongid y, trLab a)


    (* Expressions *)

    (* BUG: currently n-ary constructors are not treated properly *)

    fun trExp(I.LitExp(i,l))		= O.LitExp(i, trLit l)
      | trExp(I.VarExp(i,y))		= O.VarExp(i, trLongid y)
      | trExp(I.ConExp(i,y))		= O.ConExp(i, trLongid y)
      | trExp(I.RefExp(i))		= O.RefExp(i)
      | trExp(I.TupExp(i,es))		= O.TupExp(i, trExps es)
      | trExp(I.RowExp(i,r))		= O.RowExp(i, trExpRow r)
      | trExp(I.SelExp(i,a))		= O.SelExp(i, trLab a)
      | trExp(I.VecExp(i,es))		= O.TupExp(i, trExps es)
      | trExp(I.FunExp(i,x,e))		= O.FunExp(i, trId x, trExp e)
      | trExp(I.AppExp(i,e1,e2))	= O.AppExp(i, trExp e1, trExp e2)
      | trExp(I.CompExp(i,e1,e2))	= O.AdjExp(i, trExp e2, trExp e2)
      | trExp(I.AndExp(i,e1,e2))	= O.AndExp(i, trExp e1, trExp e2)
      | trExp(I.OrExp(i,e1,e2))		= O.OrExp(i, trExp e1, trExp e2)
      | trExp(I.IfExp(i,e1,e2,e3))	= O.IfExp(i, trExp e1, trExp e2, trExp e3)
      | trExp(I.WhileExp(i,e1,e2))	= O.WhileExp(i, trExp e1, trExp e2)
      | trExp(I.SeqExp(i,es))		= O.SeqExp(i, trExps es)
      | trExp(I.CaseExp(i,e,ms))	= O.CaseExp(i, trExp e, trMatchs ms)
      | trExp(I.RaiseExp(i,e))		= O.RaiseExp(i, trExp e)
      | trExp(I.HandleExp(i,e,ms))	= O.HandleExp(i, trExp e, trMatchs ms)
      | trExp(I.AnnExp(i,e,t))		= trExp e
      | trExp(I.LetExp(i,ds,e))		= O.LetExp(i, trDecs ds, trExp e)

    and trExps es			= List.map trExp es

    and trExpRow(I.Row(i,fs,_))		= trExpFields fs
    and trExpField(I.Field(i,a,e))	= O.Field(i, trLab a, trExp e)
    and trExpFields fs			= List.map trExpField fs


    (* Matches and Patterns *)

    and trMatch(I.Match(i,p,e))		= O.Match(i, trPat p, trExp e)
    and trMatchs ms			= List.map trMatch ms

    and trPat(I.JokPat(i))		= O.WildPat(i)
      | trPat(I.LitPat(i,l))		= O.LitPat(i, trLit l)
      | trPat(I.VarPat(i,x))		= O.VarPat(i, trId x)
      | trPat(I.ConPat(i,y,ps))		= O.ConPat(i, trLongid y, trArgPats ps)
      | trPat(I.RefPat(i,p))		= O.RefPat(i, trPat p)
      | trPat(I.TupPat(i,ps))		= O.TupPat(i, trPats ps)
      | trPat(I.RowPat(i,r))		= let val (fs',b') = trPatRow r in
					      O.RowPat(i, fs', b')
					  end
      | trPat(I.VecPat(i,ps))		= O.TupPat(i, trPats ps)
      | trPat(I.AsPat(i,p1,p2))		= O.AsPat(i, trPat p1, trPat p2)
      | trPat(I.AltPat(i,ps))		= O.AltPat(i, trPats ps)
      | trPat(I.NegPat(i,p))		= O.NegPat(i, trPat p)
      | trPat(I.GuardPat(i,p,e))	= O.GuardPat(i, trPat p, trExp e)
      | trPat(I.AnnPat(i,p,t))		= trPat p
      | trPat(I.WithPat(i,p,ds))	= O.WithPat(i, trPat p, trDecs ds)

    and trPats ps			= List.map trPat ps

    and trArgPats []			= NONE
      | trArgPats[p]			= SOME(trPat p)
      | trArgPats _			= raise Fail "n-ary ConPat"

    and trPatRow(I.Row(i,fs,b))		= (trPatFields fs, b)
    and trPatField(I.Field(i,a,p))	= O.Field(i, trLab a, trPat p)
    and trPatFields fs			= List.map trPatField fs


    (* Modules *)

    and trMod(I.VarMod(i,x))		= let val x' as O.Id(i',_,_) = trId x in
					      O.VarExp(i, O.ShortId(i', x'))
					  end
      | trMod(I.StrMod(i,ds))		= trStr ds
      | trMod(I.SelMod(i,m,a))		= O.AppExp(i, O.SelExp(i, trLab a),
						      trMod m)
      | trMod(I.FunMod(i,x,j,m))	= O.FunExp(i, trId x, trMod m)
      | trMod(I.AppMod(i,m1,m2))	= O.AppExp(i, trMod m1, trMod m2)
      | trMod(I.AnnMod(i,m,j))		= trMod m
      | trMod(I.LetMod(i,ds,m))		= O.LetExp(i, trDecs ds, trMod m)

    and trStr ds			= raise Fail "StrMod"



    (* Declarations *)

    and trDec(I.ValDec(i,p,e), ds')	= O.ValDec(i, trPat p, trExp e) :: ds'
      | trDec(I.ConDec(i,c,t), ds')	= trCon(c, ds')
      | trDec(I.TypDec(i,x,t), ds')	= ds'
      | trDec(I.DatDec(i,x,t), ds')	= trTyp(t, ds')
      | trDec(I.ModDec(i,x,m), ds')	= let val x' as O.Id(i',_,_) = trId x in
					      O.ValDec(i, O.VarPat(i',x'),
							  trMod m) :: ds'
					  end
      | trDec(I.InfDec(i,x,j), ds')	= ds'
      | trDec(I.RecDec(i,ds), ds')	= O.RecDec(i, trDecs ds) :: ds'
      | trDec(I.OpenDec(i,m), ds')	= raise Fail "OpenDec"
      | trDec(I.TypvarDec(i,x,ds), ds')	= trDecs'(ds, ds')
      | trDec(I.LocalDec(i,ds), ds')	= trDecs'(ds, ds')

    and trDecs ds			= trDecs'(ds, [])
    and trDecs'(ds, ds')		= List.foldr trDec ds' ds

    and trCon(I.Con(i,x,ts), ds')	= O.ConDec(i, trId x,
						      List.length ts > 0) :: ds'
    and trCons(cs, ds')			= List.foldr trCon ds' cs

    and trTyp(I.AbsTyp(i), ds')		= ds'
      | trTyp(I.VarTyp(i,x), ds')	= ds'
      | trTyp(I.ConTyp(i,y), ds')	= ds'
      | trTyp(I.FunTyp(i,x,t), ds')	= trTyp(t, ds')
      | trTyp(I.AppTyp(i,t1,t2), ds')	= trTyp(t1, trTyp(t2, ds'))
      | trTyp(I.RefTyp(i,t), ds')	= trTyp(t, ds')
      | trTyp(I.TupTyp(i,ts), ds')	= trTyps(ts, ds')
      | trTyp(I.RowTyp(i,r), ds')	= trTypRow(r, ds')
      | trTyp(I.ArrTyp(i,t1,t2), ds')	= trTyp(t1, trTyp(t2, ds'))
      | trTyp(I.SumTyp(i,cs), ds')	= trCons(cs, ds')
      | trTyp(I.ExtTyp(i), ds')		= ds'
      | trTyp(I.AllTyp(i,x,t), ds')	= trTyp(t, ds')
      | trTyp(I.ExTyp(i,x,t), ds')	= trTyp(t, ds')
      | trTyp(I.SingTyp(i,y), ds')	= ds'

    and trTyps(ts, ds')			= List.foldr trTyp ds' ts

    and trTypRow(I.Row(i,fs,_), ds')	= trTypFields(fs, ds')
    and trTypField(I.Field(i,a,t), ds')	= trTyp(t, ds')
    and trTypFields(fs, ds')		= List.foldr trTypField ds' fs


    (* Programs *)

    val translate = trDecs



    (* Create alias bindings for all structures and values in an environment *)
(*
    fun idToLab(O.Id(i,stamp,ExId x)) = O.Lab(i,x)

    fun shortid        (i,id') = O.ShortId(i,id')
    fun longid longid' (i,id') = O.LongId(i,longid',idToLab id')

    fun decs(i,E,toLongid) =
	let
	    fun f toString (id, (_,stamp,_), ds) =
		let
		    val id'     = O.Id(i, stamp, O.ExId(toString id))
		    val longid' = toLongid(i, id')
		    val pat'    = O.VarPat(i, id')
		    val exp'    = O.VarExp(i, longid')
		in
		    O.ValDec(i, pat', exp', false) :: ds
		end
	in
	    List.rev (foldStrs (f (toStrName o StrId.toString))
		     (foldVals (f VId.toString) nil E) E)
	end



    (* Create fields for all structures and values in an environment *)

    fun fields CE =
	let
	    fun f toString (id, (i,stamp,_), fs) =
		let
		    val s       = toString id
		    val lab'    = O.Lab(i, s)
		    val id'     = O.Id(i, stamp, O.ExId s)
		    val longid' = O.ShortId(i,id')
		in
		    O.Field(i, lab', O.VarExp(i, longid')) :: fs
		end
	in
	    (CoreEnv.foldStrs (f (toStrName o StrId.toString))
	    (CoreEnv.foldVals (f VId.toString) nil CE) CE)
	end
*)

  end
