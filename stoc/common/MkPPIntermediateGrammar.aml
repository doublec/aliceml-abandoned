functor MakePPIntermediateGrammar(
	structure IntermediateGrammar : INTERMEDIATE_GRAMMAR
	val ppLabInfo :    IntermediateGrammar.lab_info -> PrettyPrint.doc
	val ppIdInfo :     IntermediateGrammar.id_info -> PrettyPrint.doc
	val ppLongidInfo : IntermediateGrammar.longid_info -> PrettyPrint.doc
	val ppExpInfo :    IntermediateGrammar.exp_info -> PrettyPrint.doc
	val ppPatInfo :    IntermediateGrammar.pat_info -> PrettyPrint.doc
	val ppFieldInfo :  ('a -> PrettyPrint.doc) ->
			   'a IntermediateGrammar.field_info -> PrettyPrint.doc
	val ppMatchInfo :  IntermediateGrammar.match_info -> PrettyPrint.doc
	val ppDecInfo :    IntermediateGrammar.dec_info -> PrettyPrint.doc
	val ppSig :        IntermediateGrammar.sign -> PrettyPrint.doc
    ) :> PP_INTERMEDIATE_GRAMMAR where type comp = IntermediateGrammar.comp =
struct

  (* Import *)

    open IntermediateGrammar
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


  (* Semantic Objects *)

    fun ppLabel l	= text("\"" ^ Label.toString l ^ "\"")
    fun ppName n	= text("\"" ^ Name.toString n ^ "\"")
    fun ppStamp z	= text(Stamp.toString z)
    fun ppString s	= text("\"" ^ s ^ "\"")
    fun ppBool b	= text(Bool.toString b)


  (* Literals *)

    fun ppLit(IntLit n)		= text(LargeInt.toString n)
      | ppLit(WordLit w)	= text(LargeWord.toString w)
      | ppLit(CharLit c)	= text("#\"" ^ WideChar.toCString c ^ "\"")
      | ppLit(StringLit s)	= text("\"" ^ WideString.toCString s ^ "\"")
      | ppLit(RealLit r)	= text(LargeReal.toString r)


  (* Structured *)

    fun tree head info body =
	vbox(
	    abox(text head ^^ nest(break ^^ info)) ^^
	    nest(break ^^ body)
	)

    fun softtree head info body =
	abox(
	    abox(text head ^^ nest(break ^^ info)) ^^
	    nest(break ^^ body)
	)

    fun vec docs =
	let
	    val length = Vector.length docs - 1
	in
	    if length < 0
	    then text "[]"
	    else vec'(docs, length, Vector.sub(docs, length))
	end

    and vec'(docs, 0, doc) = doc
      | vec'(docs, i, doc) = vec'(docs, i-1, Vector.sub(docs, i-1) ^/^ doc)


  (* Identifiers *)

    fun ppLab(Lab(i, l))		= softtree "Lab" (ppLabInfo i) (
					    ppLabel l
					  )

    fun ppId(Id(i, z, n))		= softtree "Id" (ppIdInfo i) (
					    ppName n ^^
					    text "[" ^^ ppStamp z ^^ text "]"
					  )

    fun ppLongid(ShortId(i, id))	= softtree "ShortId" (ppLongidInfo i) (
					    ppId id
					  )
      | ppLongid(LongId(i, longid,lab))	= softtree "LongId" (ppLongidInfo i) (
					    ppLongid longid ^/^
					    ppLab lab
					  )

  (* Fields *)

    fun ppFields ppX fields		= vec(Vector.map (ppField ppX) fields)
    and ppField ppX (Field(i, lab, x))	= tree "Field" (ppFieldInfo ppX i) (
					    ppLab lab ^/^
					    ppX x
					  )

  (* Expressions *)

    fun exptree head i body		= tree (head ^ "Exp") (ppExpInfo i) body

    fun ppExps exps			= vec(Vector.map ppExp exps)
    and ppExp(LitExp(i, lit))		= exptree "Lit" i (
					    ppLit lit
					  )
      | ppExp(VarExp(i, longid))	= exptree "Var" i (
					    ppLongid longid
					  )
      | ppExp(PrimExp(i, s))		= exptree "Prim" i (
					    ppString s
					  )
      | ppExp(NewExp(i, b))		= exptree "New" i (
					    ppBool b
					  )
      | ppExp(TagExp(i, lab, exp, b))	= exptree "Tag" i (
					    ppBool b ^/^
					    ppLab lab ^/^
					    ppExp exp
					  )
      | ppExp(ConExp(i, longid, exp,b))	= exptree "Con" i (
					    ppBool b ^/^
					    ppLongid longid ^/^
					    ppExp exp
					  )
      | ppExp(RefExp(i, exp))		= exptree "Ref" i (
					    ppExp exp
					  )
      | ppExp(TupExp(i, exps))		= exptree "Tup" i (
					    ppExps exps
					  )
      | ppExp(ProdExp(i, fields))	= exptree "Prod" i (
					    ppFields ppExp fields
					  )
      | ppExp(SelExp(i, lab, exp))	= exptree "Sel" i (
					    ppLab lab ^/^
					    ppExp exp
					  )
      | ppExp(VecExp(i, exps))		= exptree "Vec" i (
					    ppExps exps
					  )
      | ppExp(FunExp(i, matchs))	= exptree "Fun" i (
					    ppMatchs matchs
					  )
      | ppExp(AppExp(i, exp1, exp2))	= exptree "App" i (
					    ppExp exp1 ^/^
					    ppExp exp2
					  )
      | ppExp(AndExp(i, exp1, exp2))	= exptree "And" i (
					    ppExp exp1 ^/^
					    ppExp exp2
					  )
      | ppExp(OrExp(i, exp1, exp2))	= exptree "Or" i (
					    ppExp exp1 ^/^
					    ppExp exp2
					  )
      | ppExp(IfExp(i, exp1,exp2,exp3))	= exptree "If" i (
					    ppExp exp1 ^/^
					    ppExp exp2 ^/^
					    ppExp exp3
					  )
      | ppExp(SeqExp(i, exps))		= exptree "Seq" i (
					    ppExps exps
					  )
      | ppExp(CaseExp(i, exp, matchs))	= exptree "Case" i (
					    ppExp exp ^/^
					    ppMatchs matchs
					  )
      | ppExp(RaiseExp(i, exp))		= exptree "Raise" i (
					    ppExp exp
					  )
      | ppExp(HandleExp(i, exp,matchs))	= exptree "Handle" i (
					    ppExp exp ^/^
					    ppMatchs matchs
					  )
      | ppExp(FailExp(i))		= exptree "Fail" i empty
      | ppExp(LazyExp(i, exp))		= exptree "Lazy" i (
					    ppExp exp
					  )
      | ppExp(LetExp(i, decs, exp))	= exptree "Let" i (
					    ppDecs decs ^/^
					    ppExp exp
					  )
      | ppExp(UpExp(i, exp))		= exptree "Up" i (
					    ppExp exp
					  )

    and ppMatchs matchs			= vec(Vector.map ppMatch matchs)
    and ppMatch(Match(i, pat, exp))	= tree "Match" (ppMatchInfo i) (
					    ppPat pat ^/^
					    ppExp exp
					  )

  (* Patterns *)

    and pattree head i body		= tree (head ^ "Pat") (ppPatInfo i) body

    and ppPats pats			= vec(Vector.map ppPat pats)
    and ppPat(JokPat(i))		= pattree "Jok" i empty
      | ppPat(LitPat(i, lit))		= pattree "Lit" i (
					    ppLit lit
					  )
      | ppPat(VarPat(i, id))		= pattree "Var" i (
					    ppId id
					  )
      | ppPat(TagPat(i, lab, pat, b))	= pattree "Tag" i (
					    ppBool b ^/^
					    ppLab lab ^/^
					    ppPat pat
					  )
      | ppPat(ConPat(i, longid, pat,b))	= pattree "Con" i (
					    ppBool b ^/^
					    ppLongid longid ^/^
					    ppPat pat
					  )
      | ppPat(RefPat(i, pat))		= pattree "Ref" i (
					    ppPat pat
					  )
      | ppPat(TupPat(i, pats))		= pattree "Tup" i (
					    ppPats pats
					  )
      | ppPat(ProdPat(i, fields))	= pattree "Prod" i (
					    ppFields ppPat fields
					  )
      | ppPat(VecPat(i, pats))		= pattree "Vec" i (
					    ppPats pats
					  )
      | ppPat(AsPat(i, pat1, pat2))	= pattree "As" i (
					    ppPat pat1 ^/^
					    ppPat pat2
					  )
      | ppPat(AltPat(i, pats))		= pattree "Alt" i (
					    ppPats pats
					  )
      | ppPat(NegPat(i, pat))		= pattree "Neg" i (
					    ppPat pat
					  )
      | ppPat(GuardPat(i, pat, exp))	= pattree "Guard" i (
					    ppPat pat ^/^
					    ppExp exp
					  )
      | ppPat(WithPat(i, pat, decs))	= pattree "With" i (
					    ppPat pat ^/^
					    ppDecs decs
					  )

  (* Declarations *)

    and dectree head i body		= tree (head ^ "Dec") (ppDecInfo i) body

    and ppDecs decs			= vec(Vector.map ppDec decs)
    and ppDec(ValDec(i, pat, exp))	= dectree "Val" i (
					    ppPat pat ^/^
					    ppExp exp
					  )
      | ppDec(RecDec(i, decs))		= dectree "Rec" i (
					    ppDecs decs
					  )

  (* Components *)

    fun ppComp (_, (exp,_)) = ppExp exp ^^ break

end
