structure AbstractionPhase :> ABSTRACTION_PHASE =
  struct

    structure I   = InputGrammar
    structure O   = AbstractGrammar
    structure Env = BindEnv

    open I
    open Env


    (* Error handling *)

    val error = Error.error

    fun errorLab  (s1, Lab  (i,x), s2)	= error(i, s1 ^   Lab.toString x ^ s2)
    fun errorVId  (s1, VId  (i,x), s2)	= error(i, s1 ^   VId.toString x ^ s2)
    fun errorTyCon(s1, TyCon(i,x), s2)	= error(i, s1 ^ TyCon.toString x ^ s2)
    fun errorTyVar(s1, TyVar(i,x), s2)	= error(i, s1 ^ TyVar.toString x ^ s2)
    fun errorStrId(s1, StrId(i,x), s2)	= error(i, s1 ^ StrId.toString x ^ s2)
    fun errorSigId(s1, SigId(i,x), s2)	= error(i, s1 ^ SigId.toString x ^ s2)
    fun errorFunId(s1, FunId(i,x), s2)	= error(i, s1 ^ FunId.toString x ^ s2)

    fun errorVId'(s1, E, vid', s2) =
	errorVId(s1, VId((#1 o Option.valOf o lookupVal)(E, vid'), vid'), s2)


    (* Miscellanous helpers *)

    fun inventId i = O.Id(i, Stamp.new(), O.InId)

    fun idToLab(O.Id(i, stamp, O.ExId s)) = O.Lab(i, s)
      | idToLab _ = raise Fail "idToLab: InId encountered"

    fun longidToMod(O.ShortId(i, id))         = O.VarMod(i, id)
      | longidToMod(O.LongId(i, longid, lab)) =
	    O.SelMod(i, longidToMod longid, lab)

    fun tupexp(i, [exp]) = exp
      | tupexp(i,  exps) = O.TupExp(i, exps)

    fun tuppat(i, [pat]) = pat
      | tuppat(i,  pats) = O.TupPat(i, pats)


    fun alltyp(  [],    typ) = typ
      | alltyp(id::ids, typ) = O.AllTyp(O.infoTyp typ, id, alltyp(ids, typ))

    fun funtyp(  [],    typ) = typ
      | funtyp(id::ids, typ) = O.FunTyp(O.infoTyp typ, id, funtyp(ids, typ))

    fun apptyp(    [],     typ1) = typ1
      | apptyp(typ2::typs, typ1) =
	let val i = Source.over(O.infoTyp typ2, O.infoTyp typ1) in
	    apptyp(typs, O.AppTyp(i, typ1, typ2))
	end

    fun typvardecs(  [],    decs) = decs
      | typvardecs(id::ids, decs) = [O.TypvarDec(O.infoId id, id,
						 typvardecs(ids, decs))]

    fun lookupIdStatus(E, vid') =
	case lookupVal(E, vid')
	  of NONE             => V
	   | SOME(i,stamp,is) => is



    (* Constants and identifiers *)

    fun toFunName s   = "$" ^ s
    fun fromFunName s = String.extract(s,1,NONE)

    fun trSCon E =
	fn SCon(i, SCon.INT n)		=> O.IntLit n
	 | SCon(i, SCon.WORD w)		=> O.WordLit w
	 | SCon(i, SCon.CHAR c)		=> O.CharLit c
	 | SCon(i, SCon.STRING s)	=> O.StringLit s
	 | SCon(i, SCon.REAL x)		=> O.RealLit x

    fun trLab E (Lab(i, lab)) = O.Lab(i, Lab.toString lab)

    fun trTyVar E (tyvar as TyVar(i, tyvar')) =
	let
	    val (_,stamp) =
		case lookupVar(E, tyvar')
		  of SOME xx => xx
		   | NONE    => errorTyVar("unbound type variable ", tyvar, "")
	in
	    O.Id(i, stamp, O.ExId(TyVar.toString tyvar'))
	end

    fun trId (lookup,infoId,idId,toString,error,class) E id =
	let
	    val id' = idId id
	    val (_,stamp,x) =
		case lookup(E, id')
		  of SOME xx => xx
		   | NONE    => error("unknown " ^ class ^ " ", id, "")
	in
	    ( O.Id(infoId id, stamp, O.ExId(toString id')), x )
	end

    val trVId   = trId(lookupVal, infoVId, idVId,
			VId.toString, errorVId, "value")
    val trTyCon = trId(lookupTy, infoTyCon, idTyCon,
			TyCon.toString, errorTyCon, "type")
    val trStrId = trId(lookupStr, infoStrId, idStrId,
			StrId.toString, errorStrId, "structure")
    val trSigId = trId(lookupSig, infoSigId, idSigId,
			SigId.toString, errorSigId, "signature")
    val trFunId = trId(lookupFun, infoFunId, idFunId,
			toFunName o FunId.toString, errorFunId, "functor")


    fun trId_bind (infoId,idId,toString) (E: Env) id =
	let val stamp = Stamp.new() in
	    ( O.Id(infoId id, stamp, O.ExId(toString(idId id))), stamp )
	end


    val trTyVar_bind = trId_bind(infoTyVar, idTyVar, TyVar.toString)
    val trVId_bind'  = trId_bind(infoVId,   idVId,   VId.toString)
    val trTyCon_bind = trId_bind(infoTyCon, idTyCon, TyCon.toString)
    val trStrId_bind = trId_bind(infoStrId, idStrId, StrId.toString)
    val trSigId_bind = trId_bind(infoSigId, idSigId, SigId.toString)
    val trFunId_bind = trId_bind(infoFunId, idFunId, toFunName o FunId.toString)

    fun trVId_bind E (vid as VId(i,vid')) =
	case VId.toString vid'
	  of ("true" | "false" | "nil" | "::" | "ref") =>
		errorVId("invalid rebinding of predefined identifier ", vid, "")
	   | _ =>
		trVId_bind' E vid


    (* With polymorphic recursion we could avoid the following code
       duplication... *)

    fun trLongStrId E =
	fn SHORTLong(i, strid) =>
	   let
		val (id',E') = trStrId E strid
	   in
		( O.ShortId(i,id'), E' )
	   end

	 | DOTLong(i, longstrid, strid) =>
	   let
		val (longid',E') = trLongStrId E longstrid
		val (id',E'')    = trStrId E' strid
		val  lab'        = idToLab id'
	   in
		( O.LongId(i,longid',lab'), E'' )
	   end

    fun trLongId trId E =
	fn SHORTLong(i, id) =>
	   let
		val (id',x) = trId E id
	   in
		( O.ShortId(i,id'), x )
	   end

	 | DOTLong(i, longstrid, id) =>
	   let
		val (longid',E') = trLongStrId E longstrid
		val (id',x)      = trId E' id
		val  lab'        = idToLab id'
	   in
		( O.LongId(i,longid',lab'), x )
	   end

    val trLongVId   = trLongId trVId
    val trLongTyCon = trLongId trTyCon
    val trLongStrId = trLongId trStrId
    val trLongSigId = trLongId trSigId
    val trLongFunId = trLongId trFunId



    (* Calculate sets of unguarded explicit type variables [Section 4.6] *)

    fun ? tyvarsX E  NONE    = []
      | ? tyvarsX E (SOME x) = tyvarsX E x

    fun unguardedTyVarsAtExp E (RECORDAtExp(_, exprow_opt)) =
	    ?unguardedTyVarsExpRow E exprow_opt

      | unguardedTyVarsAtExp E ( TUPLEAtExp(_, exps)
			       | VECTORAtExp(_, exps)
			       | SEQAtExp(_, exps) ) =
	    List.concat(List.map (unguardedTyVarsExp E) exps)

      | unguardedTyVarsAtExp E (LETAtExp(_, dec, exp)) =
	    unguardedTyVarsDec E dec @ unguardedTyVarsExp E exp

      | unguardedTyVarsAtExp E (PARAtExp(_, exp)) =
	    unguardedTyVarsExp E exp

      | unguardedTyVarsAtExp E _ = []

    and unguardedTyVarsExpRow E (ROWExpRow(_, lab, exp, exprow_opt)) =
	    unguardedTyVarsExp E exp @ ?unguardedTyVarsExpRow E exprow_opt

    and unguardedTyVarsExp E (ATEXPExp(_, atexp)) =
	    unguardedTyVarsAtExp E atexp

      | unguardedTyVarsExp E (APPExp(_, exp, atexp)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsAtExp E atexp

      | unguardedTyVarsExp E (TYPEDExp(_, exp, ty)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsTy E ty

      | unguardedTyVarsExp E ( ANDALSOExp(_, exp1, exp2)
			     | ORELSEExp(_, exp1, exp2)
			     | WHILEExp(_, exp1, exp2) ) =
	    unguardedTyVarsExp E exp1 @ unguardedTyVarsExp E exp2

      | unguardedTyVarsExp E (HANDLEExp(_, exp, match)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsMatch E match

      | unguardedTyVarsExp E (RAISEExp(_, exp)) =
	    unguardedTyVarsExp E exp

      | unguardedTyVarsExp E (IFExp(_, exp1, exp2, exp3)) =
	    unguardedTyVarsExp E exp1 @ unguardedTyVarsExp E exp2 @
	    unguardedTyVarsExp E exp3

      | unguardedTyVarsExp E (CASEExp(_, exp, match)) =
	    unguardedTyVarsExp E exp @ unguardedTyVarsMatch E match

      | unguardedTyVarsExp E (FNExp(_, match)) =
	    unguardedTyVarsMatch E match

    and unguardedTyVarsMatch E (Match(_, mrule, match_opt)) =
	    unguardedTyVarsMrule E mrule @ ?unguardedTyVarsMatch E match_opt

    and unguardedTyVarsMrule E (Mrule(_, pat, exp)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsExp E exp

    and unguardedTyVarsDec E (CONSTRUCTORDec(_, dconbind)) =
	    unguardedTyVarsDconBind E dconbind

      | unguardedTyVarsDec E (STRUCTUREDec(_, strbind)) =
	    unguardedTyVarsStrBind E strbind

      | unguardedTyVarsDec E (FUNCTORDec(_, funbind)) =
	    unguardedTyVarsFunBind E funbind

      | unguardedTyVarsDec E ( LOCALDec(_, dec1, dec2)
			     | SEQDec(_, dec1, dec2) ) =
	    unguardedTyVarsDec E dec1 @ unguardedTyVarsDec E dec2

      | unguardedTyVarsDec E _ = []

    and unguardedTyVarsValBind E (PLAINValBind(_, pat, exp, valbind_opt)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsExp E exp @
	    ?unguardedTyVarsValBind E valbind_opt

      | unguardedTyVarsValBind E (RECValBind(_, valbind)) =
	    unguardedTyVarsValBind E valbind

    and unguardedTyVarsFvalBind E (FvalBind(_, match, fvalbind_opt)) =
	    unguardedTyVarsMatch E match @
	    ?unguardedTyVarsFvalBind E fvalbind_opt

    and unguardedTyVarsDconBind E (NEWDconBind(_, _, vid, ty_opt, tyvarseq,
						     longtycon, dconbind_opt)) =
	let
	    val  _   = insertScope E
	    val  _   = trTyVarSeq E tyvarseq
	    val  _   = insertScope E
	    val ids' = ?unguardedTyVarsTy E ty_opt
	    val  _   = delete2ndScope E
	    val  _   = mergeScope E
	in
	    ids' @ ?unguardedTyVarsDconBind E dconbind_opt
	end

      | unguardedTyVarsDconBind E (EQUALDconBind(_, _,vid, _,longvid,
								dconbind_opt)) =
	    ?unguardedTyVarsDconBind E dconbind_opt

    and unguardedTyVarsStrBind E (StrBind(_, strid, strexp, strbind_opt)) =
	    unguardedTyVarsStrExp E strexp @
	    ?unguardedTyVarsStrBind E strbind_opt

    and unguardedTyVarsFunBind E (FunBind(_, funid, strid, sigexp, strexp,
								funbind_opt)) =
	    unguardedTyVarsStrExp E strexp @
	    ?unguardedTyVarsFunBind E funbind_opt

    and unguardedTyVarsAtPat E (RECORDAtPat(_, patrow_opt)) =
	    ?unguardedTyVarsPatRow E patrow_opt

      | unguardedTyVarsAtPat E ( TUPLEAtPat(_, pats)
			       | VECTORAtPat(_, pats)
			       | ALTAtPat(_, pats) ) =
	    List.concat(List.map (unguardedTyVarsPat E) pats)

      | unguardedTyVarsAtPat E (PARAtPat(_, pat)) =
	    unguardedTyVarsPat E pat

      | unguardedTyVarsAtPat E _ = []

    and unguardedTyVarsPatRow E (WILDCARDPatRow(_)) = []

      | unguardedTyVarsPatRow E (ROWPatRow(_, lab, pat, patrow_opt)) =
	    unguardedTyVarsPat E pat @ ?unguardedTyVarsPatRow E patrow_opt

    and unguardedTyVarsPat E (ATPATPat(_, atpat)) =
	    unguardedTyVarsAtPat E atpat

      | unguardedTyVarsPat E (APPPat(_, pat, atpat)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsAtPat E atpat

      | unguardedTyVarsPat E (TYPEDPat(_, pat, ty)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsTy E ty

      | unguardedTyVarsPat E (NONPat(_, pat)) =
	    unguardedTyVarsPat E pat

      | unguardedTyVarsPat E (ASPat(_, pat1, pat2)) =
	    unguardedTyVarsPat E pat1 @ unguardedTyVarsPat E pat2

      | unguardedTyVarsPat E (WHENPat(_, pat, atexp)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsAtExp E atexp

      | unguardedTyVarsPat E (WITHVALPat(_, pat, valbind)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsValBind E valbind

      | unguardedTyVarsPat E (WITHFUNPat(_, pat, fvalbind)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsFvalBind E fvalbind

    and unguardedTyVarsTy E (TYVARTy(_, tyvar as TyVar(i,tyvar'))) =
	if Option.isSome(lookupVar(E, tyvar')) then
	    []
	else
	let
	    val (id',stamp) = trTyVar_bind E tyvar
	    val  _          = insertVar(E, tyvar', (i, stamp))
	in
	    [id']
	end

      | unguardedTyVarsTy E (RECORDTy(_, tyrow_opt)) =
	    ?unguardedTyVarsTyRow E tyrow_opt

      | unguardedTyVarsTy E (TYCONTy(_, tyseq, longtycon)) =
	    unguardedTyVarsTyseq E tyseq

      | unguardedTyVarsTy E (ARROWTy(_, ty, ty')) =
	    unguardedTyVarsTy E ty @ unguardedTyVarsTy E ty'

      | unguardedTyVarsTy E (PARTy(_, ty)) =
	    unguardedTyVarsTy E ty

    and unguardedTyVarsTyRow E (ROWTyRow(_, lab, ty, tyrow_opt)) =
	    unguardedTyVarsTy E ty @ ?unguardedTyVarsTyRow E tyrow_opt

    and unguardedTyVarsTyseq E (Seq(_, tys)) =
	    List.concat(List.map (unguardedTyVarsTy E) tys)

    and unguardedTyVarsStrExp E (STRUCTStrExp(_, dec)) =
	    unguardedTyVarsDec E dec

      | unguardedTyVarsStrExp E (LONGSTRIDStrExp(_, longstrid)) =
	    []

      | unguardedTyVarsStrExp E ( TRANSStrExp(_, strexp, sigexp)
				| OPAQStrExp(_, strexp, sigexp) ) =
	    unguardedTyVarsStrExp E strexp

      | unguardedTyVarsStrExp E (APPStrExp(_, longfunid, strexp)) =
	    unguardedTyVarsStrExp E strexp

      | unguardedTyVarsStrExp E (LETStrExp(_, dec, strexp)) =
	    unguardedTyVarsDec E dec @ unguardedTyVarsStrExp E strexp



    (* Expressions *)

    and trAtExp E =
	fn SCONAtExp(i, scon)		=> O.LitExp(i, trSCon E scon)
	 | LONGVIDAtExp(i, _, longvid)	=>
	   (case trLongVId E longvid
	      of (longid', V) =>
		 O.VarExp(i, longid')

	       | (longid', C k) =>
		 O.ConExp(i, k, longid')

	       | (longid', R) =>
		 O.RefExp(i)
	   )
	 | RECORDAtExp(i, exprowo)	=>
	   let
		val  _   = insertScope E
		val row' = trExpRowo E exprowo
		val  _   = deleteScope E
	   in
		O.RowExp(i, row')
	   end
	 | HASHAtExp(i, lab)		=> O.SelExp(i, trLab E lab)
	 | TUPLEAtExp(i, exps)		=> O.TupExp(i, trExps E exps)
	 | VECTORAtExp(i, exps)		=> O.VecExp(i, trExps E exps)
	 | SEQAtExp(i, exps)		=> O.SeqExp(i, trExps E exps)
	 | LETAtExp(i, dec, exp)	=>
	   let
		val  _   = insertScope E
		val dec' = trDec E dec
		val exp' = trExp E exp
		val  _   = deleteScope E
	   in
		O.LetExp(i, dec', exp')
	   end
	 | PARAtExp(i, exp)		=> trExp E exp


    and trExpRowo E =
	fn NONE => O.Row(Source.nowhere, [], false)

	 | SOME(ROWExpRow(i, lab as Lab(i',lab'), exp, exprowo)) =>
	   let
		val i1'    = Source.over(i', infoExp exp)
		val field' = O.Field(i1', trLab E lab, trExp E exp)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
			     errorLab("duplicate label ", lab, " in record")
		val O.Row(_,fields',_) = trExpRowo E exprowo
	   in
		O.Row(i, field'::fields', false)
	   end


    and trExp E =
	fn exp as (ATEXPExp _|APPExp _)	=> trAppExp E (Infix.exp (infEnv E) exp)
 	 | TYPEDExp(i, exp, ty)		=> O.AnnExp(i,trExp E exp, trTy E ty)
	 | ANDALSOExp(i, exp1, exp2)	=> O.AndExp(i,trExp E exp1,trExp E exp2)
	 | ORELSEExp(i, exp1, exp2)	=> O.OrExp(i,trExp E exp1, trExp E exp2)
	 | HANDLEExp(i, exp, match)	=>
		O.HandleExp(i, trExp E exp, trMatcho E (SOME match))

	 | RAISEExp(i, exp)		=> O.RaiseExp(i, trExp E exp)
	 | IFExp(i, exp1, exp2, exp3)	=>
		O.IfExp(i, trExp E exp1, trExp E exp2, trExp E exp3)

	 | WHILEExp(i, exp1, exp2) =>
		O.WhileExp(i, trExp E exp1, trExp E exp2)

	 | CASEExp(i, exp, match) =>
		O.CaseExp(i, trExp E exp, trMatcho E (SOME match))

	 | FNExp(i, match) =>
	   let
		val i'       = infoMatch match
		val matches' = trMatcho E (SOME match)

		val (id',exp') =
		    case matches'
		      of [O.Match(_,O.JokPat(i),exp')]     => (inventId i, exp')
		       | [O.Match(_,O.VarPat(i,id'),exp')] => (id',exp')
		       | _                                 =>
			 let
			    val id'      = inventId i
			    val varexp'  = O.VarExp(i, O.ShortId(i, id'))
			 in
			    (id', O.CaseExp(i', varexp', matches'))
			 end
	   in
		O.FunExp(i, id', exp')
	   end

    and trAppExp E =
	fn APPExp(i, exp, atexp) => O.AppExp(i, trAppExp E exp, trAtExp E atexp)
	 | ATEXPExp(i, atexp)    => trAtExp E atexp
	 | exp                   => trExp E exp

    and trExps E = List.map(trExp E)



    (* Matches and patterns *)

    and trMatcho E =
	fn NONE                          => nil
	 | SOME(Match(i, mrule, matcho)) => trMrule E mrule :: trMatcho E matcho

    and trMrule E (Mrule(i, pat, exp)) =
	let
		val E'   = Env.new()
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E,E')
		val exp' = trExp E exp
		val  _   = deleteScope E
	in
		O.Match(i, pat', exp')
	end


    and trAtPat (E,E') =
	fn WILDCARDAtPat(i)	=> O.JokPat(i)
	 | SCONAtPat(i, scon)	=> O.LitPat(i, trSCon E scon)
	 | LONGVIDAtPat(_, _, longvid as SHORTLong(i, vid as VId(i',vid'))) =>
	   (case lookupIdStatus(E, vid')
	      of C _ => O.ConPat(i, #1(trLongVId E longvid), [])
	       | R   => O.RefPat(i, O.JokPat(i)) (* BUG: a real hack! *)
	       | V   =>
		 let
		    (* If inside an alternative pattern then E' contains
		     * an upper scope where the variable is already bound.
		     * We have to reuse the stamp found there.
		     *)
		    val _ = if Option.isSome(lookupScopeVal(E', vid')) then
			       errorVId("duplicate variable ", vid,
					" in pattern or binding group")
			    else ()
		    val (id',stamp) =
			case lookupVal(E', vid')
			  of NONE            => trVId_bind E vid
			   | SOME(_,stamp,_) => ( O.Id(i', stamp,
						      O.ExId(VId.toString vid'))
						, stamp )
		    val _ = insertVal(E', vid', (i',stamp,V))
		 in
		    O.VarPat(i, id')
		 end
	   )
	 | LONGVIDAtPat(i, _, longvid) =>
	   (case trLongVId E longvid
	      of (longid', C _) => O.ConPat(i, longid', [])
	       | (longid', R)   => O.RefPat(i, O.JokPat(i)) (* BUG: HACK! *)
	       | (longid', V)   =>
		 error(i, "non-constructor long identifier in pattern")
	   )
	 | RECORDAtPat(i, patrowo) =>
	   let
		val  _   = insertScope E
		val row' = trPatRowo (E,E') patrowo
		val  _   = deleteScope E
	   in
		O.RowPat(i, row')
	   end

	 | TUPLEAtPat(i, pats)     => O.TupPat(i, trPats (E,E') pats)
	 | VECTORAtPat(i, pats)    => O.VecPat(i, trPats (E,E') pats)
	 | ALTAtPat(i, pats)       =>
	   let
		val  _    = insertScope E'
		val pat'  = trPat (E,E') (List.hd pats)
		val pats' = trAltPats (E,E') (List.tl pats)
		val  _    = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("duplicate variable ", E',vid',
					  " in pattern or binding group")
	   in
		O.AltPat(i, pat'::pats')
	   end

	 | PARAtPat(i, pat) => trPat (E,E') pat


    and trPatRowo (E,E') =
	fn NONE => O.Row(Source.nowhere, [], false)

	 | SOME(WILDCARDPatRow(i)) => O.Row(i, [], true)

	 | SOME(ROWPatRow(i, lab as Lab(i',lab'), pat, patrowo)) =>
	   let
		val i1'    = Source.over(i', infoPat pat)
		val field' = O.Field(i1', trLab E lab, trPat (E,E') pat)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
			     errorLab("duplicate label ", lab, " in record")
		val O.Row(_,fields',dots') = trPatRowo (E,E') patrowo
	   in
		O.Row(i, field'::fields', dots')
	   end


    and trPat (E,E') =
	fn pat as (ATPATPat _|APPPat _) =>
		trAppPat (E,E') (Infix.pat (infEnv E) pat)

	 | TYPEDPat(i, pat, ty)	=> O.AnnPat(i, trPat (E,E') pat, trTy E ty)
	 | NONPat(i, pat)	=> O.NegPat(i, trPat (E,Env.new()) pat)
	 | ASPat(i, pat1, pat2) => O.AsPat(i,trPat (E,E') pat1,trPat(E,E') pat2)
	 | WHENPat(i, pat, atexp) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, copyScope E')
		val exp' = trAtExp E atexp
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("duplicate variable ", E', vid',
					  " in pattern or binding group")
	   in
		O.GuardPat(i, pat', exp')
	   end

	 | WITHVALPat(i, pat, valbind) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, copyScope E')
		val  _   = insertScope E'
		val decs'= trValBindo (E,E') (SOME valbind)
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("pattern variable ", E', vid',
					  " rebound inside value binding")
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("duplicate variable ", E', vid',
					  " in pattern or binding group")
	   in
		O.WithPat(i, pat', decs')
	   end

	 | WITHFUNPat(i, pat, fvalbind) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, copyScope E')
		val  _   = insertScope E'
		val (ids',fmatches) = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _   = inheritScope(E, copyScope E')
		val exps'= trFmatches_rhs E fmatches
		val decs'= ListPair.map
				(fn(id',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId id', id'), exp'))
				(ids',exps')
		val  _   = deleteScope E
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("pattern variable ", E', vid',
					  " rebound inside value binding")
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("duplicate variable ", E', vid',
					  " in pattern or binding group")
	   in
		O.WithPat(i, pat', [O.RecDec(infoFvalBind fvalbind, decs')])
	   end

    and trAppPat (E,E') =
	fn APPPat(i, pat, atpat) =>
	   let
		val pat1' = trAppPat (E,E') pat
		val pat2' = trAtPat (E,E') atpat
	   in
		case pat1'
		  of O.ConPat(i', longid, pats') =>
			O.ConPat(i, longid, pats'@[pat2'])

		   | O.RefPat(i', O.JokPat _) =>  (* BUG: a real hack! *)
			O.RefPat(i, pat2')

		   | _ =>
			error(i, "non-constructor application in pattern")
	   end

	 | ATPATPat(i, atpat) => trAtPat (E,E') atpat

	 | pat => trPat (E,E') pat


    and trPats (E,E') = List.map(trPat (E,E'))

    and trAltPats (E,E') = List.map(trAltPat (E,E'))

    and trAltPat (E,E') pat =
	let
	    val _    = insertScope E'
	    val pat' = trPat (E,E') pat
	    val E''  = splitScope E'
	    val _    = if Env.sizeScope E' = Env.sizeScope E'' then () else
			  error(infoPat pat, "inconsistent pattern alternative")
	    val _    = Env.appiVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat pat, "inconsistent pattern\
							\ alternative")
			    ) E'
	in
	    pat'
	end



    (* Types *)

    and trTy E =
	fn TYVARTy(i, tyvar as TyVar(i',tyvar')) =>
		O.VarTyp(i, trTyVar E tyvar)

	 | TYCONTy(i, tyseq, longtycon) =>
	   let
		val (longid',_) = trLongTyCon E longtycon
		val  typs'      = trTySeq E tyseq
	   in
		apptyp(typs', O.ConTyp(i, longid'))
	   end

	 | RECORDTy(i, tyrowo) =>
	   let
		val  _   = insertScope E
		val row' = trTyRowo E tyrowo
		val  _   = deleteScope E
	   in
		O.RowTyp(i, row')
	   end

	 | ARROWTy(i, ty1, ty2) => O.ArrTyp(i, trTy E ty1, trTy E ty2)
	 | PARTy(i, ty)         => trTy E ty



    and trTyRowo E =
	fn NONE => O.Row(Source.nowhere, [], false)

	 | SOME(ROWTyRow(i, lab as Lab(i',lab'), ty, tyrowo)) =>
	   let
		val i1'    = Source.over(i', infoTy ty)
		val field' = O.Field(i1', trLab E lab, trTy E ty)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
			     errorLab("duplicate label ", lab, " in record")
		val O.Row(_,fields',_) = trTyRowo E tyrowo
	   in
		O.Row(i, field'::fields', false)
	   end



    and trTySeq E (Seq(i, tys)) = List.map (trTy E) tys


    and trTyVarSeq E (Seq(i, tyvars)) = List.map (trSeqTyVar E) tyvars

    and trSeqTyVar E (tyvar as TyVar(i, tyvar')) =
	let
	    val (id',stamp) = trTyVar_bind E tyvar
	    val  _          = insertDisjointVar(E, tyvar', (i, stamp))
			      handle CollisionVar _ =>
				errorTyVar("duplicate type variable ", tyvar,"")
	in
	    id'
	end


    (* Tyvarseqs at a val or fun. *)

    and trValTyVarSeq E (Seq(i, tyvars)) = List.map (trValSeqTyVar E) tyvars

    and trValSeqTyVar E (tyvar as TyVar(i, tyvar')) =
	if Option.isSome(lookupVar(E, tyvar')) then
	    errorTyVar("duplicate or shadowing type variable ", tyvar, "")
	else
	let
	    val (id',stamp) = trTyVar_bind E tyvar
	    val  _          = insertVar(E, tyvar', (i, stamp))
	in
	    id'
	end


    (* Extract type variables from a type (as implicitly quantified) *)

    and trAllTy E =
	fn TYVARTy(i, tyvar as TyVar(i',tyvar')) =>
	   if Option.isSome(lookupVar(E, tyvar')) then
		[]
	   else
	   let
		val (id',stamp) = trTyVar_bind E tyvar
		val  _          = insertVar(E, tyvar', (i, stamp))
	   in
		[id']
	   end

	 | TYCONTy(i, tyseq, longtycon) => trAllTySeq E tyseq
	 | RECORDTy(i, tyrowo)          => trAllTyRowo E tyrowo
	 | ARROWTy(i, ty1, ty2)         => trAllTy E ty1 @ trAllTy E ty2
	 | PARTy(i, ty)                 => trAllTy E ty

    and trAllTyRowo E =
	fn NONE                               => []
	 | SOME(ROWTyRow(i, lab, ty, tyrowo)) =>
		trAllTy E ty @ trAllTyRowo E tyrowo

    and trAllTySeq E (Seq(i, tys)) = List.concat(List.map (trAllTy E) tys)



    (* Declarations *)

    and trDec E =
	fn VALDec(i, tyvarseq, valbind) =>
	   let
		val  E'   = Env.new()
		val  _    = insertScope E
		val ids'  = trValTyVarSeq E tyvarseq @
			    unguardedTyVarsValBind E valbind
		val decs' = trValBindo (E,E') (SOME valbind)
		val  _    = deleteScope E
		val  _    = union(E,E')
		(* BUG: detect hiding and make correspondings decs local *)
	   in
		typvardecs(ids', decs')
	   end

	 | FUNDec(i, tyvarseq, fvalbind) =>
	   let
		val E'    = Env.new()
		val (ids',fmatches) = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _    = union(E,E')
		val  _    = insertScope E
		val ids'' = trValTyVarSeq E tyvarseq @
			    unguardedTyVarsFvalBind E fvalbind
		val exps' = trFmatches_rhs E fmatches
		val  _    = deleteScope E
		val decs' = ListPair.map
				(fn(id',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId id', id'), exp'))
				(ids',exps')
		(* BUG: detect hiding and make correspondings decs local *)
	   in
		typvardecs(ids'', [O.RecDec(i, decs')])
	   end

	 | TYPEDec(i, typbind) =>
	   let
		val E'    = Env.new()
		val decs' = trTypBindo (E,E') (SOME typbind)
		val  _    = union(E,E')
		(* BUG: detect hiding and make correspondings decs local *)
	   in
		decs'
	   end

	 | DATATYPEDec(i, datbind) =>
	   let
		val E'    = Env.new()
		val  _    = trDatBindo_lhs (E,E') (SOME datbind)
		val  _    = union(E,E')
		(* BUG: detect hiding and make correspondings decs local *)
		val decs' = trDatBindo_rhs (E,E') (SOME datbind)
		val  _    = union(E,E')
		(* BUG: detect hiding and make correspondings decs local *)
	   in
		[O.RecDec(i, decs')]
	   end

	 | REPLICATIONDec(i, tycon as TyCon(i',tycon'), longtycon) =>
	   let
		val (id',stamp)  = trTyCon_bind E tycon
		val (longid',E') = trLongTyCon E longtycon
		val  longido'    = case longid'
				     of O.LongId(_,longid',_) => SOME longid'
				      | O.ShortId _           => NONE
		val _            = insertTy(E, tycon', (i', stamp, E'))
	   in
		O.TypDec(i, id', O.ConTyp(infoLong longtycon, longid')) ::
		foldiVals (trOpenDecVal (E,i,longido')) [] E'
	   end

	 | CONSTRUCTORDec(i, dconbind) =>
	   let
		val E'    = Env.new()
		val decs' = trDconBindo (E,E') (SOME dconbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | STRUCTUREDec(i, strbind) =>
	   let
		val E'    = Env.new()
		val decs' = trStrBindo (E,E') (SOME strbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | SIGNATUREDec(i, sigbind) =>
	   let
		val E'    = Env.new()
		val decs' = trSigBindo (E,E') (SOME sigbind)
		val _     = union(E,E')
	   in
		decs'
	   end

	 | FUNCTORDec(i, funbind) =>
	   let
		val E'    = Env.new()
		val decs' = trFunBindo (E,E') (SOME funbind)
		val _     = union(E,E')
	   in
		decs'
	   end

	 | LOCALDec(i, dec1, dec2) =>
	   let
		val  _     = insertScope E
		val decs1' = trDec E dec1
		val  _     = insertScope E
		val decs2' = trDec E dec2
		val  _     = delete2ndScope E
		val  _     = mergeScope E
	   in
		O.LocalDec(i, decs1') :: decs2'
	   end

	 | OPENDec(i, longstrid) =>
	   let
		val (longid', E') = trLongStrId E longstrid
		val   _           = unionInf(E,E')
	   in
		(foldiVals (trOpenDecVal(E,i,SOME longid')) 
		(foldiTys  (trOpenDecTy (E,i,longid'))
		(foldiStrs (trOpenDecStr(E,i,longid'))
		(foldiFuns (trOpenDecFun(E,i,longid'))
		(foldiSigs (trOpenDecSig(E,i,longid')) [] E') E') E') E') E')
	   end

	 | EMPTYDec(i) =>
		[]

	 | SEQDec(i, dec1, dec2) =>
		trDec E dec1 @ trDec E dec2

	 | INFIXDec(i, n, VId(i',vid')) =>
		( insertInf(E, vid', (i', SOME(LEFT, n)))
		; []
		)

	 | INFIXRDec(i, n, VId(i',vid')) =>
		( insertInf(E, vid', (i', SOME(RIGHT, n)))
		; []
		)

	 | NONFIXDec(i, VId(i',vid')) =>
		( insertInf(E, vid', (i', NONE))
		; []
		)


    and trOpenDecVal (E,i,longido') (vid', (_,stamp,is), decs') =
	let
	    val name    = VId.toString vid'
	    val id'     = O.Id(i, stamp, O.ExId name)
	    val longid' = case longido'
			    of SOME longid' => O.LongId(i,longid',O.Lab(i,name))
			     | NONE         => O.ShortId(i, id')
	    val pat'    = O.VarPat(i, id')
	    val exp'    = O.VarExp(i, longid')
	    val _       = insertVal(E, vid', (i,stamp,is))
	    (* BUG: detect hiding and make correspondings decs local *)
	in
	    (case is
	       of V => O.ValDec(i, O.VarPat(i, id'), O.VarExp(i, longid'))
		| _ => O.ConDec(i, O.Con(i, id', []), O.SingTyp(i, longid'))
	    ) :: decs'
	end

    and trOpenDecTy (E,i,longid) (tycon', (_,stamp,E'), decs') =
	let
	    val name    = TyCon.toString tycon'
	    val id'     = O.Id(i, stamp, O.ExId name)
	    val lab'    = O.Lab(i, name)
	    val longid' = O.LongId(i, longid, lab')
	    val typ'    = O.ConTyp(i, longid')
	    val _       = insertTy(E, tycon', (i,stamp,E'))
	    (* BUG: detect hiding and make correspondings decs local *)
	in
	    O.TypDec(i, id', typ') :: decs'
	end

    and trOpenDecStr (E,i,longid) (strid', (_,stamp,E'), decs') =
	let
	    val name    = StrId.toString strid'
	    val id'     = O.Id(i, stamp, O.ExId name)
	    val lab'    = O.Lab(i, name)
	    val longid' = O.LongId(i, longid, lab')
	    val mod'    = longidToMod longid'
	    val _       = insertStr(E, strid', (i,stamp,E'))
	    (* BUG: detect hiding and make correspondings decs local *)
	in
	    O.ModDec(i, id', mod') :: decs'
	end

    and trOpenDecFun (E,i,longid) (funid', (_,stamp,E'), decs') =
	let
	    val name    = FunId.toString funid'
	    val id'     = O.Id(i, stamp, O.ExId(fromFunName name))
	    val lab'    = O.Lab(i, name)
	    val longid' = O.LongId(i, longid, lab')
	    val mod'    = longidToMod longid'
	    val _       = insertFun(E, funid', (i,stamp,E'))
	    (* BUG: detect hiding and make correspondings decs local *)
	in
	    O.ModDec(i, id', mod') :: decs'
	end

    and trOpenDecSig (E,i,longid) (sigid', (_,stamp,E'), decs') =
	let
	    val name    = SigId.toString sigid'
	    val id'     = O.Id(i, stamp, O.ExId name)
	    val lab'    = O.Lab(i, name)
	    val longid' = O.LongId(i, longid, lab')
	    val inf'    = O.ConInf(i, longid')
	    val _       = insertSig(E, sigid', (i,stamp,E'))
	    (* BUG: detect hiding and make correspondings decs local *)
	in
	    O.InfDec(i, id', inf') :: decs'
	end



    (* Value bindings *)

    and trValBindo (E,E') =
	fn NONE => nil

	 | SOME(PLAINValBind(_, pat, exp, valbindo)) =>
	   let
		val i    = Source.over(infoPat pat, infoExp exp)
		val pat' = trPat (E,E') pat
		val exp' = trExp E exp
	   in
		O.ValDec(i, pat', exp') :: trValBindo (E,E') valbindo
	   end

	| SOME(RECValBind(i, valbind)) =>
	   let
		val pats' = trRecValBindo_lhs (E,E') (SOME valbind)
		val  _    = union(E,E')
		val exps' = trRecValBindo_rhs E (SOME valbind)
		val decs' = ListPair.map
				(fn(pat',exp') =>
				 O.ValDec(Source.over(O.infoPat pat',
						      O.infoExp exp'),
					  pat', exp'))
				(pats',exps')
	   in
		[O.RecDec(i, decs')]
	   end


    and trRecValBindo_lhs (E,E') =
	fn NONE => nil

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
		trPat (E,E') pat :: trRecValBindo_lhs (E,E') valbindo

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_lhs (E,E') (SOME valbind)


    and trRecValBindo_rhs E =
	fn NONE => nil

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
		trExp E exp :: trRecValBindo_rhs E valbindo
		(* BUG: no check for admissibility *)

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_rhs E (SOME valbind)



    (* Function bindings *)

    and trFvalBindo_lhs (E,E') =
	fn NONE => ( nil, nil )
	 | SOME(FvalBind(i, match, fvalbindo)) =>
	   let
		val (id', fmatch)   = trFmatch_lhs (E,E') match
		val (ids',fmatches) = trFvalBindo_lhs (E,E') fvalbindo
	   in
		( id'::ids', fmatch::fmatches )
	   end


    and trFmatch_lhs (E,E') (Match(i, mrule, matcho)) =
	   let
		val (vid,fmrule) = trFmrule_lhs E mrule
		val VId(i',vid') = vid
		val (id',stamp)  = trVId_bind E vid
		val _ = insertDisjointVal(E', vid', (i',stamp,V))
			handle CollisionVal _ =>
			       errorVId("duplicate function ", vid,
					" in binding group")
	   in
		( id', Match(i, fmrule, trFmatcho_lhs (E,E',vid) matcho) )
	   end

    and trFmatcho_lhs (E,E',vid1) =
	fn NONE => NONE
	 | SOME(Match(i, mrule, matcho)) =>
	   let
		val (vid2,fmrule) = trFmrule_lhs E mrule
		val VId(_, vid1') = vid1
		val VId(_, vid2') = vid2
	   in
		if vid2' <> vid1' then
		    errorVId("inconsistent function name ", vid2,
			     " in function clause")
		else
		    SOME(Match(i, fmrule, trFmatcho_lhs (E,E',vid1) matcho))
	   end

    and trFmrule_lhs E (Mrule(i, pat, exp)) =
	   let
		val fpat = Infix.pat (infEnv E) pat
	   in
		( trFpat_lhs E fpat, Mrule(i, fpat, exp) )
	   end

    and trFpat_lhs E =
	fn ATPATPat(i, atpat)		=> trFatPat_lhs E atpat
	 | ( APPPat(i, fpat, _)
	   | TYPEDPat(i, fpat, _)
	   | WHENPat(i, fpat, _) )	=> trFpat_lhs E fpat
	 | ( NONPat(i,_)
	   | ASPat(i,_,_)
	   | WITHVALPat(i,_,_)
	   | WITHFUNPat(i,_,_) )	=> error(i, "invalid function pattern")

    and trFatPat_lhs E =
	fn LONGVIDAtPat(i, _, SHORTLong(_, vid as VId(_, vid'))) =>
	   (case lookupIdStatus(E, vid')
	      of  V        => vid
	       | (R | C _) =>
		 errorVId("rebinding data constructor ", vid, " as value")
	   )

	 | ALTAtPat(i, fpats) =>
	   let
		val vids               = trFpats_lhs E fpats
		val vid as VId(_,vid') = List.hd vids
	   in
		case List.find (fn(VId(_,vid'')) => vid'<>vid'') (List.tl vids)
		  of NONE =>
			vid

		   | SOME vid2 =>
			errorVId("inconsistent function name ", vid2,
				 " in function clause")
	   end

	 | PARAtPat(i, fpat) =>
		trFpat_lhs E fpat

	 | atpat =>
		error(infoAtPat atpat, "no function name in function clause")

    and trFpats_lhs E = List.map(trFpat_lhs E)


    and trFmatches_rhs E = List.map(trFmatch_rhs E)

    and trFmatch_rhs E (Match(i, fmrule, fmatcho)) =
	   let
		val (match',arity) = trFmrule_rhs E fmrule
		val  matches'      = trFmatcho_rhs (E,arity) fmatcho

		val ids'           = List.tabulate(arity, fn _ => inventId i)
		val exps'          = List.map(fn id' =>
						O.VarExp(Source.nowhere,
						O.ShortId(Source.nowhere, id')))
					      ids'
		val i'             = O.infoMatch match'
		val tupexp'        = tupexp(i', exps')
		val caseexp'       = O.CaseExp(i', tupexp', match'::matches')

		fun funexp    []      = caseexp'
		  | funexp(id'::ids') = O.FunExp(i', id', funexp ids')
	   in
		funexp ids'
	   end

    and trFmatcho_rhs (E,arity) =
	fn NONE => nil

	 | SOME(Match(i, fmrule, fmatcho)) =>
	   let
		val (match',arity') = trFmrule_rhs E fmrule
	   in
		if arity <> arity' then
		    error(infoMrule fmrule, "inconsistent number of arguments \
					    \in function clause")
		else
		    match' :: trFmatcho_rhs (E,arity) fmatcho
	   end

    and trFmrule_rhs E (Mrule(i, fpat, exp)) =
	   let
		val  E'          = Env.new()
		val (pat',arity) = trFpat_rhs (E,E') fpat
		val  _           = inheritScope(E,E')
		val  exp'        = trExp E exp
		val  _           = deleteScope E
	   in
		( O.Match(i, pat', exp'), arity )
	   end

    and trFpat_rhs (E,E') =
	fn ATPATPat(i, fatpat) =>
		trFatPat_rhs (E,E') fatpat

	 | fpat as APPPat(i,_,_)   =>
	   let
		val pats' = trFappPat_rhs (E,E') fpat
	   in
		( tuppat(i, pats'), List.length pats' )
	   end

	 | TYPEDPat(i, fpat, ty) =>
	   let
		val (pat',arity) = trFpat_rhs (E,E') fpat
		val  typ'        = trTy E ty
	   in
		( O.AnnPat(i, pat', typ'), arity )
	   end

	 | WHENPat(i, fpat, atexp) =>
	   let
		val  _   = insertScope E'
		val (pat',arity) = trFpat_rhs (E,E') fpat
		val  _   = inheritScope(E, copyScope E')
		val exp' = trAtExp E atexp
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId'("duplicate variable ", E', vid',
					  " in pattern or binding group")
	   in
		( O.GuardPat(i, pat', exp'), arity )
	   end

	 | ( NONPat(i,_) | ASPat(i,_,_)
	   | WITHVALPat(i,_,_) | WITHFUNPat(i,_,_) ) =>
		error(i,"invalid function pattern")

    and trFatPat_rhs (E,E') =
	fn ALTAtPat(i, fpats) =>
	   let
		val  _              = insertScope E'
		val (pat', arity)   = trFpat_rhs (E,E') (List.hd fpats)
		val  pat'arities    = trAltFpats_rhs (E,E') (List.tl fpats)
		val (pats',arities) = ListPair.unzip pat'arities
		val  _              = mergeDisjointScope E'
				      handle CollisionVal vid' =>
				      errorVId'("duplicate variable ", E',vid',
					  " in pattern or binding group")
	   in
		case List.find (fn(_,arity') => arity<>arity') pat'arities
		  of NONE         => ( O.AltPat(i, pat'::pats'), arity )
		   | SOME(pat',_) =>
			error(O.infoPat pat', "inconsistent number of \
					       \arguments in function clause")
	   end

	 | PARAtPat(i, fpat)	=> trFpat_rhs (E,E') fpat
	 | LONGVIDAtPat(i,_,_)	=> error(i, "no arguments in function clause")
	 | fatpat		=> error(infoAtPat fatpat,
					 "invalid function pattern")

    and trAltFpats_rhs (E,E') = List.map(trAltFpat_rhs (E,E'))

    and trAltFpat_rhs (E,E') fpat =
	let
	    val _    = insertScope E'
	    val pat'arity = trFpat_rhs (E,E') fpat
	    val E''  = splitScope E'
	    val _    = if Env.sizeScope E' = Env.sizeScope E'' then () else
			  error(infoPat fpat,"inconsistent pattern alternative")
	    val _    = Env.appiVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat fpat, "inconsistent pattern\
							 \ alternative")
			    ) E'
	in
	    pat'arity
	end


    and trFappPat_rhs (E,E') =
	fn ATPATPat(i, fatpat)	  => trFappAtPat_rhs (E,E') fatpat
	 | APPPat(i, fpat, atpat) => trFappPat_rhs (E,E') fpat
				     @ [trAtPat (E,E') atpat]
	 | pat =>
		error(infoPat pat, "invalid function pattern")

    and trFappAtPat_rhs (E,E') =
	fn LONGVIDAtPat _	=> nil
	 | PARAtPat(i, fpat)	=> trFappPat_rhs (E,E') fpat
	 | fatpat		=>
		error(infoAtPat fatpat, "invalid function pattern")



    (* Type and constructor bindings *)

    and trTypBindo (E,E') =
	fn NONE => []

	 | SOME(TypBind(_, tyvarseq,tycon as TyCon(i',tycon'), ty, typbindo)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, infoTy ty)
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val typ'        = trTy E ty
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', typ')
		val _ = insertDisjointTy(E', tycon', (i', stamp, Env.new()))
			handle CollisionTy _ =>
			       errorTyCon("duplicate type construtor ", tycon,
					  " in binding group")
	   in
		O.TypDec(i', id', funtyp') :: trTypBindo (E,E') typbindo
	   end


    and trDatBindo_lhs (E,E') =
	fn NONE => ()

	 | ( SOME(CLOSEDDatBind(i, tyvarseq, tycon, _, datbindo))
	   | SOME(OPENDatBind(i, tyvarseq, tycon, datbindo)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (id',stamp)      = trTyCon_bind E tycon
		val  _  = insertDisjointTy(E', tycon', (i', stamp, Env.new()))
			  handle CollisionTy _ =>
				 errorTyCon("duplicate type constructor ",
					     tycon, " in binding group")
	   in
		trDatBindo_lhs (E,E') datbindo
	   end

    and trDatBindo_rhs (E,E') =
	fn NONE => []

	 | SOME(CLOSEDDatBind(_, tyvarseq, tycon, conbind, datbindo)) =>
	   let
		val i'        = infoConBind conbind
		val i         = Source.over(infoSeq tyvarseq, i')
		val (id',E'') = trTyCon E tycon		(* bound before *)
		val _         = insertScope E
		val ids'      = trTyVarSeq E tyvarseq
		val cons'     = trConBindo (E,E'') (SOME conbind)
		val _         = deleteScope E
		val funtyp'   = funtyp(ids', O.SumTyp(i', cons'))
		val  _        = unionDisjoint(E',E'') handle CollisionVal vid' =>
				errorVId'("duplicate data constructor ",
					  E'', vid', " in binding group")
	   in
		O.TypDec(i, id', funtyp') :: trDatBindo_rhs (E,E') datbindo
	   end

	 | SOME(OPENDatBind(_, tyvarseq, tycon, datbindo)) =>
	   let
		val i'        = infoTyCon tycon
		val i         = Source.over(infoSeq tyvarseq, i')
		val (id',E'') = trTyCon E tycon		(* bound before *)
		val _         = insertScope E
		val ids'      = trTyVarSeq E tyvarseq
		val _         = deleteScope E
		val funtyp'   = funtyp(ids', O.ExtTyp(i'))
	   in
		O.TypDec(i, id', funtyp') :: trDatBindo_rhs (E,E') datbindo
	   end


    and trConBindo (E,E') =
	fn NONE => []

	 | SOME(ConBind(i, _, vid as VId(i',vid'), tyo, conbindo)) =>
	   let
		val (id',stamp) = trVId_bind E vid
		val  typs'      = trTyo E tyo
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					    " in datatype binding")
	   in
		O.Con(i, id', typs') :: trConBindo (E,E') conbindo
	   end

    and trDconBindo (E,E') =
	fn NONE => []

	 | SOME(NEWDconBind(_, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dconbindo)) =>
	   let
		val  i          = Source.over(i', infoLong longtycon)
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i', id', typs')
		val  ids'       = trTyVarSeq E tyvarseq
		val  typ'       = trTyVarSeqLongTyCon E (tyvarseq, longtycon)
		val  _          = deleteScope E
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					    " in binding group")
	   in
		O.ConDec(i, con', typ') :: trDconBindo (E,E') dconbindo
	   end

	 | SOME(EQUALDconBind(_, _, vid as VId(i',vid'), _,
							longvid, dconbindo)) =>
	   let
		val  i           = Source.over(i', infoLong longvid)
		val (id',stamp)  = trVId_bind E vid
		val (longid',is) = trLongVId E longvid
		val  _           = if is <> V then () else
				      error(i, "non-constructor on constructor \
					       \binding right hand side")
		val  con'        = O.Con(i', id', [])
		val  typ'        = O.SingTyp(O.infoLongid longid', longid')
		val  _           = insertDisjointVal(E', vid', (i', stamp, is))
				   handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					  " in binding group")
	   in
		O.ConDec(i, con', typ') :: trDconBindo (E,E') dconbindo
	   end


    and trTyo E  NONE     = []
      | trTyo E (SOME ty) = [trTy E ty]


    and trTyVarSeqLongTyCon E (tyvarseq, longtycon) =
	let
	    val (longid',_) = trLongTyCon E longtycon
	    val  typ'  = O.ConTyp(O.infoLongid longid', longid')
	    val  ids'  = trTyVarSeq E tyvarseq
	    val  typs' = List.map (fn id' => O.VarTyp(O.infoId id', id')) ids'
	in
	    apptyp(typs', typ')
	end


    (* Structure, signature, and functor bindings *)

    and trStrBindo (E,E') =
	fn NONE => []

	 | SOME(StrBind(_, strid as StrId(i',strid'), strexp, strbindo)) =>
	   let
		val i           = Source.over(i', infoStrExp strexp)
		val (id',stamp) = trStrId_bind E strid
		val (mod',E'')  = trStrExp E strexp
		val  _          = insertDisjointStr(E', strid', (i',stamp,E''))
				  handle CollisionStr _ =>
				   errorStrId("duplicate structure name ",strid,
					      " in binding group")
	   in
		O.ModDec(i, id', mod') :: trStrBindo (E,E') strbindo
	   end


    and trSigBindo (E,E') =
	fn NONE => []

	 | SOME(SigBind(_, sigid as SigId(i',sigid'), sigexp, sigbindo)) =>
	   let
		val i           = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trSigId_bind E sigid
		val (inf',E'')  = trSigExp E sigexp
		val  _          = insertDisjointSig(E', sigid', (i',stamp,E''))
				  handle CollisionSig _ =>
				  errorSigId("duplicate signature name ", sigid,
					     " in binding group")
	   in
		O.InfDec(i, id', inf') :: trSigBindo (E,E') sigbindo
	   end


    and trFunBindo (E,E') =
	fn NONE => []

	 | SOME(FunBind(_, funid as FunId(i1,funid'), strid as StrId(i2,strid'),
			   sigexp, strexp, funbindo)) =>
	   let
		val  i            = Source.over(i1, infoStrExp strexp)
		val (id1',stamp1) = trFunId_bind E funid
		val (id2',stamp2) = trStrId_bind E strid
		val (inf',E2)     = trSigExp E sigexp
		val  _            = insertScope E
		val  _            = insertStr(E, strid', (i2, stamp2, E2))
		val (mod',E1)     = trStrExp E strexp
		val  _            = deleteScope E
		val funmod'       = O.FunMod(i, id2', inf', mod')
		val  _            = insertDisjointFun(E',funid', (i1,stamp1,E1))
				    handle CollisionFun _ =>
				    errorFunId("duplicate functor name ", funid,
					       " in binding group")
	   in
		O.ModDec(i, id1', funmod') :: trFunBindo (E,E') funbindo
	   end



    (* Structure expressions *)

    and trStrExp E =
	fn STRUCTStrExp(i, dec) =>
	   let
		val _     = insertScope E
		val decs' = trDec E dec
		val E'    = splitScope E
	   in
		( O.StrMod(i, decs'), E' )
	   end

	 | LONGSTRIDStrExp(i, longstrid) =>
	   let
		val (longid',E') = trLongStrId E longstrid
	   in
		( longidToMod longid', E' )
	   end

	 | TRANSStrExp(i, strexp, sigexp) =>
	   (* UNFINISHED *)
	   let
		val (mod',E')  = trStrExp E strexp
		val (inf',E'') = trSigExp E sigexp
	   in
		( O.AnnMod(i, mod', inf'), E'' )
	   end

	 | OPAQStrExp (i, strexp, sigexp) =>
	   let
		val (mod',E')  = trStrExp E strexp
		val (inf',E'') = trSigExp E sigexp
	   in
		( O.AnnMod(i, mod', inf'), E'' )
	   end

	 | APPStrExp(i, longfunid, strexp) =>
	   let
		val  i'          = infoLong longfunid
		val (longid',E') = trLongFunId E longfunid
		val  mod1'       = longidToMod longid'
		val (mod2',_)    = trStrExp E strexp
	   in
		( O.AppMod(i, mod1', mod2'), E' )
	   end

	 | LETStrExp(i, dec, strexp) =>
	   let
		val  _        = insertScope E
		val  decs'    = trDec E dec
		val (mod',E') = trStrExp E strexp
		val  _        = deleteScope E
	   in
		( O.LetMod(i, decs', mod'), E' )
	   end


    (* Signatures and specifications *)

    and trSigExp E =
	fn SIGSigExp(i, spec) =>
	   let
		val _      = insertScope E
		val specs' = trSpec E spec
		val E'     = splitScope E
	   in
		( O.SigInf(i, specs'), E' )
	   end

	 | LONGSIGIDSigExp(i, sigid) =>
	   let
		val (longid',E') = trLongSigId E sigid
	   in
		( O.ConInf(i, longid'), E' )
	   end

	 | WHERETYPESigExp(i, sigexp, _, _, _) =>
		(* UNFINISHED *)
		trSigExp E sigexp

	 | WHERESIGNATURESigExp(i, sigexp, _, _) =>
		(* UNFINISHED *)
		trSigExp E sigexp

	 | WHERESigExp(i, sigexp, _, _) =>
		(* UNFINISHED *)
		trSigExp E sigexp


    and trSpec E =
	fn VALSpec(i, valdesc) =>
		trValDesco E (SOME valdesc)

	 | ( TYPESpec(i, typdesc)
	   | EQTYPESpec(i, typdesc) ) =>
		trTypDesco E (SOME typdesc)

	 | DATATYPESpec(i, datdesc) =>
	   let
		val  _     = trDatDesco_lhs E (SOME datdesc)
		val specs' = trDatDesco_rhs E (SOME datdesc)
	   in
		[O.RecSpec(i, specs')]
	   end

	 | REPLICATIONSpec(i, tycon as TyCon(i', tycon'), longtycon) =>
	   let
		val (id',stamp)  = trTyCon_bind E tycon
		val (longid',E') = trLongTyCon E longtycon
		val  longido'    = case longid'
				     of O.LongId(_,longid',_) => SOME longid'
				      | O.ShortId _           => NONE
		val _ = insertDisjointTy(E, tycon', (i', stamp, E'))
			handle CollisionTy _ =>
			errorTyCon("duplicate type constructor ", tycon,
				   " in signature") ;
		val _ = unionDisjoint(E,E') handle CollisionVal vid' =>
			errorVId'("duplicate value or constructor ", E', vid',
				 " in signature")
	   in
		O.TypSpec(i, id', O.ConTyp(infoLong longtycon, longid')) ::
		foldiVals (trOpenSpecVal (E,i,longido')) [] E'
	   end

	 | CONSTRUCTORSpec(i, dcondesc) =>
		trDconDesco E (SOME dcondesc)

	 | STRUCTURESpec(i, strdesc) =>
		trStrDesco E (SOME strdesc)

	 | SIGNATURESpec(i, sigdesc) =>
		trSigDesco E (SOME sigdesc)

	 | FUNCTORSpec(i, fundesc) =>
		trFunDesco E (SOME fundesc)

	 | INCLUDESpec(i, sigexp) =>
	   let
		val (inf',E') = trSigExp E sigexp
		val _ =
		    unionDisjoint(E,E')
		    handle CollisionInf vid' =>
			errorVId("duplicate fixity specification for \
				 \identifier ", VId(i,vid'), " in signature")
		     | CollisionVal vid' =>
			errorVId("duplicate value or constructor ",
				 VId(i,vid'), " in signature")
		     | CollisionTy tycon' =>
			errorTyCon("duplicate type constructor ",
				   TyCon(i,tycon'), " in signature")
		     | CollisionStr strid' =>
			errorStrId("duplicate structure ",
				   StrId(i,strid'), " in signature")
	   in
		[O.ExtSpec(i, inf')]
	   end

	 | EMPTYSpec(i) =>
		[]

	 | SEQSpec(i, spec1, spec2) =>
		trSpec E spec1 @ trSpec E spec2

	 | ( SHARINGTYPESpec(i, spec, _)
	   | SHARINGSIGNATURESpec(i, spec, _)
	   | SHARINGSpec(i, spec, _) ) =>
		(* UNFINISHED *)
		trSpec E spec

	 | INFIXSpec(i, n, vid as VId(i',vid')) =>
		(insertDisjointInf(E, vid', (i', SOME(Infix.LEFT, n)))
		 handle CollisionInf vid' =>
			errorVId("duplicate fixity specification for \
				 \identifier ", vid, " in signature")
		; []
		)

	 | INFIXRSpec(i, n, vid as VId(i',vid')) =>
		(insertDisjointInf(E, vid', (i', SOME(Infix.RIGHT, n)))
		 handle CollisionInf vid' =>
			errorVId("duplicate fixity specification for \
				 \identifier ", vid, " in signature")
		; []
		)

	 | NONFIXSpec(i, vid as VId(i',vid')) =>
		(insertDisjointInf(E, vid', (i', NONE))
		 handle CollisionInf vid' =>
			errorVId("duplicate fixity specification for \
				 \identifier ", vid, " in signature")
		; []
		)


    and trOpenSpecVal (E,i,longido') (vid', (_,stamp,is), specs') =
	let
	    val name    = VId.toString vid'
	    val id'     = O.Id(i, stamp, O.ExId name)
	    val longid' = case longido'
			    of SOME longid' => O.LongId(i,longid',O.Lab(i,name))
			     | NONE         => O.ShortId(i, id')
	    val typ'    = O.SingTyp(i, longid')
	    val _       = insertDisjointVal(E, vid', (i,stamp,is))
			  handle CollisionVal _ =>
			  errorVId'("duplicate value or constructor ", E, vid',
				    " in signature")
	in
	    (case is
	       of V => O.ValSpec(i, id', typ')
	        | _ => O.ConSpec(i, O.Con(i, id', []), typ')
	    ) :: specs'
	end




    (* Descriptions *)

    and trValDesco E =
	fn NONE => []

	 | SOME(ValDesc(_, vid as VId(i',vid'), ty, valdesco)) =>
	   let
		val  i          = Source.over(i', infoTy ty)
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val  ids'       = trAllTy E ty
		val  typ'       = alltyp(ids', trTy E ty)
		val  _          = deleteScope E
		val  _          = insertDisjointVal(E, vid', (i', stamp, V))
				  handle CollisionVal vid' =>
				     errorVId("duplicate value or constructor ",
					      vid, " in signature")
	   in
		O.ValSpec(i, id', typ') :: trValDesco E valdesco
	   end


    and trTypDesco E =
	fn NONE => []

	 | SOME(NEWTypDesc(_, tyvarseq, tycon as TyCon(i',tycon'), typdesco)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, i')
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', O.AbsTyp(i'))
		val _ = insertDisjointTy(E, tycon', (i', stamp, Env.new()))
			handle CollisionTy _ =>
			       errorTyCon("duplicate type construtor ", tycon,
					  " in signature")
	   in
		O.TypSpec(i, id', funtyp') :: trTypDesco E typdesco
	   end

	 | SOME(EQUALTypDesc(_, tyvarseq, tycon as TyCon(i',tycon'),
							ty, typdesco)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, infoTy ty)
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val typ'        = trTy E ty
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', typ')
		val _ = insertDisjointTy(E, tycon', (i', stamp, Env.new()))
			handle CollisionTy _ =>
			       errorTyCon("duplicate type construtor ", tycon,
					  " in signature")
	   in
		O.TypSpec(i, id', funtyp') :: trTypDesco E typdesco
	   end


    and trDatDesco_lhs E =
	fn NONE => ()

	 | ( SOME(CLOSEDDatDesc(i, tyvarseq, tycon, _, datdesco))
	   | SOME(OPENDatDesc(i, tyvarseq, tycon, datdesco)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (id',stamp)      = trTyCon_bind E tycon
		val _  = insertDisjointTy(E, tycon', (i', stamp, Env.new()))
			 handle CollisionTy _ =>
				errorTyCon("duplicate type constructor ",
					   tycon, " in signature")
	   in
		trDatDesco_lhs E datdesco
	   end

    and trDatDesco_rhs E =
	fn NONE => []

	 | SOME(CLOSEDDatDesc(_, tyvarseq, tycon, condesc, datdesco)) =>
	   let
		val i'       = infoConDesc condesc
		val i        = Source.over(infoSeq tyvarseq, i')
		val (id',E') = trTyCon E tycon
		val _        = insertScope E
		val ids'     = trTyVarSeq E tyvarseq
		val cons'    = trConDesco (E,E') (SOME condesc)
		val _        = deleteScope E
		val funtyp'  = funtyp(ids', O.SumTyp(i', cons'))
		val _        = unionDisjoint(E,E') handle CollisionVal vid' =>
				errorVId'("duplicate value or constructor ",
					  E', vid', " in signature")
	   in
		O.TypSpec(i, id', funtyp') :: trDatDesco_rhs E datdesco
	   end

	 | SOME(OPENDatDesc(_, tyvarseq, tycon, datdesco)) =>
	   let
		val i'       = infoTyCon tycon
		val i        = Source.over(infoSeq tyvarseq, i')
		val (id',E') = trTyCon E tycon
		val _        = insertScope E
		val ids'     = trTyVarSeq E tyvarseq
		val _        = deleteScope E
		val funtyp'  = funtyp(ids', O.ExtTyp(i'))
	   in
		O.TypSpec(i, id', funtyp') :: trDatDesco_rhs E datdesco
	   end


    and trConDesco (E,E') =
	fn NONE => []

	 | SOME(ConDesc(i, vid as VId(i',vid'), tyo, condesco)) =>
	   let
		val (id',stamp) = trVId_bind E vid
		val  typs'      = trTyo E tyo
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					    " in datatype binding")
	   in
		O.Con(i, id', typs') :: trConDesco (E,E') condesco
	   end


    and trDconDesco E =
	fn NONE => []

	 | SOME(NEWDconDesc(_, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dcondesco)) =>
	   let
		val  i          = Source.over(i', infoLong longtycon)
		val (id',stamp) = trVId_bind E vid
		val  hasArg     = not(Misc.Option_isNone tyo)
		val  _          = insertScope E
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i', id', typs')
		val  ids'       = trTyVarSeq E tyvarseq
		val  typ'       = trTyVarSeqLongTyCon E (tyvarseq, longtycon)
		val  _          = deleteScope E
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E, vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					    " in signature")
	   in
		O.ConSpec(i', con', typ') :: trDconDesco E dcondesco
	   end

	 | SOME(EQUALDconDesc(_, vid as VId(i',vid'), longvid, dcondesco)) =>
	   let
		val  i           = Source.over(i', infoLong longvid)
		val (id',stamp)  = trVId_bind E vid
		val (longid',is) = trLongVId E longvid
		val  _           = if is <> V then () else
				   error(i, "non-constructor on constructor \
					    \description right hand side")
		val  con'        = O.Con(i', id', [])
		val  typ'        = O.SingTyp(O.infoLongid longid', longid')
		val  _           = insertDisjointVal(E, vid', (i', stamp, is))
				   handle CollisionVal _ =>
				   errorVId("duplicate data constructor ", vid,
					    " in signature")
	   in
		O.ConSpec(i, con', typ') :: trDconDesco E dcondesco
	   end



    and trStrDesco E =
	fn NONE => []

	 | SOME(NEWStrDesc(_, strid as StrId(i',strid'), sigexp, strdesco)) =>
	   let
		val  i          = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trStrId_bind E strid
		val (inf',E')   = trSigExp E sigexp
		val  _          = insertDisjointStr(E, strid', (i', stamp, E'))
				  handle CollisionStr strid' =>
				     errorStrId("duplicate structure ",
						strid, " in signature")
	   in
		O.ModSpec(i, id', inf') :: trStrDesco E strdesco
	   end

	 | SOME(EQUALStrDesc(_, strid as StrId(i',strid'), sigexpo, longstrid,
								strdesco)) =>
	   let
		val  i           = Source.over(i', infoLong longstrid)
		val (id',stamp)  = trStrId_bind E strid
		val (longid',E') = trLongStrId E longstrid
		val  mod'        = longidToMod longid'
		val (mod'',E'')  = case sigexpo
				     of NONE => (mod',E')
				      | SOME sigexp =>
					let
					    val (inf',E'') = trSigExp E sigexp
					    val i''   = Source.over(
							  infoSigExp sigexp,
							  O.infoLongid longid')
					    val mod'' = O.AnnMod(i'', mod',inf')
					in
					    (mod'',E'')
					end
		val inf'         = O.SingInf(O.infoMod mod'', mod'')
		val  _           = insertDisjointStr(E, strid', (i', stamp, E''))
				   handle CollisionStr strid' =>
				     errorStrId("duplicate structure ",
						strid, " in signature")
	   in
		O.ModSpec(i, id', inf') :: trStrDesco E strdesco
	   end



    and trSigDesco E =
	fn NONE => []

	 | SOME(NEWSigDesc(_, sigid as SigId(i',sigid'), sigdesco)) =>
	   let
		val (id',stamp) = trSigId_bind E sigid
		val  inf'       = O.AbsInf(i')
		val _ = insertDisjointSig(E, sigid', (i', stamp, Env.new()))
			handle CollisionTy _ =>
			       errorSigId("duplicate signature name ", sigid,
					  " in signature")
	   in
		O.InfSpec(i', id', inf') :: trSigDesco E sigdesco
	   end

	 | SOME(EQUALSigDesc(_, sigid as SigId(i',sigid'), sigexp, sigdesco)) =>
	   let
		val  i          = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trSigId_bind E sigid
		val (inf',E')   = trSigExp E sigexp
		val  _          = insertDisjointSig(E, sigid', (i', stamp, E'))
				  handle CollisionSig _ =>
				  errorSigId("duplicate signature name ", sigid,
					     " in binding group")
	   in
		O.InfSpec(i, id', inf') :: trSigDesco E sigdesco
	   end



    and trFunDesco E =
	fn NONE => []

	 | SOME(FunDesc(_, funid as FunId(i1,funid'), strid as StrId(i2,strid'),
			   sigexp1, sigexp2, fundesco)) =>
	   let
		val  i            = Source.over(i1, infoSigExp sigexp2)
		val (id1',stamp1) = trFunId_bind E funid
		val (id2',stamp2) = trStrId_bind E strid
		val (inf1',E1)    = trSigExp E sigexp1
		val  _            = insertScope E
		val  _            = insertStr(E, strid', (i2, stamp2, E1))
		val (inf2',E2)    = trSigExp E sigexp2
		val  _            = deleteScope E
		val  inf'         = O.ArrInf(i, id1', inf1', inf2')
		val  _            = insertDisjointFun(E, funid', (i1,stamp1,E1))
				    handle CollisionFun _ =>
				    errorFunId("duplicate functor name ", funid,
					       " in binding group")
	   in
		O.ModSpec(i, id1', inf') :: trFunDesco E fundesco
	   end



    (* Programs *)

    fun trProgramo E =
	fn NONE => []

	 | SOME(Program(i, dec, programo)) =>
	   let
		val decs1' = trDec E dec
		val decs2' = trProgramo E programo
	   in
		decs1' @ decs2'
	   end


    fun translate E program = trProgramo E (SOME program)

  end
