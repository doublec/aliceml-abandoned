structure AbstractionPhase :> ABSTRACTION_PHASE =
  struct

    structure I   = InputGrammar
    structure O   = AbstractGrammar
    structure E   = AbstractionError
    structure Env = BindEnv

    open I
    open Env


  (* Error handling *)

    val error = E.error
    val warn  = E.warn

    fun errorVId(E, vid', Error) =
	error((#1 o Option.valOf o lookupVal)(E, vid'), Error vid')


  (* Miscellanous helpers *)

    fun prebound E =
	case lookupStr(E, StrId.fromString "")
	  of SOME x => x
	   | NONE   => raise Crash.Crash "AbstractionPhase: prebounds not found"

    fun stamp_prebound E = #2 (prebound E)
    fun Env_prebound   E = #3 (prebound E)

    fun inventId i = O.Id(i, Stamp.new(), Name.InId)

    fun longidToMod(O.ShortId(i, id))         = O.VarMod(i, id)
      | longidToMod(O.LongId(i, longid, lab)) =
	    O.SelMod(i, longidToMod longid, lab)

    fun tupexp(i, [exp]) = exp
      | tupexp(i,  exps) = O.TupExp(i, exps)

    fun tuppat(i, [pat]) = pat
      | tuppat(i,  pats) = O.TupPat(i, pats)

    fun annexp(exp,    []    ) = exp
      | annexp(exp, typ::typs) = annexp(O.AnnExp(O.infoTyp typ, exp, typ), typs)


    fun alltyp(ids,typ) =
	List.foldr (fn(id,typ) => O.AllTyp(O.infoTyp typ, id, typ)) typ ids

    fun funtyp(ids,typ) =
	List.foldr (fn(id,typ) => O.FunTyp(O.infoTyp typ, id, typ)) typ ids

    fun apptyp(typs,typ) =
	List.foldl (fn(typ1,typ2) =>
	      O.AppTyp(Source.over(O.infoTyp typ1, O.infoTyp typ2), typ2, typ1)
	    ) typ typs

    fun arrtyp(typs,typ) =
	List.foldr (fn(typ1,typ2) =>
	      O.ArrTyp(Source.over(O.infoTyp typ1, O.infoTyp typ2), typ1, typ2)
	    ) typ typs

    fun funinf(idinfs,inf) =
	List.foldr (fn((id,inf1),inf2) =>
	      O.FunInf(Source.over(O.infoId id, O.infoInf inf2), id, inf1, inf2)
	    ) inf idinfs

    fun vardec(ids,dec) =
	List.foldr (fn(id,dec) => O.VarDec(O.infoId id, id, dec)) dec ids

    fun varspec(ids,spec) =
	List.foldr (fn(id,spec) => O.VarSpec(O.infoId id, id, spec)) spec ids


    fun lookupIdStatus(E, vid') =
	case lookupVal(E, vid')
	  of NONE             => V
	   | SOME(i,stamp,is) => is



  (* Constants and identifiers *)

    fun trSCon E =
	fn SCon(i, SCon.INT n)		=> O.IntLit n
	 | SCon(i, SCon.WORD w)		=> O.WordLit w
	 | SCon(i, SCon.CHAR c)		=> O.CharLit c
	 | SCon(i, SCon.STRING s)	=> O.StringLit s
	 | SCon(i, SCon.REAL x)		=> O.RealLit x

    fun trLab E (Lab(i, lab)) = O.Lab(i, Label.fromString(Lab.toString lab))

    fun trTyVar E (tyvar as TyVar(i, tyvar')) =
	let
	    val (_,stamp) =
		case lookupVar(E, tyvar')
		  of SOME xx => xx
		   | NONE    => error(i, E.TyVarUnbound tyvar')
	in
	    O.Id(i, stamp, Name.ExId(TyVar.toString tyvar'))
	end

    fun trId (lookup,infoId,idId,toString,Unbound) E id =
	let
	    val i   = infoId id
	    val id' = idId id
	    val (_,stamp,x) = case lookup(E, id')
				of SOME xx => xx
				 | NONE    => error(i, Unbound id')
	in
	    ( O.Id(i, stamp, Name.ExId(toString id')), x )
	end

    val trVId   = trId(lookupVal, infoVId, idVId, VId.toString, E.VIdUnbound)
    val trTyCon = trId(lookupTy, infoTyCon, idTyCon,
			TyCon.toString, E.TyConUnbound)
    val trStrId = trId(lookupStr, infoStrId, idStrId,
			StrId.toString, E.StrIdUnbound)
    val trSigId = trId(lookupSig, infoSigId, idSigId,
			SigId.toString, E.SigIdUnbound)


    fun trId_bind (lookup,infoId,idId,toString,Shadowed) E id =
	let
	    val i     = infoId id
	    val id'   = idId id
	    val name  = toString id'
	    val stamp = Stamp.new()
	    val _     = if not(Option.isSome(lookup(E, id'))) then () else
			   warn(i, Shadowed id')
	in
	    ( O.Id(i, stamp, Name.ExId name), stamp )
	end


    val trTyVar_bind = trId_bind(lookupVar, infoTyVar, idTyVar, TyVar.toString,
				 E.TyVarShadowed)
    val trVId_bind   = trId_bind(lookupVal, infoVId,   idVId,   VId.toString,
				 E.VIdShadowed)
    val trTyCon_bind = trId_bind(lookupTy,  infoTyCon, idTyCon, TyCon.toString,
				 E.TyConShadowed)
    val trStrId_bind = trId_bind(lookupStr, infoStrId, idStrId, StrId.toString,
				 E.StrIdShadowed)
    val trSigId_bind = trId_bind(lookupSig, infoSigId, idSigId, SigId.toString,
				 E.SigIdShadowed)

    fun trVId_bind' E (VId(i, vid')) =
	let
	    val name  = VId.toString vid'
	    val stamp = Stamp.new()
	in
	    O.Id(i, stamp, Name.ExId name)
	end


    (* With polymorphic recursion we could avoid the following code
       duplication... *)

    fun trLongStrId' E =
	fn SHORTLong(i, strid) =>
	   let
		val (id',E') = trStrId E strid
	   in
		if O.stamp id' = stamp_prebound E then
		    ( NONE, E' )
		else
		    ( SOME(O.ShortId(i,id')), E' )
	   end

	 | DOTLong(i, longstrid, strid) =>
	   let
		val (longido',E') = trLongStrId' E longstrid
		val (id',x)       = trStrId E' strid
		val  longid'      =
		     case longido'
		       of SOME longid' => O.LongId(i, longid', O.idToLab id')
			| NONE         => O.ShortId(i, id')

	   in
		( SOME longid', x )
	   end

    fun trLongId trId E =
	fn SHORTLong(i, id) =>
	   let
		val (id',x) = trId E id
	   in
		if O.stamp id' = stamp_prebound E then
		    error(i, E.PreboundFirstClass)
		else
		    ( O.ShortId(i,id'), x )
	   end

	 | DOTLong(i, longstrid, id) =>
	   let
		val (longido',E') = trLongStrId' E longstrid
		val (id',x)       = trId E' id
	   in
		case longido'
		  of SOME longid' => ( O.LongId(i,longid', O.idToLab id'), x )
		   | NONE         => ( O.ShortId(i,id'), x )
	   end

    val trLongVId   = trLongId trVId
    val trLongTyCon = trLongId trTyCon
    val trLongStrId = trLongId trStrId
    val trLongSigId = trLongId trSigId



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

      | unguardedTyVarsExp E (PACKExp(_, longstrid)) =
	    []

    and unguardedTyVarsMatch E (Match(_, mrule, match_opt)) =
	    unguardedTyVarsMrule E mrule @ ?unguardedTyVarsMatch E match_opt

    and unguardedTyVarsMrule E (Mrule(_, pat, exp)) =
	    unguardedTyVarsPat E pat @ unguardedTyVarsExp E exp

    and unguardedTyVarsDec E (CONSTRUCTORDec(_, dconbind)) =
	    unguardedTyVarsDconBind E dconbind

      | unguardedTyVarsDec E (STRUCTUREDec(_, strbind)) =
	    unguardedTyVarsStrBind E strbind

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
	    val  E'  = splitScope E
	    val  _   = deleteScope E
	    val  _   = union(E,E')
	in
	    ids' @ ?unguardedTyVarsDconBind E dconbind_opt
	end

      | unguardedTyVarsDconBind E (EQUALDconBind(_, _,vid, _,longvid,
								dconbind_opt)) =
	    ?unguardedTyVarsDconBind E dconbind_opt

    and unguardedTyVarsStrBind E (StrBind(_, strid, strexp, strbind_opt)) =
	    unguardedTyVarsStrExp E strexp @
	    ?unguardedTyVarsStrBind E strbind_opt

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

      | unguardedTyVarsTy E (TUPLETy(_, tys)) =
	    List.concat(List.map (unguardedTyVarsTy E) tys)

      | unguardedTyVarsTy E (TYCONTy(_, tyseq, longtycon)) =
	    unguardedTyVarsTyseq E tyseq

      | unguardedTyVarsTy E (ARROWTy(_, ty, ty')) =
	    unguardedTyVarsTy E ty @ unguardedTyVarsTy E ty'

      | unguardedTyVarsTy E (PACKTy(_, longsigid)) =
	    []

      | unguardedTyVarsTy E (PARTy(_, ty)) =
	    unguardedTyVarsTy E ty

    and unguardedTyVarsTyRow E (ROWTyRow(_, lab, ty, tyrow_opt)) =
	    unguardedTyVarsTy E ty @ ?unguardedTyVarsTyRow E tyrow_opt

    and unguardedTyVarsTyseq E (Seq(_, tys)) =
	    List.concat(List.map (unguardedTyVarsTy E) tys)

    and unguardedTyVarsAtStrExp E (STRUCTAtStrExp(_, dec)) =
	    unguardedTyVarsDec E dec

      | unguardedTyVarsAtStrExp E (LONGSTRIDAtStrExp(_, longstrid)) =
	    []

      | unguardedTyVarsAtStrExp E (LETAtStrExp(_, dec, strexp)) =
	    unguardedTyVarsDec E dec @ unguardedTyVarsStrExp E strexp

      | unguardedTyVarsAtStrExp E (PARAtStrExp(_, strexp)) =
	    unguardedTyVarsStrExp E strexp

    and unguardedTyVarsStrExp E (ATSTREXPStrExp(_, atstrexp)) =
	    unguardedTyVarsAtStrExp E atstrexp

      | unguardedTyVarsStrExp E (APPStrExp(_, strexp, atstrexp)) =
	    unguardedTyVarsStrExp E strexp @ unguardedTyVarsAtStrExp E atstrexp

      | unguardedTyVarsStrExp E ( TRANSStrExp(_, strexp, _)
				| OPAQStrExp(_, strexp, _)
				| FCTStrExp(_, _, strexp) ) =
	    unguardedTyVarsStrExp E strexp

      | unguardedTyVarsStrExp E (UNPACKStrExp(_, exp, sigexp)) =
	    unguardedTyVarsExp E exp

      (*UNFINISHED: if we have LETSigExp then we must check sigexps as well*)



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
	 | RECORDAtExp(i, exprowo) =>
	   let
		val  _   = insertScope E
		val row' = trExpRowo E exprowo
		val  _   = deleteScope E
	   in
		O.RowExp(i, row')
	   end
	 | UPDATEAtExp(i, atexp, exprow) =>
	   let
		val exp' = trAtExp E atexp
		val  _   = insertScope E
		val row' = trExpRowo E (SOME exprow)
		val  _   = deleteScope E
	   in
		O.CompExp(i, exp', O.RowExp(infoExpRow exprow, row'))
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
				error(i', E.ExpRowLabDuplicate lab')
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
	   in
		O.FunExp(i, matches')
	   end

	| PACKExp(i, longstrid) =>
	  let
		val (longid',E') = trLongStrId E longstrid
		val  mod'        = longidToMod longid'
	  in
		O.PackExp(i, mod')
	  end


    and trAppExp E =
	fn APPExp(i, exp, atexp) => O.AppExp(i, trAppExp E exp, trAtExp E atexp)
	 | ATEXPExp(i, atexp)    => trAtExp E atexp
	 | exp                   => trExp E exp

    and trExps E = List.map(trExp E)



  (* Matches and patterns *)

    and trMatcho  E matcho = List.rev(trMatcho' (E,[]) matcho)
    and trMatcho'(E,acc) =
	fn NONE => acc
	 | SOME(Match(i, mrule, matcho)) =>
	   let
		val match' = trMrule E mrule
	   in
		trMatcho' (E, match'::acc) matcho
	   end

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
	      of C k => O.ConPat(i, k, #1(trLongVId E longvid))
	       | R   => O.RefPat(i)
	       | V   =>
		 let
		    (* If inside an alternative pattern then E' contains
		     * an upper scope where the variable is already bound.
		     * We have to reuse the stamp found there.
		     *)
		    val _ = if Option.isSome(lookupScopeVal(E', vid')) then
			       error(i', E.PatVIdDuplicate vid')
			    else ()
		    val (id',stamp) =
			case lookupVal(E', vid')
			  of NONE            => trVId_bind E vid
			   | SOME(_,stamp,_) => ( O.Id(i', stamp,
						   Name.ExId(VId.toString vid'))
						, stamp )
		    val _ = insertVal(E', vid', (i',stamp,V))
		 in
		    O.VarPat(i, id')
		 end
	   )
	 | LONGVIDAtPat(i, _, longvid) =>
	   (case trLongVId E longvid
	      of (longid', C k) => O.ConPat(i, k, longid')
	       | (longid', R)   => O.RefPat(i)
	       | (longid', V)   => error(i, E.PatLongVIdVar)
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
				errorVId(E', vid', E.PatVIdDuplicate)
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
				error(i', E.PatRowLabDuplicate lab')
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
				errorVId(E', vid', E.PatVIdDuplicate)
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
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		O.WithPat(i, pat', decs')
	   end

	 | WITHFUNPat(i, pat, fvalbind) =>
	   let
		val  _   = insertScope E'
		val pat' = trPat (E,E') pat
		val  _   = inheritScope(E, copyScope E')
		val  _   = insertScope E'
		val ids' = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _   = inheritScope(E, copyScope E')
		val exps'= trFvalBindo_rhs E (SOME fvalbind)
		val decs'= ListPair.map
				(fn(id',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId id', id'), exp'))
				(ids',exps')
		val  _   = deleteScope E
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.WithPatVIdDuplicate)
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
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
		  of ( O.ConPat _ | O.RefPat _ | O.AppPat _ ) =>
			O.AppPat(i, pat1', pat2')

		   | _ => error(i, E.AppPatNonCon)
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
			  error(infoPat pat, E.AltPatInconsistent)
	    val _    = Env.appiScopeVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat pat, E.AltPatInconsistent)
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

	 | TUPLETy(i, tys) =>
	   let
		val typs' = trTys E tys
	   in
		O.TupTyp(i, typs')
	   end

	 | ARROWTy(i, ty1, ty2) => O.ArrTyp(i, trTy E ty1, trTy E ty2)

	 | PACKTy(i, longsigid) =>
	   let
		val (longid',E') = trLongSigId E longsigid
		val  inf'        = O.ConInf(infoLong longsigid, longid')
	   in
		O.PackTyp(i, inf')
	   end

	 | PARTy(i, ty) => trTy E ty

    and trTys E = List.map (trTy E)


    and trTyRowo E =
	fn NONE => O.Row(Source.nowhere, [], false)

	 | SOME(ROWTyRow(i, lab as Lab(i',lab'), ty, tyrowo)) =>
	   let
		val i1'    = Source.over(i', infoTy ty)
		val field' = O.Field(i1', trLab E lab, trTy E ty)
		val _      = insertFld(E, lab', i') handle CollisionFld _ =>
				error(i', E.TyRowLabDuplicate lab')
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
				error(i, E.TyVarSeqDuplicate tyvar')
	in
	    id'
	end


    (* Tyvarseqs at a val or fun *)

    and trValTyVarSeq E (Seq(i, tyvars)) = List.map (trValSeqTyVar E) tyvars

    and trValSeqTyVar E (tyvar as TyVar(i, tyvar')) =
	if Option.isSome(lookupVar(E, tyvar')) then
	    error(i, E.ValTyVarSeqDuplicate tyvar')
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
	 | RECORDTy(i, tyrowo)   => trAllTyRowo E tyrowo
	 | TUPLETy(i, tys)       => List.concat(List.map (trAllTy E) tys)
	 | ARROWTy(i, ty1, ty2)  => trAllTy E ty1 @ trAllTy E ty2
	 | PACKTy(i, longsigid)  => []
	 | PARTy(i, ty)          => trAllTy E ty

    and trAllTyRowo E =
	fn NONE                               => []
	 | SOME(ROWTyRow(i, lab, ty, tyrowo)) =>
		trAllTy E ty @ trAllTyRowo E tyrowo

    and trAllTySeq E (Seq(i, tys)) = List.concat(List.map (trAllTy E) tys)



  (* Declarations *)

    and trDec  E dec  = List.rev(trDec' (E,[]) dec)
    and trDec'(E,acc) =
	fn VALDec(i, tyvarseq, valbind) =>
	   let
		val  E'   = Env.new()
		val  _    = insertScope E
		val ids'  = trValTyVarSeq E tyvarseq @
			    unguardedTyVarsValBind E valbind
		val decs' = (if List.null ids'
			     then trValBindo'(E,E',acc)
			     else trValBindo (E,E') ) (SOME valbind)
		val  _    = deleteScope E
		val  _    = union(E,E')
	   in
		if List.null ids'
		then decs'
		else List.map (fn dec' => vardec(ids', dec')) decs' @ acc
	   (* UNFINISHED: violates uniqueness of stamps in bindings *)
	   end

	 | FUNDec(i, tyvarseq, fvalbind) =>
	   let
		val E'    = Env.new()
		val ids'  = trFvalBindo_lhs (E,E') (SOME fvalbind)
		val  _    = union(E,E')
		val  _    = insertScope E
		val ids'' = trValTyVarSeq E tyvarseq @
			    unguardedTyVarsFvalBind E fvalbind
		val exps' = trFvalBindo_rhs E (SOME fvalbind)
		val  _    = deleteScope E
		val decs' = ListPair.map
				(fn(id',exp') =>
				 O.ValDec(O.infoExp exp',
					  O.VarPat(O.infoId id', id'), exp'))
				(ids',exps')
	   in
		vardec(ids'', O.RecDec(i, decs')) :: acc
	   end

	 | TYPEDec(i, typbind) =>
	   let
		val E'    = Env.new()
		val decs' = trTypBindo' (E,E',acc) (SOME typbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | EQTYPEDec(i, typbind) =>
	   (* UNFINISHED *)
	   let
		val E'    = Env.new()
		val decs' = trTypBindo' (E,E',acc) (SOME typbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | EQEQTYPEDec(i, typbind) =>
	   (* UNFINISHED *)
	   let
		val E'    = Env.new()
		val decs' = trTypBindo' (E,E',acc) (SOME typbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | DATATYPEDec(i, datbind) =>
	   let
		val E'    = Env.new()
		val  _    = trDatBindo_lhs (E,E') (SOME datbind)
		val  _    = union(E,E')
		val decs' = trDatBindo_rhs (E,E') (SOME datbind)
		val  _    = union(E,E')
	   in
		O.RecDec(i, decs') :: acc
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
		foldiVals (trOpenDecVal (E,i,longido'))
		  (O.TypDec(i, id', O.ConTyp(infoLong longtycon, longid'))::acc)
		  E'
	   end

	 | CONSTRUCTORDec(i, dconbind) =>
	   let
		val E'    = Env.new()
		val decs' = trDconBindo' (E,E',acc) (SOME dconbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | STRUCTUREDec(i, strbind) =>
	   let
		val E'    = Env.new()
		val decs' = trStrBindo' (E,E',acc) (SOME strbind)
		val  _    = union(E,E')
	   in
		decs'
	   end

	 | SIGNATUREDec(i, sigbind) =>
	   let
		val E'    = Env.new()
		val decs' = trSigBindo' (E,E',acc) (SOME sigbind)
		val _     = union(E,E')
	   in
		decs'
	   end

	 | LOCALDec(i, dec1, dec2) =>
	   let
		val  _     = insertScope E
		val decs1' = trDec E dec1
		val  _     = insertScope E
		val decs2' = trDec' (E, O.LocalDec(i, decs1')::acc) dec2
		val  E'    = splitScope E
		val  _     = deleteScope E
		val  _     = union(E,E')
	   in
		decs2'
	   end

	 | OPENDec(i, longstrid) =>
	   let
		val (longid', E') = trLongStrId E longstrid
		val   _           = unionInf(E,E')
	   in
		(foldiVals (trOpenDecVal(E,i,SOME longid'))
		(foldiTys  (trOpenDecTy (E,i,longid'))
		(foldiStrs (trOpenDecStr(E,i,longid'))
		(foldiSigs (trOpenDecSig(E,i,longid')) acc E') E') E') E')
	   end

	 | EMPTYDec(i) =>
		acc

	 | SEQDec(i, dec1, dec2) =>
		trDec' (E, trDec' (E,acc) dec1) dec2

	 | PREBOUNDDec(i, strid as StrId(i',strid')) =>
	   let
		val  _           = trStrId_bind E strid
		val (_,stamp,E') = prebound E
		val  _           = insertStr(E, strid', (i',stamp,E'))
	   in
		acc
	   end

	 | PRIMITIVEVALDec(i, _, vid as VId(i',vid'), ty, s) =>
	   let
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val  ids'       = trAllTy E ty
		val  typ'       = trTy E ty
		val  _          = deleteScope E
		val  pat'       = O.VarPat(i', id')
		val  exp'       = O.PrimExp(i, s, typ')
		val  dec'       = O.ValDec(i, pat', exp')
		val  _          = insertVal(E, vid', (i, stamp, V))
	   in
		vardec(ids', dec') :: acc
	   end

	 | PRIMITIVECONSTRUCTORDec
		(i, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon, s) =>
	   let
		val  id1'        = inventId i
		val (id2',stamp) = trVId_bind E vid
		val  _           = insertScope E
		val (ids',typ')  = trTyVarSeqLongTyCon E (tyvarseq, longtycon)
		val  typs'       = trTyo E tyo
		val  _           = deleteScope E
		val  typ1'       = arrtyp(typs',typ')
		val  pat1'       = O.VarPat(i', id1')
		val  exp1'       = O.PrimExp(i, s, typ1')
		val  dec1'       = O.ValDec(i, pat1', exp1')
		val  con'        = O.Con(i', id2', [])
		val  typ2'       = O.SingTyp(i, O.ShortId(i',id1'))
		val  dec2'       = O.ConDec(i, con', typ2')
		val  k           = List.length typs'
		val  _           = insertVal(E, vid', (i', stamp, C k))
	   in
		dec2' :: vardec(ids', dec1') :: acc
	   end

	 | PRIMITIVESTRUCTUREDec(i, strid as StrId(i',strid'), sigexp, s) =>
	   let
		val (id',stamp) = trStrId_bind E strid
		val (inf',E')   = trSigExp E sigexp
		val  mod'       = O.PrimMod(i, s, inf')
		val  dec'       = O.ModDec(i, id', mod')
		val  _          = insertStr(E, strid', (i, stamp, E'))
	   in
		dec' :: acc
	   end

	 | OVERLOADDec(i, _, vid, tyvar, ty) =>
	   (*UNFINISHED*)
	   let
		val (id',stamp) = trVId_bind E vid
		val _           = insertScope E
		val id1'        = trTyVar_bind E tyvar
		val _           = insertScope E
		val ids'        = trAllTy E ty
		val typ'        = alltyp(ids', trTy E ty)
		val _           = deleteScope E
		val _           = deleteScope E
	   in
		acc
	   end

	 | INSTANCEDec(i, _, vid, longtycon, longvid) =>
	   (*UNFINISHED*)
		acc

	 | INSTANCESCONDec(i, scon, longtycon) =>
	   (*UNFINISHED*)
		acc

	 | INFIXDec(i, n, vid as VId(i',vid')) =>
	   let
		val id'  = trVId_bind' E vid
		val fix' = Fixity.INFIX(n, Fixity.LEFT)
		val dec' = O.FixDec(i, id', fix')
		val _    = insertInf(E, vid', (i', SOME(LEFT, n)))
	   in
		dec' :: acc
	   end

	 | INFIXRDec(i, n, vid as VId(i',vid')) =>
	   let
		val id'  = trVId_bind' E vid
		val fix' = Fixity.INFIX(n, Fixity.RIGHT)
		val dec' = O.FixDec(i, id', fix')
		val _    = insertInf(E, vid', (i', SOME(RIGHT, n)))
	   in
		dec' :: acc
	   end

	 | NONFIXDec(i, vid as VId(i',vid')) =>
	   let
		val id'  = trVId_bind' E vid
		val fix' = Fixity.NONFIX
		val dec' = O.FixDec(i, id', fix')
		val _    = insertInf(E, vid', (i', NONE))
	   in
		dec' :: acc
	   end


    and trOpenDecVal (E,i,longido') (vid', (_,stamp1,is), acc) =
	let
	    val name    = VId.toString vid'
	    val stamp2  = Stamp.new()
	    val id1'    = O.Id(i, stamp1, Name.ExId name)
	    val id2'    = O.Id(i, stamp2, Name.ExId name)
	    val longid' = case longido'
			    of SOME longid' => O.LongId(i,longid',O.Lab(i,
							Label.fromString name))
			     | NONE         => O.ShortId(i, id1')
	    val pat'    = O.VarPat(i, id2')
	    val exp'    = O.VarExp(i, longid')
	    val _       = insertVal(E, vid', (i,stamp2,is))
	in
	    (case is
	       of V => O.ValDec(i, O.VarPat(i, id2'), O.VarExp(i, longid'))
		| _ => O.ConDec(i, O.Con(i, id2', []), O.SingTyp(i, longid'))
	    ) :: acc
	end

    and trOpenDecTy (E,i,longid) (tycon', (_,stamp1,E'), acc) =
	let
	    val name    = TyCon.toString tycon'
	    val stamp2  = Stamp.new()
	    val id'     = O.Id(i, stamp2, Name.ExId name)
	    val lab'    = O.Lab(i, Label.fromString name)
	    val longid' = O.LongId(i, longid, lab')
	    val typ'    = O.ConTyp(i, longid')
	    val _       = insertTy(E, tycon', (i,stamp2,E'))
	in
	    O.TypDec(i, id', typ') :: acc
	end

    and trOpenDecStr (E,i,longid) (strid', (_,stamp1,E'), acc) =
	let
	    val name    = StrId.toString strid'
	    val stamp2  = Stamp.new()
	    val id'     = O.Id(i, stamp2, Name.ExId name)
	    val lab'    = O.Lab(i, Label.fromString name)
	    val longid' = O.LongId(i, longid, lab')
	    val mod'    = longidToMod longid'
	    val _       = insertStr(E, strid', (i,stamp2,E'))
	in
	    O.ModDec(i, id', mod') :: acc
	end

    and trOpenDecSig (E,i,longid) (sigid', (_,stamp1,E'), acc) =
	let
	    val name    = SigId.toString sigid'
	    val stamp2  = Stamp.new()
	    val id'     = O.Id(i, stamp2, Name.ExId name)
	    val lab'    = O.Lab(i, Label.fromString name)
	    val longid' = O.LongId(i, longid, lab')
	    val inf'    = O.ConInf(i, longid')
	    val _       = insertSig(E, sigid', (i,stamp2,E'))
	in
	    O.InfDec(i, id', inf') :: acc
	end



  (* Value bindings *)

    and trValBindo (E,E') valbindo = List.rev(trValBindo' (E,E',[]) valbindo)
    and trValBindo'(E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(_, pat, exp, valbindo)) =>
	   let
		val i    = Source.over(infoPat pat, infoExp exp)
		val pat' = trPat (E,E') pat
		val exp' = trExp E exp
		val dec' = O.ValDec(i, pat', exp')
	   in
		trValBindo' (E,E', dec'::acc) valbindo
	   end

	| SOME(RECValBind(i, valbind)) =>
	   let
		val pats' = trRecValBindo_lhs' (E,E',[]) (SOME valbind)
		val  _    = union(E,E')
		val exps' = trRecValBindo_rhs' (E,[]) (SOME valbind)
		val decs' = ListPair.map
				(fn(pat',exp') =>
				 O.ValDec(Source.over(O.infoPat pat',
						      O.infoExp exp'),
					  pat', exp'))
				(pats',exps')
	   in
		O.RecDec(i, decs') :: acc
	   end


    and trRecValBindo_lhs' (E,E',acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
	   let
		val pat' = trPat (E,E') pat
	   in
		trRecValBindo_lhs' (E,E', pat'::acc) valbindo
	   end

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_lhs' (E,E',acc) (SOME valbind)


    and trRecValBindo_rhs' (E,acc) =
	fn NONE => acc

	 | SOME(PLAINValBind(i, pat, exp, valbindo)) =>
	   (* BUG: no check for admissibility *)
	   let
		val exp' = trExp E exp
	   in
		trRecValBindo_rhs' (E, exp'::acc) valbindo
	   end

	 | SOME(RECValBind(i, valbind)) =>
		trRecValBindo_rhs' (E,acc) (SOME valbind)



  (* Function bindings *)

    and trFvalBindo_lhs (E,E') fvalbindo =
	    List.rev(trFvalBindo_lhs' (E,E',[]) fvalbindo)

    and trFvalBindo_lhs'(E,E',acc) =
	fn NONE => acc
	 | SOME(FvalBind(i, fmatch, fvalbindo)) =>
	   let
		val id' = trFmatch_lhs (E,E') fmatch
	   in
		trFvalBindo_lhs' (E,E', id'::acc) fvalbindo
	   end


    and trFmatch_lhs (E,E') (Match(i, fmrule, fmatcho)) =
	   let
		val vid as VId(i',vid') = trFmrule_lhs E fmrule
		val (id',stamp)         = trVId_bind E vid
		val _ = trFmatcho_lhs (E,vid) fmatcho
		val _ = insertDisjointVal(E', vid', (i',stamp,V))
			handle CollisionVal _ =>
			       error(i', E.FvalBindDuplicate vid')
	   in
		id'
	   end

    and trFmatcho_lhs (E,vid1) =
	fn NONE => ()
	 | SOME(Match(i, fmrule, fmatcho)) =>
	   let
		val vid2 as VId(i',vid2') = trFmrule_lhs E fmrule
	   in
		if idVId vid1 = idVId vid2 then
		    trFmatcho_lhs (E,vid1) fmatcho
		else
		    error(i', E.FvalBindNameInconsistent vid2')
	   end

    and trFmrule_lhs E (Mrule(i, fpat, exp)) =
	   trFpat_lhs E fpat

    and trFpat_lhs E =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trFappPat_lhs E (Infix.pat (infEnv E) fpat)
	 | ( TYPEDPat(i, fpat, _)
	   | WHENPat(i, fpat, _) )	=> trFpat_lhs E fpat
	 | ( NONPat(i,_)
	   | ASPat(i,_,_)
	   | WITHVALPat(i,_,_)
	   | WITHFUNPat(i,_,_) )	=> error(i, E.FvalBindPatInvalid)

    and trFappPat_lhs E =
	fn APPPat(i, fpat, atpat)	=> trFappPat_lhs E fpat
	 | ATPATPat(i, atpat)		=> trFatPat_lhs E atpat
	 | fpat				=> trFpat_lhs E fpat

    and trFatPat_lhs E =
	fn LONGVIDAtPat(i, _, SHORTLong(_, vid as VId(i', vid'))) =>
	   (case lookupIdStatus(E, vid')
	      of  V        => vid
	       | (R | C _) => error(i', E.FvalBindNameCon vid')
	   )

	 | ALTAtPat(i, fpats) =>
	   let
		val vids               = trFpats_lhs E fpats
		val vid as VId(_,vid') = List.hd vids
	   in
		case List.find (fn(VId(_,vid'')) => vid'<>vid'') (List.tl vids)
		  of NONE                => vid
		   | SOME(VId(i',vid2')) =>
			error(i', E.FvalBindNameInconsistent vid2')
	   end

	 | PARAtPat(i, fpat) =>
		trFpat_lhs E fpat

	 | atpat =>
		error(infoAtPat atpat, E.FvalBindNameMissing)

    and trFpats_lhs E = List.map(trFpat_lhs E)



    and trFvalBindo_rhs E fvalbindo =
	    List.rev(trFvalBindo_rhs' (E,[]) fvalbindo)

    and trFvalBindo_rhs'(E,acc) =
	fn NONE => acc
	 | SOME(FvalBind(i, fmatch, fvalbindo)) =>
	   let
		val exp' = trFmatch_rhs E fmatch
	   in
		trFvalBindo_rhs' (E, exp'::acc) fvalbindo
	   end

    and trFmatch_rhs E (Match(i, fmrule, fmatcho)) =
	   let
		val (match',arity) = trFmrule_rhs E fmrule
		val  matches'      = match' :: trFmatcho_rhs (E,arity) fmatcho
		val  i'            = O.infoMatch match'
	   in
		if arity = 1 then
		    O.FunExp(i', matches')
		else
		    let
			val ids'     = List.tabulate(arity, fn _ => inventId i)
			val exps'    = List.map(fn id' =>
						O.VarExp(Source.nowhere,
						O.ShortId(Source.nowhere, id')))
					      ids'
			val tupexp'  = tupexp(i', exps')
			val caseexp' = O.CaseExp(i', tupexp', matches')

			fun funexp    []      = caseexp'
			  | funexp(id'::ids') =
			    let
				val pat'   = O.VarPat(i, id')
				val match' = O.Match(i, pat', funexp ids')
			    in
				O.FunExp(i', [match'])
			    end
		    in
			funexp ids'
		    end
	   end

    and trFmatcho_rhs (E,arity) fmatcho =
	    List.rev(trFmatcho_rhs' (E,arity,[]) fmatcho)

    and trFmatcho_rhs' (E,arity,acc) =
	fn NONE => acc

	 | SOME(Match(i, fmrule, fmatcho)) =>
	   let
		val (match',arity') = trFmrule_rhs E fmrule
	   in
		if arity <> arity' then
		    error(infoMrule fmrule, E.FvalBindArityInconsistent)
		else
		    trFmatcho_rhs' (E, arity, match'::acc) fmatcho
	   end

    and trFmrule_rhs E (Mrule(i, fpat, exp)) =
	   let
		val  E'                = Env.new()
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  _                 = inheritScope(E,E')
		val  exp'              = trExp E exp
		val  _                 = deleteScope E
	   in
		( O.Match(i, pat', annexp(exp',typs')), arity )
	   end

    and trFpat_rhs (E,E') =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trFappPat_rhs (E,E') (Infix.pat (infEnv E) fpat)

	 | TYPEDPat(i, fpat, ty) =>
	   let
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  typ'              = trTy E ty
	   in
		( pat', arity, typ'::typs' )
	   end

	 | WHENPat(i, fpat, atexp) =>
	   let
		val  _   = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') fpat
		val  _   = inheritScope(E, copyScope E')
		val exp' = trAtExp E atexp
		val  _   = deleteScope E
		val  _   = mergeDisjointScope E' handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		( O.GuardPat(i, pat', exp'), arity, typs' )
	   end

	 | ( NONPat(i,_) | ASPat(i,_,_)
	   | WITHVALPat(i,_,_) | WITHFUNPat(i,_,_) ) =>
		error(i, E.FvalBindPatInvalid)

    and trFappPat_rhs (E,E') =
	fn fpat as APPPat _ =>
	   let
		val pats' = trAppliedFappPat_rhs (E,E') fpat
	   in
		( tuppat(infoPat fpat, pats'), List.length pats', [] )
	   end
	 | ATPATPat(i, atpat)		=> trFatPat_rhs (E,E') atpat
	 | fpat				=> trFpat_rhs (E,E') fpat

    and trFatPat_rhs (E,E') =
	fn ALTAtPat(i, fpats) =>
	   let
		val  _                 = insertScope E'
		val (pat',arity,typs') = trFpat_rhs (E,E') (List.hd fpats)
		val  pat'aritytyps's   = trAltFpats_rhs (E,E') (List.tl fpats)
		val (pats',arities,typs'') =
			 List.foldr (fn((p,a,ts), (pl,al,tl)) =>
					(p::pl, a::al, ts@tl)
				    ) ([],[],[]) pat'aritytyps's
		val  _ = mergeDisjointScope E'
			 handle CollisionVal vid' =>
				errorVId(E', vid', E.PatVIdDuplicate)
	   in
		case List.find (fn(_,arity',_) => arity<>arity') pat'aritytyps's
		  of NONE => ( O.AltPat(i, pat'::pats'), arity, typs' @ typs'' )
		   | SOME(pat',_,_) =>
			error(O.infoPat pat', E.FvalBindArityInconsistent)
	   end

	 | PARAtPat(i, fpat)	=> trFpat_rhs (E,E') fpat
	 | LONGVIDAtPat(i,_,_)	=> error(i, E.FvalBindArityZero)
	 | fatpat		=> error(infoAtPat fatpat, E.FvalBindPatInvalid)

    and trAltFpats_rhs (E,E') = List.map(trAltFpat_rhs (E,E'))

    and trAltFpat_rhs (E,E') fpat =
	let
	    val _    = insertScope E'
	    val pat'aritytyps' = trFpat_rhs (E,E') fpat
	    val E''  = splitScope E'
	    val _    = if Env.sizeScope E' = Env.sizeScope E'' then () else
			  error(infoPat fpat, E.AltPatInconsistent)
	    val _    = Env.appiVals
			    (fn(vid,_) =>
				if Option.isSome(lookupVal(E'',vid)) then ()
				else error(infoPat fpat, E.AltPatInconsistent)
			    ) E'
	in
	    pat'aritytyps'
	end


    and trAppliedFpat_rhs (E,E') =
	fn fpat as (ATPATPat _ | APPPat _) =>
		trAppliedFappPat_rhs (E,E') (Infix.pat (infEnv E) fpat)
	 | fpat => error(infoPat fpat, E.FvalBindPatInvalid)

    and trAppliedFappPat_rhs (E,E') =
	fn ATPATPat(i, fatpat)	  => trAppliedFatPat_rhs (E,E') fatpat
	 | APPPat(i, fpat, atpat) => trAppliedFappPat_rhs (E,E') fpat
				     @ [trAtPat (E,E') atpat]
	 | fpat => error(infoPat fpat, E.FvalBindPatInvalid)

    and trAppliedFatPat_rhs (E,E') =
	fn LONGVIDAtPat _	=> []
	 | PARAtPat(i, fpat)	=> trAppliedFpat_rhs (E,E') fpat
	 | fatpat => error(infoAtPat fatpat, E.FvalBindPatInvalid)



  (* Type and constructor bindings *)

    and trTypBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(NEWTypBind(_, tyvarseq, tycon as TyCon(i',tycon'), typbindo)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, i')
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', O.AbsTyp(i'))
		val dec'        = O.DatDec(i, id', funtyp')
		val _           = insertDisjointTy(E', tycon',
						  (i', stamp, Env.new()))
				  handle CollisionTy _ =>
				      error(i', E.TypBindDuplicate tycon')
	   in
		trTypBindo' (E,E', dec'::acc) typbindo
	   end

	 | SOME(EQUALTypBind(_, tyvarseq, tycon as TyCon(i',tycon'), ty,
								typbindo)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, infoTy ty)
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val typ'        = trTy E ty
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', typ')
		val dec'        = O.TypDec(i', id', funtyp')
		val _           = insertDisjointTy(E', tycon',
						   (i', stamp, Env.new()))
				  handle CollisionTy _ =>
				      error(i', E.TypBindDuplicate tycon')
	   in
		trTypBindo' (E,E', dec'::acc) typbindo
	   end


    and trDatBindo_lhs (E,E') =
	fn NONE => ()

	 | ( SOME(CLOSEDDatBind(i, tyvarseq, tycon, _, datbindo))
	   | SOME(OPENDatBind(i, tyvarseq, tycon, datbindo)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (id',stamp)      = trTyCon_bind E tycon
		val  _               = insertDisjointTy(E', tycon',
							(i', stamp, Env.new()))
				       handle CollisionTy _ =>
					   error(i', E.DatBindDuplicate tycon')
	   in
		trDatBindo_lhs (E,E') datbindo
	   end

    and trDatBindo_rhs (E,E') datbindo =
	    List.rev(trDatBindo_rhs' (E,E',[]) datbindo)

    and trDatBindo_rhs'(E,E',acc) =
	fn NONE => acc

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
		val dec'      = O.DatDec(i, id', funtyp')
		val  _        = unionDisjoint(E',E'')
				handle CollisionVal vid' =>
				    errorVId(E'', vid', E.DatBindConDuplicate)
	   in
		trDatBindo_rhs' (E,E', dec'::acc) datbindo
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
		val dec'      = O.DatDec(i, id', funtyp')
	   in
		trDatBindo_rhs' (E,E', dec'::acc) datbindo
	   end


    and trConBindo (E,E') conbindo = List.rev(trConBindo' (E,E',[]) conbindo)
    and trConBindo'(E,E',acc) =
	fn NONE => acc

	 | SOME(ConBind(i, _, vid as VId(i',vid'), tyo, conbindo)) =>
	   let
		val (id',stamp) = trVId_bind E vid
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i, id', typs')
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				      error(i', E.ConBindDuplicate vid')
	   in
		trConBindo' (E,E', con'::acc) conbindo
	   end

    and trDconBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(NEWDconBind(_, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dconbindo)) =>
	   let
		val  i          = Source.over(i', infoLong longtycon)
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val (ids',typ') = trTyVarSeqLongTyCon E (tyvarseq, longtycon)
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i', id', typs')
		val  dec'       = O.ConDec(i, con', typ')
		val  _          = deleteScope E
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				      error(i', E.DconBindDuplicate vid')
	   in
		trDconBindo' (E,E', vardec(ids', dec')::acc) dconbindo
	   end

	 | SOME(EQUALDconBind(_, _, vid as VId(i',vid'), _,
							longvid, dconbindo)) =>
	   let
		val  i           = Source.over(i', infoLong longvid)
		val (id',stamp)  = trVId_bind E vid
		val (longid',is) = trLongVId E longvid
		val  _           = if is <> V then () else
				      error(i, E.DconBindNonCon)
		val  con'        = O.Con(i', id', [])
		val  typ'        = O.SingTyp(O.infoLongid longid', longid')
		val  dec'        = O.ConDec(i, con', typ')
		val  _           = insertDisjointVal(E', vid', (i', stamp, is))
				   handle CollisionVal _ =>
				       error(i', E.DconBindDuplicate vid')
	   in
		trDconBindo' (E,E', dec'::acc) dconbindo
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
	    ( ids', apptyp(typs', typ') )
	end


  (* Structure and signature bindings *)

    and trStrBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(StrBind(_, strid as StrId(i',strid'), strexp, strbindo)) =>
	   let
		val i           = Source.over(i', infoStrExp strexp)
		val (id',stamp) = trStrId_bind E strid
		val (mod',E'')  = trStrExp E strexp
		val  dec'       = O.ModDec(i, id', mod')
		val  _          = insertDisjointStr(E', strid', (i',stamp,E''))
				  handle CollisionStr _ =>
				      error(i', E.StrBindDuplicate strid')
	   in
		trStrBindo' (E,E', dec'::acc) strbindo
	   end


    and trSigBindo' (E,E',acc) =
	fn NONE => acc

	 | SOME(SigBind(_, sigid as SigId(i',sigid'), strpats, sigexp,
								sigbindo)) =>
	   let
		val  i          = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trSigId_bind E sigid
		val  _          = insertScope E
		val  idinfs'    = trStrPats E strpats
		val (inf',E'')  = trSigExp E sigexp
		val  _          = deleteScope E
		val  dec'       = O.InfDec(i, id', funinf(idinfs',inf'))
		val  _          = insertDisjointSig(E', sigid', (i',stamp,E''))
				  handle CollisionSig _ =>
				      error(i', E.SigBindDuplicate sigid')
	   in
		trSigBindo' (E,E', dec'::acc) sigbindo
	   end


  (* Structure expressions *)

    and trAtStrExp E =
	fn STRUCTAtStrExp(i, dec) =>
	   let
		val _     = insertScope E
		val decs' = trDec E dec
		val E'    = splitScope E
	   in
		( O.StrMod(i, decs'), E' )
	   end

	 | LONGSTRIDAtStrExp(i, longstrid) =>
	   let
		val (longid',E') = trLongStrId E longstrid
	   in
		( longidToMod longid', E' )
	   end

	 | LETAtStrExp(i, dec, strexp) =>
	   let
		val  _        = insertScope E
		val  decs'    = trDec E dec
		val (mod',E') = trStrExp E strexp
		val  _        = deleteScope E
	   in
		( O.LetMod(i, decs', mod'), E' )
	   end

	 | PARAtStrExp(i, strexp) => trStrExp E strexp

    and trStrExp E =
	fn ATSTREXPStrExp(i, atstrexp) => trAtStrExp E atstrexp

	 | APPStrExp(i, strexp, atstrexp) =>
	   let
		val (mod1',E1') = trStrExp E strexp
		val (mod2',E2') = trAtStrExp E atstrexp
	   in
		( O.AppMod(i, mod1', mod2'), E1' )
	   end

	 | TRANSStrExp(i, strexp, sigexp) =>
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
		( O.UpMod(i, mod', inf'), E'' )
	   end

	 | FCTStrExp(i, strpat, strexp) =>
	   let
		val  _         = insertScope E
		val (id',inf') = trStrPat E strpat
		val (mod',E')  = trStrExp E strexp
		val  _         = deleteScope E
	   in
		( O.FunMod(i, id', inf', mod'), E' )
	   end

	 | UNPACKStrExp(i, exp, sigexp) =>
	   let
		val  exp'     = trExp E exp
		val (inf',E') = trSigExp E sigexp
	   in
		( O.UnpackMod(i, exp', inf'), E' )
	   end


    and trStrPat E (StrPat(i, strid as StrId(i', strid'), sigexp)) =
	let
	    val (id',stamp) = trStrId_bind E strid
	    val (inf',E')   = trSigExp E sigexp
	    val  _          = insertStr(E, strid', (i', stamp, E'))
	in
	    (id', inf')
	end

    and trStrPats E = List.map (trStrPat E)



  (* Signatures and specifications *)

    and trAtSigExp E =
	fn ANYAtSigExp(i) =>
	   let
		val E' = Env.new()
	   in
		( O.TopInf(i), E' )
	   end

	 | SIGAtSigExp(i, spec) =>
	   let
		val _      = insertScope E
		val specs' = trSpec E spec
		val E'     = splitScope E
	   in
		( O.SigInf(i, specs'), E' )
	   end

	 | LONGSIGIDAtSigExp(i, sigid) =>
	   let
		val (longid',E') = trLongSigId E sigid
	   in
		( O.ConInf(i, longid'), E' )
	   end

	 | LETAtSigExp(i, dec, sigexp) =>
	   let
		val  _        = insertScope E
		val  decs'    = trDec E dec
		val (inf',E') = trSigExp E sigexp
		val  _        = deleteScope E
	   in
		(*UNFINISHED*)
		(* Mmh, is there really no better way than having LetInf? *)
		raise Crash.Crash "unimplemented: let in signatures \
				  \(may be induced by functor derived form)"
	   end

	 | PARAtSigExp(i, sigexp) => trSigExp E sigexp


    and trSigExp E =
	fn ATSIGEXPSigExp(i, atsigexp) => trAtSigExp E atsigexp

	 | APPSigExp(i, sigexp, atstrexp) =>
	   let
		val (inf',E') = trSigExp E sigexp
		val (mod',_)  = trAtStrExp E atstrexp
	   in
		( O.AppInf(i, inf', mod'), E' )
	   end

	 | FCTSigExp(i, strpat, sigexp) =>
	   let
		val  _          = insertScope E
		val (id',inf1') = trStrPat E strpat
		val (inf2',E')  = trSigExp E sigexp
		val  _          = deleteScope E
	   in
		( O.ArrInf(i, id', inf1', inf2'), E' )
	   end

	 | WHERESigExp(i, sigexp1, sigexp2) =>
	   let
		val (inf1',E1) = trSigExp E sigexp1
		val (inf2',E2) = trSigExp E sigexp2
		val  _         = unionCompose(E1, E2)
	   in
		( O.CompInf(i, inf1', inf2'), E1 )
	   end


    and trSpec  E spec = List.rev(trSpec' (E,[]) spec)
    and trSpec'(E,acc) =
	fn VALSpec(i, valdesc) =>
		trValDesco' (E,acc) (SOME valdesc)

	 | TYPESpec(i, typdesc) =>
		trTypDesco' (E,acc) (SOME typdesc)

	 | EQTYPESpec(i, typdesc) =>
		(* UNFINISHED *)
		trTypDesco' (E,acc) (SOME typdesc)

	 | EQEQTYPESpec(i, typdesc) =>
		(* UNFINISHED *)
		trTypDesco' (E,acc) (SOME typdesc)

	 | DATATYPESpec(i, datdesc) =>
	   let
		val  _     = trDatDesco_lhs E (SOME datdesc)
		val specs' = trDatDesco_rhs E (SOME datdesc)
	   in
		O.RecSpec(i, specs') :: acc
	   end

	 | REPLICATIONSpec(i, tycon as TyCon(i', tycon'), longtycon) =>
	   let
		val (id',stamp)  = trTyCon_bind E tycon
		val (longid',E') = trLongTyCon E longtycon
		val  longido'    = case longid'
				     of O.LongId(_,longid',_) => SOME longid'
				      | O.ShortId _           => NONE
		val  _           = insertDisjointTy(E, tycon', (i', stamp, E'))
				   handle CollisionTy _ =>
				       error(i', E.SpecTyConDuplicate tycon')
	   in
		foldiVals (trOpenSpecVal (E,i,longido'))
		 (O.TypSpec(i, id', O.ConTyp(infoLong longtycon, longid'))::acc)
		 E'
	   end

	 | CONSTRUCTORSpec(i, dcondesc) =>
		trDconDesco' (E,acc) (SOME dcondesc)

	 | STRUCTURESpec(i, strdesc) =>
		trStrDesco' (E,acc) (SOME strdesc)

	 | SIGNATURESpec(i, sigdesc) =>
		trSigDesco' (E,acc) (SOME sigdesc)

	 | INCLUDESpec(i, sigexp) =>
	   let
		val (inf',E') = trSigExp E sigexp
		val _ =
		    unionDisjoint(E,E')
		    handle CollisionInf x => error(i, E.SpecFixDuplicate x)
			 | CollisionVal x => error(i, E.SpecVIdDuplicate x)
			 | CollisionTy  x => error(i, E.SpecTyConDuplicate x)
			 | CollisionStr x => error(i, E.SpecStrIdDuplicate x)
			 | CollisionSig x => error(i, E.SpecSigIdDuplicate x)
	   in
		O.ExtSpec(i, inf') :: acc
	   end

	 | EMPTYSpec(i) =>
		acc

	 | SEQSpec(i, spec1, spec2) =>
		trSpec' (E, trSpec' (E,acc) spec1) spec2

	 | SHARINGTYPESpec(i, spec, longtycons) =>
	   let
		val specs'   = trSpec E spec
		val longids' = List.map (#1 o trLongTyCon E) longtycons
		val rspecs'  = List.rev(Sharing.shareTyp(specs', longids'))
	   in
		rspecs' @ acc
	   end

	 | SHARINGSIGNATURESpec(i, spec, longsigids) =>
	   let
		val specs'   = trSpec E spec
		val longids' = List.map (#1 o trLongSigId E) longsigids
		val rspecs'  = List.rev(Sharing.shareSig(specs', longids'))
	   in
		rspecs' @ acc
	   end

	 | SHARINGSpec(i, spec, longstrids) =>
	   let
		val specs'   = trSpec E spec
		val longids' = List.map (#1 o trLongStrId E) longstrids
		val rspecs'  = List.rev(Sharing.shareStr(specs', longids'))
	   in
		rspecs' @ acc
	   end

	 | PREBOUNDSpec(i, strid as StrId(i',strid')) =>
	   let
		val  _           = trStrId_bind E strid
		val (_,stamp,E') = prebound E
		val  _           = insertStr(E, strid', (i',stamp,E'))
	   in
		acc
	   end

	 | OVERLOADSpec(i, _, vid, tyvar, ty) =>
	   (*UNFINISHED*)
		acc

	 | INSTANCESpec(i, _, vid, longtycon, longvid) =>
	   (*UNFINISHED*)
		acc

	 | INSTANCESCONSpec(i, scon, longtycon) =>
	   (*UNFINISHED*)
		acc

	 | INFIXSpec(i, n, vid as VId(i',vid')) =>
	   let
		val id'   = trVId_bind' E vid
		val fix'  = Fixity.INFIX(n, Fixity.LEFT)
		val spec' = O.FixSpec(i, id', fix')
		val _     = insertDisjointInf(E, vid', (i', SOME(LEFT, n)))
			    handle CollisionInf vid' =>
				   error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end

	 | INFIXRSpec(i, n, vid as VId(i',vid')) =>
	   let
		val id'   = trVId_bind' E vid
		val fix'  = Fixity.INFIX(n, Fixity.RIGHT)
		val spec' = O.FixSpec(i, id', fix')
		val _     = insertDisjointInf(E, vid', (i', SOME(RIGHT, n)))
			    handle CollisionInf vid' =>
				   error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end

	 | NONFIXSpec(i, vid as VId(i',vid')) =>
	   let
		val id'   = trVId_bind' E vid
		val fix'  = Fixity.NONFIX
		val spec' = O.FixSpec(i, id', fix')
		val _     = insertDisjointInf(E, vid', (i', NONE))
			    handle CollisionInf vid' =>
				   error(i', E.SpecFixDuplicate vid')
	   in
		spec' :: acc
	   end


    and trOpenSpecVal (E,i,longido') (vid', (_,stamp1,is), acc) =
	let
	    val name    = VId.toString vid'
	    val stamp2  = Stamp.new()
	    val id1'    = O.Id(i, stamp1, Name.ExId name)
	    val id2'    = O.Id(i, stamp2, Name.ExId name)
	    val longid' = case longido'
			    of SOME longid' => O.LongId(i,longid',O.Lab(i,
							Label.fromString name))
			     | NONE         => O.ShortId(i, id1')
	    val typ'    = O.SingTyp(i, longid')
	    val _       = insertDisjointVal(E, vid', (i,stamp2,is))
			  handle CollisionVal _ =>
			      error(i, E.SpecVIdDuplicate vid')
	in
	    (case is
	       of V => O.ValSpec(i, id2', typ')
	        | _ => O.ConSpec(i, O.Con(i, id2', []), typ')
	    ) :: acc
	end




  (* Descriptions *)

    and trValDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWValDesc(_, _, vid as VId(i',vid'), ty, valdesco)) =>
	   let
		val  i          = Source.over(i', infoTy ty)
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val  ids'       = trAllTy E ty
		val  typ'       = alltyp(ids', trTy E ty)
		val  _          = deleteScope E
		val  spec'      = O.ValSpec(i, id', typ')
		val  _          = insertDisjointVal(E, vid', (i', stamp, V))
				  handle CollisionVal vid' =>
				      error(i', E.SpecVIdDuplicate vid')
	   in
		trValDesco' (E, spec'::acc) valdesco
	   end

	 | SOME(EQUALValDesc(i, _, vid as VId(i',vid'), _, longvid, valdesco))=>
	   let
		val (id',stamp)  = trVId_bind E vid
		val (longid',is) = trLongVId E longvid
		val  typ'        = O.SingTyp(O.infoLongid longid', longid')
		val  spec'       = O.ValSpec(i, id', typ')
		val  _           = insertDisjointVal(E, vid', (i', stamp, V))
				   handle CollisionVal vid' =>
				      error(i', E.SpecVIdDuplicate vid')
	   in
		trValDesco' (E, spec'::acc) valdesco
	   end


    and trTypDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWTypDesc(_, tyvarseq, tycon as TyCon(i',tycon'), typdesco)) =>
	   let
		val i           = Source.over(infoSeq tyvarseq, i')
		val (id',stamp) = trTyCon_bind E tycon
		val _           = insertScope E
		val ids'        = trTyVarSeq E tyvarseq
		val _           = deleteScope E
		val funtyp'     = funtyp(ids', O.AbsTyp(i'))
		val spec'       = O.DatSpec(i, id', funtyp')
		val _           = insertDisjointTy(E, tycon',
						   (i', stamp, Env.new()))
				  handle CollisionTy _ =>
				      error(i', E.SpecTyConDuplicate tycon')
	   in
		trTypDesco' (E, spec'::acc) typdesco
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
		val spec'       = O.TypSpec(i, id', funtyp')
		val _           = insertDisjointTy(E, tycon',
						   (i', stamp, Env.new()))
				  handle CollisionTy _ =>
				      error(i', E.SpecTyConDuplicate tycon')
	   in
		trTypDesco' (E, spec'::acc) typdesco
	   end


    and trDatDesco_lhs E =
	fn NONE => ()

	 | ( SOME(CLOSEDDatDesc(i, tyvarseq, tycon, _, datdesco))
	   | SOME(OPENDatDesc(i, tyvarseq, tycon, datdesco)) ) =>
	   let
		val TyCon(i',tycon') = tycon
		val (id',stamp)      = trTyCon_bind E tycon
		val _                = insertDisjointTy(E, tycon',
						        (i', stamp, Env.new()))
				       handle CollisionTy _ =>
					 error(i', E.SpecTyConDuplicate tycon')
	   in
		trDatDesco_lhs E datdesco
	   end

    and trDatDesco_rhs E datdesco = List.rev(trDatDesco_rhs' (E,[]) datdesco)
    and trDatDesco_rhs' (E,acc) =
	fn NONE => acc

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
		val spec'    = O.DatSpec(i, id', funtyp')
		val _        = unionDisjoint(E,E') handle CollisionVal vid' =>
				   errorVId(E', vid', E.SpecVIdDuplicate)
	   in
		trDatDesco_rhs' (E, spec'::acc) datdesco
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
		val spec'    = O.DatSpec(i, id', funtyp')
	   in
		trDatDesco_rhs' (E, spec'::acc) datdesco
	   end


    and trConDesco (E,E') condesco = List.rev(trConDesco' (E,E',[]) condesco)
    and trConDesco'(E,E',acc) =
	fn NONE => acc

	 | SOME(ConDesc(i, _, vid as VId(i',vid'), tyo, condesco)) =>
	   let
		val (id',stamp) = trVId_bind E vid
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i, id', typs')
		val  k          = List.length typs'
		val  _          = insertDisjointVal(E', vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				      error(i', E.ConDescDuplicate vid')
	   in
		trConDesco' (E,E', con'::acc) condesco
	   end


    and trDconDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWDconDesc(_, _, vid as VId(i',vid'), tyo, tyvarseq, longtycon,
								 dcondesco)) =>
	   let
		val  i          = Source.over(i', infoLong longtycon)
		val (id',stamp) = trVId_bind E vid
		val  _          = insertScope E
		val (ids',typ') = trTyVarSeqLongTyCon E (tyvarseq, longtycon)
		val  typs'      = trTyo E tyo
		val  con'       = O.Con(i', id', typs')
		val  _          = deleteScope E
		val  k          = List.length typs'
		val  spec'      = O.ConSpec(i', con', typ')
		val  _          = insertDisjointVal(E, vid', (i', stamp, C k))
				  handle CollisionVal _ =>
				      error(i', E.SpecVIdDuplicate vid')
	   in
		trDconDesco' (E, varspec(ids', spec')::acc) dcondesco
	   end

	 | SOME(EQUALDconDesc(_, _, vid as VId(i',vid'), _, longvid,
								dcondesco)) =>
	   let
		val  i           = Source.over(i', infoLong longvid)
		val (id',stamp)  = trVId_bind E vid
		val (longid',is) = trLongVId E longvid
		val  _           = if is <> V then () else
				   error(i, E.DconDescNonCon)
		val  con'        = O.Con(i', id', [])
		val  typ'        = O.SingTyp(O.infoLongid longid', longid')
		val  spec'       = O.ConSpec(i', con', typ')
		val  _           = insertDisjointVal(E, vid', (i', stamp, is))
				   handle CollisionVal _ =>
				       error(i', E.SpecVIdDuplicate vid')
	   in
		trDconDesco' (E, spec'::acc) dcondesco
	   end



    and trStrDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWStrDesc(_, strid as StrId(i',strid'), sigexp, strdesco)) =>
	   let
		val  i          = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trStrId_bind E strid
		val (inf',E')   = trSigExp E sigexp
		val  spec'      = O.ModSpec(i, id', inf')
		val  _          = insertDisjointStr(E, strid', (i', stamp, E'))
				  handle CollisionStr strid' =>
				      error(i', E.SpecStrIdDuplicate strid')
	   in
		trStrDesco' (E, spec'::acc) strdesco
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
		val  inf'        = O.SingInf(O.infoMod mod'', mod'')
		val  spec'       = O.ModSpec(i, id', inf')
		val  _           = insertDisjointStr(E, strid', (i', stamp, E''))
				   handle CollisionStr strid' =>
				       error(i', E.SpecStrIdDuplicate strid')
	   in
		trStrDesco' (E, spec'::acc) strdesco
	   end



    and trSigDesco' (E,acc) =
	fn NONE => acc

	 | SOME(NEWSigDesc(_, sigid as SigId(i',sigid'), strpats, sigdesco)) =>
	   let
		val (id',stamp) = trSigId_bind E sigid
		val  _          = insertScope E
		val  idinfs'    = trStrPats E strpats
		val  inf'       = funinf(idinfs', O.AbsInf(i'))
		val  _          = deleteScope E
		val  spec'      = O.InfSpec(i', id', inf')
		val  _          = insertDisjointSig(E, sigid',
						    (i', stamp, Env.new()))
				  handle CollisionSig _ =>
				      error(i', E.SpecSigIdDuplicate sigid')
	   in
		trSigDesco' (E, spec'::acc) sigdesco
	   end

	 | SOME(EQUALSigDesc(_, sigid as SigId(i',sigid'), strpats, sigexp,
								sigdesco)) =>
	   let
		val  i          = Source.over(i', infoSigExp sigexp)
		val (id',stamp) = trSigId_bind E sigid
		val  _          = insertScope E
		val  idinfs'    = trStrPats E strpats
		val (inf',E')   = trSigExp E sigexp
		val  inf''      = funinf(idinfs', inf')
		val  _          = deleteScope E
		val  spec'      = O.InfSpec(i', id', inf'')
		val  _          = insertDisjointSig(E, sigid', (i', stamp, E'))
				  handle CollisionSig _ =>
				      error(i', E.SpecSigIdDuplicate sigid')
	   in
		trSigDesco' (E, spec'::acc) sigdesco
	   end



  (* Programs and components *)

    fun trProgramo  E programo = List.rev(trProgramo' (E,[]) programo)
    and trProgramo'(E,acc) =
	fn NONE => acc

	 | SOME(Program(i, dec, programo)) =>
	   let
		val acc' = trDec' (E,acc) dec
	   in
		trProgramo' (E,acc') programo
	   end


    fun trComponent E (Component(i, imp, programo)) =
	let
	    val imps' = trImport E imp
	    val decs' = trProgramo E programo
	in
	    O.Comp(i, imps', decs')
	end


    and trImport  E imp  = List.rev(trImport' (E,[]) imp)
    and trImport'(E,acc) =
	fn IMPORTImport(i, spec, s) =>
	   let
		val specs' = trSpec E spec
		val url    = Url.fromString s
	   in
		O.Imp(i, specs', url) :: acc
	   end

	 | EMPTYImport(i) =>
		acc

	 | SEQImport(i, imp1, imp2) =>
		trImport' (E, trImport' (E,acc) imp1) imp2


    val translate = trComponent

  end
