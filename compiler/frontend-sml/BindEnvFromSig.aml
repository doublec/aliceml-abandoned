structure BindEnvFromSig :> BIND_ENV_FROM_SIG =
  struct

    open BindEnv


    (*
     * Fixity modes not available in SML are converted as follows:
     * - non-associative infix -> left-associative infix
     * - prefix  -> nonfix
     * - postfix -> nonfix
     *)

    fun assocFromAssoc(Fixity.LEFT)	= Infix.LEFT
      | assocFromAssoc(Fixity.RIGHT)	= Infix.RIGHT
      | assocFromAssoc(Fixity.NEITHER)	= Infix.LEFT

    fun infFromFix(Fixity.NONFIX)	= NONE
      | infFromFix(Fixity.PREFIX n)	= NONE
      | infFromFix(Fixity.POSTFIX n)	= NONE
      | infFromFix(Fixity.INFIX(n,a))	= SOME(assocFromAssoc a, n)


    fun idStatusFromSort(Inf.VALUE,         t) = V
      | idStatusFromSort(Inf.CONSTRUCTOR k, t) = idStatusFromArity(k,t)

    and idStatusFromArity(k, t) =
	if Type.isAll t then
	    idStatusFromArity(k, #2(Type.asAll t))
	else if Type.isExist t then
	    idStatusFromArity(k, #2(Type.asExist t))
	else if Type.isArrow t then
	    idStatusFromArity(k, #2(Type.asArrow t))
	else if Type.isLambda t then
	    idStatusFromArity(k, #2(Type.asLambda t))
	else if Type.isApply t then
	    idStatusFromArity(k, #1(Type.asApply t))
	else if Type.isCon t
	andalso Path.equals(#3(Type.asCon t), PreboundType.path_ref) then
	    R
	else
	    C k


    fun envFromTyp(I,s,t) =
	if Type.isSum t then
	    envFromRow(I, s, Type.asSum t)
	else if Type.isLambda t then
	    envFromTyp(I, s, #2(Type.asLambda t))
	else if Type.isMu t then
	    envFromTyp(I, s, Type.asMu t)
	else if Type.isCon t
	andalso Path.equals(#3(Type.asCon t), PreboundType.path_ref) then
	    let
		val E   = new()
		val x   = Stamp.new()
		val vid = VId.fromString(Name.toString(Prebound.valname_ref))
	    in
		insertVal(E, vid, (I,x,R)) ; E
	    end
	else
	    new()

    and envFromRow(I,s,r) =
	let
	    val E = new()

	    fun loop r =
		if Type.isEmptyRow r then () else
		let
		    val (a,ts) = Type.headRow r
		    val  x     = Stamp.new()
		    val  vid   = VId.fromString(Label.toString a)
		    val  is    = case Inf.lookupValSort(s,a)
				   of Inf.VALUE         => raise Inf.Lookup
				    | Inf.CONSTRUCTOR k => C k
					(* UNFINISHED: check that the
					 * constructor actually constructs
					 * the type in question. *)
					(* UNFINISHED: what if the
					 * constructors are actually hidden
					 * by following members??? *)
		in
		    insertVal(E, vid, (I,x,is)) ;
		    loop(Type.tailRow r)
		end
	in
	    ( loop r ; E )
	    handle Inf.Lookup => new()
	end


    fun envFromInf(I,j) =
	if Inf.isTop j orelse Inf.isCon j then
	    new()
	else if Inf.isSig j then
	    envFromSig(I, Inf.asSig j)
	else if Inf.isArrow j then
	    envFromInf(I, #3(Inf.asArrow j))
	else if Inf.isLambda j then
	    envFromInf(I, #3(Inf.asLambda j))
	else if Inf.isApply j then
	    envFromInf(I, #1(Inf.asApply j))
	else
	    raise Crash.Crash "BindEnvFromSig.envFromInf: unknown interface"

    and envFromSig(I,s) =
	let
	    val E = new()
	in
	    List.app (insertItem(E,I,s)) (Inf.items s);
	    E
	end

    and insertItem (E,I,s) item =
	if Inf.isFixItem item then
	    let
		val (l,f) = Inf.asFixItem item
		val  vid  = VId.fromString(Label.toString l)
		val  is   = infFromFix f
	    in
		insertInf(E, vid, (I,is))
	    end
	else if Inf.isValItem item then
	    let
		val (l,t,w,_) = Inf.asValItem item
		val  vid      = VId.fromString(Label.toString l)
		val  x        = Stamp.new()
		val  is       = idStatusFromSort(w,t)
	    in
		insertVal(E, vid, (I,x,is))
	    end
	else if Inf.isTypItem item then
	    let
		val (l,k,w,d) = Inf.asTypItem item
		val  tycon    = TyCon.fromString(Label.toString l)
		val  x        = Stamp.new()
		val  E'       = case d
				  of NONE   => new()
				   | SOME t => envFromTyp(I, s, t)
	    in
		insertTy(E, tycon, (I,x,E'))
	    end
	else if Inf.isModItem item then
	    let
		val (l,j,_) = Inf.asModItem item
		val  strid  = StrId.fromString(Label.toString l)
		val  x      = Stamp.new()
		val  E'     = envFromInf(I, j)
	    in
		insertStr(E, strid, (I,x,E'))
	    end
	else if Inf.isInfItem item then
	    let
		val (l,k,jo) = Inf.asInfItem item
		val  sigid   = SigId.fromString(Label.toString l)
		val  x       = Stamp.new()
		val  E'      = envFromInf(I, Option.getOpt(jo, Inf.inTop()))
	    in
		insertSig(E, sigid, (I,x,E'))
	    end
	else
	     raise Crash.Crash "BindEnvFromSig.insertItem: unknown item"

  end
