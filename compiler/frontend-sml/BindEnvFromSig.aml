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


    fun idStatusFromTyp t =
	if Type.isAll t then idStatusFromTyp(#2(Type.asAll t)) else
	let
	    val (t', t2)  = Type.asApply t
	    val (t'',t1)  = Type.asApply t'
	    val (t11,t12) = Type.asArrow t1
	in
(*ASSERT    assert Type.isCon t'' andalso
	       Path.equals(#3(Type.asCon t''), PervasiveType.path_conarrow) =>*)
	    if not(Type.isCon t'' andalso
	       Path.equals(#3(Type.asCon t''), PervasiveType.path_conarrow))
	    then raise Assert.failure else
	    idStatusFromTyp'(t12, t2)
	end
	handle Type.Type =>
	    raise Crash.Crash "BindEnvFromSig: strange constructor field"

    and idStatusFromTyp'(t1,t2) =
	if      Type.isApply' t1 then idStatusFromTyp'(#1(Type.asApply' t1), t2)
	else if Type.isSum' t1 then
	    T(arityFromNatTyp t2)
	else if Type.isCon' t1
	andalso Path.equals(#3(Type.asCon' t1), PervasiveType.path_ref) then
	    R
	else if Type.isCon' t1 andalso #2(Type.asCon' t1) = Type.OPEN then
	    C(arityFromNatTyp t2)
	else
	    raise Type.Type

    and arityFromNatTyp t =
	if Type.isApply t then
	    1 + arityFromNatTyp(#1(Type.asApply t))
	else 0


    fun envFromTyp(I,s,t) =
	if      Type.isSum t    then envFromRow(I, s, Type.asSum t)
	else if Type.isLambda t then envFromTyp(I, s, #2(Type.asLambda t))
	else if Type.isMu t     then envFromTyp(I, s, Type.asMu t)
	else if Type.isCon t
	andalso Path.equals(#3(Type.asCon t), PervasiveType.path_ref) then
	    let
		(*UNFINISHED: constructor currently gets the same name
		 * as the tycon, because we have no way of knowing its
		 * real name. HACK, ref is still broken... *)
		val E   = new()
		val x   = Stamp.new()
		val n   = Label.toName(Path.toLab(#3(Type.asCon t)))
		val vid = VId.fromString(Name.toString n)
	    in
		insertVal(E, vid, (I,x,R)) ;
		E
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
		    val  name  = Label.toString a
		    val  vid   = VId.fromString name
		    val  a'    = Label.fromString("'" ^ name)
		    val  is    = idStatusFromTyp(Inf.lookupVal(s,a'))
		    (* UNFINISHED: check that the constructor actually
		     * constructs the type in question.
		     * What if the constructors are actually hidden
		     * by following members???
		     *)
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
		val (l,t,d) = Inf.asValItem item
		val  name   = Label.toString l
		val  vid    = VId.fromString name
		val  x      = Stamp.new()
	    in
		if String.sub(name,0) = #"'" then
		    insertVal(E, vid, (I,x,V))
		else
		let
		    (* UNFINISHED: again we have a hiding problem here:
		     * What if the item is not actually connected to the
		     * constructor field of the same name, because it hides it?
		     *)
		    val l' = Label.fromString("'" ^ name)
		    val is = idStatusFromTyp(Inf.lookupVal(s,l'))
			     handle Inf.Lookup => V
		in
		    insertVal(E, vid, (I,x,is))
		end
	    end
	else if Inf.isTypItem item then
	    let
		val (l,k,d) = Inf.asTypItem item
		val  tycon  = TyCon.fromString(Label.toString l)
		val  x      = Stamp.new()
		val  E'     = case d
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
