signature ENV =
  sig

    type valid = AbstractGrammar.valid
    type typid = AbstractGrammar.typid
    type modid = AbstractGrammar.modid
    type infid = AbstractGrammar.infid
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type var   = Type.var
    type inf   = Inf.t

    type env
    type t = env

    type val_entry = { id: valid, path: path, typ: typ }
    type typ_entry = { id: typid, path: path, typ: typ }
    type var_entry = { id: typid, var: var }
    type mod_entry = { id: modid, path: path, inf: inf }
    type inf_entry = { id: infid, path: path, inf: inf }

    exception Collision of stamp
    exception Lookup    of stamp

    val new :		unit -> env
    val clone :		env -> env
    val cloneScope :	env -> env
    val splitScope :	env -> env
    val insertScope :	env -> unit
    val deleteScope :	env -> unit
    val mergeScope :	env -> unit

    val union :		env * env -> unit		(* Collision *)

    val insertVal :	env * stamp * val_entry -> unit	(* Collision *)
    val insertTyp :	env * stamp * typ_entry -> unit	(* Collision *)
    val insertVar :	env * stamp * var_entry -> unit	(* Collision *)
    val insertMod :	env * stamp * mod_entry -> unit	(* Collision *)
    val insertInf :	env * stamp * inf_entry -> unit	(* Collision *)

    val lookupVal :	env * stamp -> val_entry	(* Lookup *)
    val lookupTyp :	env * stamp -> typ_entry	(* Lookup *)
    val lookupVar :	env * stamp -> var_entry	(* Lookup *)
    val lookupMod :	env * stamp -> mod_entry	(* Lookup *)
    val lookupInf :	env * stamp -> inf_entry	(* Lookup *)

    val appVals :	(stamp * val_entry -> unit) -> env -> unit
    val appTyps :	(stamp * typ_entry -> unit) -> env -> unit
    val appVars :	(stamp * var_entry -> unit) -> env -> unit
    val appMods :	(stamp * mod_entry -> unit) -> env -> unit
    val appInfs :	(stamp * inf_entry -> unit) -> env -> unit

    val foldVals :	(stamp * val_entry * 'a -> 'a) -> 'a -> env -> 'a
    val foldTyps :	(stamp * typ_entry * 'a -> 'a) -> 'a -> env -> 'a
    val foldVars :	(stamp * var_entry * 'a -> 'a) -> 'a -> env -> 'a
    val foldMods :	(stamp * mod_entry * 'a -> 'a) -> 'a -> env -> 'a
    val foldInfs :	(stamp * inf_entry * 'a -> 'a) -> 'a -> env -> 'a

  end
