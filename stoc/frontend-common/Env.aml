structure Env :> ENV =
  struct

    type valid = AbstractGrammar.valid
    type typid = AbstractGrammar.typid
    type modid = AbstractGrammar.modid
    type infid = AbstractGrammar.infid
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type var   = Type.var
    type inf   = Inf.t


    (* The map implementing the environment *)

    structure Map = MakeHashScopedImpMap(FromEqHashKey(Stamp))

    datatype env = ENV of ran Map.t
    and      ran = VAL of val_entry
		 | TYP of typ_entry
		 | VAR of var_entry
		 | MOD of mod_entry
		 | INF of inf_entry

    withtype val_entry = { id: valid, path: path, typ: typ }
    and      typ_entry = { id: typid, path: path, typ: typ }
    and      var_entry = { id: typid, var: var }
    and      mod_entry = { id: modid, path: path, inf: inf }
    and      inf_entry = { id: infid, path: path, inf: inf }

    type t = env


    (* Conversions *)

    fun asVal(VAL x) = x | asVal _ = raise Crash.Crash "Env.asVal: inconsistent"
    fun asTyp(TYP x) = x | asTyp _ = raise Crash.Crash "Env.asTyp: inconsistent"
    fun asVar(VAR x) = x | asVar _ = raise Crash.Crash "Env.asVar: inconsistent"
    fun asMod(MOD x) = x | asMod _ = raise Crash.Crash "Env.asMod: inconsistent"
    fun asInf(INF x) = x | asInf _ = raise Crash.Crash "Env.asInf: inconsistent"

    fun appVal f (x, VAL y) = f(x,y) | appVal f _ = ()
    fun appTyp f (x, TYP y) = f(x,y) | appTyp f _ = ()
    fun appVar f (x, VAR y) = f(x,y) | appVar f _ = ()
    fun appMod f (x, MOD y) = f(x,y) | appMod f _ = ()
    fun appInf f (x, INF y) = f(x,y) | appInf f _ = ()

    fun foldVal f (x, VAL y, a) = f(x,y,a) | foldVal f (_,_,a) = a
    fun foldTyp f (x, TYP y, a) = f(x,y,a) | foldTyp f (_,_,a) = a
    fun foldVar f (x, VAR y, a) = f(x,y,a) | foldVar f (_,_,a) = a
    fun foldMod f (x, MOD y, a) = f(x,y,a) | foldMod f (_,_,a) = a
    fun foldInf f (x, INF y, a) = f(x,y,a) | foldInf f (_,_,a) = a


    (* Operation wrappers *)

    exception Collision = Map.Collision
    exception Lookup    = Map.Lookup

    fun new()				= ENV(Map.new())
    fun clone(ENV E)			= ENV(Map.clone E)
    fun cloneScope(ENV E)		= ENV(Map.cloneScope E)
    fun splitScope(ENV E)		= ENV(Map.splitScope E)
    fun insertScope(ENV E)		= Map.insertScope E
    fun deleteScope(ENV E)		= Map.deleteScope E
    fun mergeScope(ENV E)		= Map.mergeScope E

    fun union(ENV E1, ENV E2)		= Map.unionDisjoint(E1,E2)

    fun insertVal(ENV E, x, y)		= Map.insertDisjoint(E, x, VAL y)
(*UNFINISHED: quick hack*)
    fun insertTyp(ENV E, x, y)		= Map.insert(E, x, TYP y)
    fun insertVar(ENV E, x, y)		= Map.insert(E, x, VAR y)
    fun insertMod(ENV E, x, y)		= Map.insertDisjoint(E, x, MOD y)
    fun insertInf(ENV E, x, y)		= Map.insertDisjoint(E, x, INF y)

    fun lookupVal(ENV E, x)		= asVal(Map.lookupExistent(E,x))
    fun lookupTyp(ENV E, x)		= asTyp(Map.lookupExistent(E,x))
    fun lookupVar(ENV E, x)		= asVar(Map.lookupExistent(E,x))
    fun lookupMod(ENV E, x)		= asMod(Map.lookupExistent(E,x))
    fun lookupInf(ENV E, x)		= asInf(Map.lookupExistent(E,x))

    fun appVals f (ENV E)		= Map.appi (appVal f) E
    fun appTyps f (ENV E)		= Map.appi (appTyp f) E
    fun appVars f (ENV E)		= Map.appi (appVar f) E
    fun appMods f (ENV E)		= Map.appi (appMod f) E
    fun appInfs f (ENV E)		= Map.appi (appInf f) E

    fun foldVals f a (ENV E)		= Map.foldi (foldVal f) a E
    fun foldTyps f a (ENV E)		= Map.foldi (foldTyp f) a E
    fun foldVars f a (ENV E)		= Map.foldi (foldVar f) a E
    fun foldMods f a (ENV E)		= Map.foldi (foldMod f) a E
    fun foldInfs f a (ENV E)		= Map.foldi (foldInf f) a E

  end
