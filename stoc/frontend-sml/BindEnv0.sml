structure BindEnv0 :> BIND_ENV0 =
  struct

    open BindEnv

    structure P = Prebound

    val i = Source.nowhere

    (* Prebound *)

    val E = new()

    (* Type environment *)

    val E_empty = new()
    val E_bool  = new()
    val E_list  = new()
    val E_ref   = new()

    val _ = insertVal(E_bool, VId.fromString "false", (i,P.stamp_false,C 0))
    val _ = insertVal(E_bool, VId.fromString "true",  (i,P.stamp_true, C 0))
    val _ = insertVal(E_list, VId.fromString "nil",   (i,P.stamp_nil,  C 0))
    val _ = insertVal(E_list, VId.fromString "::",    (i,P.stamp_cons, C 1))
    val _ = insertVal(E_ref,  VId.fromString "ref",   (i,P.stamp_ref,  R))

    val _ = insertTy(E, TyCon.fromString "bool",   (i, P.stamp_bool,   E_bool))
    val _ = insertTy(E, TyCon.fromString "int",    (i, P.stamp_int,    E_empty))
    val _ = insertTy(E, TyCon.fromString "word",   (i, P.stamp_word,   E_empty))
    val _ = insertTy(E, TyCon.fromString "real",   (i, P.stamp_real,   E_empty))
    val _ = insertTy(E, TyCon.fromString "char",   (i, P.stamp_char,   E_empty))
    val _ = insertTy(E, TyCon.fromString "string", (i, P.stamp_string, E_empty))
    val _ = insertTy(E, TyCon.fromString "list",   (i, P.stamp_list,   E_list))
    val _ = insertTy(E, TyCon.fromString "ref",    (i, P.stamp_tref,   E_ref))
    val _ = insertTy(E, TyCon.fromString "exn",    (i, P.stamp_exn,    E_empty))

    (* Value environment *)

    val _ = union(E, E_bool)
    val _ = union(E, E_list)
    val _ = union(E, E_ref)

    val _ = insertVal(E, VId.fromString "Match", (i, P.stamp_Match, C 0))
    val _ = insertVal(E, VId.fromString "Bind",  (i, P.stamp_Bind,  C 0))

    (* Global *)

    val E0 = new()

    val _  = insertStr(E0, StrId.fromString "", (i, Stamp.new(), E))

  end
