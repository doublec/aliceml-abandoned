structure BindEnv0 :> BIND_ENV0 =
  struct

    open BindEnv
    open Prebound

    structure P = Prebound

    val i = Source.nowhere

    (* Prebound *)

    val E = new()

    (* Type environment *)

    val E_empty = new()
    val E_bool  = new()
    val E_list  = new()
    val E_ref   = new()

    val _ = insertVal(E_bool, VId.fromString "false", (i,valstamp_false,C 0))
    val _ = insertVal(E_bool, VId.fromString "true",  (i,valstamp_true, C 0))
    val _ = insertVal(E_list, VId.fromString "nil",   (i,valstamp_nil,  C 0))
    val _ = insertVal(E_list, VId.fromString "::",    (i,valstamp_cons, C 1))
    val _ = insertVal(E_ref,  VId.fromString "ref",   (i,valstamp_ref,  R))

    val _ = insertTy(E, TyCon.fromString "bool",   (i, typstamp_bool,  E_bool))
    val _ = insertTy(E, TyCon.fromString "int",    (i, typstamp_int,   E_empty))
    val _ = insertTy(E, TyCon.fromString "word",   (i, typstamp_word,  E_empty))
    val _ = insertTy(E, TyCon.fromString "real",   (i, typstamp_real,  E_empty))
    val _ = insertTy(E, TyCon.fromString "char",   (i, typstamp_char,  E_empty))
    val _ = insertTy(E, TyCon.fromString "string", (i, typstamp_string,E_empty))
    val _ = insertTy(E, TyCon.fromString "vector", (i, typstamp_vec,   E_empty))
    val _ = insertTy(E, TyCon.fromString "list",   (i, typstamp_list,  E_list))
    val _ = insertTy(E, TyCon.fromString "ref",    (i, typstamp_ref,   E_ref))
    val _ = insertTy(E, TyCon.fromString "exn",    (i, typstamp_exn,   E_empty))

    (* Value environment *)

    val _ = union(E, E_bool)
    val _ = union(E, E_list)
    val _ = union(E, E_ref)

    val _ = insertVal(E, VId.fromString "Match", (i, valstamp_match, C 0))
    val _ = insertVal(E, VId.fromString "Bind",  (i, valstamp_bind,  C 0))

    (* Global *)

    val E0 = new()

    val _  = insertStr(E0, StrId.fromString "", (i, Stamp.new(), E))

  end
