structure BindEnv0 :> BIND_ENV0 =
  struct

    open BindEnv

    structure P = Prebound

    val E0 = new()

    val i = Source.nowhere

    (* Infix environment *)

    val _ = insertInf(E0, VId.fromString "::", (i, SOME(Infix.RIGHT, 5)))
    val _ = insertInf(E0, VId.fromString "=",  (i, SOME(Infix.LEFT,  4)))
    val _ = insertInf(E0, VId.fromString ":=", (i, SOME(Infix.LEFT,  3)))

    val _ = insertInf(E0, VId.fromString "<",  (i, SOME(Infix.LEFT, 4)))
    val _ = insertInf(E0, VId.fromString "+",  (i, SOME(Infix.LEFT, 6)))
    val _ = insertInf(E0, VId.fromString "*",  (i, SOME(Infix.LEFT, 7)))

    (* Type environment *)

    val E_empty = new()
    val E_bool  = new()
    val E_list  = new()
    val E_ref   = new()

    val _ = insertVal(E_bool, VId.fromString "false", (i,P.stamp_false,C false))
    val _ = insertVal(E_bool, VId.fromString "true",  (i,P.stamp_true, C false))
    val _ = insertVal(E_list, VId.fromString "nil",   (i,P.stamp_nil,  C false))
    val _ = insertVal(E_list, VId.fromString "::",    (i,P.stamp_cons, C true))
    val _ = insertVal(E_ref,  VId.fromString "ref",   (i,P.stamp_ref,  C true))
(*
    val _ = insertTy(E0, TyCon.fromString "unit",   (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "bool",   (i, E_bool))
    val _ = insertTy(E0, TyCon.fromString "int",    (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "word",   (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "real",   (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "char",   (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "string", (i, E_empty))
    val _ = insertTy(E0, TyCon.fromString "list",   (i, E_list))
    val _ = insertTy(E0, TyCon.fromString "ref",    (i, E_ref))
    val _ = insertTy(E0, TyCon.fromString "exn",    (i, E_empty))
*)
    (* Value environment *)

    val _ = union(E0, E_bool)
    val _ = union(E0, E_list)
    val _ = union(E0, E_ref)
    val _ = insertVal(E0, VId.fromString "Match", (i, P.stamp_Match, C false))
    val _ = insertVal(E0, VId.fromString "Bind",  (i, P.stamp_Bind,  C false))
    val _ = insertVal(E0, VId.fromString "=",     (i, P.stamp_eq,    V))
    val _ = insertVal(E0, VId.fromString ":=",    (i, P.stamp_assign,V))

    val stamp_leq   = Stamp.new()
    val stamp_plus  = Stamp.new()
    val stamp_times = Stamp.new()

    val _ = insertVal(E0, VId.fromString "<", (i, stamp_leq,   V))
    val _ = insertVal(E0, VId.fromString "+", (i, stamp_plus,  V))
    val _ = insertVal(E0, VId.fromString "*", (i, stamp_times, V))

  end
