(* This will vanish after bootstrapping. The compiler does then start
 * with an empty(!) environment, builtin entities have to be obtained via
 * the __prebound construct.
 *)

structure BindEnv0 :> BIND_ENV0 =
  struct

    open BindEnv

    structure P  = Prebound
    structure P' = Prebound'

    val E0 = new()

    val i = Source.nowhere

    (* Infix environment *)

    val _ = insertInf(E0, VId.fromString "::", (i, SOME(BindEnv.RIGHT, 5)))
    val _ = insertInf(E0, VId.fromString "=",  (i, SOME(BindEnv.LEFT,  4)))
    val _ = insertInf(E0, VId.fromString ":=", (i, SOME(BindEnv.LEFT,  3)))

    (* Type environment *)

    val E_empty = new()
    val E_bool  = new()
    val E_list  = new()
    val E_ref   = new()

    val _ = insertVal(E_bool, VId.fromString "false", (i,P.stamp_false,C 0))
    val _ = insertVal(E_bool, VId.fromString "true",  (i,P.stamp_true, C 0))
    val _ = insertVal(E_list, VId.fromString "nil",   (i,P.stamp_nil,  C 0))
    val _ = insertVal(E_list, VId.fromString "::",    (i,P.stamp_cons, C 1))
    val _ = insertVal(E_ref,  VId.fromString "ref",   (i,P.stamp_ref,  C 1))

    val _ = insertTy(E0, TyCon.fromString "unit",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "bool",   (i, Stamp.new(), E_bool))
    val _ = insertTy(E0, TyCon.fromString "int",    (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "word",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "real",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "char",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "string", (i, Stamp.new(), E_empty))
    val _ = insertTy(E0, TyCon.fromString "list",   (i, Stamp.new(), E_list))
    val _ = insertTy(E0, TyCon.fromString "ref",    (i, Stamp.new(), E_ref))
    val _ = insertTy(E0, TyCon.fromString "exn",    (i, Stamp.new(), E_empty))

    (* Value environment *)

    val _ = union(E0, E_bool)
    val _ = union(E0, E_list)
    val _ = union(E0, E_ref)
    val _ = insertVal(E0, VId.fromString "Match", (i, P.stamp_Match, C 0))
    val _ = insertVal(E0, VId.fromString "Bind",  (i, P.stamp_Bind,  C 0))
    val _ = insertVal(E0, VId.fromString "=",     (i, P.stamp_eq,    V))
    val _ = insertVal(E0, VId.fromString ":=",    (i, P.stamp_assign,V))

    val _ = insertVal(E0, VId.fromString "builtin", (i, P'.stamp_builtin, V))

  end
