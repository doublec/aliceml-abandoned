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

    val _ = insertTy(E, TyCon.fromString "bool",   (i, Stamp.new(), E_bool))
    val _ = insertTy(E, TyCon.fromString "int",    (i, Stamp.new(), E_empty))
    val _ = insertTy(E, TyCon.fromString "word",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E, TyCon.fromString "real",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E, TyCon.fromString "char",   (i, Stamp.new(), E_empty))
    val _ = insertTy(E, TyCon.fromString "string", (i, Stamp.new(), E_empty))
    val _ = insertTy(E, TyCon.fromString "list",   (i, Stamp.new(), E_list))
    val _ = insertTy(E, TyCon.fromString "ref",    (i, Stamp.new(), E_ref))
    val _ = insertTy(E, TyCon.fromString "exn",    (i, Stamp.new(), E_empty))

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
