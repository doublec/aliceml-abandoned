structure Env0 :> ENV0 =
  struct

    open Env

    structure P = Prebound

    datatype id = datatype AbstractGrammar.id

    (* Prebound *)

    val E0 = new()

    (* Type environment *)

    val path_word    = Path.fromLab(Lab.fromString "word")
    val path_int     = Path.fromLab(Lab.fromString "int")
    val path_char    = Path.fromLab(Lab.fromString "char")
    val path_string  = Path.fromLab(Lab.fromString "string")
    val path_real    = Path.fromLab(Lab.fromString"real")
    val path_bool    = Path.fromLab(Lab.fromString"bool")
    val path_exn     = Path.fromLab(Lab.fromString"exn")
    val path_ref     = Path.fromLab(Lab.fromString"ref")
    val path_vec     = Path.fromLab(Lab.fromString"vector")
    val path_list    = Path.fromLab(Lab.fromString"list")

    val con_word     = (Type.STAR, Type.CLOSED, path_word)
    val con_int      = (Type.STAR, Type.CLOSED, path_int)
    val con_char     = (Type.STAR, Type.CLOSED, path_char)
    val con_string   = (Type.STAR, Type.CLOSED, path_string)
    val con_real     = (Type.STAR, Type.CLOSED, path_real)
    val con_bool     = (Type.STAR, Type.CLOSED, path_bool)
    val con_exn      = (Type.STAR, Type.CLOSED, path_exn)
    val con_ref      = (Type.ARROW(Type.STAR,Type.STAR), Type.CLOSED, path_ref)
    val con_vec      = (Type.ARROW(Type.STAR,Type.STAR), Type.CLOSED, path_vec)
    val con_list     = (Type.ARROW(Type.STAR,Type.STAR), Type.CLOSED, path_list)

    val typ_word     = Type.inCon con_word   (* Remember to maximise sharing! *)
    val typ_int      = Type.inCon con_int
    val typ_char     = Type.inCon con_char
    val typ_string   = Type.inCon con_string
    val typ_real     = Type.inCon con_real
    val typ_bool     = Type.inCon con_bool
    val typ_exn      = Type.inCon con_exn
    val typ_ref      = Type.inCon con_ref
    val typ_vec      = Type.inCon con_vec
    val typ_list     = Type.inCon con_list

    fun insertTyp'(stamp, typ, s) =
	let
	    val entry = ( Id(Source.nowhere, stamp, Name.ExId s), typ )
	in
	    insertTyp(E0, stamp, entry)
	end

    val _ = insertTyp'(P.stamp_word,   typ_word,   "word")
    val _ = insertTyp'(P.stamp_int,    typ_int,    "int")
    val _ = insertTyp'(P.stamp_char,   typ_char,   "char")
    val _ = insertTyp'(P.stamp_string, typ_string, "string")
    val _ = insertTyp'(P.stamp_real,   typ_real,   "real")
    val _ = insertTyp'(P.stamp_bool,   typ_bool,   "bool")
    val _ = insertTyp'(P.stamp_exn,    typ_exn,    "exn")
    val _ = insertTyp'(P.stamp_tref,   typ_ref,    "ref")
    val _ = insertTyp'(P.stamp_vec,    typ_vec,    "vector")
    val _ = insertTyp'(P.stamp_list,   typ_list,   "list")

    (* Value environment *)

    fun poly typF =
	let
	    val alpha = Type.var Type.STAR
	in
	    Type.inAll(alpha, typF(Type.inVar alpha))
	end

    val typ_false = typ_bool
    val typ_true  = typ_bool
    val typ_nil   = poly (fn a => Type.inApp(typ_list, a))
    val typ_cons  = poly (fn a => let val listA = Type.inApp(typ_list, a) in
				      Type.inArrow(Type.inTuple[a,listA], listA)
				  end)
    val typ_ref   = poly (fn a => Type.inArrow(a, Type.inApp(typ_ref, a)))
    val typ_Match = typ_exn
    val typ_Bind  = typ_exn

    fun insertCon'(stamp, typ, s) =
	let
	    val entry = ( Id(Source.nowhere, stamp, Name.ExId s), typ, true )
	in
	    insertVal(E0, stamp, entry)
	end

    val _ = insertCon'(P.stamp_false, typ_false, "false")
    val _ = insertCon'(P.stamp_true,  typ_true,  "true")
    val _ = insertCon'(P.stamp_nil,   typ_nil,   "nil")
    val _ = insertCon'(P.stamp_cons,  typ_cons,  "::")
    val _ = insertCon'(P.stamp_ref,   typ_ref,   "ref")
    val _ = insertCon'(P.stamp_Match, typ_Match, "Match")
    val _ = insertCon'(P.stamp_Bind,  typ_Bind,  "Bind")

  end
