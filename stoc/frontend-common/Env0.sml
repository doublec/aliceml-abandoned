structure Env0 :> ENV0 =
  struct

    open Env
    open Prebound
    open PreboundType

    datatype id   = datatype AbstractGrammar.id
    datatype sort = datatype Type.sort


  (* Prebound *)

    val E0 = new()


  (* Type environment *)

    fun insertTyp'(stamp, path, typ, sort, name) =
	let
	    val entry = { id   = Id(Source.nowhere, stamp, name)
			, path = path
			, typ  = typ
			, sort = sort
			}
	in
	    insertTyp(E0, stamp, entry)
	end

    val _ = insertTyp'(typstamp_int,    typpath_int,    typ_int,    CLOSED,
		       typname_int)
    val _ = insertTyp'(typstamp_word,   typpath_word,   typ_word,   CLOSED,
		       typname_word)
    val _ = insertTyp'(typstamp_char,   typpath_char,   typ_char,   CLOSED,
		       typname_char)
    val _ = insertTyp'(typstamp_string, typpath_string, typ_string, CLOSED,
		       typname_string)
    val _ = insertTyp'(typstamp_real,   typpath_real,   typ_real,   CLOSED,
		       typname_real)
    val _ = insertTyp'(typstamp_bool,   typpath_bool,   typ_bool,   CLOSED,
		       typname_bool)
    val _ = insertTyp'(typstamp_exn,    typpath_exn,    typ_exn,    OPEN,
		       typname_exn)
    val _ = insertTyp'(typstamp_ref,    typpath_ref,    typ_ref,    CLOSED,
		       typname_ref)
    val _ = insertTyp'(typstamp_vec,    typpath_vec,    typ_vec,    CLOSED,
		       typname_vec)
    val _ = insertTyp'(typstamp_list,   typpath_list,   typ_list,   CLOSED,
		       typname_list)


  (* Value environment *)

    fun poly typF =
	let
	    val alpha = Type.var Type.STAR
	in
	    Type.inAll(alpha, typF(Type.inVar alpha))
	end

    val typ_false = typ_bool
    val typ_true  = typ_bool
    val typ_nil   = poly (fn a => Type.inApply(typ_list, a))
    val typ_cons  = poly (fn a => let val listA = Type.inApply(typ_list, a) in
				      Type.inArrow(Type.inTuple[a,listA], listA)
				  end)
    val typ_ref   = poly (fn a => Type.inArrow(a, Type.inApply(typ_ref, a)))
    val typ_match = typ_exn
    val typ_bind  = typ_exn

    fun insertCon'(stamp, path, typ, name) =
	let
	    val entry = { id   = Id(Source.nowhere, stamp, name)
			, path = path
			, typ  = typ
			, sort = Inf.CONSTRUCTOR
			}
	in
	    insertVal(E0, stamp, entry)
	end

    val _ = insertCon'(valstamp_false, valpath_false, typ_false, valname_false)
    val _ = insertCon'(valstamp_true,  valpath_true,  typ_true,  valname_true)
    val _ = insertCon'(valstamp_nil,   valpath_nil,   typ_nil,   valname_nil)
    val _ = insertCon'(valstamp_cons,  valpath_cons,  typ_cons,  valname_cons)
    val _ = insertCon'(valstamp_ref,   valpath_ref,   typ_ref,   valname_ref)
    val _ = insertCon'(valstamp_match, valpath_match, typ_match, valname_match)
    val _ = insertCon'(valstamp_bind,  valpath_bind,  typ_bind,  valname_bind)

  end
