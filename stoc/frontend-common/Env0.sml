structure Env0 :> ENV0 =
  struct

    open Env

    structure P   = Prebound
    datatype id   = datatype AbstractGrammar.id
    datatype kind = datatype Type.kind
    datatype sort = datatype Type.sort


  (* Prebound *)

    val E0 = new()


  (* Type environment *)

    val s_int		= "int"
    val s_word		= "word"
    val s_char		= "char"
    val s_string	= "string"
    val s_real		= "real"
    val s_bool		= "bool"
    val s_exn		= "exn"
    val s_ref		= "ref"
    val s_vec		= "vector"
    val s_list		= "list"

    val path_int	= Path.fromLab(Lab.fromString s_int)
    val path_word	= Path.fromLab(Lab.fromString s_word)
    val path_char	= Path.fromLab(Lab.fromString s_char)
    val path_string	= Path.fromLab(Lab.fromString s_string)
    val path_real	= Path.fromLab(Lab.fromString s_real)
    val path_bool	= Path.fromLab(Lab.fromString s_bool)
    val path_exn	= Path.fromLab(Lab.fromString s_exn)
    val path_ref	= Path.fromLab(Lab.fromString s_ref)
    val path_vec	= Path.fromLab(Lab.fromString s_vec)
    val path_list	= Path.fromLab(Lab.fromString s_list)

    val con_word	= (STAR, CLOSED, path_word)
    val con_int		= (STAR, CLOSED, path_int)
    val con_char	= (STAR, CLOSED, path_char)
    val con_string	= (STAR, CLOSED, path_string)
    val con_real	= (STAR, CLOSED, path_real)
    val con_bool	= (STAR, CLOSED, path_bool)
    val con_exn		= (STAR, CLOSED, path_exn)
    val con_ref		= (ARROW(STAR,STAR), CLOSED, path_ref)
    val con_vec		= (ARROW(STAR,STAR), CLOSED, path_vec)
    val con_list	= (ARROW(STAR,STAR), CLOSED, path_list)

    val typ_int		= Type.inCon con_int	(* Always maximise sharing! *)
    val typ_word	= Type.inCon con_word
    val typ_char	= Type.inCon con_char
    val typ_string	= Type.inCon con_string
    val typ_real	= Type.inCon con_real
    val typ_bool	= Type.inCon con_bool
    val typ_exn		= Type.inCon con_exn
    val typ_ref		= Type.inCon con_ref
    val typ_vec		= Type.inCon con_vec
    val typ_list	= Type.inCon con_list

    fun insertTyp'(stamp, path, typ, sort, s) =
	let
	    val entry = { id   = Id(Source.nowhere, stamp, Name.ExId s)
			, path = path
			, typ  = typ
			, sort = sort
			}
	in
	    insertTyp(E0, stamp, entry)
	end

    val _ = insertTyp'(P.stamp_int,    path_int,   typ_int,    CLOSED, s_int)
    val _ = insertTyp'(P.stamp_word,   path_word,  typ_word,   CLOSED, s_word)
    val _ = insertTyp'(P.stamp_char,   path_char,  typ_char,   CLOSED, s_char)
    val _ = insertTyp'(P.stamp_string, path_string,typ_string, CLOSED, s_string)
    val _ = insertTyp'(P.stamp_real,   path_real,  typ_real,   CLOSED, s_real)
    val _ = insertTyp'(P.stamp_bool,   path_bool,  typ_bool,   CLOSED, s_bool)
    val _ = insertTyp'(P.stamp_exn,    path_exn,   typ_exn,    OPEN,   s_exn)
    val _ = insertTyp'(P.stamp_tref,   path_ref,   typ_ref,    CLOSED, s_ref)
    val _ = insertTyp'(P.stamp_vec,    path_vec,   typ_vec,    CLOSED, s_vec)
    val _ = insertTyp'(P.stamp_list,   path_list,  typ_list,   CLOSED, s_list)


  (* Value environment *)

    val s_false		= "false"
    val s_true		= "true"
    val s_nil		= "nil"
    val s_cons		= "cons"
    val s_ref		= "ref"
    val s_match		= "Match"
    val s_bind		= "Bind"

    val path_false	= Path.fromLab(Lab.fromString s_false)
    val path_true	= Path.fromLab(Lab.fromString s_true)
    val path_nil	= Path.fromLab(Lab.fromString s_nil)
    val path_cons	= Path.fromLab(Lab.fromString s_cons)
    val path_ref	= Path.fromLab(Lab.fromString s_ref)
    val path_match	= Path.fromLab(Lab.fromString s_match)
    val path_bind	= Path.fromLab(Lab.fromString s_bind)

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

    fun insertCon'(stamp, path, typ, s) =
	let
	    val entry = { id   = Id(Source.nowhere, stamp, Name.ExId s)
			, path = path
			, typ  = typ
			, sort = Inf.CONSTRUCTOR
			}
	in
	    insertVal(E0, stamp, entry)
	end

    val _ = insertCon'(P.stamp_false, path_false, typ_false, s_false)
    val _ = insertCon'(P.stamp_true,  path_true,  typ_true,  s_true)
    val _ = insertCon'(P.stamp_nil,   path_nil,   typ_nil,   s_nil)
    val _ = insertCon'(P.stamp_cons,  path_cons,  typ_cons,  s_cons)
    val _ = insertCon'(P.stamp_ref,   path_ref,   typ_ref,   s_ref)
    val _ = insertCon'(P.stamp_Match, path_match, typ_Match, s_match)
    val _ = insertCon'(P.stamp_Bind,  path_bind,  typ_Bind,  s_bind)

  end
