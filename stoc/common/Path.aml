(* Notes:
 * - Paths are shared, i.e. each path has to be unique.
 * - Since SML allows multiple definitions of the same id in a structure,
 *   labels are not enough for paths. So we added an index.
 *)

structure PathPrivate =
  struct

  (* Types *)

    type lab	= Label.t
    type name	= Name.t
    type stamp	= GlobalStamp.t

    datatype path' =
	  PLAIN of name
	| DOT   of path * lab * int

    withtype path = stamp * path' ref
    type t = path


  (* Creation and projection *)

    fun invent()		= (GlobalStamp.new(), ref(PLAIN(Name.InId)))
    fun fromLab l		= (GlobalStamp.new(), ref(PLAIN(Label.toName l)))
    fun path pln		= (GlobalStamp.new(), ref(DOT pln))
    fun pervasive s		= (GlobalStamp.fromString s,
				   ref(PLAIN(Name.ExId s)))

    fun toLab (_, ref(PLAIN n))	= Label.fromName n
      | toLab (_, ref(DOT pln))	= #2 pln

    fun isDot (_, ref(DOT _))	= true
      | isDot   _		= false

    fun asDot (_, ref(DOT pln))	= pln
      | asDot   _		= raise Crash.Crash "Path.asDot"


  (* Comparison and hashing *)

    fun equals((stamp1,_), (stamp2,_))	= stamp1 = stamp2
    fun compare((stamp1,_), (stamp2,_))	= GlobalStamp.compare(stamp1,stamp2)
    fun hash (stamp, _)			= GlobalStamp.hash stamp


  (* Strengthening *)

    fun strengthen(p,l,n, (_, p' as ref(PLAIN _))) = p' := DOT(p,l,n)
      | strengthen _                               = ()


  (* Cloning *)

    fun instance lookup (rea, p) =
	let
	    fun clone p1 =
		case lookup(rea, p1)
		 of SOME p2 => p2
		  | NONE    =>
		case !(#2 p1)
		 of DOT(p,l,n) => (GlobalStamp.new(), ref(DOT(clone p, l, n)))
		  | p'         => (GlobalStamp.new(), ref p')
	in
	    clone p
	end

    fun clone p = instance (fn _ => NONE) ((), p)

  end


structure Path : PATH = PathPrivate
