(* Notes:
 * - Paths are shared, i.e. each path has to be unique.
 * - Since SML allows multiple definitions of the same id in a structure,
 *   labels are not enough for paths. So we added an index.
 *)

structure PathPrivate =
  struct

  (* Types *)

    type lab   = Lab.t
    type name  = Name.t

    datatype path' =
	  PLAIN  of name
	| DOT    of path * lab * int
	(*UNFINISHED
	| IMPORT of string * lab * int
	*)

    withtype path = path' ref
    type t = path


  (* Creation and projection *)

    fun invent()		= ref(PLAIN(Name.InId))
    fun fromLab l		= ref(PLAIN(Lab.toName l))
    fun path pln		= ref(DOT pln)

    fun toLab(ref(PLAIN n))	= Lab.fromName n
      | toLab _			= raise Crash.crash "Path.toLab"

    fun isDot(ref(DOT _))	= true
      | isDot _			= false

    fun asDot(ref(DOT pln))	= pln
      | asDot _			= raise Crash.crash "Path.asDot"


  (* Ordering and hashing *)

    fun compare(p1 as ref p1', p2 as ref p2')	= if p1 = p2 then EQUAL
						  else compare'(p1', p2')
    and compare'(PLAIN _,       DOT _)		= LESS
      | compare'(DOT _,         PLAIN _)	= GREATER
      | compare'(PLAIN(x1),     PLAIN(x2))	= Name.compare(x1,x2)
      | compare'(DOT(p1,l1,n1), DOT(p2,l2,n2))	= (case compare(p1,p2)
						     of r as (LESS|GREATER) => r
						      | EQUAL =>
						   case Lab.compare(l1,l2)
						     of r as (LESS|GREATER) => r
						      | EQUAL =>
						   Int.compare(n1,n2))

    fun hash(ref(PLAIN x))	= Name.hash x
      | hash(ref(DOT(p,l,n)))	= Lab.hash l


  (* Strengthening *)

    (* Strengthening has to be used carefully, as it results in a new
     * hash value, thereby invalidating eventual hash maps and sets! *)

    fun strengthen(p1, (p as ref(PLAIN _),l,n)) = p := DOT(p1,l,n)
      | strengthen _                            = ()


  (* Cloning *)

    fun instance lookup (rea, p) =
	let
	    fun clone p1 =
		case lookup(rea, p1)
		 of SOME p2 => p2
		  | NONE    =>
		case !p1
		 of p' as PLAIN _ => ref p'
		  | DOT(_,l,_)    => fromLab l
	in
	    clone p
	end
  end


structure Path : PATH = PathPrivate
