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
	| LINK   of path

    withtype path = path' ref
    type t = path


  (* Creation and projection *)

    fun invent()	= ref(PLAIN(Name.InId))
    fun fromLab l	= ref(PLAIN(Lab.toName l))
    fun path pln	= ref(DOT pln)

    fun toLab(ref p')	= toLab' p'
    and toLab'(PLAIN n)	= Lab.fromName n
      | toLab'(LINK p)	= toLab p
      | toLab' _	= raise Crash.crash "Path.toLab"


  (* Ordering and hashing *)

    fun compare(ref(LINK p1), p2)		= compare(p1, p2)
      | compare(p1, ref(LINK p2))		= compare(p1, p2)
      | compare(p1 as ref p1', p2 as ref p2')	= if p1 = p2 then EQUAL
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
      | compare' _ = Crash.crash "Path.compare"

    fun hash(ref p')		= hash' p'
    and hash'(PLAIN x)		= Name.hash x
      | hash'(DOT(p,l,n))	= Lab.hash l
      | hash'(LINK p)		= hash p


  (* Substitution and Realisation *)

    fun substitute(p1, p, l, n) = p1 := DOT(p,l,n)

    fun realise lookup (rea, p) =
	case lookup(rea, p)
	  of NONE    => realise' lookup (rea, !p)
	   | SOME p2 => p := LINK p2

    and realise' lookup (rea, PLAIN _)    = ()
      | realise' lookup (rea, DOT(p,l,n)) = realise lookup (rea, p)
      | realise' lookup (rea, LINK p)     = realise lookup (rea, p)


  (* Cloning *)

    fun clone (isBinder, lookup) (rea, p) =
	let
	    exception External

	    fun clone' p1 =
		case lookup(rea, p1)
		 of SOME p2 => p2
		  | NONE    =>
		case !p1
		 of LINK p2       => clone' p2
		  | p' as PLAIN _ => if isBinder then ref p' else raise External
		  | DOT(p2,l,n)   => ref(DOT(clone' p2, l, n))
	in
	    clone' p handle External => p
	end

    fun cloneBinder lookup = clone(true,  lookup)
    fun cloneFree   lookup = clone(false, lookup)

  end


structure Path : PATH = PathPrivate
