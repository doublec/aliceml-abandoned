functor MakeHashImpSet(Item: HASH_KEY) :> IMP_SET where type item = Item.t =
  struct

    type item = Item.t

    datatype entry = EMPTY | ENTRY of item | DELETED

    type set = int ref * entry array ref
    type t   = set

    exception Delete    of item
    exception Collision of item


    fun appEntry f (ENTRY k)	= f k
      | appEntry f    _		= ()

    fun foldEntry f a (ENTRY k)	= f(k,a)
      | foldEntry f a    _	= a


    val initialSize		= 19

    fun new()			= (ref 0, ref(Array.array(initialSize,EMPTY)))
    fun deleteAll (k,s)		= (k := 0 ; s := Array.array(initialSize,EMPTY))

    fun size(ref n, _)		= n
    fun isEmpty(ref n, _)	= n = 0


    fun clone(ref n, ref t) =
	let
	    val t' = Array.array(Array.length t, EMPTY)
	in
	    Array.copy{src=t, dst=t', si=0, di=0, len=NONE} ;
	    (ref n, ref t')
	end


    fun findItem(t,k) =
	let
	    val i0 = Item.hash k mod Array.length t

	    fun loop(i,jo) =
		case Array.sub(t,i)
		  of EMPTY    => (false, Option.getOpt(jo,i))
		   | DELETED  => next(i, SOME(Option.getOpt(jo,i)))
		   | ENTRY k' => if k = k' then (true,i)
					   else next(i,jo)
	    and next(i,jo) =
		let val i' = (i+1) mod Array.length t in
		    if i' = i0 then (false, Option.valOf jo)
			       else loop(i',jo)
		end
	in
	    loop(i0,NONE)
	end


    fun member((_, ref t),k) = #1(findItem(t,k))


    fun deleteWith f ((n, ref t), k) =
	case findItem(t,k)
	  of (false,_) => f k
	   | (true, i) => ( Array.update(t,i,DELETED) ; n := !n-1 )

    val delete		= deleteWith ignore
    val deleteExistent	= deleteWith (fn k => raise Delete k)


    fun reinsert t k	= Array.update(t, #2(findItem(t,k)), ENTRY k)

    fun resize(ref n, r as ref t) =
	if 3 * n < 2 * Array.length t then () else
	let
	    val t' = Array.array(2*Array.length t-1, EMPTY)
	in
	    Array.app (appEntry(reinsert t')) t ;
	    r := t'
	end


    fun insertWith f (s,k) =
	let
	    val   _        = resize s
	    val (n, ref t) = s
	in
	    case findItem(t,k)
	      of (true, _) => f k
	       | (false,i) => ( Array.update(t,i,ENTRY k) ; n := !n+1 )
	end

    val insert			= insertWith ignore
    val insertDisjoint		= insertWith(fn k => raise Collision k)

    fun app f (_, ref t)	= Array.app (appEntry f) t
    fun fold f a (_, ref t)	= Array.foldl (fn(ko,a) => foldEntry f a ko) a t
    fun find p (_, ref t)	= let val size   = Array.length t
				      fun iter i =
					  if i = size then NONE else
					  case Array.sub(t,i)
					    of ENTRY k => if p k then SOME k
					                         else iter(i+1)
					     | _       => iter(i+1)
				  in iter 0 end

    fun union(s1,s2)		= app (fn k => insert(s1,k)) s2
    fun unionDisjoint(s1,s2)	= app (fn k => insertDisjoint(s1,k)) s2
    fun unionWith f (s1,s2)	= app (fn k => insertWith f (s1,k)) s2

  end
