functor MakeHashImpSet(Item: HASH_KEY) :> IMP_SET where type item = Item.t =
  struct

    type item = Item.t

    datatype entry = EMPTY | ENTRY of item | DELETED

    type set = (int ref * entry array) ref
    type t   = set

    exception Delete    of item
    exception Collision of item


    fun appEntry f (ENTRY k)	= f k
      | appEntry f    _		= ()

    fun foldEntry f a (ENTRY k)	= f(k,a)
      | foldEntry f a    _	= a


    val initialSize		= 19

    fun new()			= ref(ref 0, Array.array(initialSize,EMPTY))

    fun size(ref(n,_))		= !n
    fun isEmpty(ref(n,_))	= !n = 0


    fun copy(ref(n,t)) =
	let
	    val t' = Array.array(Array.length t, EMPTY)
	in
	    Array.copy{src=t, dst=t', si=0, di=0, len=NONE} ;
	    ref(ref(!n), t')
	end


    fun find(t,k) =
	let
	    fun loop(i,jo) =
		let val i' = i mod Array.length t in
		    case Array.sub(t,i')
		      of EMPTY    => (false, Option.getOpt(jo,i'))
		       | DELETED  => loop(i'+1, SOME(Option.getOpt(jo,i')))
		       | ENTRY k' => if k = k' then (true,i')
					       else loop(i'+1, jo)
		end
	in
	    loop(Item.hash k, NONE)
	end


    fun member(ref(_,t),k) = #1(find(t,k))


    fun deleteWith f (ref(n,t), k) =
	case find(t,k)
	  of (false,_) => f k
	   | (true, i) => ( Array.update(t,i,DELETED) ; n := !n-1 )

    val delete		= deleteWith ignore
    val deleteExistent	= deleteWith (fn k => raise Delete k)


    fun reinsert t k	= Array.update(t, #2(find(t,k)), ENTRY k)

    fun resize(s as ref(n,t)) =
	if 3 * !n < 2 * Array.length t then () else
	let
	    val t' = Array.array(2*Array.length t-1, EMPTY)
	in
	    Array.app (appEntry(reinsert t')) t ;
	    s := (n,t')
	end


    fun insertWith f (s,k) =
	let
	    val   _   = resize s
	    val (n,t) = !s
	in
	    case find(t,k)
	      of (true, _) => f k
	       | (false,i) => ( Array.update(t,i,ENTRY k) ; n := !n+1 )
	end

    val insert			= insertWith ignore
    val insertDisjoint		= insertWith(fn k => raise Collision k)

    fun app f (ref(_,t))	= Array.app (appEntry f) t
    fun fold f a (ref(_,t))	= Array.foldl (fn(ko,a) => foldEntry f a ko) a t

    fun union(s1,s2)		= app (fn k => insert(s1,k)) s2
    fun unionDisjoint(s1,s2)	= app (fn k => insertDisjoint(s1,k)) s2
    fun unionWith f (s1,s2)	= app (fn k => insertWith f (s1,k)) s2

  end
