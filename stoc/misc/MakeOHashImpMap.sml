functor MakeHashImpMap(Key: HASH_KEY) :> IMP_MAP where type key = Key.t =
  struct

    type key = Key.t

    datatype 'a entry = EMPTY | ENTRY of key * 'a | DELETED

    type 'a map = int ref * 'a entry array ref
    type 'a t   = 'a map

    exception Delete    of key
    exception Collision of key


    fun valOfEntry (ENTRY(k,a))		= a
      | valOfEntry    _			= raise Match

    fun appiEntry f (ENTRY(k,a))	= f(k,a)
      | appiEntry f    _		= ()

    fun foldiEntry f b (ENTRY(k,a))	= f(k,a,b)
      | foldiEntry f b    _		= b


    val initialSize		= 19

    fun new()			= (ref 0, ref(Array.array(initialSize,EMPTY)))

    fun size(ref n, _)		= n
    fun isEmpty(ref n, _)	= n = 0

    fun copy(ref n, ref t) =
	let
	    val t' = Array.array(Array.length t, EMPTY)
	in
	    Array.copy{src=t, dst=t', si=0, di=0, len=NONE} ;
	    (ref n, ref t')
	end


    fun find(t,k) =
	let
	    val i0 = Key.hash k mod Array.length t

	    fun loop(i,jo) =
		case Array.sub(t,i)
		  of EMPTY       => (false, Option.getOpt(jo,i))
		   | DELETED     => next(i, SOME(Option.getOpt(jo,i)))
		   | ENTRY(k',_) => if k = k' then (true,i)
					      else next(i,jo)
	    and next(i,jo) =
		let val i' = (i+1) mod Array.length t in
		    if i' = i0 then (false, Option.valOf jo)
			       else loop(i',jo)
		end
	in
	    loop(i0,NONE)
	end

    fun lookup((_, ref t),k) =
	case find(t,k)
	  of (false,_) => NONE
	   | (true, i) => SOME(valOfEntry(Array.sub(t,i)))


    fun deleteWith f ((n, ref t), k) =
	case find(t,k)
	  of (false,_) => f k
	   | (true, i) => ( Array.update(t,i,DELETED) ; n := !n-1 )

    fun delete x		= deleteWith ignore x
    fun deleteExistent x	= deleteWith (fn k => raise Delete k) x


    fun reinsert t (ka as (k,a)) = Array.update(t, #2(find(t,k)), ENTRY ka)

    fun resize(ref n, r as ref t) =
	if 3 * n < 2 * Array.length t then () else
	let
	    val t' = Array.array(2 * Array.length t - 1, EMPTY)
	in
	    Array.app (appiEntry(reinsert t')) t ;
	    r := t'
	end


    fun insertWithi f (m,k,a) =
	let
	    val   _        = resize m
	    val (n, ref t) = m
	    val (b,i)      = find(t,k)
	    val   a'       = if not b then a
				      else f(k, valOfEntry(Array.sub(t,i)), a)
	in
	    Array.update(t, i, ENTRY(k,a')) ; n := !n+1
	end

    fun insertWith f		= insertWithi(fn(k,a1,a2) => f(a1,a2))
    fun insert x		= insertWithi #3 x
    fun insertDisjoint x	= insertWithi(fn(k,_,_) => raise Collision k) x

    fun appi f (_, ref t)	= Array.app (appiEntry f) t
    fun foldi f b (_, ref t)	= Array.foldl (fn(e,b) => foldiEntry f b e) b t
    fun app f			= appi (fn(k,a) => f a)
    fun fold f			= foldi (fn(k,a,b) => f(a,b))

    fun union' insert (m1,m2)	= appi (fn(k,a) => insert(m1,k,a)) m2
    fun union x			= union' insert x
    fun unionDisjoint x		= union' insertDisjoint x
    fun unionWith f		= union'(insertWith f)
    fun unionWithi f		= union'(insertWithi f)

  end
