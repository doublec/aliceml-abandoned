functor MakeHashImpMap(Key: HASH_KEY) :> IMP_MAP where type key = Key.t =
  struct

    open Misc

    type key    = Key.t
    type 'a map = (int ref * (key * 'a) option array) ref
    type 'a t   = 'a map

    exception Delete    of key
    exception Collision of key

    val initialSize		= 19


    fun new()			= ref(ref 0, Array.array(initialSize,NONE))

    fun copy(ref(n,t))		= let val t' = Array.array(Array.length t,NONE)
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      ref(ref(!n), t')
				  end


    fun size(ref(n,_))		= !n
    fun isEmpty(ref(n,_))	= !n = 0


    fun find(t,k)		= find'(t, Key.hash k mod Array.length t, k)
    and find'(t,i,k)		= case Array.sub(t,i)
				    of NONE       => (false,i)
				     | SOME(k',_) =>
				       if k = k' then (true,i)
				       else find'(t,(i+1) mod Array.length t, k)

    fun lookup(ref(_,t),k)	= Option.map #2 (Array.sub(t, #2(find(t,k))))


    fun deleteWith f (ref(n,t), k)
				= case find(t,k)
				    of (false,_) => f k
				     | (true, i) => ( Array.update(t,i,NONE)
						    ; n := !n-1
						    )

    fun delete x		= deleteWith ignore x
    fun deleteExistent x	= deleteWith (fn k => raise Delete k) x


    fun reinsert t (ka as(k,a))	= Array.update(t, #2(find(t,k)), SOME ka)
    fun resize(m as ref(n,t))	= if 3 * !n < 2 * Array.length t then () else
				  let
				      val t' = Array.array(2*Array.length t-1,
							   NONE)
				  in
				      Array.app (Option_app(reinsert t')) t ;
				      m := (n,t')
				  end


    fun valOf(t,i)		= #2(Option.valOf(Array.sub(t,i)) : key * 'a)
    fun insertWithi f (m,k,a)	= let val   _   = resize m
				      val (n,t) = !m
				      val (b,i) = find(t,k)
				      val   a'  = if not b then a
						  else f(k, valOf(t,i), a)
				  in
				      Array.update(t,i,SOME(k,a')) ;
				      n := !n+1
				  end

    fun insertWith f		= insertWithi(fn(k,a1,a2) => f(a1,a2))
    fun insert x		= insertWithi #3 x
    fun insertDisjoint x	= insertWithi(fn(k,_,_) => raise Collision k) x

    fun appi f (ref(_,t))	= Array.app (Option_app f) t
    fun foldi f b (ref(_,t))	= Array.foldl (fn(NONE,b)      => b
						|(SOME(k,a),b) => f(k,a,b)) b t
    fun app f			= appi (fn(k,a) => f a)
    fun fold f			= foldi (fn(k,a,b) => f(a,b))

    fun union' insert (m1,m2)	= appi (fn(k,a) => insert(m1,k,a)) m2
    fun union x			= union' insert x
    fun unionDisjoint x		= union' insertDisjoint x
    fun unionWith f		= union'(insertWith f)
    fun unionWithi f		= union'(insertWithi f)

  end
