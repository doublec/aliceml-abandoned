functor MakeHashImpMap(Key: HASH_KEY) :> IMP_MAP where type key = Key.t =
  struct

    type key    = Key.t
    type 'a map = (key * 'a) list array ref * int ref
    type 'a t   = 'a map

    exception Delete    of key
    exception Collision of key
    exception Lookup    of key


    val initialSize		= 19

    fun new()			= (ref(Array.array(initialSize,[])), ref 0)
    fun deleteAll (m,k)		= ( m := Array.array(initialSize,[]) ; k := 0 )

    fun size(_, ref n)		= n
    fun isEmpty(_, ref n)	= n = 0

    fun appi f (ref t, _)	= Array.app (List.app f) t
    fun foldi f b (ref t, _)	= let fun f'((k,a),b) = f(k,a,b) in
				      Array.foldl (fn(kas,b) =>
							List.foldl f' b kas
						  ) b t
				  end
    fun app f			= appi(fn(k,a) => f a)
    fun fold f			= foldi(fn(k,a,b) => f(a,b))


    fun clone(ref t, ref n)	= let val t' = Array.array(Array.length t, [])
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      (ref t', ref n)
				  end


    fun hash(t,k)		= Key.hash k mod Array.length t
    fun isEntryFor k (k',_)	= Key.equals(k,k')

    fun member((ref t,_), k)	= let val kas = Array.sub(t, hash(t,k)) in
				    List.exists (isEntryFor k) kas
				  end

    fun lookup((ref t,_), k)	= let val kas = Array.sub(t, hash(t,k)) in
				    Option.map #2 (List.find (isEntryFor k) kas)
				  end

    fun lookupExistent((ref t,_), k)
    				= let val kas = Array.sub(t, hash(t,k)) in
				    case List.find (isEntryFor k) kas
				      of NONE      => raise Lookup k
				       | SOME(k,a) => a
				  end

    exception Delete'

    fun delete'(  [],    k)	= raise Delete'
      | delete'(ka::kas, k)	= if Key.equals(#1 ka, k)
				  then kas : (key * 'a) list
				  else ka :: delete'(kas,k)

    fun deleteWith f (m,k)	= let val (ref t,n) = m
				      val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				      val kas' = delete'(kas,k) before n := !n-1
						 handle Delete' => (f k ; kas)
				  in
				      Array.update(t, i, kas')
				  end

    fun delete x		= deleteWith ignore x
    fun deleteExistent x	= deleteWith(fn k => raise Delete k) x

    fun reinsert t (ka as(k,_))	= let val i = hash(t,k) in
				      Array.update(t, i, ka::Array.sub(t,i))
				  end

    fun resize(r as ref t,ref n)= if 3 * n < 2 * Array.length t then () else
				  let
				      val t'= Array.array(2*Array.length t-1,[])
				  in
				      Array.app(List.app (reinsert t')) t ;
				      r := t'
				  end

    fun insertWithi f (m,k,a)	= let val _  = resize m
				      val (ref t,n) = m
				      val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				      val kas' =
					case List.find (isEntryFor k) kas
					  of NONE =>
						(k,a)::kas before n := !n+1
					   | SOME(k,a') =>
						(k, f(k,a',a))::delete'(kas,k)
				  in
				      Array.update(t, i, kas')
				  end

    fun insertWith f		= insertWithi(fn(k,a1,a2) => f(a1,a2))
    fun insert x		= insertWithi #3 x
    fun insertDisjoint x	= insertWithi(fn(k,_,_) => raise Collision k) x


    fun union' insert (m1,m2)	= appi (fn(k,a) => insert(m1,k,a)) m2
    fun union x			= union' insert x
    fun unionDisjoint x		= union' insertDisjoint x
    fun unionWith f		= union'(insertWith f)
    fun unionWithi f		= union'(insertWithi f)

  end
