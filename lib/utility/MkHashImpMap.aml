functor MakeHashImpMap(Key: HASH_KEY) :> IMP_MAP where type key = Key.t =
  struct

    type key    = Key.t
    type 'a map = (key * 'a) list array
    type 'a t   = 'a map

    exception Delete
    exception Collision of key


    fun hash(t,k)		= Key.hash k mod Array.length t
    fun isEntryFor k (k',_)	= k = k'


    fun new n			= Array.array(n,[])

    fun copy t			= let val t' = Array.array(Array.length t, [])
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      t'
				  end

    fun isEmpty t		= Misc.Array_all List.null t

    fun lookup(t,k)		= let val kas = Array.sub(t, hash(t,k)) in
				    Option.map #2 (List.find (isEntryFor k) kas)
				  end

    fun delete'(  [],    k)	= raise Delete
      | delete'(ka::kas, k)	= if #1 ka = k then kas : (key * 'a) list
					       else ka::delete'(kas,k)

    fun delete(t,k)		= let val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				      val kas' = delete'(kas,k)
				  in
				      Array.update(t, i, kas')
				  end

    fun insertWithi f (t,k,a)	= let val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				      val kas' =
					case List.find (isEntryFor k) kas
					  of NONE       => (k,a)::kas
					   | SOME(k,a') =>
						(k, f(k,a',a))::delete'(kas,k)
				  in
				      Array.update(t, i, kas')
				  end

    fun insertWith f		= insertWithi(fn(k,a1,a2) => f(a1,a2))
    fun insert x		= insertWithi #3 x
    fun insertDisjoint x	= insertWithi(fn(k,_,_) => raise Collision k) x

    fun appi f			= Array.app(List.app f)
    fun foldi f			= Array.foldl(fn(kas,b) => List.foldl f b kas)
    fun app f			= appi(fn(k,a) => f a)
    fun fold f			= foldi(fn((k,a),b) => f(a,b))

    fun union' insert (m1,m2)	= appi (fn(k,a) => insert(m1,k,a)) m2
    fun union x			= union' insert x
    fun unionDisjoint x		= union' insertDisjoint x
    fun unionWith f		= union'(insertWith f)
    fun unionWithi f		= union'(insertWithi f)

  end
