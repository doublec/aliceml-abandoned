functor Hashtable(Key: HASH_KEY) :> HASHTABLE where type key = Key.t =
  struct

    type key          = Key.t
    type 'a hashtable = (key * 'a) list array
    type 'a t         = 'a hashtable

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

    fun insert(t,k,a)		= let val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				      val kas' = delete'(kas,k)
						 handle Delete => kas
				  in
				      Array.update(t, i, (k,a)::kas')
				  end

    fun insertDisjoint(t,k,a)	= let val i    = hash(t,k)
				      val kas  = Array.sub(t,i)
				  in
				      if List.exists (isEntryFor k) kas
				      then raise Collision k
				      else Array.update(t, i, (k,a)::kas)
				  end

    fun app f			= Array.app(List.app f)
    fun fold f			= Array.foldl(fn(kas,b) => List.foldl f b kas)

    fun plus(t1,t2)		= app (fn(k,a) => insert(t1,k,a)) t2
    fun plusDisjoint(t1,t2)	= app (fn(k,a) => insertDisjoint(t1,k,a)) t2

  end
