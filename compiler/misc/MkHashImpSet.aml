functor MakeHashImpSet(Item: HASH_KEY) :> IMP_SET where type item = Item.t =
  struct

    type item = Item.t
    type set  = item list array
    type t    = set

    exception Delete
    exception Collision of item


    fun hash(t,k)		= Item.hash k mod Array.length t

    fun new n			= Array.array(n,[])

    fun copy t			= let val t' = Array.array(Array.length t, [])
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      t'
				  end

    fun isEmpty t		= Misc.Array_all List.null t

    fun member(t,i)		= let val is = Array.sub(t, hash(t,i)) in
				      List.exists (fn i' => i = i') is
				  end

    fun delete'( [],   i')	= raise Delete
      | delete'(i::is, i')	= if i = i' then is : item list
					    else i::delete'(is,i')

    fun delete(s,i)		= let val n   = hash(s,i)
				      val is  = Array.sub(s,n)
				      val is' = delete'(is,i)
				  in
				      Array.update(s, n, is')
				  end

    fun insertWith f (s,i)	= let val n   = hash(s,i)
				      val is  = Array.sub(s,n)
				  in
				      if List.exists (fn i' => i = i') is
				      then f i
				      else Array.update(s, n, i::is)
				  end

    val insert			= insertWith(fn i => ())
    val insertDisjoint		= insertWith(fn i => raise Collision i)

    fun app f			= Array.app(List.app f)
    fun fold f			= Array.foldl(fn(is,a) => List.foldl f a is)

    fun union(s1,s2)		= app (fn i => insert(s1,i)) s2
    fun unionDisjoint(s1,s2)	= app (fn i => insertDisjoint(s1,i)) s2
    fun unionWith f (s1,s2)	= app (fn i => insertWith f (s1,i)) s2

  end
