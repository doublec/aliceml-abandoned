functor Symtable(Key: HASH_KEY) :> SYMTABLE where type key = Key.t =
  struct

    structure Hashtable = Hashtable(Key)

    type key            = Key.t
    type 'a symtable    = 'a Hashtable.t list ref
    type 'a t           = 'a symtable

    exception Lookup    = Hashtable.Lookup
    exception Collision = Hashtable.Collision


    val scopeSize = 19

    fun new()			= ref[Hashtable.new scopeSize]
    fun copy(ref ts)		= ref(List.map Hashtable.copy ts)
    fun copyScope(ref ts)	= ref[Hashtable.copy(List.hd ts)]

    fun insertScope r		= r := Hashtable.new scopeSize :: !r
    fun deleteScope r		= r := List.tl(!r)
    fun delete2ndScope r	= r := List.hd(!r)::List.tl(List.tl(!r))

    fun mergeScope(r as ref ts)	= let val ts' = List.tl ts in
				      Hashtable.plus(List.hd ts', List.hd ts) ;
				      r := ts'
				  end


    fun lookup'( [],   k)	= raise Lookup
      | lookup'(t::ts, k)	= Hashtable.lookup(t,k)
				  handle Hashtable.Lookup => lookup'(ts,k)

    fun lookup(ref ts, k)	= lookup'(ts,k)

    fun lookupScope(ref ts, k)	= Hashtable.lookup(List.hd ts, k)

    fun isEmptyScope(ref ts)	= Hashtable.isEmpty(List.hd ts)
    fun isEmpty(ref ts)		= List.all Hashtable.isEmpty ts


    fun appScope f (ref ts)	= Hashtable.app f (List.hd ts)
    fun app f (ref ts)		= List.app (Hashtable.app f) (List.rev ts)
    fun foldScope f b (ref ts)	= Hashtable.fold f b (List.hd ts)
    fun fold f b (ref ts)	= List.foldr (fn(t,b') => Hashtable.fold f b' t)
					     b ts

    fun insert(ref ts, k, a)	= Hashtable.insert(List.hd ts, k, a)
    fun insertDisjoint(ref ts, k, a)
				= Hashtable.insertDisjoint(List.hd ts, k, a)

    fun plus(ref ts1, ref ts2)	= let val t = List.hd ts1 in
				    List.app (fn t' => Hashtable.plus(t,t')) ts2
				  end

    fun plusDisjoint(ref ts1, ref ts2)
				= let val t = List.hd ts1 in
				      List.app (fn t' =>
					     Hashtable.plusDisjoint(t,t')) ts2
				  end

  end
