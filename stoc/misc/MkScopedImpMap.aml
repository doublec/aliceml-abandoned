functor MakeScopedImpMap(ImpMap: IMP_MAP) :>
  SCOPED_IMP_MAP where type key = ImpMap.key =
  struct

    type key    = ImpMap.key
    type 'a map = 'a ImpMap.t list ref
    type 'a t   = 'a map

    exception Delete    = ImpMap.Delete
    exception Collision = ImpMap.Collision
    exception Lookup    = ImpMap.Lookup


    fun new()			= ref[ImpMap.new()]
    fun clone(ref ms)		= ref(List.map ImpMap.clone ms)
    fun cloneScope(ref ms)	= ref[ImpMap.clone(List.hd ms)]
    fun insertScope r		= r := ImpMap.new() :: !r
    fun deleteScope r		= r := List.tl(!r)
    fun deleteAll r		= r := [ImpMap.new()]
    fun splitScope(r as ref ms)	= ( deleteScope r ; ref[List.hd ms] )
    fun inheritScope(r,r')	= r := List.hd(!(splitScope r')) :: !r

    fun mergeScope' unionMap (r as ref ms)
				= let val ms' = List.tl ms in
				      unionMap(List.hd ms', List.hd ms) ;
				      r := ms'
				  end

    fun mergeScope r		= mergeScope' ImpMap.union r
    fun mergeDisjointScope r	= mergeScope' ImpMap.unionDisjoint r
    fun mergeScopeWith f	= mergeScope'(ImpMap.unionWith f)
    fun mergeScopeWithi f	= mergeScope'(ImpMap.unionWithi f)


    fun lookup'( [],   k)	= raise Lookup k
      | lookup'([m],   k)	= ImpMap.lookupExistent(m,k)
      | lookup'(m::ms, k)	= case ImpMap.lookup(m,k)
				    of NONE   => lookup'(ms,k)
				     | SOME a => a

    fun lookup(ref ms, k)		= SOME(lookup'(ms,k))
					  handle Lookup _ => NONE
    fun lookupExistent(ref ms, k)	= lookup'(ms,k)
    fun lookupScope(ref ms, k)		= ImpMap.lookup(List.hd ms, k)
    fun lookupExistentScope(ref ms, k)	= ImpMap.lookupExistent(List.hd ms, k)

    fun member(ref ms, k)	= ( lookup'(ms, k) ; true )
				  handle Lookup _ => false
    fun memberScope(ref ms, k)	= ImpMap.member(List.hd ms, k)

    fun isEmptyScope(ref ms)	= ImpMap.isEmpty(List.hd ms)
    fun isEmpty(ref ms)		= List.all ImpMap.isEmpty ms

    fun sizeScope(ref ms)	= ImpMap.size(List.hd ms)
    fun size(ref ms)		= List.foldl (fn(m,n) => n + ImpMap.size m) 0 ms

    fun appScope f (ref ms)	= ImpMap.app f (List.hd ms)
    fun app f (ref ms)		= List.app (ImpMap.app f) (List.rev ms)
    fun foldScope f b (ref ms)	= ImpMap.fold f b (List.hd ms)
    fun fold f b (ref ms)	= List.foldr (fn(m,b') => ImpMap.fold f b' m)
					     b ms
    fun appiScope f (ref ms)	= ImpMap.appi f (List.hd ms)
    fun appi f (ref ms)		= List.app (ImpMap.appi f) (List.rev ms)
    fun foldiScope f b (ref ms)	= ImpMap.foldi f b (List.hd ms)
    fun foldi f b (ref ms)	= List.foldr (fn(m,b') => ImpMap.foldi f b' m)
					     b ms

    fun delete(ref ms, k)		= ImpMap.delete(List.hd ms, k)
    fun deleteExistent(ref ms, k)	= ImpMap.deleteExistent(List.hd ms, k)
    fun deleteWith f (ref ms, k)	= ImpMap.deleteWith f (List.hd ms, k)

    fun insert(ref ms, k, a)		= ImpMap.insert(List.hd ms, k, a)
    fun insertDisjoint(ref ms, k, a)	= ImpMap.insertDisjoint(List.hd ms, k,a)
    fun insertWith f (ref ms, k, a)	= ImpMap.insertWith f (List.hd ms, k, a)
    fun insertWithi f (ref ms, k, a)	= ImpMap.insertWithi f (List.hd ms, k,a)

    fun union' mapUnion (ref ms1, ref ms2)
				= let val m = List.hd ms1 in
				      List.app (fn m' => mapUnion(m,m')) ms2
				  end

    fun union x			= union' ImpMap.union x
    fun unionDisjoint x		= union' ImpMap.unionDisjoint x
    fun unionWith f		= union'(ImpMap.unionWith f)
    fun unionWithi f		= union'(ImpMap.unionWithi f)

  end
