functor MakeHashScopedImpMap(Key: HASH_KEY) :>
  SCOPED_IMP_MAP where type key = Key.t =
  struct

    structure ImpMap = MakeHashImpMap(Key)

    type key    = Key.t
    type 'a map = 'a ImpMap.t list ref
    type 'a t   = 'a map

    exception Delete    = ImpMap.Delete
    exception Collision = ImpMap.Collision


    val scopeSize = 19

    fun new()			= ref[ImpMap.new scopeSize]
    fun copy(ref ms)		= ref(List.map ImpMap.copy ms)
    fun copyScope(ref ms)	= ref[ImpMap.copy(List.hd ms)]

    fun insertScope r		= r := ImpMap.new scopeSize :: !r
    fun deleteScope r		= r := List.tl(!r)
    fun delete2ndScope r	= r := List.hd(!r)::List.tl(List.tl(!r))


    fun mergeScope' unionMap (r as ref ms)
				= let val ms' = List.tl ms in
				      unionMap(List.hd ms', List.hd ms) ;
				      r := ms'
				  end

    fun mergeScope r		= mergeScope' ImpMap.union r
    fun mergeDisjointScope r	= mergeScope' ImpMap.unionDisjoint r
    fun mergeScopeWith f	= mergeScope'(ImpMap.unionWith f)
    fun mergeScopeWithi f	= mergeScope'(ImpMap.unionWithi f)


    fun lookup'( [],   k)	= NONE
      | lookup'(m::ms, k)	= case ImpMap.lookup(m,k)
				    of NONE => lookup'(ms,k)
				     | some => some

    fun lookup(ref ms, k)	= lookup'(ms,k)

    fun lookupScope(ref ms, k)	= ImpMap.lookup(List.hd ms, k)

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

    fun delete(ref ms, k)	= ImpMap.delete(List.hd ms, k)

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
