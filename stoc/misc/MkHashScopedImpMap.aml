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

    fun mergeScope(r as ref ms)	= let val ms' = List.tl ms in
				      ImpMap.plus(List.hd ms', List.hd ms) ;
				      r := ms'
				  end


    fun lookup'( [],   k)	= NONE
      | lookup'(m::ms, k)	= case ImpMap.lookup(m,k)
				    of NONE => lookup'(ms,k)
				     | some => some

    fun lookup(ref ms, k)	= lookup'(ms,k)

    fun lookupScope(ref ms, k)	= ImpMap.lookup(List.hd ms, k)

    fun isEmptyScope(ref ms)	= ImpMap.isEmpty(List.hd ms)
    fun isEmpty(ref ms)		= List.all ImpMap.isEmpty ms


    fun appScope f (ref ms)	= ImpMap.app f (List.hd ms)
    fun app f (ref ms)		= List.app (ImpMap.app f) (List.rev ms)
    fun foldScope f b (ref ms)	= ImpMap.fold f b (List.hd ms)
    fun fold f b (ref ms)	= List.foldr (fn(m,b') => ImpMap.fold f b' m)
					     b ms

    fun delete(ref ms, k)	= ImpMap.delete(List.hd ms, k)
    fun insert(ref ms, k, a)	= ImpMap.insert(List.hd ms, k, a)
    fun insertDisjoint(ref ms, k, a)
				= ImpMap.insertDisjoint(List.hd ms, k, a)

    fun plus(ref ms1, ref ms2)	= let val m = List.hd ms1 in
				    List.app (fn m' => ImpMap.plus(m,m')) ms2
				  end

    fun plusDisjoint(ref ms1, ref ms2)
				= let val m = List.hd ms1 in
				      List.app (fn m' =>
					     ImpMap.plusDisjoint(m,m')) ms2
				  end

  end
