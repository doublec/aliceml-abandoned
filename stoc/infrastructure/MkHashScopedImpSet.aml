functor MakeHashScopedImpSet(Item: HASH_KEY) :>
  SCOPED_IMP_SET where type item = Item.t =
  struct

    structure ImpSet = MakeHashImpSet(Item)

    type item = Item.t
    type set  = ImpSet.t list ref
    type t    = set

    exception Delete    = ImpSet.Delete
    exception Collision = ImpSet.Collision


    val scopeSize = 19

    fun new()			= ref[ImpSet.new()]
    fun copy(ref ss)		= ref(List.map ImpSet.copy ss)
    fun copyScope(ref ss)	= ref[ImpSet.copy(List.hd ss)]
    fun insertScope r		= r := ImpSet.new() :: !r
    fun deleteScope r		= r := List.tl(!r)
    fun delete2ndScope r	= r := List.hd(!r)::List.tl(List.tl(!r))
    fun splitScope(r as ref ss)	= ( deleteScope r ; ref[List.hd ss] )
    fun inheritScope(r,r')	= r := List.hd(!(splitScope r')) :: !r

    fun mergeScope' unionSet (r as ref ss)
				= let val ss' = List.tl ss in
				      unionSet(List.hd ss', List.hd ss) ;
				      r := ss'
				  end

    fun mergeScope r		= mergeScope' ImpSet.union r
    fun mergeDisjointScope r	= mergeScope' ImpSet.unionDisjoint r
    fun mergeScopeWith f	= mergeScope'(ImpSet.unionWith f)


    fun member'( [],   i)	= false
      | member'(s::ss, i)	= ImpSet.member(s,i) orelse member'(ss,i)

    fun member(ref ss, i)	= member'(ss,i)

    fun memberScope(ref ss, i)	= ImpSet.member(List.hd ss, i)

    fun isEmptyScope(ref ss)	= ImpSet.isEmpty(List.hd ss)
    fun isEmpty(ref ss)		= List.all ImpSet.isEmpty ss

    fun sizeScope(ref ss)	= ImpSet.size(List.hd ss)
    fun size(ref ss)		= List.foldl (fn(s,n) => n + ImpSet.size s) 0 ss

    fun appScope f (ref ss)	= ImpSet.app f (List.hd ss)
    fun app f (ref ss)		= List.app (ImpSet.app f) (List.rev ss)
    fun foldScope f a (ref ss)	= ImpSet.fold f a (List.hd ss)
    fun fold f a (ref ss)	= List.foldr (fn(s,a') => ImpSet.fold f a' s)
					     a ss

    fun delete(ref ss, i)	= ImpSet.delete(List.hd ss, i)

    fun insert(ref ss, i)	= ImpSet.insert(List.hd ss, i)
    fun insertDisjoint(ref ss, i) = ImpSet.insertDisjoint(List.hd ss, i)
    fun insertWith f (ref ss, i)  = ImpSet.insertWith f (List.hd ss, i)

    fun union' setUnion (ref ss1, ref ss2)
				= let val s1 = List.hd ss1 in
				      List.app (fn s2 => setUnion(s1,s2)) ss2
				  end

    fun union x			= union' ImpSet.union x
    fun unionDisjoint x		= union' ImpSet.unionDisjoint x
    fun unionWith f		= union'(ImpSet.unionWith f)

  end
