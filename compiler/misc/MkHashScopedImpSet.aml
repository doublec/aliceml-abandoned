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

    fun new()			= ref[ImpSet.new scopeSize]
    fun copy(ref ss)		= ref(List.map ImpSet.copy ss)
    fun copyScope(ref ss)	= ref[ImpSet.copy(List.hd ss)]

    fun insertScope r		= r := ImpSet.new scopeSize :: !r
    fun deleteScope r		= r := List.tl(!r)
    fun delete2ndScope r	= r := List.hd(!r)::List.tl(List.tl(!r))

    fun mergeScope(r as ref ss)	= let val ss' = List.tl ss in
				      ImpSet.union(List.hd ss', List.hd ss) ;
				      r := ss'
				  end


    fun member'( [],   i)	= false
      | member'(s::ss, i)	= ImpSet.member(s,i) orelse member'(ss,i)

    fun member(ref ss, i)	= member'(ss,i)

    fun memberScope(ref ss, i)	= ImpSet.member(List.hd ss, i)

    fun isEmptyScope(ref ss)	= ImpSet.isEmpty(List.hd ss)
    fun isEmpty(ref ss)		= List.all ImpSet.isEmpty ss


    fun appScope f (ref ss)	= ImpSet.app f (List.hd ss)
    fun app f (ref ss)		= List.app (ImpSet.app f) (List.rev ss)
    fun foldScope f a (ref ss)	= ImpSet.fold f a (List.hd ss)
    fun fold f a (ref ss)	= List.foldr (fn(s,a') => ImpSet.fold f a' s)
					     a ss

    fun delete(ref ss, i)	= ImpSet.delete(List.hd ss, i)
    fun insert(ref ss, i)	= ImpSet.insert(List.hd ss, i)
    fun insertDisjoint(ref ss, i)
				= ImpSet.insertDisjoint(List.hd ss, i)

    fun union(ref ss1, ref ss2)	= let val s = List.hd ss1 in
				    List.app (fn s' => ImpSet.union(s,s')) ss2
				  end

    fun unionDisjoint(ref ss1, ref ss2)
				= let val s = List.hd ss1 in
				      List.app (fn s' =>
					     ImpSet.unionDisjoint(s,s')) ss2
				  end

  end
