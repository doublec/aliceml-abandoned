functor MakeHashImpSet(Item: HASH_KEY) :> IMP_SET where type item = Item.t =
  struct

    type item = Item.t
    type set  = item list array ref * int ref
    type t    = set

    exception Delete    of item
    exception Collision of item


    val initialSize		= 19

    fun new()			= (ref(Array.array(initialSize,[])), ref 0)
    fun deleteAll (s,k)		= ( s := Array.array(initialSize,[]) ; k := 0 )

    fun size(_, ref n)		= n
    fun isEmpty(_, ref n)	= n = 0

    fun app f (ref t, _)	= Array.app (List.app f) t
    fun fold f a (ref t, _)	= Array.foldl(fn(ks,a) => List.foldl f a ks) a t
    fun find p (ref t, _)	= let val size   = Array.length t
				      fun iter i =
					  if i = size then NONE else
					  case List.find p (Array.sub(t,i))
					    of NONE => iter(i+1)
					     | some => some
				  in iter 0 end



    fun clone(ref t, ref n)	= let val t' = Array.array(Array.length t, [])
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      (ref t', ref n)
				  end


    fun hash(t,k)		= Item.hash k mod Array.length t

    fun member((ref t,_), k)	= let val ks = Array.sub(t, hash(t,k)) in
				      List.exists (fn k'=> Item.equals(k,k')) ks
				  end


    exception Delete'

    fun delete'( [],   k')	= raise Delete'
      | delete'(k::ks, k')	= if Item.equals(k,k') then ks
						       else k :: delete'(ks,k')

    fun deleteWith f (s,k)	= let val (ref t,n) = s
				      val i   = hash(t,k)
				      val ks  = Array.sub(t,i)
				      val ks' = delete'(ks,k) before n := !n-1
						handle Delete' =>
						       (f k ; ks)
				  in
				      Array.update(t,i,ks')
				  end

    val delete			= deleteWith ignore
    val deleteExistent		= deleteWith(fn k => raise Delete k)


    fun reinsert t k		= let val i = hash(t,k) in
				      Array.update(t, i, k::Array.sub(t,i))
				  end

    fun resize(r as ref t,ref n)= if 3 * n < 2 * Array.length t then () else
				  let
				      val t'= Array.array(2*Array.length t-1,[])
				  in
				      Array.app(List.app (reinsert t')) t ;
				      r := t'
				  end

    fun insertWith f (s,k)	= let val _  = resize s
				      val (ref t,n) = s
				      val i  = hash(t,k)
				      val ks = Array.sub(t,i)
				  in
				      if List.exists
						(fn k' => Item.equals(k,k')) ks
				      then f k
				      else ( Array.update(t, i, k::ks)
					   ; n := !n+1 )
				  end

    val insert			= insertWith ignore
    val insertDisjoint		= insertWith(fn k => raise Collision k)

    fun union(s1,s2)		= app (fn k => insert(s1,k)) s2
    fun unionDisjoint(s1,s2)	= app (fn k => insertDisjoint(s1,k)) s2
    fun unionWith f (s1,s2)	= app (fn k => insertWith f (s1,k)) s2

  end
