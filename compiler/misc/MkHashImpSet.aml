functor MakeHashImpSet(Item: HASH_KEY) :> IMP_SET where type item = Item.t =
  struct

    open Misc

    type item = Item.t
    type set  = (int ref * item option array) ref
    type t    = set

    exception Delete    of item
    exception Collision of item

    val initialSize		= 19


    fun new()			= ref(ref 0, Array.array(initialSize,NONE))

    fun copy(ref(n,t))		= let val t' = Array.array(Array.length t,NONE)
				  in
				      Array.copy{src=t, dst=t', si=0, di=0,
						 len=NONE} ;
				      ref(ref(!n), t')
				  end


    fun size(ref(n,_))		= !n
    fun isEmpty(ref(n,_))	= !n = 0


    fun find(t,k)		= find'(t, Item.hash k mod Array.length t, k)
    and find'(t,i,k)		= case Array.sub(t,i)
				    of NONE    => (false,i)
				     | SOME k' =>
				       if k = k' then (true,i)
				       else find'(t,(i+1) mod Array.length t, k)

    fun member(ref(_,t),k)	= #1(find(t,k))


    fun deleteWith f (ref(n,t), k)
				= case find(t,k)
				    of (false,_) => f k
				     | (true, i) => ( Array.update(t,i,NONE)
						    ; n := !n-1
						    )

    val delete			= deleteWith ignore
    val deleteExistent		= deleteWith (fn k => raise Delete k)


    fun reinsert t k		= Array.update(t, #2(find(t,k)), SOME k)
    fun resize(s as ref(n,t))	= if 3 * !n < 2 * Array.length t then () else
				  let
				      val t' = Array.array(2*Array.length t-1,
							   NONE)
				  in
				      Array.app (Option_app(reinsert t')) t ;
				      s := (n,t')
				  end


    fun insertWith f (s,k)	= let val   _   = resize s
				      val (n,t) = !s
				  in
				     case find(t,k)
				       of (true, _) => f k
					| (false,i) =>
					  ( Array.update(t,i,SOME k)
					  ; n := !n+1
					  )
				  end

    val insert			= insertWith ignore
    val insertDisjoint		= insertWith(fn k => raise Collision k)

    fun app f (ref(_,t))	= Array.app (Option_app f) t
    fun fold f a (ref(_,t))	= Array.foldl (fn(ko,a)=>Option_fold f a ko) a t

    fun union(s1,s2)		= app (fn k => insert(s1,k)) s2
    fun unionDisjoint(s1,s2)	= app (fn k => insertDisjoint(s1,k)) s2
    fun unionWith f (s1,s2)	= app (fn k => insertWith f (s1,k)) s2

  end
