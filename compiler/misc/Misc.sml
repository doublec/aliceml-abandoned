(*
 * Stuff that should be in the standard structures.
 *)

structure Misc :> MISC =
  struct

    fun Option_isNone NONE	= true
      | Option_isNone  _	= false

    fun Option_app f  NONE	= ()
      | Option_app f (SOME x)	= f x

    fun Option_fold f b  NONE	= b
      | Option_fold f b (SOME a)= f(a,b)


    fun ListPair_find f (nil,_)	= NONE
      | ListPair_find f (_,nil)	= NONE
      | ListPair_find f (x::xs, y::ys)
				= if f(x,y) then SOME(x,y)
					    else ListPair_find f (xs,ys)


    fun Array_all p a		= let val size   = Array.length a

				      fun iter i = if i = size then true
				  		   else p(Array.sub(a,i))
							andalso iter(i+1)
				  in iter 0 end

    fun Array_exists p a	= let val size   = Array.length a

				      fun iter i = if i = size then false
				  		   else p(Array.sub(a,i))
							orelse iter(i+1)
				  in iter 0 end

  end
