(*
 * Stuff that should be in the standard structures.
 *)

structure Misc :> MISC =
  struct

    structure General =
	struct
	    open General

	    infix 3 :=:
	    fun (r1 as ref x1) :=: (r2 as ref x2) = ( r1 := x2 ; r2 := x1 )
	    val swap = op :=:
	end


    structure Option =
	struct
	    open Option

	    fun isNone NONE		= true
	      | isNone  _		= false

	    fun app f  NONE		= ()
	      | app f (SOME x)		= f x

	    fun fold f b  NONE		= b
	      | fold f b (SOME a)	= f(a,b)
	end


    structure String =
	struct
	    open String

	    val maxLen   = maxSize
	    val length   = size
	    val append   = op ^
	    val fromList = implode
	    val toList   = explode
	    val tabulate = implode o List.tabulate
	end


    structure List =
	struct
	    open List

	    val append			= op @
	    val sub			= nth

	    fun appr f   nil		= ()
	      | appr f (x::xs)		= ( appr f xs ; f x )

	    fun foldli f z xs =
		let
		    fun foldli'(x::xr, z, i) = foldli'(xr, f(i,x,z), i+1)
		      | foldli'( nil,  z, _) = z
		in
		    foldli'(xs,z,0)
		end

	    fun foldri f z xs =
		let
		    fun foldri'(x::xr, z, i) = f(i, x, foldri'(xr, z, i+1))
		      | foldri'( nil,  z, _) = z
		in
		    foldri'(xs,z,0)
		end

	    fun mapi f xs =
		let
		    fun mapi'(x::xr, i) = f(i,x) :: mapi'(xr, i+1)
		      | mapi'( nil,  _) = nil
		in
		    mapi'(xs, 0)
		end

	    fun appi f xs =
		let
		    fun appi'(x::xr, i) = ( f(i,x) ; appi'(xr, i+1) )
		      | appi'( nil,  _) = ()
		in
		    appi'(xs, 0)
		end

	    fun appri f xs =
		let
		    fun appri'(x::xr, i) = ( appri'(xr, i+1) ; f(i,x) )
		      | appri'( nil,  _) = ()
		in
		    appri'(xs,0)
		end
	end


    structure ListPair =
	struct
	    open ListPair

	    fun find f ( nil,    _  )	= NONE
	      | find f (  _,    nil )	= NONE
	      | find f (x::xs, y::ys)	= if f(x,y) then SOME(x,y)
						    else find f (xs,ys)
	end


    structure Vector =
	struct
	    open Vector

	    fun append(v1,v2)	= concat[v1,v2]

	    fun rev v		= let val len = length v
				      fun f i = sub(v, len-i-1)
				  in tabulate(len, f) end

	    fun appr f v	= let val len    = length v
				      fun iter i =
					  if i = len then ()
					  else ( iter(i+1) ; f(sub(v,i)) )
				  in iter 0 end

	    fun appri f (v,i,no) =
				  let val h = case no of SOME n => i + n
						       | NONE   => length v
				      fun iter i =
					  if i = h then ()
					  else ( iter(i+1) ; f(i, sub(v,i)) )
				  in iter 0 end

	    fun all p v		= let val len    = length v
				      fun iter i =
					  if i = len then true
					  else p(sub(v,i)) andalso iter(i+1)
				  in iter 0 end

	    fun exists p v	= let val len    = length v
				      fun iter i =
					  if i = len then false
					  else p(sub(v,i)) orelse iter(i+1)
				  in iter 0 end

	    fun find p v	= let val len    = length v
				      fun iter i =
					  if i = len then NONE else
					  let val x = sub(v,i) in
					      if p x then SOME x
						     else iter(i+1)
					  end
				  in iter 0 end
	end


    structure Array =
	struct
	    open Array

	    fun fromVector v	= tabulate(Vector.length v,
					   fn i => Vector.sub(v,i))

	    fun swap(a,i,j)	= let val x = sub(a,i) in
				      update(a, i, sub(a,j)) ;
				      update(a, j, x)
				  end

	    fun reverse a	= let fun reverse'(i,j) =
				      if i >= j then () else
					  ( swap(a,i,j) ; reverse'(i+1, j-1) )
				  in reverse'(0, length a - 1) end

	    fun all p a		= let val len    = length a
				      fun iter i =
					  if i = len then true
					  else p(sub(a,i)) andalso iter(i+1)
				  in iter 0 end

	    fun exists p a	= let val len    = length a
				      fun iter i =
					  if i = len then false
					  else p(sub(a,i)) orelse iter(i+1)
				  in iter 0 end

	    fun find p a	= let val len    = length a
				      fun iter i =
					  if i = len then NONE else
					  let val x = sub(a,i) in
					      if p x then SOME x
						     else iter(i+1)
					  end
				  in iter 0 end
	end


    val General_swap	= General.swap
    val Option_isNone	= Option.isNone
    val Option_app	= Option.app
    val Option_fold	= Option.fold
    val List_appr	= List.appr
    val List_foldli	= List.foldli
    val List_foldri	= List.foldri
    val List_mapi	= List.mapi
    val List_appi	= List.appi
    val List_appri	= List.appi
    val ListPair_find	= ListPair.find
    val Vector_append	= Vector.append
    val Vector_rev	= Vector.rev
    val Vector_appr	= Vector.appr
    val Vector_appri	= Vector.appri
    val Vector_all	= Vector.all
    val Vector_exists	= Vector.exists
    val Vector_find	= Vector.find
    val Array_fromVector= Array.fromVector
    val Array_swap	= Array.swap
    val Array_reverse	= Array.reverse
    val Array_all	= Array.all
    val Array_exists	= Array.exists
    val Array_find	= Array.find
    val Char_toWide	= WideChar.chr o Char.ord
    val Char_fromWide	= Char.chr o WideChar.ord
    val String_toWide	= WideString.implode o List.map Char_toWide o String.explode
    val String_fromWide	= String.implode o List.map Char_fromWide o WideString.explode

  end
