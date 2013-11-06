type u
type v
datatype w

type t = u * v

(*--------------------------------------------------------------------*)

signature S =
    sig
        constructor C of t : w
    end

signature S' =
    sig
        val C : u * v -> w
    end

functor F1(X :	sig
		    functor F(functor G(X:S) : S') :
			sig
			    functor H(X:S') : S
			end
		    end)
	=  X :>	sig
		    functor F(functor G(X:S') : S) :
			sig
			    functor H(X:S) : S'
			end
		end

(*--------------------------------------------------------------------*)

functor F2(X :	sig
		    functor F(val C : u * v -> w) : S
		end)
	=  X :>	sig
		    functor F(constructor C of t : w) : S
		end

(*--------------------------------------------------------------------*)
