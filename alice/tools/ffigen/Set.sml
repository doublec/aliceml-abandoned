
structure Set :> Set = struct
  type 'a set = ('a list * ('a * 'a -> order))
  fun emptyset f = (nil, f)

  fun isEmpty(s,f) = null s

  fun member((s,f),x) = List.exists(fn y => f(x,y)=EQUAL) s

  fun insert((s,f),x) = (x::s, f)

  fun remove ((l,f),x) = 
      let 
	  fun remove'((nil),x)     = nil
	    | remove'((y::ys),x) = if f(x,y)=EQUAL then remove'(ys,x) else y::(remove'(ys,x))
      in 
	  (remove'(l,x),f)
      end;

  fun union((s,f),(t,_)) = (s@t,f)
end
