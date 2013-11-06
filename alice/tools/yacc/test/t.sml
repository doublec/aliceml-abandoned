(* replaced 'lexer' with 'lexxer' because of problems with hose *)

local fun split l1 l2 [] = (l1,l2)
	| split l1 l2 (x::xs) = split l2 (x::l1) xs
      fun merge l1 [] = l1
	| merge [] l2 = l2
	| merge (x::xs :string list) (y::ys) = if x<y then x::(merge xs (y::ys))
				  else y::(merge (x::xs) ys)
in 
    fun sort [] = []
      | sort [x] = [x]
      | sort l =
	let val (l1,l2) = split [] [] l
	in
	    merge (sort l1) (sort l2)
	end
end 

fun remove (x::y::l) = if x=y then remove (y::l) else x::remove (y::l)
  | remove l = l


fun mkGet f g =
let val t = ref true 
in fn () => if !t then (t:=false; f ()) else g ()
end

val parser = fn lexxer =>
    let val (a as (_,p1,p2)) = lexxer ()
	val f = fn () => ("A",p1,p2)
	val g = Various.mkGet (fn () => a) lexxer
    in mkGet f g
    end

datatype token = INT of int | PLUS | MINUS | LPAR | RPAR

datatype bintree = Lf | Sub of bintree * bintree | Add of bintree * bintree

fun ptree 0 = Lf
  | ptree n = let val t = ptree (n-1) in Add (t,t) end

fun stree 0 = Lf
  | stree n = let val t = stree (n-1) in Sub (t,t) end

fun tree n = Sub(ptree n,stree n)

fun toList Lf = [INT 1]
  | toList (Sub(t1,t2)) = LPAR::(toList t1)@(MINUS::(toList t2)@[RPAR])
  | toList (Add(t1,t2)) = LPAR::(toList t1)@(PLUS::(toList t2)@[RPAR])

val test = toList o tree
