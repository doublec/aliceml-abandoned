(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

structure PatternMatching :> PATTERNMATCHING =
struct

    open Util;

    (* Repräsentation eines Musters *)

    datatype pat = STR of char list | VAR of string

    type pattern = pat list
    type assignment = (string * (char list)) list

    (* Debugausgabe *)

    fun printPattern nil = print "\n"
      | printPattern ((STR s)::xr) = (print ("(STR "^(String.implode s)^")");
				      printPattern xr)
      | printPattern ((VAR s)::xr) = (print ("(VAR"^s^")");
				      printPattern xr)
	
    (* Erstellt ein Muster aus einem String *)

    fun createPattern' nil = nil
      | createPattern' s = 
	if not(occurs s #"#") then [STR s]
	else let val (a,b) = split_list s #"#"
		 val (x,e) = split_list b #"#"
	     in (STR a)::(VAR(String.implode x))::(createPattern' e)
	     end
	 
    fun createPattern s = createPattern' (String.explode s)

    (* Matchen einer Zeichenfolge auf ein Muster *)
	    
    local

	fun match nil nil = SOME nil
	  | match nil _ = NONE
	  | match ar nil = SOME ar
	  | match (a::ar) (b::br) = if a = b then match ar br
				    else NONE

	fun matchPattern' nil nil acc = SOME acc
	  | matchPattern' (_::_) nil acc = NONE
	  | matchPattern' s ((STR s')::pr) acc = 
	    (case match s s' of
		 NONE => NONE
	       |(SOME s'') => matchPattern' s'' pr acc)

	  | matchPattern' s ((VAR x)::pr) acc = 
		 (case matchPattern' s pr acc of
		      NONE => matchVar s pr (x,nil) acc
		    | s => s)
		      
	and matchVar nil pr (x,s) acc = matchPattern' nil pr ((x,s)::acc)
	  | matchVar (y::yr) pr (x,s) acc = 
	 (case matchPattern' yr pr acc of
	      NONE => matchVar yr pr (x,s@[y]) acc
	    |(SOME l) => SOME((x,s@[y])::l))
    in
	 fun matchPattern(s,p) = matchPattern' (String.explode s) p nil
    end

    (* Instanziiert ein Mustern anhand einer Belegung neu *)

    fun substPattern' (nil,ass) = nil
      | substPattern' ((STR s')::xr, ass) = s' @ (substPattern'(xr,ass))
      | substPattern' ((VAR x)::xr, ass) = 
	let val (var,capital) = case separate(x,#":") of
	    ["CAPITAL",x'] => (x',true)
	    | _ => (x,false)
	in
	    (case lookup ass var of 
		NONE => nil 
	      | (SOME s) => if capital then Capital s else s)
	    @ (substPattern'(xr,ass))
	end

    fun substPattern (p,ass) = String.implode(substPattern'(p,ass))

end
