functor HoseLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Hose_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AbsSyn

exception Error of string

fun pError (e, po) = (print ("Parse Error in position " ^ posToString po ^ ": " ^ e ^ "\n");
	              raise Error e)

(* makeVector : bool array -> BoolVector.vector, converts array to Vector
 *)
fun makeVector c = (BoolVector.tabulate (257, (fn x => Array.sub(c,x))))
	
	
(* used by: stringToExp
 * shrink : char list * char list -> char list, takes (char list, nil),
 * combines several chars to one (if possible) and returns the new char list
 * Example: shrink([#"\\",#"1",#"2",#"3",#"3"], []) = [#"\123",#"3"] = [#"{",#"3"]
 *)
fun shrink (nil, xs, po) = rev xs
  | shrink (#"\\"::cs, xs, po) = shrink (escape (cs, xs, po))
  | shrink (c::cs, xs, po) = shrink (cs, c::xs, po)
    
    
(* escape takes (char list, char list), reads an escaped char from the first list, attaches it to the second list
 * and returns the pair
 *)
and escape (#"a"::cs, xs, po)  = (cs, #"\007"::xs, po)
  | escape (#"b"::cs, xs, po)  = (cs, #"\008"::xs, po)
  | escape (#"t"::cs, xs, po)  = (cs, #"\009"::xs, po) 
  | escape (#"n"::cs, xs, po)  = (cs, #"\010"::xs, po)
  | escape (#"v"::cs, xs, po)  = (cs, #"\011"::xs, po)
  | escape (#"f"::cs, xs, po)  = (cs, #"\012"::xs, po)
  | escape (#"r"::cs, xs, po)  = (cs, #"\013"::xs, po)
  | escape (#"\""::cs, xs, po) = (cs, #"\""::xs, po)
  | escape (#"\\"::cs, xs, po) = (cs, #"\\"::xs, po)
  | escape (#"^"::cs, xs, po)  = escape2 (cs, xs, po)
  | escape (c::cs, xs, po)     = if Char.isDigit c then escape3 (c::cs, xs, po)
				 else if Char.isSpace c then escape4 (cs, xs, po)
				      else pError ( "bad escape character \\" ^ (Char.toString c), po)
  | escape ( _ , _ , po)       = pError ("bad escape character \\", po)
			     
			     
(* escape2 scans for chars from #"\^@" to #"\^_"
 *)
and escape2 (c::cs, xs, po) =
    let 
	val range = Char.ord c 
    in
	if range>63 andalso range<96 then (cs, Char.chr(range-64) ::xs, po)
	else pError ("bad escape character \\^" ^ (Char.toString c), po)
    end
  | escape2 ( _   , _ , po)   = pError ("bad escape character \\^", po)
    
    
(* escape3 scans for chars like #"\123"
 *)
and escape3 (a::b::c::cs, xs, po) =
    let
	val zero = Char.ord #"0"
	val a_ = Char.ord a - zero
	val b_ = if Char.isDigit b then Char.ord b - zero
		 else pError ("bad escape character \\" ^ (implode [a,b,c]), po)
	val c_ = if Char.isDigit c then Char.ord c - zero
		 else pError ("bad escape character \\" ^ (implode [a,b,c]), po)
	val num = 100*a_+10*b_+c_
    in 
	if num < 256 then (cs, Char.chr(num)::xs, po)
	else pError ("bad escape character \\" ^ (implode [a,b,c]), po)
    end
  | escape3 (cs, xs, po) = pError ("bad escape character \\" ^ implode cs, po)
    
    
(* escape4 scans for end of a gap like #" \n \\"
 *)
and escape4 (#"\\"::cs, xs, po) = (cs, xs, po)
  | escape4 (c::cs, xs, po) = if Char.isSpace c then escape4 (cs, xs, po)
			      else pError ("bad character in gap: " ^ Char.toString c, po)
  | escape4 ( _   , _ , po) = pError ("bad gap", po)


(* wildcard : unit -> BoolVector.vector, returns a vector filled with true, except for 'eof'
 *)
fun wildcard ( ) =
    let
	val c = Array.array (257, true)
    in
	Array.update (c, 256, false);
	makeVector c
    end
    
    
(* used by: stringToExp
 * oneChar : char -> AbsSyn.regexp, returns CHARS, a vector filled with false except for ord char
 *)
fun oneChar (c, p) =
    let
	val arr = Array.array (257, false)
    in
	(Array.update(arr,Char.ord c, true);
	 CHARS (makeVector arr, 0, p) )
    end


(* stringToExp string * position -> regexp, returns the regexp representing the string
 *)
fun stringToExp (s, po) =
    let
	val xs = shrink (explode (String.substring(s,1,String.size s - 2)), nil, po)
	fun first nil = (nil, EPS)
	  | first (c::cs) = (cs, oneChar (c, po) )
	fun fold (nil , a) = a
	  | fold (c::cs, a) = fold(cs, CAT(a, oneChar (c, po), po))
    in
	fold (first xs)
    end
    
		    
(* makeRep : regexp * int * int -> regexp, returns a regexp representing the given regexp repeated n to m times
 *)
fun makeRep (r, n, m, p) =
    let
	val i = m-n
	fun concat 0 = AbsSyn.EPS
	  | concat 1 = r
	  | concat y = AbsSyn.CAT(concat (y-1), r, p)
	val t = concat n
	fun alt (0,dat) = dat 
	  | alt (1,AbsSyn.EPS) = AbsSyn.ALT(AbsSyn.EPS, r, p) 
	  | alt (x,dat) = alt(x-1,AbsSyn.CAT(dat,AbsSyn.ALT(r,AbsSyn.EPS, p), p))
    in
	alt (i,t)
    end


(* makeArray : string * bool -> bool array,
 * returns an array with all field exept the char ords of the string set to the bool value
 * example: makeArray ("A-Z -", true) = false for A,B,...,Z,' ','-' and true for the rest
 *)
fun makeArray (s, b, po) =
    let
	val nb = not b
	val c = Array.array (257, b)
	val cl = shrink (explode (String.substring(s,1,String.size s - 2)), nil, po)
	fun insert (a::(#"-")::b::ys, x) = ( insertSequence (a,b,nb,x); insert (ys,x) )
	  | insert (a::ys, x) = ( Array.update(x,Char.ord a,nb); insert (ys, x) )
	  | insert (nil, x) = x
    in
	Array.update (c, 256, false);
	insert (cl, c)
    end
    
    
(* insertSequence : char * char * bool * bool array -> unit,
 * updates the array x from a to b with bool value t
 *)
and insertSequence (a,b,t,x) = 
    let
	val i = ref (Char.ord a)
	val j = Char.ord b
    in
	while !i<=j do
	    (Array.update(x,!i,t);
	     i:= !i + 1 )
    end









end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\091\000\002\000\091\000\004\000\091\000\007\000\091\000\
\\026\000\091\000\000\000\
\\001\000\001\000\092\000\002\000\092\000\004\000\092\000\007\000\092\000\
\\026\000\092\000\000\000\
\\001\000\001\000\093\000\002\000\093\000\004\000\093\000\007\000\093\000\
\\026\000\093\000\000\000\
\\001\000\001\000\094\000\002\000\094\000\004\000\094\000\007\000\094\000\
\\026\000\094\000\000\000\
\\001\000\001\000\095\000\002\000\095\000\003\000\066\000\004\000\095\000\
\\007\000\095\000\026\000\095\000\000\000\
\\001\000\001\000\096\000\002\000\096\000\004\000\096\000\007\000\096\000\
\\026\000\096\000\000\000\
\\001\000\001\000\097\000\002\000\097\000\003\000\062\000\004\000\097\000\
\\007\000\097\000\026\000\097\000\000\000\
\\001\000\001\000\098\000\002\000\098\000\003\000\098\000\004\000\098\000\
\\007\000\098\000\026\000\098\000\000\000\
\\001\000\001\000\099\000\002\000\099\000\003\000\099\000\004\000\099\000\
\\007\000\099\000\012\000\061\000\026\000\099\000\000\000\
\\001\000\001\000\100\000\002\000\100\000\003\000\100\000\004\000\100\000\
\\007\000\100\000\012\000\100\000\026\000\100\000\000\000\
\\001\000\001\000\101\000\002\000\101\000\003\000\101\000\004\000\101\000\
\\007\000\101\000\012\000\101\000\026\000\101\000\000\000\
\\001\000\001\000\102\000\002\000\102\000\003\000\102\000\004\000\102\000\
\\007\000\102\000\011\000\102\000\026\000\102\000\000\000\
\\001\000\001\000\103\000\002\000\103\000\003\000\103\000\004\000\103\000\
\\007\000\103\000\011\000\103\000\012\000\055\000\026\000\103\000\000\000\
\\001\000\001\000\104\000\002\000\104\000\003\000\104\000\004\000\104\000\
\\006\000\052\000\007\000\104\000\008\000\060\000\009\000\059\000\
\\010\000\058\000\011\000\104\000\012\000\104\000\014\000\051\000\
\\017\000\057\000\019\000\050\000\023\000\049\000\024\000\048\000\
\\026\000\104\000\000\000\
\\001\000\001\000\105\000\002\000\105\000\003\000\105\000\004\000\105\000\
\\007\000\105\000\011\000\105\000\012\000\105\000\026\000\105\000\000\000\
\\001\000\001\000\106\000\002\000\106\000\003\000\106\000\004\000\106\000\
\\006\000\106\000\007\000\106\000\008\000\106\000\009\000\106\000\
\\010\000\106\000\011\000\106\000\012\000\106\000\014\000\106\000\
\\017\000\106\000\019\000\106\000\023\000\106\000\024\000\106\000\
\\026\000\106\000\000\000\
\\001\000\001\000\107\000\002\000\107\000\003\000\107\000\004\000\107\000\
\\006\000\107\000\007\000\107\000\008\000\107\000\009\000\107\000\
\\010\000\107\000\011\000\107\000\012\000\107\000\014\000\107\000\
\\017\000\107\000\019\000\107\000\023\000\107\000\024\000\107\000\
\\026\000\107\000\000\000\
\\001\000\001\000\108\000\002\000\108\000\003\000\108\000\004\000\108\000\
\\006\000\108\000\007\000\108\000\008\000\108\000\009\000\108\000\
\\010\000\108\000\011\000\108\000\012\000\108\000\014\000\108\000\
\\017\000\108\000\019\000\108\000\023\000\108\000\024\000\108\000\
\\026\000\108\000\000\000\
\\001\000\001\000\109\000\002\000\109\000\003\000\109\000\004\000\109\000\
\\006\000\109\000\007\000\109\000\008\000\109\000\009\000\109\000\
\\010\000\109\000\011\000\109\000\012\000\109\000\014\000\109\000\
\\017\000\109\000\019\000\109\000\023\000\109\000\024\000\109\000\
\\026\000\109\000\000\000\
\\001\000\001\000\110\000\002\000\110\000\003\000\110\000\004\000\110\000\
\\006\000\110\000\007\000\110\000\008\000\110\000\009\000\110\000\
\\010\000\110\000\011\000\110\000\012\000\110\000\014\000\110\000\
\\017\000\110\000\019\000\110\000\023\000\110\000\024\000\110\000\
\\026\000\110\000\000\000\
\\001\000\001\000\111\000\002\000\111\000\003\000\111\000\004\000\111\000\
\\006\000\111\000\007\000\111\000\008\000\111\000\009\000\111\000\
\\010\000\111\000\011\000\111\000\012\000\111\000\014\000\111\000\
\\017\000\111\000\019\000\111\000\023\000\111\000\024\000\111\000\
\\026\000\111\000\000\000\
\\001\000\001\000\112\000\002\000\112\000\003\000\112\000\004\000\112\000\
\\006\000\112\000\007\000\112\000\008\000\112\000\009\000\112\000\
\\010\000\112\000\011\000\112\000\012\000\112\000\014\000\112\000\
\\017\000\112\000\019\000\112\000\023\000\112\000\024\000\112\000\
\\026\000\112\000\000\000\
\\001\000\001\000\113\000\002\000\113\000\003\000\113\000\004\000\113\000\
\\006\000\113\000\007\000\113\000\008\000\113\000\009\000\113\000\
\\010\000\113\000\011\000\113\000\012\000\113\000\014\000\113\000\
\\017\000\113\000\019\000\113\000\023\000\113\000\024\000\113\000\
\\026\000\113\000\000\000\
\\001\000\001\000\114\000\002\000\114\000\003\000\114\000\004\000\114\000\
\\006\000\114\000\007\000\114\000\008\000\114\000\009\000\114\000\
\\010\000\114\000\011\000\114\000\012\000\114\000\014\000\114\000\
\\017\000\114\000\019\000\114\000\023\000\114\000\024\000\114\000\
\\026\000\114\000\000\000\
\\001\000\001\000\115\000\002\000\115\000\003\000\115\000\004\000\115\000\
\\006\000\115\000\007\000\115\000\008\000\115\000\009\000\115\000\
\\010\000\115\000\011\000\115\000\012\000\115\000\014\000\115\000\
\\017\000\115\000\019\000\115\000\023\000\115\000\024\000\115\000\
\\026\000\115\000\000\000\
\\001\000\001\000\116\000\002\000\116\000\003\000\116\000\004\000\116\000\
\\006\000\116\000\007\000\116\000\008\000\116\000\009\000\116\000\
\\010\000\116\000\011\000\116\000\012\000\116\000\014\000\116\000\
\\017\000\116\000\019\000\116\000\023\000\116\000\024\000\116\000\
\\026\000\116\000\000\000\
\\001\000\001\000\117\000\002\000\117\000\004\000\117\000\007\000\117\000\
\\026\000\117\000\000\000\
\\001\000\001\000\118\000\002\000\118\000\003\000\026\000\004\000\118\000\
\\005\000\025\000\006\000\024\000\007\000\118\000\008\000\023\000\
\\009\000\022\000\010\000\021\000\011\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\015\000\016\000\016\000\015\000\
\\017\000\014\000\018\000\013\000\019\000\012\000\020\000\011\000\
\\021\000\010\000\022\000\009\000\023\000\008\000\024\000\007\000\
\\025\000\006\000\000\000\
\\001\000\001\000\118\000\002\000\118\000\003\000\026\000\004\000\118\000\
\\005\000\025\000\006\000\024\000\007\000\118\000\008\000\023\000\
\\009\000\022\000\010\000\021\000\011\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\015\000\016\000\016\000\015\000\
\\017\000\014\000\018\000\013\000\019\000\012\000\020\000\011\000\
\\021\000\010\000\022\000\009\000\023\000\008\000\024\000\007\000\
\\025\000\006\000\026\000\118\000\000\000\
\\001\000\001\000\118\000\002\000\118\000\003\000\026\000\004\000\118\000\
\\005\000\025\000\006\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\014\000\017\000\015\000\016\000\016\000\015\000\017\000\014\000\
\\018\000\013\000\019\000\012\000\020\000\011\000\021\000\010\000\
\\022\000\009\000\023\000\008\000\024\000\007\000\025\000\006\000\
\\026\000\118\000\000\000\
\\001\000\001\000\119\000\002\000\119\000\003\000\119\000\004\000\119\000\
\\005\000\119\000\006\000\119\000\007\000\119\000\008\000\119\000\
\\009\000\119\000\010\000\119\000\011\000\119\000\012\000\119\000\
\\013\000\119\000\014\000\119\000\015\000\119\000\016\000\119\000\
\\017\000\119\000\018\000\119\000\019\000\119\000\020\000\119\000\
\\021\000\119\000\022\000\119\000\023\000\119\000\024\000\119\000\
\\025\000\119\000\026\000\119\000\000\000\
\\001\000\001\000\120\000\002\000\120\000\003\000\120\000\004\000\120\000\
\\005\000\120\000\006\000\120\000\007\000\120\000\008\000\120\000\
\\009\000\120\000\010\000\120\000\011\000\120\000\012\000\120\000\
\\013\000\120\000\014\000\120\000\015\000\120\000\016\000\120\000\
\\017\000\120\000\018\000\120\000\019\000\120\000\020\000\120\000\
\\021\000\120\000\022\000\120\000\023\000\120\000\024\000\120\000\
\\025\000\120\000\026\000\120\000\000\000\
\\001\000\001\000\121\000\002\000\121\000\003\000\121\000\004\000\121\000\
\\005\000\121\000\006\000\121\000\007\000\121\000\008\000\121\000\
\\009\000\121\000\010\000\121\000\011\000\121\000\012\000\121\000\
\\013\000\121\000\014\000\121\000\015\000\121\000\016\000\121\000\
\\017\000\121\000\018\000\121\000\019\000\121\000\020\000\121\000\
\\021\000\121\000\022\000\121\000\023\000\121\000\024\000\121\000\
\\025\000\121\000\026\000\121\000\000\000\
\\001\000\001\000\122\000\002\000\122\000\003\000\122\000\004\000\122\000\
\\005\000\122\000\006\000\122\000\007\000\122\000\008\000\122\000\
\\009\000\122\000\010\000\122\000\011\000\122\000\012\000\122\000\
\\013\000\122\000\014\000\122\000\015\000\122\000\016\000\122\000\
\\017\000\122\000\018\000\122\000\019\000\122\000\020\000\122\000\
\\021\000\122\000\022\000\122\000\023\000\122\000\024\000\122\000\
\\025\000\122\000\026\000\122\000\000\000\
\\001\000\001\000\123\000\002\000\123\000\003\000\123\000\004\000\123\000\
\\005\000\123\000\006\000\123\000\007\000\123\000\008\000\123\000\
\\009\000\123\000\010\000\123\000\011\000\123\000\012\000\123\000\
\\013\000\123\000\014\000\123\000\015\000\123\000\016\000\123\000\
\\017\000\123\000\018\000\123\000\019\000\123\000\020\000\123\000\
\\021\000\123\000\022\000\123\000\023\000\123\000\024\000\123\000\
\\025\000\123\000\026\000\123\000\000\000\
\\001\000\001\000\124\000\002\000\124\000\003\000\124\000\004\000\124\000\
\\005\000\124\000\006\000\124\000\007\000\124\000\008\000\124\000\
\\009\000\124\000\010\000\124\000\011\000\124\000\012\000\124\000\
\\013\000\124\000\014\000\124\000\015\000\124\000\016\000\124\000\
\\017\000\124\000\018\000\124\000\019\000\124\000\020\000\124\000\
\\021\000\124\000\022\000\124\000\023\000\124\000\024\000\124\000\
\\025\000\124\000\026\000\124\000\000\000\
\\001\000\001\000\125\000\002\000\125\000\003\000\125\000\004\000\125\000\
\\005\000\125\000\006\000\125\000\007\000\125\000\008\000\125\000\
\\009\000\125\000\010\000\125\000\011\000\125\000\012\000\125\000\
\\013\000\125\000\014\000\125\000\015\000\125\000\016\000\125\000\
\\017\000\125\000\018\000\125\000\019\000\125\000\020\000\125\000\
\\021\000\125\000\022\000\125\000\023\000\125\000\024\000\125\000\
\\025\000\125\000\026\000\125\000\000\000\
\\001\000\001\000\126\000\002\000\126\000\003\000\126\000\004\000\126\000\
\\005\000\126\000\006\000\126\000\007\000\126\000\008\000\126\000\
\\009\000\126\000\010\000\126\000\011\000\126\000\012\000\126\000\
\\013\000\126\000\014\000\126\000\015\000\126\000\016\000\126\000\
\\017\000\126\000\018\000\126\000\019\000\126\000\020\000\126\000\
\\021\000\126\000\022\000\126\000\023\000\126\000\024\000\126\000\
\\025\000\126\000\026\000\126\000\000\000\
\\001\000\001\000\127\000\002\000\127\000\003\000\127\000\004\000\127\000\
\\005\000\127\000\006\000\127\000\007\000\127\000\008\000\127\000\
\\009\000\127\000\010\000\127\000\011\000\127\000\012\000\127\000\
\\013\000\127\000\014\000\127\000\015\000\127\000\016\000\127\000\
\\017\000\127\000\018\000\127\000\019\000\127\000\020\000\127\000\
\\021\000\127\000\022\000\127\000\023\000\127\000\024\000\127\000\
\\025\000\127\000\026\000\127\000\000\000\
\\001\000\001\000\128\000\002\000\128\000\003\000\128\000\004\000\128\000\
\\005\000\128\000\006\000\128\000\007\000\128\000\008\000\128\000\
\\009\000\128\000\010\000\128\000\011\000\128\000\012\000\128\000\
\\013\000\128\000\014\000\128\000\015\000\128\000\016\000\128\000\
\\017\000\128\000\018\000\128\000\019\000\128\000\020\000\128\000\
\\021\000\128\000\022\000\128\000\023\000\128\000\024\000\128\000\
\\025\000\128\000\026\000\128\000\000\000\
\\001\000\001\000\129\000\002\000\129\000\003\000\129\000\004\000\129\000\
\\005\000\129\000\006\000\129\000\007\000\129\000\008\000\129\000\
\\009\000\129\000\010\000\129\000\011\000\129\000\012\000\129\000\
\\013\000\129\000\014\000\129\000\015\000\129\000\016\000\129\000\
\\017\000\129\000\018\000\129\000\019\000\129\000\020\000\129\000\
\\021\000\129\000\022\000\129\000\023\000\129\000\024\000\129\000\
\\025\000\129\000\026\000\129\000\000\000\
\\001\000\001\000\130\000\002\000\130\000\003\000\130\000\004\000\130\000\
\\005\000\130\000\006\000\130\000\007\000\130\000\008\000\130\000\
\\009\000\130\000\010\000\130\000\011\000\130\000\012\000\130\000\
\\013\000\130\000\014\000\130\000\015\000\130\000\016\000\130\000\
\\017\000\130\000\018\000\130\000\019\000\130\000\020\000\130\000\
\\021\000\130\000\022\000\130\000\023\000\130\000\024\000\130\000\
\\025\000\130\000\026\000\130\000\000\000\
\\001\000\001\000\131\000\002\000\131\000\003\000\131\000\004\000\131\000\
\\005\000\131\000\006\000\131\000\007\000\131\000\008\000\131\000\
\\009\000\131\000\010\000\131\000\011\000\131\000\012\000\131\000\
\\013\000\131\000\014\000\131\000\015\000\131\000\016\000\131\000\
\\017\000\131\000\018\000\131\000\019\000\131\000\020\000\131\000\
\\021\000\131\000\022\000\131\000\023\000\131\000\024\000\131\000\
\\025\000\131\000\026\000\131\000\000\000\
\\001\000\001\000\132\000\002\000\132\000\003\000\132\000\004\000\132\000\
\\005\000\132\000\006\000\132\000\007\000\132\000\008\000\132\000\
\\009\000\132\000\010\000\132\000\011\000\132\000\012\000\132\000\
\\013\000\132\000\014\000\132\000\015\000\132\000\016\000\132\000\
\\017\000\132\000\018\000\132\000\019\000\132\000\020\000\132\000\
\\021\000\132\000\022\000\132\000\023\000\132\000\024\000\132\000\
\\025\000\132\000\026\000\132\000\000\000\
\\001\000\001\000\133\000\002\000\133\000\003\000\133\000\004\000\133\000\
\\005\000\133\000\006\000\133\000\007\000\133\000\008\000\133\000\
\\009\000\133\000\010\000\133\000\011\000\133\000\012\000\133\000\
\\013\000\133\000\014\000\133\000\015\000\133\000\016\000\133\000\
\\017\000\133\000\018\000\133\000\019\000\133\000\020\000\133\000\
\\021\000\133\000\022\000\133\000\023\000\133\000\024\000\133\000\
\\025\000\133\000\026\000\133\000\000\000\
\\001\000\001\000\134\000\002\000\134\000\003\000\134\000\004\000\134\000\
\\005\000\134\000\006\000\134\000\007\000\134\000\008\000\134\000\
\\009\000\134\000\010\000\134\000\011\000\134\000\012\000\134\000\
\\013\000\134\000\014\000\134\000\015\000\134\000\016\000\134\000\
\\017\000\134\000\018\000\134\000\019\000\134\000\020\000\134\000\
\\021\000\134\000\022\000\134\000\023\000\134\000\024\000\134\000\
\\025\000\134\000\026\000\134\000\000\000\
\\001\000\001\000\135\000\002\000\135\000\003\000\135\000\004\000\135\000\
\\005\000\135\000\006\000\135\000\007\000\135\000\008\000\135\000\
\\009\000\135\000\010\000\135\000\011\000\135\000\012\000\135\000\
\\013\000\135\000\014\000\135\000\015\000\135\000\016\000\135\000\
\\017\000\135\000\018\000\135\000\019\000\135\000\020\000\135\000\
\\021\000\135\000\022\000\135\000\023\000\135\000\024\000\135\000\
\\025\000\135\000\026\000\135\000\000\000\
\\001\000\001\000\136\000\002\000\136\000\003\000\136\000\004\000\136\000\
\\005\000\136\000\006\000\136\000\007\000\136\000\008\000\136\000\
\\009\000\136\000\010\000\136\000\011\000\136\000\012\000\136\000\
\\013\000\136\000\014\000\136\000\015\000\136\000\016\000\136\000\
\\017\000\136\000\018\000\136\000\019\000\136\000\020\000\136\000\
\\021\000\136\000\022\000\136\000\023\000\136\000\024\000\136\000\
\\025\000\136\000\026\000\136\000\000\000\
\\001\000\001\000\137\000\002\000\137\000\003\000\137\000\004\000\137\000\
\\005\000\137\000\006\000\137\000\007\000\137\000\008\000\137\000\
\\009\000\137\000\010\000\137\000\011\000\137\000\012\000\137\000\
\\013\000\137\000\014\000\137\000\015\000\137\000\016\000\137\000\
\\017\000\137\000\018\000\137\000\019\000\137\000\020\000\137\000\
\\021\000\137\000\022\000\137\000\023\000\137\000\024\000\137\000\
\\025\000\137\000\026\000\137\000\000\000\
\\001\000\001\000\138\000\002\000\138\000\003\000\138\000\004\000\138\000\
\\005\000\138\000\006\000\138\000\007\000\138\000\008\000\138\000\
\\009\000\138\000\010\000\138\000\011\000\138\000\012\000\138\000\
\\013\000\138\000\014\000\138\000\015\000\138\000\016\000\138\000\
\\017\000\138\000\018\000\138\000\019\000\138\000\020\000\138\000\
\\021\000\138\000\022\000\138\000\023\000\138\000\024\000\138\000\
\\025\000\138\000\026\000\138\000\000\000\
\\001\000\001\000\139\000\002\000\139\000\003\000\139\000\004\000\139\000\
\\005\000\139\000\006\000\139\000\007\000\139\000\008\000\139\000\
\\009\000\139\000\010\000\139\000\011\000\139\000\012\000\139\000\
\\013\000\139\000\014\000\139\000\015\000\139\000\016\000\139\000\
\\017\000\139\000\018\000\139\000\019\000\139\000\020\000\139\000\
\\021\000\139\000\022\000\139\000\023\000\139\000\024\000\139\000\
\\025\000\139\000\026\000\139\000\000\000\
\\001\000\001\000\032\000\002\000\031\000\004\000\030\000\007\000\088\000\
\\026\000\088\000\000\000\
\\001\000\001\000\032\000\002\000\031\000\004\000\030\000\007\000\090\000\
\\026\000\090\000\000\000\
\\001\000\001\000\032\000\002\000\031\000\004\000\030\000\007\000\081\000\000\000\
\\001\000\005\000\041\000\000\000\
\\001\000\005\000\042\000\000\000\
\\001\000\006\000\052\000\014\000\051\000\019\000\050\000\023\000\049\000\
\\024\000\048\000\000\000\
\\001\000\006\000\067\000\000\000\
\\001\000\007\000\087\000\026\000\087\000\000\000\
\\001\000\007\000\089\000\026\000\089\000\000\000\
\\001\000\007\000\040\000\000\000\
\\001\000\007\000\074\000\000\000\
\\001\000\007\000\083\000\000\000\
\\001\000\011\000\054\000\000\000\
\\001\000\013\000\064\000\023\000\063\000\000\000\
\\001\000\015\000\072\000\000\000\
\\001\000\015\000\079\000\000\000\
\\001\000\016\000\078\000\018\000\077\000\000\000\
\\001\000\018\000\084\000\000\000\
\\001\000\021\000\069\000\000\000\
\\001\000\021\000\082\000\000\000\
\\001\000\023\000\073\000\000\000\
\\001\000\024\000\037\000\000\000\
\\001\000\024\000\039\000\000\000\
\\001\000\026\000\000\000\000\000\
\\001\000\026\000\086\000\000\000\
\"
val actionRowNumbers =
"\029\000\028\000\051\000\075\000\
\\050\000\049\000\048\000\047\000\
\\046\000\045\000\044\000\042\000\
\\041\000\040\000\039\000\038\000\
\\037\000\036\000\035\000\034\000\
\\033\000\032\000\027\000\031\000\
\\030\000\026\000\052\000\058\000\
\\028\000\072\000\073\000\060\000\
\\059\000\002\000\001\000\054\000\
\\000\000\055\000\043\000\056\000\
\\056\000\063\000\012\000\013\000\
\\008\000\006\000\020\000\019\000\
\\016\000\064\000\056\000\004\000\
\\057\000\056\000\014\000\069\000\
\\023\000\022\000\021\000\056\000\
\\072\000\065\000\071\000\061\000\
\\073\000\027\000\011\000\067\000\
\\007\000\005\000\017\000\066\000\
\\015\000\003\000\053\000\024\000\
\\070\000\018\000\062\000\010\000\
\\068\000\009\000\025\000\074\000"
val gotoT =
"\
\\001\000\083\000\002\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\012\000\025\000\013\000\001\000\000\000\
\\003\000\027\000\004\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\031\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\032\000\004\000\026\000\000\000\
\\000\000\
\\012\000\033\000\013\000\001\000\000\000\
\\006\000\034\000\000\000\
\\005\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\045\000\008\000\044\000\009\000\043\000\010\000\042\000\
\\011\000\041\000\000\000\
\\009\000\043\000\010\000\042\000\011\000\051\000\000\000\
\\000\000\
\\000\000\
\\009\000\043\000\010\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\043\000\010\000\042\000\011\000\063\000\000\000\
\\000\000\
\\000\000\
\\009\000\043\000\010\000\042\000\011\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\068\000\008\000\044\000\009\000\043\000\010\000\042\000\
\\011\000\041\000\000\000\
\\006\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\073\000\000\000\
\\012\000\074\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\078\000\004\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 84
val numrules = 54
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | MLTOK of unit ->  (string) | ID of unit ->  (string)
 | STRING of unit ->  (string) | REAL of unit ->  (real)
 | NUM of unit ->  (int) | MLOP of unit ->  (string)
 | MLKEY of unit ->  (string) | atexp of unit ->  (atexp)
 | explist of unit ->  (atexp list) | regexp'' of unit ->  (regexp)
 | regexp' of unit ->  (regexp) | regexp of unit ->  (regexp)
 | lrule of unit ->  (lrule) | lmatch of unit ->  (lrule list)
 | lexbind of unit ->  (lexbind list)
 | regbind of unit ->  (regbind list) | lexdec of unit ->  (lex)
 | lexdec' of unit ->  (lex list) | program of unit ->  (lex list)
 | start of unit ->  (lex list)
end
type svalue = MlyValue.svalue
type result = lex list
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 25) => true | _ => false
val showTerminal =
fn (T 0) => "REGEXP"
  | (T 1) => "LEXER"
  | (T 2) => "AND"
  | (T 3) => "MLKEY"
  | (T 4) => "EQ"
  | (T 5) => "LPAR"
  | (T 6) => "RPAR"
  | (T 7) => "TIMES"
  | (T 8) => "PLUS"
  | (T 9) => "QMARK"
  | (T 10) => "DRARROW"
  | (T 11) => "BAR"
  | (T 12) => "CARAT"
  | (T 13) => "LBRACK"
  | (T 14) => "RBRACK"
  | (T 15) => "COMMA"
  | (T 16) => "LBRACE"
  | (T 17) => "RBRACE"
  | (T 18) => "WILDCARD"
  | (T 19) => "MLOP"
  | (T 20) => "NUM"
  | (T 21) => "REAL"
  | (T 22) => "STRING"
  | (T 23) => "ID"
  | (T 24) => "MLTOK"
  | (T 25) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 4) :: (T 5) :: (T 6) :: (T 7
) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: (T 14)
 :: (T 15) :: (T 16) :: (T 17) :: (T 18) :: (T 25) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.program program1,program1left,program1right))::
rest671) => let val result=MlyValue.start(fn _ => let val program as 
program1=program1 ()
 in ( program ) end
)
 in (LrTable.NT 0,(result,program1left,program1right),rest671) end
| (1,(_,(MlyValue.lexdec' lexdec'1,_,lexdec'1right))::(_,(
MlyValue.explist explist1,explistleft as explist1left,explistright))::
rest671) => let val result=MlyValue.program(fn _ => let val explist
 as explist1=explist1 ()
val lexdec' as lexdec'1=lexdec'1 ()
 in (
 (SML (EXP (explist, (explistleft, explistright ) ), (explistleft, explistright) ) :: lexdec') 
) end
)
 in (LrTable.NT 1,(result,explist1left,lexdec'1right),rest671) end
| (2,(_,(MlyValue.explist explist1,explistleft as explist1left,
explistright as explist1right))::rest671) => let val result=
MlyValue.program(fn _ => let val explist as explist1=explist1 ()
 in (
 [SML (EXP (explist, (explistleft, explistright) ), (explistleft, explistright) )] 
) end
)
 in (LrTable.NT 1,(result,explist1left,explist1right),rest671) end
| (3,(_,(MlyValue.lexdec' lexdec'1,_,lexdec'1right))::(_,(
MlyValue.lexdec lexdec1,lexdec1left,_))::rest671) => let val result=
MlyValue.lexdec'(fn _ => let val lexdec as lexdec1=lexdec1 ()
val lexdec' as lexdec'1=lexdec'1 ()
 in ( lexdec::lexdec' ) end
)
 in (LrTable.NT 2,(result,lexdec1left,lexdec'1right),rest671) end
| (4,(_,(MlyValue.lexdec lexdec1,lexdec1left,lexdec1right))::rest671)
 => let val result=MlyValue.lexdec'(fn _ => let val lexdec as lexdec1=
lexdec1 ()
 in ( [lexdec] ) end
)
 in (LrTable.NT 2,(result,lexdec1left,lexdec1right),rest671) end
| (5,(_,(MlyValue.regbind regbind1,_,regbindright as regbind1right))::
(_,(_,REGEXPleft as REGEXP1left,_))::rest671) => let val result=
MlyValue.lexdec(fn _ => let val regbind as regbind1=regbind1 ()
 in ( REG (regbind, (REGEXPleft, regbindright) ) ) end
)
 in (LrTable.NT 3,(result,REGEXP1left,regbind1right),rest671) end
| (6,(_,(MlyValue.lexbind lexbind1,_,lexbindright as lexbind1right))::
(_,(_,LEXERleft as LEXER1left,_))::rest671) => let val result=
MlyValue.lexdec(fn _ => let val lexbind as lexbind1=lexbind1 ()
 in ( LEX (lexbind, (LEXERleft, lexbindright) ) ) end
)
 in (LrTable.NT 3,(result,LEXER1left,lexbind1right),rest671) end
| (7,(_,(MlyValue.explist explist1,_,explistright as explist1right))::
(_,(MlyValue.MLKEY MLKEY1,MLKEYleft as MLKEY1left,MLKEYright))::
rest671) => let val result=MlyValue.lexdec(fn _ => let val MLKEY as 
MLKEY1=MLKEY1 ()
val explist as explist1=explist1 ()
 in (
 SML (EXP (ATEXP (MLKEY, (MLKEYleft, MLKEYright) )::explist, (MLKEYleft, explistright) ), (MLKEYleft, explistright) ) 
) end
)
 in (LrTable.NT 3,(result,MLKEY1left,explist1right),rest671) end
| (8,(_,(MlyValue.regbind regbind1,_,regbind1right))::_::(_,(
MlyValue.regexp'' regexp''1,_,regexp''right))::_::(_,(MlyValue.ID ID1,
IDleft as ID1left,_))::rest671) => let val result=MlyValue.regbind(fn 
_ => let val ID as ID1=ID1 ()
val regexp'' as regexp''1=regexp''1 ()
val regbind as regbind1=regbind1 ()
 in ( REGBIND (ID,regexp'', (IDleft, regexp''right) )::regbind ) end
)
 in (LrTable.NT 4,(result,ID1left,regbind1right),rest671) end
| (9,(_,(MlyValue.regexp'' regexp''1,_,regexp''right as regexp''1right
))::_::(_,(MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val 
result=MlyValue.regbind(fn _ => let val ID as ID1=ID1 ()
val regexp'' as regexp''1=regexp''1 ()
 in ( [REGBIND (ID,regexp'', (IDleft, regexp''right) )] ) end
)
 in (LrTable.NT 4,(result,ID1left,regexp''1right),rest671) end
| (10,(_,(MlyValue.lexbind lexbind1,_,lexbind1right))::_::(_,(
MlyValue.lmatch lmatch1,lmatchleft,lmatchright))::_::(_,(MlyValue.ID 
ID1,IDleft as ID1left,_))::rest671) => let val result=MlyValue.lexbind
(fn _ => let val ID as ID1=ID1 ()
val lmatch as lmatch1=lmatch1 ()
val lexbind as lexbind1=lexbind1 ()
 in (
 LEXBIND (ID,LMATCH (lmatch, (lmatchleft, lmatchright) ), (IDleft, lmatchright) )::lexbind 
) end
)
 in (LrTable.NT 5,(result,ID1left,lexbind1right),rest671) end
| (11,(_,(MlyValue.lmatch lmatch1,lmatchleft,lmatchright as 
lmatch1right))::_::(_,(MlyValue.ID ID1,IDleft as ID1left,_))::rest671)
 => let val result=MlyValue.lexbind(fn _ => let val ID as ID1=ID1 ()
val lmatch as lmatch1=lmatch1 ()
 in (
 [LEXBIND (ID,LMATCH (lmatch, (lmatchleft, lmatchright) ), (IDleft, lmatchright) )] 
) end
)
 in (LrTable.NT 5,(result,ID1left,lmatch1right),rest671) end
| (12,(_,(MlyValue.lmatch lmatch1,_,lmatch1right))::_::(_,(
MlyValue.lrule lrule1,lrule1left,_))::rest671) => let val result=
MlyValue.lmatch(fn _ => let val lrule as lrule1=lrule1 ()
val lmatch as lmatch1=lmatch1 ()
 in ( (lrule::lmatch) ) end
)
 in (LrTable.NT 6,(result,lrule1left,lmatch1right),rest671) end
| (13,(_,(MlyValue.lrule lrule1,lrule1left,lrule1right))::rest671) => 
let val result=MlyValue.lmatch(fn _ => let val lrule as lrule1=lrule1 
()
 in ( [lrule] ) end
)
 in (LrTable.NT 6,(result,lrule1left,lrule1right),rest671) end
| (14,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.lexdec' lexdec'1
,_,_))::(_,(MlyValue.explist explist1,explistleft,explistright))::(_,(
_,LPARleft,_))::_::(_,(MlyValue.regexp'' regexp''1,regexp''left as 
regexp''1left,_))::rest671) => let val result=MlyValue.lrule(fn _ => 
let val regexp'' as regexp''1=regexp''1 ()
val explist as explist1=explist1 ()
val lexdec' as lexdec'1=lexdec'1 ()
 in (
 LRULE (regexp'',
	 PAREXP (SML (EXP (explist, (explistleft, explistright) ), (explistleft, explistright) )::lexdec', (LPARleft, RPARright) ),
	(regexp''left, RPARright) ) 
) end
)
 in (LrTable.NT 7,(result,regexp''1left,RPAR1right),rest671) end
| (15,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.explist explist1
,explistleft,explistright))::(_,(_,LPARleft,_))::_::(_,(
MlyValue.regexp'' regexp''1,regexp''left as regexp''1left,_))::rest671
) => let val result=MlyValue.lrule(fn _ => let val regexp'' as 
regexp''1=regexp''1 ()
val explist as explist1=explist1 ()
 in (
 LRULE (regexp'',
	 PAREXP ([(SML (EXP (explist, (explistleft, explistright) ), (explistleft, explistright) ) ) ], (LPARleft, RPARright) ),
	(regexp''left, RPARright) ) 
) end
)
 in (LrTable.NT 7,(result,regexp''1left,RPAR1right),rest671) end
| (16,(_,(MlyValue.regexp'' regexp''1,_,regexp''right as 
regexp''1right))::_::(_,(MlyValue.regexp' regexp'1,regexp'left as 
regexp'1left,_))::rest671) => let val result=MlyValue.regexp''(fn _
 => let val regexp' as regexp'1=regexp'1 ()
val regexp'' as regexp''1=regexp''1 ()
 in ( ALT(regexp', regexp'', (regexp'left, regexp''right) ) ) end
)
 in (LrTable.NT 10,(result,regexp'1left,regexp''1right),rest671) end
| (17,(_,(MlyValue.regexp' regexp'1,regexp'1left,regexp'1right))::
rest671) => let val result=MlyValue.regexp''(fn _ => let val regexp'
 as regexp'1=regexp'1 ()
 in ( regexp' ) end
)
 in (LrTable.NT 10,(result,regexp'1left,regexp'1right),rest671) end
| (18,(_,(MlyValue.regexp regexp1,regexp1left,regexp1right))::rest671)
 => let val result=MlyValue.regexp'(fn _ => let val regexp as regexp1=
regexp1 ()
 in ( regexp ) end
)
 in (LrTable.NT 9,(result,regexp1left,regexp1right),rest671) end
| (19,(_,(MlyValue.regexp' regexp'1,_,regexp'right as regexp'1right))
::(_,(MlyValue.regexp regexp1,regexpleft as regexp1left,_))::rest671)
 => let val result=MlyValue.regexp'(fn _ => let val regexp as regexp1=
regexp1 ()
val regexp' as regexp'1=regexp'1 ()
 in ( CAT(regexp, regexp', (regexpleft, regexp'right) ) ) end
)
 in (LrTable.NT 9,(result,regexp1left,regexp'1right),rest671) end
| (20,(_,(_,_,RPAR1right))::(_,(MlyValue.regexp'' regexp''1,_,_))::(_,
(_,LPAR1left,_))::rest671) => let val result=MlyValue.regexp(fn _ => 
let val regexp'' as regexp''1=regexp''1 ()
 in ( regexp'' ) end
)
 in (LrTable.NT 8,(result,LPAR1left,RPAR1right),rest671) end
| (21,(_,(_,WILDCARDleft as WILDCARD1left,WILDCARDright as 
WILDCARD1right))::rest671) => let val result=MlyValue.regexp(fn _ => (
 CHARS(wildcard(), 0, (WILDCARDleft, WILDCARDright) ) ))
 in (LrTable.NT 8,(result,WILDCARD1left,WILDCARD1right),rest671) end
| (22,(_,(_,_,RBRACKright as RBRACK1right))::(_,(MlyValue.STRING 
STRING1,_,_))::(_,(_,LBRACKleft as LBRACK1left,_))::rest671) => let 
val result=MlyValue.regexp(fn _ => let val STRING as STRING1=STRING1 
()
 in (
 CHARS(makeVector(makeArray(STRING, false, (LBRACKleft, RBRACKright) ) ), 0, (LBRACKleft, RBRACKright) ) 
) end
)
 in (LrTable.NT 8,(result,LBRACK1left,RBRACK1right),rest671) end
| (23,(_,(_,_,RBRACKright as RBRACK1right))::(_,(MlyValue.STRING 
STRING1,_,_))::_::(_,(_,LBRACKleft as LBRACK1left,_))::rest671) => 
let val result=MlyValue.regexp(fn _ => let val STRING as STRING1=
STRING1 ()
 in (
 CHARS(makeVector(makeArray(STRING, true, (LBRACKleft, RBRACKright) ) ), 0, (LBRACKleft, RBRACKright) ) 
) end
)
 in (LrTable.NT 8,(result,LBRACK1left,RBRACK1right),rest671) end
| (24,(_,(MlyValue.STRING STRING1,STRINGleft as STRING1left,
STRINGright as STRING1right))::rest671) => let val result=
MlyValue.regexp(fn _ => let val STRING as STRING1=STRING1 ()
 in ( stringToExp(STRING, (STRINGleft, STRINGright) ) ) end
)
 in (LrTable.NT 8,(result,STRING1left,STRING1right),rest671) end
| (25,(_,(MlyValue.ID ID1,IDleft as ID1left,IDright as ID1right))::
rest671) => let val result=MlyValue.regexp(fn _ => let val ID as ID1=
ID1 ()
 in ( REGID (ID, (IDleft, IDright) ) ) end
)
 in (LrTable.NT 8,(result,ID1left,ID1right),rest671) end
| (26,(_,(_,_,TIMESright as TIMES1right))::(_,(MlyValue.regexp regexp1
,regexpleft as regexp1left,_))::rest671) => let val result=
MlyValue.regexp(fn _ => let val regexp as regexp1=regexp1 ()
 in ( CLOSURE(regexp, (regexpleft, TIMESright) ) ) end
)
 in (LrTable.NT 8,(result,regexp1left,TIMES1right),rest671) end
| (27,(_,(_,_,PLUSright as PLUS1right))::(_,(MlyValue.regexp regexp1,
regexpleft as regexp1left,_))::rest671) => let val result=
MlyValue.regexp(fn _ => let val regexp as regexp1=regexp1 ()
 in (
 CAT(regexp, CLOSURE(regexp, (regexpleft, PLUSright) ), (regexpleft, PLUSright) ) 
) end
)
 in (LrTable.NT 8,(result,regexp1left,PLUS1right),rest671) end
| (28,(_,(_,_,QMARKright as QMARK1right))::(_,(MlyValue.regexp regexp1
,regexpleft as regexp1left,_))::rest671) => let val result=
MlyValue.regexp(fn _ => let val regexp as regexp1=regexp1 ()
 in ( ALT(EPS, regexp, (regexpleft, QMARKright) ) ) end
)
 in (LrTable.NT 8,(result,regexp1left,QMARK1right),rest671) end
| (29,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.NUM NUM1,_,_
))::_::(_,(MlyValue.regexp regexp1,regexpleft as regexp1left,_))::
rest671) => let val result=MlyValue.regexp(fn _ => let val regexp as 
regexp1=regexp1 ()
val NUM as NUM1=NUM1 ()
 in ( makeRep(regexp, NUM, NUM, (regexpleft, RBRACEright) ) ) end
)
 in (LrTable.NT 8,(result,regexp1left,RBRACE1right),rest671) end
| (30,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.NUM NUM2,_,_
))::_::(_,(MlyValue.NUM NUM1,_,_))::_::(_,(MlyValue.regexp regexp1,
regexpleft as regexp1left,_))::rest671) => let val result=
MlyValue.regexp(fn _ => let val regexp as regexp1=regexp1 ()
val NUM1=NUM1 ()
val NUM2=NUM2 ()
 in ( makeRep(regexp, NUM1, NUM2, (regexpleft, RBRACEright) ) ) end
)
 in (LrTable.NT 8,(result,regexp1left,RBRACE1right),rest671) end
| (31,(_,(MlyValue.explist explist1,_,explist1right))::(_,(
MlyValue.atexp atexp1,atexp1left,_))::rest671) => let val result=
MlyValue.explist(fn _ => let val atexp as atexp1=atexp1 ()
val explist as explist1=explist1 ()
 in ( atexp::explist ) end
)
 in (LrTable.NT 11,(result,atexp1left,explist1right),rest671) end
| (32,rest671) => let val result=MlyValue.explist(fn _ => ( [] ))
 in (LrTable.NT 11,(result,defaultPos,defaultPos),rest671) end
| (33,(_,(_,ANDleft as AND1left,ANDright as AND1right))::rest671) => 
let val result=MlyValue.atexp(fn _ => (
 ATEXP ("and", (ANDleft, ANDright) ) ))
 in (LrTable.NT 12,(result,AND1left,AND1right),rest671) end
| (34,(_,(_,EQleft as EQ1left,EQright as EQ1right))::rest671) => let 
val result=MlyValue.atexp(fn _ => ( ATEXP ("=", (EQleft, EQright) ) ))
 in (LrTable.NT 12,(result,EQ1left,EQ1right),rest671) end
| (35,(_,(_,TIMESleft as TIMES1left,TIMESright as TIMES1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("*", (TIMESleft, TIMESright) ) ))
 in (LrTable.NT 12,(result,TIMES1left,TIMES1right),rest671) end
| (36,(_,(_,PLUSleft as PLUS1left,PLUSright as PLUS1right))::rest671)
 => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("+", (PLUSleft, PLUSright) ) ))
 in (LrTable.NT 12,(result,PLUS1left,PLUS1right),rest671) end
| (37,(_,(_,QMARKleft as QMARK1left,QMARKright as QMARK1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("?", (QMARKleft, QMARKright) ) ))
 in (LrTable.NT 12,(result,QMARK1left,QMARK1right),rest671) end
| (38,(_,(_,DRARROWleft as DRARROW1left,DRARROWright as DRARROW1right)
)::rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("=>", (DRARROWleft, DRARROWright) ) ))
 in (LrTable.NT 12,(result,DRARROW1left,DRARROW1right),rest671) end
| (39,(_,(_,BARleft as BAR1left,BARright as BAR1right))::rest671) => 
let val result=MlyValue.atexp(fn _ => (
 ATEXP ("|", (BARleft, BARright) ) ))
 in (LrTable.NT 12,(result,BAR1left,BAR1right),rest671) end
| (40,(_,(_,CARATleft as CARAT1left,CARATright as CARAT1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("^", (CARATleft, CARATright) ) ))
 in (LrTable.NT 12,(result,CARAT1left,CARAT1right),rest671) end
| (41,(_,(_,LBRACKleft as LBRACK1left,LBRACKright as LBRACK1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("[", (LBRACKleft, LBRACKright) ) ))
 in (LrTable.NT 12,(result,LBRACK1left,LBRACK1right),rest671) end
| (42,(_,(_,RBRACKleft as RBRACK1left,RBRACKright as RBRACK1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("]", (RBRACKleft, RBRACKright) ) ))
 in (LrTable.NT 12,(result,RBRACK1left,RBRACK1right),rest671) end
| (43,(_,(_,COMMAleft as COMMA1left,COMMAright as COMMA1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP (",", (COMMAleft, COMMAright) ) ))
 in (LrTable.NT 12,(result,COMMA1left,COMMA1right),rest671) end
| (44,(_,(_,LBRACEleft as LBRACE1left,LBRACEright as LBRACE1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("{", (LBRACEleft, LBRACEright) ) ))
 in (LrTable.NT 12,(result,LBRACE1left,LBRACE1right),rest671) end
| (45,(_,(_,RBRACEleft as RBRACE1left,RBRACEright as RBRACE1right))::
rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("}", (RBRACEleft, RBRACEright) ) ))
 in (LrTable.NT 12,(result,RBRACE1left,RBRACE1right),rest671) end
| (46,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.program program1
,_,_))::(_,(_,LPARleft as LPAR1left,_))::rest671) => let val result=
MlyValue.atexp(fn _ => let val program as program1=program1 ()
 in ( PAREXP (program, (LPARleft, RPARright) ) ) end
)
 in (LrTable.NT 12,(result,LPAR1left,RPAR1right),rest671) end
| (47,(_,(_,WILDCARDleft as WILDCARD1left,WILDCARDright as 
WILDCARD1right))::rest671) => let val result=MlyValue.atexp(fn _ => (
 ATEXP ("_", (WILDCARDleft, WILDCARDright) ) ))
 in (LrTable.NT 12,(result,WILDCARD1left,WILDCARD1right),rest671) end
| (48,(_,(MlyValue.MLOP MLOP1,MLOPleft as MLOP1left,MLOPright as 
MLOP1right))::rest671) => let val result=MlyValue.atexp(fn _ => let 
val MLOP as MLOP1=MLOP1 ()
 in ( ATEXP (MLOP, (MLOPleft, MLOPright) ) ) end
)
 in (LrTable.NT 12,(result,MLOP1left,MLOP1right),rest671) end
| (49,(_,(MlyValue.NUM NUM1,NUMleft as NUM1left,NUMright as NUM1right)
)::rest671) => let val result=MlyValue.atexp(fn _ => let val NUM as 
NUM1=NUM1 ()
 in ( ATEXP (Int.toString NUM, (NUMleft, NUMright) ) ) end
)
 in (LrTable.NT 12,(result,NUM1left,NUM1right),rest671) end
| (50,(_,(MlyValue.REAL REAL1,REALleft as REAL1left,REALright as 
REAL1right))::rest671) => let val result=MlyValue.atexp(fn _ => let 
val REAL as REAL1=REAL1 ()
 in ( ATEXP (Real.toString REAL, (REALleft, REALright) ) ) end
)
 in (LrTable.NT 12,(result,REAL1left,REAL1right),rest671) end
| (51,(_,(MlyValue.STRING STRING1,STRINGleft as STRING1left,
STRINGright as STRING1right))::rest671) => let val result=
MlyValue.atexp(fn _ => let val STRING as STRING1=STRING1 ()
 in ( ATEXP (STRING, (STRINGleft, STRINGright) ) ) end
)
 in (LrTable.NT 12,(result,STRING1left,STRING1right),rest671) end
| (52,(_,(MlyValue.ID ID1,IDleft as ID1left,IDright as ID1right))::
rest671) => let val result=MlyValue.atexp(fn _ => let val ID as ID1=
ID1 ()
 in ( ATEXP (ID, (IDleft, IDright) ) ) end
)
 in (LrTable.NT 12,(result,ID1left,ID1right),rest671) end
| (53,(_,(MlyValue.MLTOK MLTOK1,MLTOKleft as MLTOK1left,MLTOKright as 
MLTOK1right))::rest671) => let val result=MlyValue.atexp(fn _ => let 
val MLTOK as MLTOK1=MLTOK1 ()
 in ( ATEXP (MLTOK, (MLTOKleft, MLTOKright) ) ) end
)
 in (LrTable.NT 12,(result,MLTOK1left,MLTOK1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Hose_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun REGEXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LEXER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MLKEY (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.MLKEY (fn () => i),p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun QMARK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DRARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun WILDCARD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MLOP (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.MLOP (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.REAL (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun MLTOK (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.MLTOK (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
end
end
