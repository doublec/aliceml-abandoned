(* datatype and translation functions for normalization of rules *)

structure NormalForm =
struct
    structure A = AbsSyn

    fun mkNewNamefunction s m =
	let val r = ref m 
	in 
	    fn () => (r:=(!r+1); s^"__"^Int.toString(!r-1))^"__"
	end

    val newRulename = mkNewNamefunction "rule" 0
    val newIdname = mkNewNamefunction "id" 0

    local fun split l1 l2 [] = (l1,l2)
	    | split l1 l2 (x::xs) = split l2 (x::l1) xs
	  fun merge l1 [] = l1
	    | merge [] l2 = l2
	    | merge (x::xs :string list) (y::ys) = 
	      if x<y then x::(merge xs (y::ys))
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

    fun names (A.As (s,_)) = [s]
      | names (A.Seq l) = List.concat (List.map names l)
      | names A.Skip = ["()"]
      | names _ = []

    fun tupling l =
	let fun seq [] = ""
	      | seq [n] = n
	      | seq (n::ns) = n^", "^(seq ns)
	in "("^(seq l)^")" end
    
    (* introduce explicit names for toplevel expressions where necessary *)
    fun naming (A.As (a,bnf)) = A.As (a,bnf)
      | naming (A.Symbol s) = A.As (s, A.Symbol s)
      | naming (A.Seq l) = A.Seq (map naming l)
      | naming A.Skip = A.Skip
      | naming bnf = A.As (newIdname(), bnf) 

    fun nameTopLevel (A.Prec (bnf,s)) = A.Prec (nameTopLevel bnf,s)
      | nameTopLevel (A.Transform (bnf,c)) = A.Transform (naming bnf,c)
      | nameTopLevel bnf = naming bnf

    (* remove unnecessary sequencing *)
    fun flattenSeq [] = []
      | flattenSeq ((A.Seq l)::l') =
	let val l = flattenSeq l
	    val l' = flattenSeq l'
	in l@l' end
      | flattenSeq (x::l) = (flatten x)::(flattenSeq l)
    and flatten (A.Seq l) = A.Seq (flattenSeq l)
      | flatten (A.Skip) = A.Skip
      | flatten (A.As (s,bnf)) = A.As (s, flatten bnf)
      | flatten (A.Prec (bnf,s)) = A.Prec (flatten bnf,s)
      | flatten (A.Transform (bnf,c)) = A.Transform (flatten bnf,c)
      | flatten (A.Alt l) = A.Alt (map flatten l)
      | flatten (A.Symbol s) = A.Symbol s

    (* remove unnecessary alternatives *)
    fun flattenAlt [] = []
      | flattenAlt ((A.Alt l)::l') =
	let val l = flattenAlt l
	    val l' = flattenAlt l'
	in l@l' end
      | flattenAlt (x::l) = (flatten1 x)::(flattenAlt l)
    and flatten1 (A.Seq l) = A.Seq (map flatten1 l)
      | flatten1 (A.Skip) = A.Skip
      | flatten1 (A.As (s,bnf)) = A.As (s, flatten1 bnf)
      | flatten1 (A.Prec (bnf,s)) = A.Prec (flatten1 bnf,s)
      | flatten1 (A.Transform (bnf,c)) = A.Transform (flatten1 bnf,c)
      | flatten1 (A.Alt l) = A.Alt (flattenAlt l)
      | flatten1 (A.Symbol s) = A.Symbol s


    (* introduce toplevel transformations *)
    fun toSeq (A.Seq l) = A.Seq l
      | toSeq b = A.Seq [b]

    fun semAction (A.Transform (t,c)) = A.Transform (toSeq t,c)
      | semAction (A.Prec (A.Transform (bnf,c),s)) = 
	A.Prec (A.Transform (toSeq bnf,c), s)
      | semAction (A.Prec (bnf,s)) = 
		A.Prec (A.Transform (toSeq bnf, [tupling(names bnf)]),s)
      | semAction bnf = A.Transform (toSeq bnf, [tupling(names bnf)])
 
    (* normalization *)
    fun norm (A.Prec (bnf,s)) = 
	let val (bnf',rs) = norm bnf
	in 
	    (A.Prec (bnf',s),rs)
	end
      | norm (A.Transform (bnf,c)) = 
	let val (bnf',rs) = norm bnf
	in 
	    (A.Transform (bnf',c),rs)
	end
      | norm (A.Seq bnf) = 
	let val (a,rs) = normalizeSubexps bnf
	in 
	    (A.Seq a, rs)
	end
    and normalize (name,ty,bnf) = 
	let val (bnf',rs) = norm (semAction (nameTopLevel ((flatten (flatten1 bnf)))))
	    in 
		(name,ty,bnf')::rs
	end
    and  normalizeSubexps [] = ([],[])
      | normalizeSubexps ((x as (A.As (s,A.Symbol s')))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	in
	    (x::a,rs)
	end
      | normalizeSubexps (A.Skip::xs) =
	let val (a,rs) =normalizeSubexps xs
	in
	    (A.Skip ::a,rs)
	end
      | normalizeSubexps ((A.As (s,A.Alt l))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	    val r = newRulename ()
	    val r' = List.concat (List.map normalize 
				  (List.map (fn bnf => (r,NONE,bnf)) l))
	in 
	    ((A.As (s,A.Symbol r))::a, r'@rs)
	end
      | normalizeSubexps ((A.As (s,bnf))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	    val r = newRulename ()
	    val r' = normalize (r,NONE,bnf)
	in 
	    ((A.As (s,A.Symbol r))::a, r'@rs)
	end

    fun toNormalForm p =
	let val h = List.concat o (List.map normalize)
	in 
	    List.map (fn A.RuleDec l => A.RuleDec (h l) | x => x) p
	end     
end
