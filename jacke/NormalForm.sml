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

    (* introduce toplevel transformations *)
    fun toSeq (A.Seq l) = A.Seq l
      | toSeq b = A.Seq [b]

    fun semAction (A.Transform t) = A.Transform t
      | semAction (A.Prec (bnf,s)) = A.Prec (semAction bnf, s)
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
	let val (bnf',rs) = norm (semAction (nameTopLevel bnf))
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

end
