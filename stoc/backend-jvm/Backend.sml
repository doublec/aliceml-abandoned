(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Backend=
    struct
	open Common

	type stamp=IntermediateGrammar.stamp
	type label = JVMInst.label

	(* Hashtable of Stamps. *)
	structure StampHash = MakeHashImpMap(type t=stamp val hash=Stamp.hash)

	(* Hashtable of string lists. Needed for static generation of
	 record arities. *)
	structure StringListHash = MakeHashImpMap(StringListHashKey)

	(* Hashtable of int. Used for labelfusion. *)
	structure IntHash = MakeHashImpMap(type t=int fun hash x = x)

	(* Set of int. Also used for labelfusion. *)
	structure IntSet = MakeHashImpSet(type t=int fun hash x = x)

	(* Hashtable of integers. Needed for static generation of
	 integer constants. *)
	structure LitHash = MakeHashImpMap (LitHashKey)

	(* Sets of Stamps. Used for computation of free variables *)
	structure StampSet = MakeHashImpSet(type t=stamp val hash=Stamp.hash)

	(* Hashtable of (stamp*int) pairs. Used for the optimization of
	 merging several SML functions in one apply when defined as
	 fun ... and *)
	structure StampIntHash = MakeHashImpMap(type t=(stamp*int)
						fun hash (stamp', int') =
						    Stamp.hash stamp' + int')

	(* Set of (stamp*int) pairs. *)
	structure StampIntSet = MakeHashImpSet(type t=(stamp*int)
						fun hash (stamp', int') =
						    Stamp.hash stamp' + int')

	structure Lambda = MakeLambda(structure StampSet=StampSet
				      structure StampHash=StampHash
				      structure StampIntSet=StampIntSet
				      structure StampIntHash=StampIntHash
				      val toplevel=toplevel)

	(* Structure for managing labels in JVM-methods *)
	structure Label =
	    struct
		(* the actual label number *)
		val labelcount = ref (0:label)

		(* Labels are stacked, so each function starts counting at 1. *)
		val stack : (int list) ref= ref nil

		fun new () =
		    (labelcount := !labelcount + 1;
		     !labelcount)

		fun toString lab =
		    "label"^Int.toString lab

		(* For each method, we start counting at 1. *)
		fun push () =
		    (stack := (!labelcount :: (!stack));
		     labelcount := 0)

		fun pop () = (labelcount:=hd(!stack);
				    stack:=tl(!stack))
	    end

	(* Administration of JVM registers *)
	structure Register =
	    struct
		local
		    (* number of the highest register in use.
		     In virtual Methods, Register 0 is reserved for
		     'this'-Pointer. Our apply functions have up to
		     4 parameters, so we use Register 1-4 as parameter
		     registers. Keep in mind that these registers will
		     be reused due lifeness analysis when possible *)
		    val localscount = ref 4

		    (* map stamps to registers *)
		    val register: int StampHash.t ref   = ref (StampHash.new ())

		    (* Registers are stacked for each function. *)
		    val stack:(int * int StampHash.t) list ref = ref nil

		    (* assign stamps to JVM registers where the defining
		     function closure is stored in *)
		    val lambda  : int StampHash.t  = StampHash.new ()

		    (* assign stamps to corresponding field names.
		     This is used for generating debug information in compiled
		     (Jasmin) code *)
		    val fields  : string StampHash.t = StampHash.new ()
		in
		    (* return next free register of this method *)
		    fun new () = (localscount := !localscount + 1;
				       !localscount)

		    (* enter a subfunction *)
		    fun push () = (stack := (!localscount, !register)::(!stack);
				   localscount := 4;
				   register := StampHash.new())

		    (* leave a subfunction*)
		    fun pop () =
			case !stack of
			    ((lc,regs)::rest) => (stack := rest;
						  localscount := lc;
						  register := regs)
			  | nil => Crash.crash "empty locals stack"

		    (* save a former state *)
		    fun save () = (!localscount, !register)

		    (* restore a former state *)
		    fun restore (n, rg) = (localscount := n;
					 register:=rg)

		    (* return the number of the highest register in use *)
		    fun max () = !localscount

		    (* assign ids to JVM registers *)
		    fun assign (Id(_,stamp',InId), wohin) =
			(StampHash.insert (!register, stamp', wohin);
			 wohin)
		      | assign (Id(_,stamp',ExId name), wohin) =
			(StampHash.insert (!register, stamp', wohin);
			 StampHash.insert (fields, stamp', name);
			 wohin)

		    (* return the JVM register of a stamp. *)
		    fun get stamp' =
			case StampHash.lookup (!register, stamp') of
			    NONE => 1 (* unassigned stamps are formal parameters *)
			  | SOME reg => reg

		    (* assign parameters of special apply methods apply2/3/4
		     to registers *)
		    fun assignParms (id'::rest, reg) =
			(assign (id', reg);
			 assignParms (rest, reg+1))
		      | assignParms (nil, reg) = localscount := (reg-1)

		    (* Assign ids to JVM registers where their surrounding
		     function closure can be found. *)
		    (* xxx inzwischen überflüssig ? *)
		    fun assignLambda (Id(_,stamp',_), wohin) =
			(StampHash.insert(lambda,stamp',wohin);
			 wohin)

		    fun getLambda stamp' = (* xxx Unterschied zu Lambda.getLambda? *)
			case StampHash.lookup(lambda,stamp') of
			    NONE => ~1
			  | SOME lambda => lambda

		    (* create some debugging informations *)
		    fun generateVariableTable rest =
			StampHash.foldi
			(fn (stamp',register',rest') =>
			 Comment ("var "^(Int.toString register')^": ("^
			      (case StampHash.lookup(fields, stamp') of
				  NONE => "anonymous, "^(Stamp.toString stamp')
				| SOME x => x^", "^(Stamp.toString stamp'))^")")::
			 rest')
			rest
			(!register)
		end
	    end

	(* structure for free variabes *)
	structure FreeVars =
	    struct
		(* assign (function) ids to free variable list *)
		val free:stamp list StampHash.t=StampHash.new ()

		(* Assign ids to formal parameters of defining functions.
		 Each function has a unique formal parameter (OneArg)
		 while it can have multiple or no name at all *)
		val defFun:stamp StampHash.t=StampHash.new ()

		(* assign stamps to free variables *)
		fun setVars (stamp', freeVarList) =
		    StampHash.insert (free, stamp', freeVarList)

		(* get free variable list of an id. If not set previously,
		 raise exception option. *)
		fun getVars (Id(_,stamp',_)) =
		    valOf (StampHash.lookup (free, stamp'))

		(* assign ids to their  defining function closure. *)
		fun setFun (Id(_,stamp',_), stamp'') =
		    StampHash.insert(defFun, stamp', stamp'')

		(* get the defining function closure of a stamp *)
		fun getFun stamp' =
		    case StampHash.lookup (defFun, stamp') of
			NONE => toplevel
		      | SOME stamp'' => stamp''

		(* print all pairs of stamps and defining function closures.
		 used in verbose level 2. *)
		fun printFun () =
		    StampHash.appi (fn (stamp', stamp'') =>
				    print ("("^Stamp.toString stamp'^","^
					   Stamp.toString stamp''^")"))
		    defFun
	    end

	(* load an integer as JVM integer constant. *)
	fun atCodeInt (i:LargeInt.int) =
	    if LargeInt.>= (i, Int.toLarge ~1) andalso LargeInt.>= (Int.toLarge 5, i)
		then Iconst (Int.fromLarge i) else
		    if LargeInt.>= (i, Int.toLarge ~128)
			andalso LargeInt.>= (Int.toLarge 127, i)
			then Bipush (Int.fromLarge i) else
			    if LargeInt.>= (i, Int.toLarge ~32768)
				andalso LargeInt.>= (Int.toLarge 32767, i)
				then Sipush (Int.fromLarge i)
			    else Ldc (JVMInt i)

	(* load a word as JVM integer constant. *)
	fun atCodeWord (i:LargeWord.word) =
	    atCodeInt (LargeWord.toLargeInt i)

	(* load a JVM literal *)
	fun atCode (CharLit c) = atCodeInt(Int.toLarge (ord c))
	  | atCode (IntLit i)  = atCodeInt i
	  | atCode (RealLit r) =
	    let
		val r = valOf(Real.fromString r)
	    in
		if (Real.compare (r,0.0)=EQUAL)
		    orelse (Real.compare(r,1.0)=EQUAL)
		    orelse (Real.compare(r,2.0)=EQUAL) then Fconst (Real.trunc r)
		else Ldc (JVMFloat r)
	    end
	  | atCode (StringLit s)= Ldc (JVMString s)
	  | atCode (WordLit w)  = atCodeWord w

	(* Record arities can be statically built. On run time, a static
	 field with the arity is loaded *)
	structure RecordLabel =
	    struct
		(* Record arities are stored in a hashtable to avoid
		 multiple generations. *)
		val arity: int StringListHash.t = StringListHash.new ()

		(* Number of the actual record arity *)
		val number = ref 0

		(* create the fieldname of an arity out of its number *)
		fun fieldname number = "arity"^Int.toString number

		(* get the name of the static JVM field of an arity *)
		fun staticfield number =
		    (Class.getLiteralName()^"/"^(fieldname number),
		     [Arraysig, Classsig CString])

		(* create a new record arity and return its JVM field *)
		fun insert (strings as (s::_)) =
		     (case StringListHash.lookup (arity, strings) of
			 NONE => (number := ((!number)+1);
				  StringListHash.insert (arity, strings, !number);
				  staticfield (!number))
		       | SOME number' => staticfield number')
		  | insert _ = raise Mitch

		(* Generate the record arities at compilitaion time. *)
		fun generate () =
		    let
			fun codeall (strs, aritynumber, acc) =
			    let
				val size=List.length strs
				fun codeone (str, (acc, n)) =
				    let
					val c =
					    (Dup::
					     (atCodeInt n)::
					     (Ldc (JVMString str))::
					     Aastore::
					     acc)
					val d = LargeInt.+ (n, Int.toLarge 1)
				    in
					if d=(Int.toLarge size)
					    then
						(atCodeInt (Int.toLarge size)::
						 (Anewarray CString)::
						 c,d)
					else (c,d)
				    end

				val (r,_) = List.foldr
				    codeone
				    (Putstatic (staticfield aritynumber)::
				     acc,
				     0)
				    strs
			    in
				r
			    end
		    in
			StringListHash.foldi
			codeall
			[Return]
			arity
		    end

		(* Generate field entries for the record arities. *)
		fun makefields () =
		    StringListHash.fold
		    (fn (number, fields) =>
		     Field ([FPublic, FStatic],
			    fieldname number,
			    [Arraysig, Classsig CString])::fields)
		    nil
		    arity
	    end

	(* JVM is statically typed, DML dynamically. Therefore, we have
	 to use expensive wrapper classes for primitive types.
	 For performance reasons, we create all literals at compilation
	 time and store them in static fields. *)
	structure Literals =
	    struct
		(* All literals are stored in a Hashtable to avoid
		 redundancy. *)
		val lithash: int LitHash.t = LitHash.new ()

		(* The number of the actual literal *)
		val number = ref 0

		(* compute a literal's fieldname of its number *)
		fun fieldname number = "lit"^Int.toString number

		(* compute the whole static JVM field of a literal *)
		fun staticfield number =
		    Class.getLiteralName()^"/"^(fieldname number)

		(* add a literal (if necessary) and return its JVM field *)
		fun insert lit' =
		    case LitHash.lookup (lithash, lit') of
			NONE => (number := ((!number)+1);
				 LitHash.insert (lithash, lit', !number);
				 staticfield (!number))
		      | SOME number' => staticfield number'

		fun litClass (CharLit _) = CChar
		  | litClass (IntLit _)    = CInt
		  | litClass (RealLit _)   = CReal
		  | litClass (StringLit _) = CStr
		  | litClass (WordLit _)   = CWord

		(* Generate all literals at compilation time. *)
		fun generate startwert =
		     let
			 fun codelits (lit', constnumber, acc) =
			     let
				 val jType = case lit' of
				     CharLit _   => ([Charsig],[Voidsig])
				   | IntLit _    => ([Intsig],[Voidsig])
				   | RealLit _   => ([Floatsig],[Voidsig])
				   | StringLit _ => ([Classsig CString], [Voidsig])
				   | WordLit _   => ([Intsig],[Voidsig])
				 and scon = litClass lit'
			     in
				 (New scon ::
				  Dup ::
				  atCode lit' ::
				  (Invokespecial (scon,"<init>",jType)) ::
				  (Putstatic ((staticfield constnumber),[Classsig scon])) ::
				  acc)

			     end
		     in
			 LitHash.foldi codelits startwert lithash
		     end

		(* Generate JVM field entries for all literals *)
		fun makefields startwert =
		    LitHash.foldi
		    (fn (lit', number, fields) =>
		     Field ([FPublic, FStatic],
			    fieldname number,
			    [Classsig (litClass lit')])::fields)
		    startwert
		    lithash
	    end

	(* print a list of stamps. Used in verbose mode for debugging. *)
	fun printStampList xs = print ("Free: ("^(psl xs)^")\n")
	and psl (x::nil) = Stamp.toString x
	  | psl (x::xs)  = Stamp.toString x^", "^(psl xs)
	  | psl nil = ""
    end
