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
	 Abbrev

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

	(* structure for free variabes *)
	structure FreeVars =
	    struct
		(* assign (function) ids to free variable sets *)
		val free:StampSet.set StampHash.t=StampHash.new ()

		(* Assign ids to their defining function. *)
		val defFun:stamp StampHash.t=StampHash.new ()

		(* assign stamps to free variables *)
		fun addVars (stamp', freeVarSet) =
		    case StampHash.lookup (free, stamp') of
			NONE => StampHash.insert (free, stamp', freeVarSet)
		      | SOME freeVars =>
			(print ("setVars twice for "^Stamp.toString stamp'^"\n");
			 StampSet.app
			 (fn stamp' => StampSet.insert(freeVars, stamp'))
			 freeVarSet)

		(* get free variable set of an id. *)
		fun getVars stamp' =
		    case StampHash.lookup (free, stamp') of
			NONE => let
				    val f = StampSet.new ()
				in
				    print "unset freevars";
				    StampSet.insert (f, Lambda.getClassStamp (stamp', 1));
				    StampHash.insert(free, stamp', f);
				    f
				end
		      | SOME v => (print ("FreeVars for "^Stamp.toString stamp'^": ");
				   StampSet.app (fn stamp'' => print (Stamp.toString stamp''^" ")) v;
				   print "\n";
				   v)

		(* assign ids to their defining function closure. *)
		fun setFun (Id (_,stamp',_), stamp'') =
		    (if !VERBOSE >= 2 then print (Stamp.toString stamp'^" is defined in "^Stamp.toString stamp''^"\n")
		     else ();
			 StampHash.insert(defFun, stamp', stamp''))

		(* get the defining function closure of a stamp *)
		fun getFun stamp' =
		    case StampHash.lookup (defFun, stamp') of
			NONE => (if !VERBOSE >= 2 then print (Stamp.toString stamp'^" was defined at toplevel.\n")
				 else ();
				     toplevel)
		      | SOME stamp'' => (if !VERBOSE >= 2 then print (Stamp.toString stamp'^" was defined at "^
								      Stamp.toString stamp''^".\n") else ();
					     stamp'')

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
		val arity: (int StringListHash.t) StampHash.t = StampHash.new ()

		(* Number of the actual record arity *)
		val number: int StampHash.t = StampHash.new ()

		(* create the fieldname of an arity out of its number *)
		fun fieldname number = "arity"^Int.toString number

		(* get the name of the static JVM field of an arity *)
		fun staticfield (curCls, number) =
		    (classNameFromStamp curCls^"/"^(fieldname number),
		     [Arraysig, Classsig CString])

		(* create a new record arity and return its JVM field *)
		fun insert (stamp', strings as (s::_)) =
		    let
			val l =
			    case StampHash.lookup (arity, stamp') of
				NONE =>
				    let
					val l' = StringListHash.new ()
				    in
					StampHash.insert (arity, stamp', l');
					l'
				    end
			      | SOME l' => l'
		    in
			case StringListHash.lookup (l, strings) of
			    NONE =>
				let
				    val nr =
					case StampHash.lookup(number, stamp') of
					    NONE => 0
					  | SOME n =>n+1
				in
				    StringListHash.insert (l, strings, nr);
				    StampHash.insert (number, stamp', nr);
				    staticfield (stamp', nr)
				end
			  | SOME number' => staticfield (stamp', number')
		    end
		  | insert _ = Crash.crash "RecordLabel.insert: empty Stringlist"

		(* Generate the record arities of a certain class at compilitaion time. *)
		fun generate stamp' =
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
				    (Putstatic (staticfield (stamp', aritynumber))::
				     acc,
				     0)
				    strs
			    in
				r
			    end
		    in
			(case StampHash.lookup (arity, stamp') of
			     NONE => [Return]
			   | SOME a =>
				 (StringListHash.foldi
				  codeall
				  nil
				  a))
		    end

		(* Generate field entries for the record arities of a certain class. *)
		fun makefields (stamp', init) =
		    case StampHash.lookup (arity, stamp') of
			NONE => init
		      | SOME a =>
			    StringListHash.fold
			    (fn (stamp'', fields) =>
			     Field ([FPublic, FStatic],
				    fieldname stamp'',
				    [Arraysig, Classsig CString])::fields)
			    init
			    a
	    end

	(* JVM is statically typed, DML dynamically. Therefore, we have
	 to use expensive wrapper classes for primitive types.
	 For performance reasons, we create all literals at compilation
	 time and store them in static fields. *)
	structure Literals =
	    struct
		(* All literals are stored in a Hashtable to avoid
		 redundancy. *)
		val lithash: int LitHash.map StampHash.t = StampHash.new ()

		(* The number of the next literal of a certain class *)
		val number: int StampHash.t = StampHash.new ()

		(* compute a literal's fieldname of its number *)
		fun fieldname number = "lit"^Int.toString number

		(* compute the whole static JVM field of a literal *)
		fun staticfield (stamp', number, cls) =
		    (classNameFromStamp stamp'^"/"^(fieldname number),
		     cls)

		fun litClass (CharLit _) = CChar
		  | litClass (IntLit _)    = CInt
		  | litClass (RealLit _)   = CReal
		  | litClass (StringLit _) = CStr
		  | litClass (WordLit _)   = CWord

		(* add a literal (if necessary) and return its JVM field *)
		fun insert (stamp',lit') =
		    let
			val lit =
			    case StampHash.lookup(lithash, stamp') of
				NONE => let
					    val l = LitHash.new ()
					in
					    StampHash.insert (lithash, stamp', l);
					    l
					end
			      | SOME l => l
		    in
			case LitHash.lookup (lit, lit') of
			    NONE =>
				let
				    val nr =
					case StampHash.lookup(number, stamp') of
					    NONE => 0
					  | SOME n => n+1
				in
				    LitHash.insert (lit, lit', nr);
				    StampHash.insert (number, stamp', nr);
				    staticfield (stamp', nr, [Classsig (litClass lit')])
				end
			  | SOME number' =>
				staticfield (stamp', number', [Classsig (litClass lit')])
		    end

		(* Generate all literals of a certain class at compilation time. *)
		fun generate (stamp', init) =
		     let
			 fun codelits (lit', constnumber, acc) =
			     let
				 val jType = case lit' of
				     CharLit _   => [Charsig]
				   | IntLit _    => [Intsig]
				   | RealLit _   => [Floatsig]
				   | StringLit _ => [Classsig CString]
				   | WordLit _   => [Intsig]
				 and scon = litClass lit'
			     in
				 (New scon ::
				  Dup ::
				  atCode lit' ::
				  (Invokespecial (scon,"<init>",(jType, [Voidsig]))) ::
				  (Putstatic ((staticfield (stamp', constnumber,
							    [Classsig scon])))) ::
				  acc)

			     end
		     in
				  case StampHash.lookup (lithash, stamp') of
				      NONE => init
				    | SOME l => LitHash.foldi codelits init l
		     end

		(* Generate JVM field entries for all literals *)
		fun makefields (stamp',init) =
				  (case StampHash.lookup (lithash, stamp') of
				       NONE => init
				     | SOME l => LitHash.foldi
					   (fn (lit', number, fields) =>
					    Field ([FPublic, FStatic],
						   fieldname number,
						   [Classsig (litClass lit')])::fields)
					   init
					   l)
	    end

	(* print a list of stamps. Used in verbose mode for debugging. *)
	fun printStampList xs = print ("Free: ("^(psl xs)^")\n")
	and psl (x::nil) = Stamp.toString x
	  | psl (x::xs)  = Stamp.toString x^", "^(psl xs)
	  | psl nil = ""

    end
