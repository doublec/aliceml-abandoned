(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure OutputPickle :> OUTPUT_PICKLE =
    struct
	open PickleGrammar

	structure Label =
	    struct
		datatype t =
		    ARRAY
		  | ARRAY_ZERO
		  | CELL
		  | CONSTRUCTOR
		  | CON_VAL
		  | GLOBAL_STAMP
		  | TUPLE
		  | VECTOR
		  | VECTOR_ZERO
		  | TAG of LargeInt.int

		val none = 0: LargeInt.int
		val some = TAG 1

		val idDef = TAG 0
		val wildcard = 0: LargeInt.int

		val local_ = TAG 0
		val global = TAG 1

		val oneArg = TAG 0
		val tupArgs = TAG 1

		val function = TAG 0

		val appConst = TAG 0
		val appPrim = TAG 1
		val appVar = TAG 2
		val conTest = TAG 3
		val endHandle = TAG 4
		val endTry = TAG 5
		val getRef = TAG 6
		val getTup = TAG 7
		val intTest = TAG 8
		val kill = TAG 9
		val putCon = TAG 10
		val putConst = TAG 11
		val putFun = TAG 12
		val putNew = TAG 13
		val putRef = TAG 14
		val putTag = TAG 15
		val putTup = TAG 16
		val putVar = TAG 17
		val putVec = TAG 18
		val raise_ = TAG 19
		val realTest = TAG 20
		val return = TAG 21
		val shared = TAG 22
		val stringTest = TAG 23
		val tagTest = TAG 24
		val try = TAG 25
		val vecTest = TAG 26

		val con = TAG 0
		val staticCon = TAG 1

		fun toInt ARRAY        = 0
		  | toInt ARRAY_ZERO   = 1
		  | toInt CELL         = 2
		  | toInt CONSTRUCTOR  = 3
		  | toInt CON_VAL      = 4
		  | toInt GLOBAL_STAMP = 5
		  | toInt TUPLE        = 6
		  | toInt VECTOR       = 7
		  | toInt VECTOR_ZERO  = 8
		  | toInt (TAG i)      = 9 + i
	    end

	type context = {outstream: PrimPickle.outstream,
			shared: PrimPickle.id StampMap.t}

	fun outputInt ({outstream, ...}: context, i) =
	    PrimPickle.outputInt (outstream, i)
	fun outputChunk ({outstream, ...}: context, words) =
	    PrimPickle.outputChunk (outstream, words)
	fun outputBlock ({outstream, ...}: context, label, size) =
	    PrimPickle.outputBlock (outstream, Label.toInt label, size)
	fun outputClosure ({outstream, ...}: context, size) =
	    PrimPickle.outputClosure (outstream, size)
	fun outputReference ({outstream, ...}: context, id) =
	    PrimPickle.outputReference (outstream, id)
	fun outputString ({outstream, ...}: context, s) =
	    PrimPickle.outputString (outstream, s)
	fun outputTransform ({outstream, ...}: context, name) =
	    PrimPickle.outputTransform (outstream, name)

	fun outputOption _ (context, NONE) =
	    outputInt (context, Label.none)
	  | outputOption output (context, SOME x) =
	    (outputBlock (context, Label.some, 1); output (context, x))

	fun outputVector _ (context, #[]) =
	    (outputBlock (context, Label.VECTOR_ZERO, 1);
	     outputInt (context, 0))
	  | outputVector output (context, xs) =
	    (outputBlock (context, Label.VECTOR, Vector.length xs);
	     Vector.app (fn x => output (context, x)) xs)

	fun outputStamp (context, stamp) =
	    outputInt (context, LargeInt.fromInt (Stamp.hash stamp))

	fun outputId (context, id) = outputInt (context, LargeInt.fromInt id)

	fun outputIdDef (context, IdDef id) =
	    (outputBlock (context, Label.idDef, 1); outputId (context, id))
	  | outputIdDef (context, Wildcard) =
	    outputInt (context, Label.wildcard)

	fun outputIdRef (context, Local id) =
	    (outputBlock (context, Label.local_, 1); outputId (context, id))
	  | outputIdRef (context, Global id) =
	    (outputBlock (context, Label.global, 1); outputId (context, id))

	fun outputArgs outputArg (context, OneArg x) =
	    (outputBlock (context, Label.oneArg, 1); outputArg (context, x))
	  | outputArgs outputArg (context, TupArgs xs) =
	    (outputBlock (context, Label.tupArgs, 1);
	     outputVector outputArg (context, xs))

	val filler8 = Char.chr 1
	val filler16 = WideChar.chr 1

	fun outputWideString (context, s) =
	    outputChunk (context,
			 Vector.fromList
			 (List.foldr (fn (c, cs) =>
				      let
					  val i = Char.ord c
				      in
					  Word8.fromInt (i div 0x100)::
					  Word8.fromInt (i mod 0x100)::cs
				      end) nil (String.explode s)))

	fun outputReal (context, r) =
	    let
		val vec = Unsafe.blastWrite r
		val mkIndex =
		    case Word8Vector.sub (vec, 0) of
			0wx33 => (fn i => 103 - i)
		      | 0wx00 => (fn i => i + 96)
		      | _ => raise Crash.Crash "OutputPickle.outputReal"
	    in
		outputChunk (context,
			     Vector.tabulate
			     (8, fn i => Word8Vector.sub (vec, mkIndex i)));
		()
	    end

	fun outputValue (context, Prim name) =
	    (outputTransform (context, "Alice.primitive");
	     outputBlock (context, Label.TAG 0, 1);
	     outputString (context, name); ())
	  | outputValue (context, Int i) = outputInt (context, i)
	  | outputValue (context, String s) =
	    ignore (outputString (context, s))
	  | outputValue (context, WideString s) =
	    ignore (outputWideString (context, s))
	  | outputValue (context, Real r) = outputReal (context, r)
	  | outputValue (context as {shared, ...}, Constructor stamp) =
	    (case StampMap.lookup (shared, stamp) of
		 SOME id => outputReference (context, id)
	       | NONE =>
		     let
			 val id = outputBlock (context, Label.CONSTRUCTOR, 1)
		     in
			 outputInt (context, 0); (*--** print name? *)
			 StampMap.insertDisjoint (shared, stamp, id)
		     end)
	  | outputValue (context, Tuple values) =
	    (outputBlock (context, Label.TUPLE, Vector.length values);
	     Vector.app (fn value => outputValue (context, value)) values)
	  | outputValue (context, Vector #[]) =
	    (outputBlock (context, Label.VECTOR_ZERO, 1);
	     outputInt (context, 0))
	  | outputValue (context, Vector values) =
	    (outputBlock (context, Label.VECTOR, Vector.length values);
	     Vector.app (fn value => outputValue (context, value)) values)
	  | outputValue (context, Closure (function, values)) =
	    (outputClosure (context, 1 + Vector.length values);
	     outputFunction (context, function);
	     Vector.app (fn value => outputValue (context, value)) values)
	  | outputValue (context, Sign sign) = outputInt (context, 0) (*--** *)
	and outputFunction (context,
			    Function (nglobals, nlocals, args, body)) =
	    (outputTransform (context, "Alice.function");
	     outputBlock (context, Label.function, 4);
	     outputInt (context, LargeInt.fromInt nglobals);
	     outputInt (context, LargeInt.fromInt nlocals);
	     outputArgs outputIdDef (context, args);
	     outputInstr (context, body))
	and outputInstr (context, Kill (ids, instr)) =
	    (outputBlock (context, Label.kill, 2);
	     outputVector outputId (context, ids);
	     outputInstr (context, instr))
	  | outputInstr (context, PutConst (id, value, instr)) =
	    (outputBlock (context, Label.putConst, 3); outputId (context, id);
	     outputValue (context, value); outputInstr (context, instr))
	  | outputInstr (context, PutVar (id, idRef, instr)) =
	    (outputBlock (context, Label.putVar, 3); outputId (context, id);
	     outputIdRef (context, idRef); outputInstr (context, instr))
	  | outputInstr (context, PutNew (id, instr)) =
	    (outputBlock (context, Label.putNew, 2); outputId (context, id);
	     outputInstr (context, instr))
	  | outputInstr (context, PutTag (id, tag, idRefs, instr)) =
	    (outputBlock (context, Label.putTag, 4); outputId (context, id);
	     outputInt (context, LargeInt.fromInt tag);
	     outputVector outputIdRef (context, idRefs);
	     outputInstr (context, instr))
	  | outputInstr (context, PutCon (id, con, idRefs, instr)) =
	    (outputBlock (context, Label.putCon, 4); outputId (context, id);
	     outputCon (context, con);
	     outputVector outputIdRef (context, idRefs);
	     outputInstr (context, instr))
	  | outputInstr (context, PutRef (id, idRef, instr)) =
	    (outputBlock (context, Label.putRef, 3); outputId (context, id);
	     outputIdRef (context, idRef); outputInstr (context, instr))
	  | outputInstr (context, PutTup (id, idRefs, instr)) =
	    (outputBlock (context, Label.putTup, 3); outputId (context, id);
	     outputVector outputIdRef (context, idRefs);
	     outputInstr (context, instr))
	  | outputInstr (context, PutVec (id, idRefs, instr)) =
	    (outputBlock (context, Label.putVec, 3); outputId (context, id);
	     outputVector outputIdRef (context, idRefs);
	     outputInstr (context, instr))
	  | outputInstr (context, PutFun (id, idRefs, function, instr)) =
	    (outputBlock (context, Label.putFun, 4); outputId (context, id);
	     outputVector outputIdRef (context, idRefs);
	     outputFunction (context, function); outputInstr (context, instr))
	  | outputInstr (context, AppPrim (idDef, name, idRefs, instrOpt)) =
	    (outputBlock (context, Label.appPrim, 4);
	     outputIdDef (context, idDef); outputString (context, name);
	     outputVector outputIdRef (context, idRefs);
	     outputOption outputInstr (context, instrOpt))
	  | outputInstr (context, AppVar (outArgs, idRef, inArgs, instrOpt)) =
	    (outputBlock (context, Label.appVar, 4);
	     outputArgs outputIdDef (context, outArgs);
	     outputIdRef (context, idRef);
	     outputArgs outputIdRef (context, inArgs);
	     outputOption outputInstr (context, instrOpt))
	  | outputInstr (context,
			 AppConst (outArgs, value, inArgs, instrOpt)) =
	    (outputBlock (context, Label.appConst, 4);
	     outputArgs outputIdDef (context, outArgs);
	     outputValue (context, value);
	     outputArgs outputIdRef (context, inArgs);
	     outputOption outputInstr (context, instrOpt))
	  | outputInstr (context, GetRef (id, idRef, instr)) =
	    (outputBlock (context, Label.getRef, 3); outputId (context, id);
	     outputIdRef (context, idRef); outputInstr (context, instr))
	  | outputInstr (context, GetTup  (idDefs, idRef, instr)) =
	    (outputBlock (context, Label.getTup, 3);
	     outputVector outputIdDef (context, idDefs);
	     outputIdRef (context, idRef); outputInstr (context, instr))
	  | outputInstr (context, Raise idRef) =
	    (outputBlock (context, Label.raise_, 1);
	     outputIdRef (context, idRef))
	  | outputInstr (context, Try (tryInstr, idDef, handleInstr)) =
	    (outputBlock (context, Label.try, 3);
	     outputInstr (context, tryInstr); outputIdDef (context, idDef);
	     outputInstr (context, handleInstr))
	  | outputInstr (context, EndTry instr) =
	    (outputBlock (context, Label.endTry, 1);
	     outputInstr (context, instr))
	  | outputInstr (context, EndHandle instr) =
	    (outputBlock (context, Label.endHandle, 1);
	     outputInstr (context, instr))
	  | outputInstr (context, IntTest (idRef, tests, instr)) =
	    (outputBlock (context, Label.intTest, 3);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (int, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputInt (context, int);
			    outputInstr (context, instr))) (context, tests);
	     outputInstr (context, instr))
	  | outputInstr (context, RealTest (idRef, tests, instr)) =
	    (outputBlock (context, Label.realTest, 3);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (real, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputReal (context, real);
			    outputInstr (context, instr))) (context, tests);
	     outputInstr (context, instr))
	  | outputInstr (context, StringTest (idRef, tests, instr)) =
	    (outputBlock (context, Label.stringTest, 3);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (string, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputString (context, string);
			    outputInstr (context, instr))) (context, tests);
	     outputInstr (context, instr))
	  | outputInstr (context, WideStringTest (idRef, tests, instr)) =
	    (outputBlock (context, Label.stringTest, 3);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (string, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputWideString (context, string);
			    outputInstr (context, instr))) (context, tests);
	     outputInstr (context, instr))
	  | outputInstr (context, TagTest (idRef, tests1, tests2, instr)) =
	    (outputBlock (context, Label.tagTest, 4);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (tag, instr)) =>
			  (outputBlock (context, Label.TUPLE, 2);
			   outputInt (context, LargeInt.fromInt tag);
			   outputInstr (context, instr))) (context, tests1);
	     outputVector (fn (context, (tag, idDefs, instr)) =>
			   (outputBlock (context, Label.TUPLE, 3);
			    outputInt (context, LargeInt.fromInt tag);
			    outputVector outputIdDef (context, idDefs);
			    outputInstr (context, instr))) (context, tests2);
	     outputInstr (context, instr))
	  | outputInstr (context, ConTest (idRef, tests1, tests2, instr)) =
	    (outputBlock (context, Label.conTest, 4);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (con, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputCon (context, con);
			    outputInstr (context, instr))) (context, tests1);
	     outputVector (fn (context, (con, idDefs, instr)) =>
			   (outputBlock (context, Label.TUPLE, 3);
			    outputCon (context, con);
			    outputVector outputIdDef (context, idDefs);
			    outputInstr (context, instr))) (context, tests2);
	     outputInstr (context, instr))
	  | outputInstr (context, VecTest (idRef, tests, instr)) =
	    (outputBlock (context, Label.vecTest, 3);
	     outputIdRef (context, idRef);
	     outputVector (fn (context, (idDefs, instr)) =>
			   (outputBlock (context, Label.TUPLE, 2);
			    outputVector outputIdDef (context, idDefs);
			    outputInstr (context, instr))) (context, tests);
	     outputInstr (context, instr))
	  | outputInstr (context as {shared, ...}, Shared (stamp, instr)) =
	    (case StampMap.lookup (shared, stamp) of
		 SOME id => outputReference (context, id)
	       | NONE =>
		     let
			 val id = outputBlock (context, Label.shared, 2)
		     in
			 outputStamp (context, stamp);
			 outputInstr (context, instr);
			 StampMap.insert (shared, stamp, id)
		     end)
	  | outputInstr (context, Return args) =
	    (outputBlock (context, Label.return, 1);
	     outputArgs outputIdRef (context, args))
	and outputCon (context, Con idRef) =
	    (outputBlock (context, Label.con, 1); outputIdRef (context, idRef))
	  | outputCon (context, StaticCon value) =
	    (outputBlock (context, Label.staticCon, 1);
	     outputValue (context, value))

	fun output (outstream, value) =
	    outputValue ({outstream = outstream, shared = StampMap.new ()},
			 value)
    end
