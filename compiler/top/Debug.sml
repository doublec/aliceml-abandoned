(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature DEBUG =
    sig
	val parseString:	string -> InputGrammar.t
	val parseFile:		string -> InputGrammar.t

	val abstractString:	string -> AbstractGrammar.t
	val abstractFile:	string -> AbstractGrammar.t

	val elabString:		string -> TypedGrammar.t
	val elabFile:		string -> TypedGrammar.t

	val translateString:	string -> IntermediateGrammar.t
	val translateFile:	string -> IntermediateGrammar.t

	val flattenString:	string -> FlatGrammar.t
	val flattenFile:	string -> FlatGrammar.t
    end

structure Debug :> DEBUG =
    struct
	(* Build a recursive compiler - to obtain acquireSign *)

	structure Switches = MakeSwitches(val logOut = TextIO.stdOut)

	val f: (Source.desc * Url.t -> Composer.Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "SMLToMozartMain.f")

	structure MozartTarget =
	    MakeMozartTarget(structure Switches = Switches
			     structure Sig = Signature)

	structure FrontendSML =
	    MakeFrontendSML(fun loadSign (desc, url) = !f (desc, url)
			    structure Switches = Switches)

	structure FrontendCommon =
	    MakeFrontendCommon(fun loadSign (desc, url) = !f (desc, url)
			       structure Switches = Switches)

	structure BackendCommon = MakeBackendCommon(Switches)

	structure BackendMozart =
	    MakeBackendMozart(structure Switches = Switches
			      structure MozartTarget = MozartTarget)

	structure Compiler =
	    MakeCompiler(structure Switches         = Switches
			 structure Target           = MozartTarget
			 structure FrontendSpecific = FrontendSML
			 structure FrontendCommon   = FrontendCommon
			 structure BackendCommon    = BackendCommon
			 structure BackendSpecific  = BackendMozart)

	structure RecursiveCompiler =
	    MakeRecursiveCompiler(structure Composer = Composer
				  structure Compiler = Compiler
				  val extension = "ozf")

	(* Debugging functions *)

(*	val _ = Switches.Bootstrap.rttLevel := Switches.Bootstrap.FULL_RTT
*)

	structure ParsingPhase =
	    MakeTracingPhase(
		  structure Phase    = MakeParsingPhase(Switches)
		  structure Switches = Switches
		  val name = "Parsing"
	    )
	structure AbstractionPhase =
	    MakeTracingPhase(
		  structure Phase    =
		      MakeAbstractionPhase(val loadSign =
					       RecursiveCompiler.acquireSign
					   structure Switches = Switches)
		  structure Switches = Switches
		  val name = "Abstraction"
	    )
	structure AbstractionPhase =
	    MakeDumpingPhase(
		  structure Phase    = AbstractionPhase
		  structure Switches = Switches
		  val header = "Abstract Syntax"
		  val pp     = PPAbstractGrammar.ppComp
		  val switch = Switches.Debug.dumpAbstractionResult
	    )
	structure ElaborationPhase =
	    MakeTracingPhase(
		  structure Phase    =
		      MakeElaborationPhase(val loadSign =
					       RecursiveCompiler.acquireSign)
		  structure Switches = Switches
		  val name = "Elaboration"
	    )
	structure ElaborationPhase =
	    MakeDumpingPhase(
		  structure Phase    = ElaborationPhase
		  structure Switches = Switches
		  val header = "Component Signature"
		  val pp     = PPInf.ppSig o #sign o TypedGrammar.infoComp
		  val switch = Switches.Debug.dumpElaborationSig
	    )
	structure TranslationPhase =
	    MakeTracingPhase(
		  structure Phase    = MakeTranslationPhase(Switches)
		  structure Switches = Switches
		  val name = "Translation"
	    )
	structure TranslationPhase =
	    MakeDumpingPhase(
		  structure Phase    = TranslationPhase
		  structure Switches = Switches
		  val header = "Intermediate Syntax"
		  val pp     = PPIntermediateGrammar.ppComp
		  val switch = Switches.Debug.dumpIntermediate
	    )
	structure BackendCommon = MakeBackendCommon(Switches)

	fun processString process source =
	    RecursiveCompiler.processString process source
	    handle e as Crash.Crash message =>
		(TextIO.output (TextIO.stdErr, "CRASH: " ^ message ^ "\n");
		 raise e)

	fun processFile process source =
	    RecursiveCompiler.processFile process source
	    handle e as Crash.Crash message =>
		(TextIO.output (TextIO.stdErr, "CRASH: " ^ message ^ "\n");
		 raise e)

	fun parse' x     = ParsingPhase.translate () x
	fun abstract' x  = AbstractionPhase.translate (BindEnv.new()) x
	fun elab' x      = let val comp = ElaborationPhase.translate
					    (Env.new()) x
			       val i = TypedGrammar.infoComp comp
			   in  BindEnvFromSig.envFromSig(#region i,#sign i);
			       comp
			   end
	fun translate' x = TranslationPhase.translate () x
	fun flatten' x   = BackendCommon.translate (BackendCommon.C.new ()) x

	infix 3 oo
	fun (f oo g) (desc, x) = f (desc, g (desc, x))

	fun source (_, s) = Source.fromString s
	val parse         = parse' oo source
	val abstract      = abstract' oo parse
	val elab          = elab' oo abstract
	val translate     = translate' oo elab
	val flatten       = flatten' oo translate

	val parseString		= processString parse
	val parseFile		= processFile parse

	val abstractString	= processString abstract
	val abstractFile	= processFile abstract

	val elabString		= processString elab
	val elabFile		= processFile elab

	val translateString	= processString translate
	val translateFile	= processFile translate

	val flattenString	= processString flatten
	val flattenFile		= processFile flatten
    end
