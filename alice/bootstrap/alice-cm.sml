local
    val tool    = "Alice-to-SML"
    val class   = "alice"
    val suffix  = "aml"
    val command = case OS.Process.getEnv "AML2SML"
		    of SOME command => command
		     | NONE         => "./aml2sml"

(*
    fun simplerule file =
	[(OS.Path.joinBaseExt {base = OS.Path.base file, ext = SOME "sml"},
	  SOME "sml")]
*)
    fun simplerule path =
	let
	    val {dir, file} = OS.Path.splitDirFile path
	    val  dir'       = OS.Path.joinDirFile {dir = dir, file = "NJ"}
	    val  file'      = OS.Path.joinBaseExt {base = OS.Path.base file,
						   ext = SOME "sml"}
	    val  path'      = OS.Path.joinDirFile {dir = dir', file = file'}
	in
	    [(path', SOME "sml")]
	end
in
    val _ = CM.Tools.addToolClass
		{ class     = class
		, rule      = CM.Tools.dontcare simplerule
		, validator = CM.Tools.stdTStampValidator
		, processor = CM.Tools.stdShellProcessor
					{ command = command, tool = tool }
		}

    val _ = CM.Tools.addClassifier(CM.Tools.stdSfxClassifier
					{ sfx = suffix, class = class })
end;
