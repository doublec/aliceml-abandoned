local
    val tool    = "Alice-to-SML"
    val class   = "alice"
    val suffix  = "aml"
    val command = "aml2sml"

    fun simplerule file = [(OS.Path.base file ^ ".sml", SOME "sml")]
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
end
