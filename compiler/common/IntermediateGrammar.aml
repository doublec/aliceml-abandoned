structure PostTranslationIntermediate =
		Intermediate(type info = Source.position
			     val dummy = Source.nowhere
			     fun output_info(os,(l,r)) =
				( TextIO.output(os, Int.toString l)
				; TextIO.output(os, "#")
				; TextIO.output(os, Int.toString r)
				)
			    )
