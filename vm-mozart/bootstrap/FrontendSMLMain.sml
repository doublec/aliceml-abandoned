import
structure AbstractionPhase: sig val translate: 'a end
structure BindEnv: sig val copy: 'a end
structure BindEnv0: sig val E0: 'a end
from "FrontendSML.ozf"

import
structure ParsingPhase: sig val parse: 'a end
from "FrontendSMLParser.ozf"

fun abstract x =
    (AbstractionPhase.translate (BindEnv.copy BindEnv0.E0) o
     ParsingPhase.parse) x
