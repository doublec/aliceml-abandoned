structure Color :> COLOR =
    struct
	type color = {red: real, green: real, blue: real}
	type t = color

	fun scale (k, {red, green, blue}): color =
	    {red = k * red, green = k * green, blue = k * blue}

	fun prod ({red = r1, green = g1, blue = b1},
		  {red = r2, green = g2, blue = b2}): color =
	    {red = r1 * r2, green = g1 * g2, blue = b1 * b2}
    end
