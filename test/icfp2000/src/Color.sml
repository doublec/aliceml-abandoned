signature COLOR =
    sig
	type color = {red: real, green: real, blue: real}

	val black: color
	val color: real * real * real -> color
	val scale: real * color -> color
	val add: color * color -> color
	val prod: color * color -> color
	val clamp: color -> color
	val nclamp: color -> color
    end


structure Color :> COLOR =
    struct
	type color = {red: real, green: real, blue: real}

	fun color (r,g,b) = {red = r, green = g, blue = b}

	val black = {red = 0.0, green = 0.0, blue = 0.0}

	fun scale (k, {red, green, blue}): color =
	    {red = k * red, green = k * green, blue = k * blue}

	fun add ({red = r1, green = g1, blue = b1},
		 {red = r2, green = g2, blue = b2}): color =
	    {red = r1 + r2, green = g1 + g2, blue = b1 + b2}

	fun prod ({red = r1, green = g1, blue = b1},
		  {red = r2, green = g2, blue = b2}): color =
	    {red = r1 * r2, green = g1 * g2, blue = b1 * b2}

	fun clamp {red, green, blue} =
	    {red = clamp' red, green = clamp' green, blue = clamp' blue}
	and clamp' x = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

	fun nclamp {red, green, blue} =
	    {red = nclamp' red, green = nclamp' green, blue = nclamp' blue}
	and nclamp' x =
	    if Real.isNan x orelse not (Real.isFinite x) orelse x < 0.0 then
		0.0
	    else x
    end
