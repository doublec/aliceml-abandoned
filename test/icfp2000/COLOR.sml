signature COLOR =
    sig
	type color = {red: real, green: real, blue: real}
	type t = color

	val scale: real * color -> color
	val prod: color * color -> color
    end
