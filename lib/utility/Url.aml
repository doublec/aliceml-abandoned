structure Url :> URL =
    struct
	type url = string
	type t = url

	exception Malformed

	fun fromString s = s
	fun toString s = s
	fun resolve _ s = s
	fun isAbsolute _ = true
    end
