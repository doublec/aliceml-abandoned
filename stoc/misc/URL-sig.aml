signature URL =
    sig
	eqtype url
	type t = url

	exception Malformed

	val fromString: string -> url   (* Malformed *)
	val toString: url -> string
	val resolve: url -> url -> url
	val isAbsolute: url -> bool
	val compare: url * url -> order
	val hash: url -> int
    end
