BUILD

	make depend
	make

EXECUTE

	alicerun Main

SYNTAX

	exp	::=	appexp
		 |	if exp1 then exp2 else exp3
		 |	fn x : typ => exp
		 |	rec x1 ( x2 : typ2 ) : typ1 => exp
	appexp	::=	atexp
		 |	appexp atexp
	atexp	::=	id
		 |	con
		 |	( exp )

	typ	::=	attyp
		 |	attyp -> typ
	attyp	::=	bool
		 |	int
		 |	( typ )

	id	 =	[a-zA-Z][a-zA-Z0-9]*
	con	 =	[0-9]+


CONSTANTS

	add	:	int -> int -> int
	sub	:	int -> int -> int
	mul	:	int -> int -> int
	equal	:	int -> int -> bool
	less	:	int -> int -> bool
