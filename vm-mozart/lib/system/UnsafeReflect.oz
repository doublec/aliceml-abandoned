%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   'UnsafeReflect$': Reflect
define
   Reflect =
   'Reflect'('cast':
		fun {$ A} A end
	     'Reflect$':
		fun {$ '#'('$S$': _ 'X$': X)} '#'('x': X) end
	     'Unreflect$':
		fun {$ '#'('x': X '$S$': _)} X end
	     'ReflectSig$':
		fun {$ '#'('$S$': S)} '#'('x': {S unit}.1) end
%	     'UnreflectSig$':
%		fun {$ '#'('x': X)} '#'('$S$': X) end

	    )
end
