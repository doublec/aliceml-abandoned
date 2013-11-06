%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001-2003
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Pickle(pack)
export
   'UnsafeReflect$': Reflect
define
   Reflect =
   'Reflect'('cast': fun {$ A} A end
	     'Reflect$':
		fun {$ '#'('$S$': _ 'X$': X)} '#'('x': X) end
	     'Reify$':
		fun {$ '#'('x': X '$S$': _)} X end
	     'ReflectSig$':
		fun {$ '#'('$S$': S)} '#'('x': S) end
	     'ReifySig$':
		fun {$ '#'('x': X)} '#'('$S$': X) end)
end
