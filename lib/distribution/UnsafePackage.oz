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
   'UnsafePackage$': UnsafePackage
define
   UnsafePackage =
   'UnsafePackage'('Pack$':
		      fun {$ '#'('$S$': _ 'X$': X)}
			 '#'(package: X)
		      end
		   'Unpack$':
		      fun {$ '#'(package: X '$S$': _)}
			 X
		      end)
end
