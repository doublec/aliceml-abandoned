%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   System(show)
export
   'Debug$': Debug
define
   fun {Show X}
      {System.show X} unit
   end

   Debug =
   'Debug'('show': Show
	   'Show$': Show)
end
