%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   OS(getEnv)
export
   '$OS': OS_Module
define
   OS_Module =
   'OS'('Process':
	   'Process'('getEnv':
			fun {$ S}
			   case {OS.getEnv S} of false then 'NONE'
			   elseof S2 then 'SOME'({ByteString.make S2})
			   end
			end))
end
