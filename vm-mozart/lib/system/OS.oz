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
   OS(system getEnv getCWD)
   Application(exit)
export
   'OS$': OS_Module
define
   OS_Module =
   'OS'('FileSys$':
	   'FileSys'('getDir':
			fun {$ unit}
			   {ByteString.make {OS.getCWD}}
			end)
	'Process$':
	   'Process'('$status': {Value.byNeedFail rttNotImplemented}
		     'success': 0
		     'failure': 1
		     'system': OS.system
		     'exit':
			proc {$ N _}
			   {Application.exit N}
			end
		     'getEnv':
			fun {$ S}
			   case {OS.getEnv S} of false then 'NONE'
			   elseof S2 then 'SOME'({ByteString.make S2})
			   end
			end))
end
