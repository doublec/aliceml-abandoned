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
   OS(system getEnv)
   Property(put)
   Error(printException)
   System(onToplevel)
   Application(exit)
export
   'Process$': Process
define
   Process =
   'Process'('$status': {Value.byNeedFail rttNotImplemented}
	     'success': 0
	     'failure': 1
	     'system': OS.system
	     'atExn':
		fun {$ P}
		   {Property.put 'errors.handler'
		    proc {$ E}
		       case E of system(kernel(terminate) ...) then skip
		       [] error(alice(Exn ...) ...) then
			  _ = {P Exn}
		       else
			  {Error.printException E}
			  if {System.onToplevel} then
			     {Application.exit 1}
			  end
		       end
		    end}
		   unit
		end
	     'exit':
		proc {$ N _}
		   {Application.exit N}
		end
	     'getEnv':
		fun {$ S}
		   case {OS.getEnv S} of false then 'NONE'
		   elseof S2 then 'SOME'({ByteString.make S2})
		   end
		end)
end
