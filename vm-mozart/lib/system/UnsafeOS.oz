%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   OzOS(getCWD system getEnv tmpnam) at 'x-oz://system/OS.ozf'
   Property(put)
   Error(printException)
   System(onToplevel)
   Application(exit)
export
   'UnsafeOS$': OS
define
   OS =
   'OS'('FileSys$':
	   'FileSys'('getDir':
			fun {$ unit}
			   {ByteString.make {OzOS.getCWD}}
			end
		     'tmpName':
			fun {$ unit}
			   {ByteString.make {OzOS.tmpnam}}
			end)
	'Process$':
	   'Process'('success': 0
		     'failure': 1
		     'system': OzOS.system
		     'atExn':
			fun {$ P}
			   {Property.put 'errors.handler'
			    proc {$ E}
			       case E of system(kernel(terminate) ...) then
				  skip
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
			   case {OzOS.getEnv S} of false then 'NONE'
			   elseof S2 then 'SOME'({ByteString.make S2})
			   end
			end))
end
