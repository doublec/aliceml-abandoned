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
   OS(getCWD)
export
   'FileSys$': FileSys
define
   FileSys =
   'FileSys'('getDir':
		fun {$ unit}
		   {ByteString.make {OS.getCWD}}
		end)
end
