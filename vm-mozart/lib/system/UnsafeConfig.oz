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
import
   Property(get)
export
   'NativeConfig$': NativeConfig
define
   NativeConfig =
   'NativeConfig'('platform':
		     case {Property.get 'platform.os'} of win32 then 'WIN32'
		     else 'UNIX'
		     end)
end
