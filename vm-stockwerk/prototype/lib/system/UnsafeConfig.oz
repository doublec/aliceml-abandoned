%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Property(get)
export
   module: ConfigComponent
define
   ConfigComponent = tuple(Config)

   UNIX  = 0
   WIN32 = 1

   I_platform = 1

   Config =
   tuple(I_platform:
	    value(case {Property.get 'platform.os'} of win32 then WIN32
		  else UNIX
		  end))
end
