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
   Property(get)
   Application(getArgs)
export
   'UnsafeCommandLine$': CommandLine
define
   CommandLine =
   'CommandLine'('name':
		    fun {$ unit}
		       {ByteString.make {Property.get 'alice.rootUrl'}}
		    end
		 'arguments':
		    fun {$ unit}
		       {Map {Application.getArgs plain} ByteString.make}
		    end)
end
