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
   Property(get)
   Application(getArgs)
export
   '$CommandLine': CommandLine
define
   CommandLine =
   'CommandLine'('name':
		    fun {$ unit}
		       {ByteString.make {Property.get 'root.url'}}
		    end
		 'arguments':
		    fun {$ unit}
		       {Application.getArgs plain}
		    end)
end
