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
   Application(getArgs)
export
   module: CommandLineComponent
define
   CommandLineComponent = tuple(CommandLine)

   CONS = 0
   NIL = 1

   I_arguments = 1
   I_name      = 2

   CommandLine =
   tuple(I_name: fun {$} {ByteString.make {Property.get 'root.url'}} end#n_v
	 I_arguments:
	    fun {$}
	       {FoldR {Application.getArgs plain}
		fun {$ S In} tag(CONS {ByteString.make S} In) end NIL}
	    end#r_v)
end
