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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
export
   module: IOComponent
define
   IOComponent = tuple(IO)

   I_PrimeIo = 1
   I_Io      = 2

   Io = {NewUniqueName 'IO.Io'}

   IO = tuple(I_PrimeIo: value(Io)
	      I_Io: fun {$ X} con(Io X) end#i_v)
end
