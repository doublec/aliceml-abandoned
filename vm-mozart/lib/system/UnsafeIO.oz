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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
export
   'UnsafeIO$': IO
define
   Io = {NewUniqueName 'IO.Io'}

   IO = 'IO'('\'Io': Io
	     'Io': fun {$ X} Io(X) end)
end
