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
   '$IO': IO
define
   IO = 'IO'('Io': {NewUniqueName 'IO.Io'})
end
