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
   System(show)
   Property(put get)
   Browser(browse)
   Inspector(inspect)
export
   '$Tools': Tools
define
   Tools = 'Tools'('setPrintDepth':
		      fun {$ N} {Property.put 'print.depth' N} unit end
		   'setPrintWidth':
		      fun {$ N} {Property.put 'print.width' N} unit end
		   'print':
		      fun {$ X} {System.show X} unit end
		   'toString':
		      fun {$ X}
			 {ByteString.make
			  {Value.toVirtualString X
			   {Property.get 'print.depth'}
			   {Property.get 'print.width'}}}
		      end
		   'inspect':
		      fun {$ X} {Inspector.inspect X} unit end
		   'browse':
		      fun {$ X} {Browser.browse X} unit end)
end
