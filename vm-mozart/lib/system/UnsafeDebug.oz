%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2001
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
   'UnsafeDebug$': Debug
define
   Debug =
   'Debug'('setPrintDepth':
	      fun {$ N} {Property.put 'print.depth' N} unit end
	   'setPrintWidth':
	      fun {$ N} {Property.put 'print.width' N} unit end
	   'toString':
	      fun {$ X}
		 {ByteString.make
		  {Value.toVirtualString X
		   {Property.get 'print.depth'}
		   {Property.get 'print.width'}}}
	      end
	   'print':
	      fun {$ X} {System.show X} unit end
	   'inspect':
	      fun {$ X} {Inspector.inspect X} unit end
	   'Print$':
	      fun {$ X} {System.show X} unit end
	   'Inspect$':
	      fun {$ X} {Inspector.inspect X} unit end
	   'InspectType$':
	      fun {$ X} {Inspector.inspect X} unit end
	   'InspectSig$':
	      fun {$ X} {Inspector.inspect X} unit end)
end
