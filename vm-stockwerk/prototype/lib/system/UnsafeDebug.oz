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
   System(show)
   Property(put get)
   Browser(browse)
   Inspector(inspect)
export
   module: DebugComponent
define
   DebugComponent = tuple(Debug)

   I_Inspect       = 1
   I_InspectSig    = 2
   I_InspectType   = 3
   I_Print         = 4
   I_inspect       = 5
   I_print         = 6
   I_setPrintDepth = 7
   I_setPrintWidth = 8
   I_toString      = 9

   Debug =
   tuple(I_setPrintDepth:
	    fun {$ N} {Property.put 'print.depth' N} tuple() end#r_v
	 I_setPrintWidth:
	    fun {$ N} {Property.put 'print.width' N} tuple() end#r_v
	 I_toString:
	    fun {$ X}
	       {ByteString.make
		{Value.toVirtualString X
		 {Property.get 'print.depth'}
		 {Property.get 'print.width'}}}
	    end#i_v
	 I_print:
	    fun {$ X} {System.show X} tuple() end#i_v
	 I_inspect:
	    fun {$ X} {Inspector.inspect X} table() end#i_v
	 I_Print:
	    fun {$ X} {System.show X} tuple() end#i_v
	 I_Inspect:
	    fun {$ X} {Inspector.inspect X} tuple() end#i_v
	 I_InspectType:
	    fun {$ X} {Inspector.inspect X} tuple() end#i_v
	 I_InspectSig:
	    fun {$ X} {Inspector.inspect X} tuple() end#i_v)
end
