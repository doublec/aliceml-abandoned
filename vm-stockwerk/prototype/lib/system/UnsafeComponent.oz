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
   Property(get)
   Pickle(load save) at '../../Pickle.ozf'
require
   Helper(pushCall: PushCall) at '../../Helper.ozf'
export
   module: ComponentComponent
define
   ComponentComponent = tuple(Component)

   SitedException = {NewUniqueName 'Component.Sited'}
   CorruptException = {NewUniqueName 'Component.Corrupt'}

   I_PrimeCorrupt    = 1
   I_PrimeSited      = 2
   I_Corrupt         = 3
   I_Sited           = 4
   I_apply           = 5
   I_extension       = 6
   I_getInitialTable = 7
   I_load            = 8
   I_save            = 9

   Component =
   tuple(I_Sited: value(SitedException)
	 I_PrimeSited: value(SitedException)
	 I_Corrupt: value(CorruptException)
	 I_PrimeCorrupt: value(CorruptException)
	 I_extension: value({ByteString.make "stc"})
	 I_getInitialTable: {Property.get 'alice.getInitialTable'}#n_v
	 I_save: Pickle.save#ri_t
	 I_load: Pickle.load#r_t
	 I_apply:
	    fun {$ Body Imports TaskStack}
	       {PushCall arg({Record.map Imports fun {$ tuple(_ Str)} Str end})
		Body TaskStack.2}
	    end#rr_t)
end
