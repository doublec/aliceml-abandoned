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
   I_extension       = 5
   I_getInitialTable = 6
   I_load            = 7
   I_save            = 8

   Component =
   tuple(I_Sited: value(SitedException)
	 I_PrimeSited: value(SitedException)
	 I_Corrupt: value(CorruptException)
	 I_PrimeCorrupt: value(CorruptException)
	 I_extension: value({ByteString.make "stc"})
	 I_getInitialTable: {Property.get 'alice.getInitialTable'}#n_v
	 I_save: Pickle.save#ri_t
	 I_load: Pickle.load#r_t)
end
