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
   OzURL(make toVirtualString) at 'x-oz://system/URL'
   Module(link)
   DefaultURL(ozScheme nameToUrl)
   Property(get)
   Resolve(trace)
   System(printError)
   Pickle(load save) at '../../Pickle.ozf'
export
   module: ComponentComponent
define
   ComponentComponent = tuple(Component)

   I_cause    = 2
   I_function = 3
   I_name     = 4

   %I_body    = 2
   %I_imports = 3
   %I_sign    = 4

   NONE = 0
   %SOME = 1

   EVALUATED   = 0
   %UNEVALUATED = 1

   IoException = {NewUniqueName 'IO.Io'}
   SitedException = {NewUniqueName 'Component.Sited'}
   CorruptException = {NewUniqueName 'Component.Corrupt'}

   proc {Trace Title Msg}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#'\n'}
      end
   end

   Extension = "stc"

   local
      OzScheme = {VirtualString.toString DefaultURL.ozScheme}

      fun {IsOzScheme URL}
	 {CondSelect URL scheme unit} == OzScheme
      end

      fun {IsNative URL}
	 {HasFeature {CondSelect URL info info} 'native'}
      end

      fun {HackInfo S}
	 case S of &%|&7|&b|Rest then &{|{HackInfo Rest}
	 [] &%|&7|&d|Rest then &}|{HackInfo Rest}
	 [] &%|&7|&B|Rest then &{|{HackInfo Rest}
	 [] &%|&7|&D|Rest then &}|{HackInfo Rest}
	 [] C|Rest then C|{HackInfo Rest}
	 [] nil then nil
	 end
      end
   in
      fun {Load U TaskStack}
	 HU  = {HackInfo {ByteString.toString U}}
	 URL = {OzURL.make HU}
      in
	 if {IsOzScheme URL} orelse {IsNative URL} then
	    tag(EVALUATED NONE {Module.link [HU]}.1)
	 else
	    try continue(arg({Pickle.load HU}) TaskStack.2)
	    catch E=system(os(os ...) ...) then
	       try continue(arg({Pickle.load HU#'.'#Extension}) TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil con(IoException
				    I_name: U
				    I_function: {ByteString.make 'load'}
				    I_cause: E)   %--** cause not of type exn
			    TaskStack.2)
	       end
	    end
	 end
      end
   end

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
	 I_extension: value(Extension)
	 I_getInitialTable: {Property.get 'alice.getInitialTable'}#n_v
	 I_save: Pickle.save#ri_t
	 I_load:
	    fun {$ URL TaskStack}
	       {Trace 'component' 'load '#URL}
	       {Load URL TaskStack}
	    end#r_t
	 I_apply:
	    fun {$ Body Imports TaskStack}
	       continue(arg({Record.map Imports fun {$ tuple(_ Str)} Str end})
			{Body.1.1.pushCall Body TaskStack.2})
	    end#rr_t)
end
