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
   Pickle(load saveWithCells)
   DefaultURL(functorExt ozScheme nameToUrl)
   Property(get)
   Resolve(trace)
   System(printError)
   UrlComponent('Url$': Url) at '../utility/Url'   %--** does not work
export
   %% For use by other Oz programs:
   ComponentToFunctor
   FunctorToComponent
   %% For use by Alice programs:
   module: ComponentComponent
define
   ComponentComponent = tuple(Component)

   I_cause    = 2
   I_function = 3
   I_name     = 4

   I_body    = 2
   I_imports = 3
   I_sign    = 4

   ALPHA = 0
   NUM   = 1

   NONE = 0
   SOME = 1

   EVALUATED   = 0
   UNEVALUATED = 1

   IoException = {NewUniqueName 'IO.Io'}
   SitedException = {NewUniqueName 'Component.Sited'}
   CorruptException = {NewUniqueName 'Component.Corrupt'}

   proc {Trace Title Msg}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#'\n'}
      end
   end

   Extension = {ByteString.make
		case {VirtualString.toString DefaultURL.functorExt}
		of &.|Rest then Rest
		[] S then S
		end}

   fun {LabelToOz Label}
      case Label of tag(!NUM I) then I
      [] tag(!ALPHA S) then {VirtualString.toAtom S}
      end
   end

   fun {OzToLabel F}
      if {IsInt F} then tag(NUM F)
      else tag(ALPHA {ByteString.make F})
      end
   end

   fun {SigToOz SigOpt}
      %--**
      case SigOpt of !NONE then nil
      [] tag(!SOME Sig) then sig(Sig)
      end
   end

   fun {OzToSig Sig}
      %--**
      case Sig of sig(unit) then NONE   % produced by the hybrid compiler
      [] sig(Sig) then tag(SOME Sig)   % Stockhausen component
      else NONE   % non-Stockhausen component
      end
   end

   fun {ComponentToFunctor Component}
      case Component
      of tag(!UNEVALUATED I_imports: Imports I_body: Body I_sign: Sig) then
	 {Functor.new
	  {List.toRecord 'import'
	   {Record.foldR Imports
	    fun {$ tuple(Label URL Sig) In}
	       {LabelToOz Label}#
	       info('from': {VirtualString.toAtom {Url.toString URL}}
		    'type': {SigToOz Sig})|In
	    end nil}}
	  {SigToOz Sig} Body}
      [] tag(!EVALUATED Sig Str) then
	 {Functor.new 'import' {SigToOz Sig} fun {$ _} Str end}
      end
   end

   fun {FunctorToComponent F}
      if {Not {Functor.is F}} then
	 {Exception.raiseError alice(CorruptException)}
      end
      tag(UNEVALUATED
	  I_imports:
	     {List.toTuple vector
	      {Record.foldRInd F.'import'
	       fun {$ ModName Desc Rest}
		  tuple({OzToLabel ModName}
			{Url.fromString
			 {ByteString.make
			  case {CondSelect Desc 'from' unit} of unit then
			     {OzURL.toVirtualString
			      {DefaultURL.nameToUrl ModName}}
			  [] URL then URL
			  end}}
			{OzToSig {CondSelect Desc 'type' nil}})|Rest
	       end nil}}
	  I_body: F.apply   %--** does not work
	  I_sign: {OzToSig F.'export'})
   end

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
	 %--** try to load a Stockhausen pickle
	 HU  = {HackInfo {ByteString.toString U}} 
	 URL = {OzURL.make HU}
      in
	 if {IsOzScheme URL} then
	    %--** just acquire (do not link)
	    tag(EVALUATED NONE {Module.link [HU]}.1)
	 elseif {IsNative URL} then
	    tag(EVALUATED NONE {Module.link [HU]}.1)
	 else
	    {FunctorToComponent
	     try continue(arg({Pickle.load HU}) TaskStack.2)
	     catch E=error(url(load _) ...) then
		exception(nil con(IoException
				  I_name: U
				  I_function: {ByteString.make 'load'}
				  I_cause: E)   %--** cause not of type exn
			  TaskStack.2)
	     end}
	 end
      end
   end

   I_PrimeCorrupt = 1
   I_PrimeSited   = 2
   I_Corrupt      = 3
   I_Sited        = 4
   I_apply        = 5
   I_extension    = 6
   I_load         = 7
   I_save         = 8

   Component =
   tuple(I_Sited: value(SitedException)
	 I_PrimeSited: value(SitedException)
	 I_Corrupt: value(CorruptException)
	 I_PrimeCorrupt: value(CorruptException)
	 I_extension: value(Extension)   %--** "stc"
	 I_save:
	    fun {$ Filename Component TaskStack}
	       {Trace 'component' 'save '#Filename}
	       try
		  {Pickle.saveWithCells {ComponentToFunctor Component}
		   Filename '' 9}   %--**
		  continue(tuple() TaskStack.2)
	       catch error(dp(generic 'pickle:nogoods' ...) ...)
	       then exception(nil SitedException TaskStack.2)
	       end
	    end#rr_t
	 I_load:
	    fun {$ URL TaskStack} U in
	       U = {Url.toString URL}
	       {Trace 'component' 'load '#U}
	       {Load U TaskStack}
	    end#r_t
	 I_apply:
	    fun {$ Body Imports}
	       %--** suspend on labels
	       %--** is wrong
	       {Body {List.toRecord 'IMPORT'
		      {Record.foldR Imports
		       fun {$ tuple(Label Str) Rest}
			  {LabelToOz Label}#Str|Rest
		       end nil}}}
	    end#rr_v)
end
