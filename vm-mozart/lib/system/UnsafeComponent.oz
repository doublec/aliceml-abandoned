%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
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
   UrlComponent('Url$': Url) at '../utility/Url'
export
   %% For use by other Oz programs:
   ComponentToFunctor
   FunctorToComponent
   %% For use by Alice programs:
   'UnsafeComponent$': Component
define
   IoException = {NewUniqueName 'IO.Io'}
   SitedException = {NewUniqueName 'Component.Sited'}
   CorruptException = {NewUniqueName 'Component.Corrupt'}

   proc {Trace Title Msg}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#'\n'}
      end
   end

   proc {RaiseIoException U N E}
      %--** cause not of type exn
      {Exception.raiseError
       alice(IoException(name: U function: {ByteString.make N} cause: E))}
   end

   Extension = {ByteString.make
		case {VirtualString.toString DefaultURL.functorExt}
		of &.|Rest then Rest
		[] S then S
		end}

   fun {LabelToOz Label}
      case Label of 'NUM'(I) then I
      [] 'ALPHA'(S) then {VirtualString.toAtom S}
      end
   end

   fun {OzToLabel F}
      if {IsInt F} then 'NUM'(F)
      else 'ALPHA'({ByteString.make F})
      end
   end

   fun {SigToOz SigOpt}
      case SigOpt of 'NONE' then nil
      [] 'SOME'(Sig) then sig(Sig)
      end
   end

   fun {OzToSig Sig}
      case Sig of sig(unit) then 'NONE'   % produced by the hybrid compiler
      [] sig(Sig) then 'SOME'(Sig)   % Stockhausen component
      else 'NONE'   % non-Stockhausen component
      end
   end

   fun {ComponentToFunctor Component}
      case Component
      of 'UNEVALUATED'(imports: Imports body: Body sign: Sig) then
	 {Functor.new
	  {List.toRecord 'import'
	   {Record.foldR Imports
	    fun {$ Label#URL#Sig In}
	       {LabelToOz Label}#
	       info('from': {VirtualString.toAtom {Url.toString URL}}
		    'type': {SigToOz Sig})|In
	    end nil}}
	  {SigToOz Sig} Body}
      [] 'EVALUATED'(Sig Str) then
	 {Functor.new 'import' {SigToOz Sig} fun {$ _} Str end}
      end
   end

   fun {FunctorToComponent F}
      if {Not {Functor.is F}} then
	 {Exception.raiseError alice(CorruptException)}
      end
      'UNEVALUATED'(imports:
		       {List.toTuple '#[]'
			{Record.foldRInd F.'import'
			 fun {$ ModName Desc Rest}
			    {OzToLabel ModName}#
			    {Url.fromString
			     {ByteString.make
			      case {CondSelect Desc 'from' unit}
			      of unit then
				 {OzURL.toVirtualString
				  {DefaultURL.nameToUrl ModName}}
			      [] URL then URL
			      end}}#
			    {OzToSig {CondSelect Desc 'type' nil}}|Rest
			 end nil}}
		    body: F.apply
		    sign: {OzToSig F.'export'})
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
      fun {Load U}
	 HU  = {HackInfo {ByteString.toString U}} 
	 URL = {OzURL.make HU}
      in
	 if {IsOzScheme URL} then
	    %--** just acquire (do not link)
	    'EVALUATED'('NONE' {Module.link [HU]}.1)
	 elseif {IsNative URL} then
	    'EVALUATED'('NONE' {Module.link [HU]}.1)
	 else
	    {FunctorToComponent try {Pickle.load HU}
				catch E=error(url(load _) ...) then
				   {RaiseIoException U 'load' E} unit
				end}
	 end
      end
   end

   Component =
   'Component'('Sited': SitedException
	       '\'Sited': SitedException
	       'Corrupt': CorruptException
	       '\'Corrupt': CorruptException
	       'extension': Extension
	       'save':
		  fun {$ Filename Component}
		     {Trace 'component' 'save '#Filename}
		     try
			{Pickle.saveWithCells {ComponentToFunctor Component}
			 Filename '' 9}
		     catch error(dp(generic 'pickle:nogoods' ...) ...)
		     then {Exception.raiseError alice(SitedException)}
		     end
		     unit
		  end
	       'load':
		  fun {$ URL} U in
		     U = {Url.toString URL}
		     {Trace 'component' 'load '#U}
		     {Load U}
		  end
	       'apply':
		  fun {$ Body Imports}
		     {Body {List.toRecord 'IMPORT'
			    {Record.foldR Imports
			     fun {$ Label#Str Rest}
				{LabelToOz Label}#Str|Rest
			     end nil}}}
		  end)
end
