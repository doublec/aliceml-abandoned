%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2002
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
	  {List.toTuple 'import'
	   {Record.foldR Imports
	    fun {$ URL#Sig In}
	       info('from': {VirtualString.toAtom URL}
		    'type': {SigToOz Sig})|In
	    end nil}}
	  {SigToOz Sig} Body}
      [] 'EVALUATED'(Sig Str) then
	 {Functor.new 'import' {SigToOz Sig} fun {$ _} Str end}
      end
   end

   local
      proc {ForAll2 Xs Ys P}
	 case Xs#Ys of nil#nil then skip
	 [] (X|Xr)#(Y|Yr) then {P X Y} {ForAll2 Xr Yr P}
	 end
      end
   in
      proc {ReplaceArity R Ari ?NewR}
	 NewR = {Record.make {Label R} Ari}
	 {ForAll2 {Arity R} Ari proc {$ Old New} NewR.New = R.Old end}
      end
   end

   fun {FunctorToComponent F} Rec Body in
      if {Not {Functor.is F}} then
	 {Exception.raiseError alice(CorruptException)}
      end
      Rec = {Record.make '#' {Record.foldRInd F.'import'
			      fun {$ Fea _ Rest} Fea|Rest end nil}}
      Body = if {IsTuple Rec} then F.apply
	     else Ari in
		Ari = {Arity Rec}
		fun {$ IMPORT} {F.apply {ReplaceArity IMPORT Ari}} end
	     end
      'UNEVALUATED'(imports:
		       {List.toTuple '#[]'
			{Record.foldRInd F.'import'
			 fun {$ ModName Desc Rest}
			    {ByteString.make
			     case {CondSelect Desc 'from' unit} of unit then
				{OzURL.toVirtualString
				 {DefaultURL.nameToUrl ModName}}
			     [] URL then URL
			     end}#
			    {OzToSig {CondSelect Desc 'type' nil}}|Rest
			 end nil}}
		    body: Body
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
	       'getInitialTable': fun {$ unit} '#[]' end   %--**
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
		  fun {$ U}
		     {Trace 'component' 'load '#U}
		     {Load U}
		  end)
end
