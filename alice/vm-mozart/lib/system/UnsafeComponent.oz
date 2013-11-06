%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Contributor:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
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
   Pickle(saveWithCells load packWithCells unpack)
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
   SitedInternalException = {NewUniqueName 'UnsafeComponent.SitedInternal'}
   CorruptInternalException = {NewUniqueName 'UnsafeComponent.CorruptInternal'}
   SitedException    = {NewUniqueName 'Component.Sited'}
   CorruptException  = {NewUniqueName 'Component.Corrupt'}
   NotFoundException = {NewUniqueName 'Component.NotFound'}
   MismatchException = {NewUniqueName 'Component.Mismatch'}
   EvalException     = {NewUniqueName 'Component.Eval'}
   FailureException  = {NewUniqueName 'Component.Failure'}
   NativeException   = {NewUniqueName 'Component.Native'}

   proc {Trace Title Msg U}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#' '#U#'\n'}
      end
   end

   proc {RaiseNative Msg}
      {Exception.raiseError alice(NativeException({ByteString.make Msg}))}
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
      case Sig of sig(Sig) andthen {IsDet Sig} andthen Sig == unit then
	 'NONE'   % produced by the bootstrap compiler
      [] sig(Sig) then 'SOME'(Sig)   % Alice component
      else 'NONE'   % non-Alice component
      end
   end

   fun {ComponentToFunctor Component}
      case Component
      of 'UNEVALUATED'(imports: Imports body: Body inf: Sig) then
	 {Functor.new
	  {List.toTuple 'import'
	   {Record.foldR Imports
	    fun {$ URL#Sig In}
	       info('from': {VirtualString.toAtom URL}
		    'type': {SigToOz Sig})|In
	    end nil}}
	  {SigToOz Sig} Body}
      [] 'EVALUATED'(inf: Sig 'mod': Mod) then
	 {Functor.new 'import' {SigToOz Sig} fun {$ _} Mod end}
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
      if {Not {Functor.is F}} then unit
      else
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
		       inf: {OzToSig F.'export'})
      end
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
	    'EVALUATED'(inf: 'NONE' 'mod': {Module.link [HU]}.1)
	 elseif {IsNative URL} then
	    'EVALUATED'(inf: 'NONE' 'mod': {Module.link [HU]}.1)
	 else {FunctorToComponent {Pickle.load HU}}
	 end
      end
   end

   proc {Save X Filename}
      try
	 {Pickle.saveWithCells X Filename '' 9}
      catch error(dp(generic 'pickle:resources' _ Args) ...) then
	 for Y in {List.toRecord '#' Args}.'Resources' do
	    case {Value.status Y} of free then {Wait Y}
	    [] kinded(_) then {Wait Y}
	    [] future then {Wait Y}
	    [] det(int) then skip
	    [] det(float) then skip
	    [] det(record) then skip
	    [] det(tuple) then skip
	    [] det(atom) then skip
	    [] det(name) then skip
	    [] det(procedure) then skip
	    [] det(cell) then skip
	    [] det(byteString) then skip
	    [] det(bitString) then skip
	    [] det(array) then skip
	    [] det(bitArray) then skip
	    [] det('class') then skip
	    [] det(word) then skip
	    else {Exception.raiseError alice(SitedException)}
	    end
	 end
	 {Save X Filename}
      end
   end

   Component =
   'Component'('SitedInternal': SitedInternalException
	       '\'SitedInternal': SitedInternalException
	       'CorruptInternal': CorruptInternalException
	       '\'CorruptInternal': CorruptInternalException
	       'Sited': SitedException
	       '\'Sited': SitedException
	       'Corrupt': CorruptException
	       '\'Corrupt': CorruptException
	       'NotFound': NotFoundException
	       '\'NotFound': NotFoundException
	       'Mismatch': fun {$ X} {Adjoin X MismatchException} end
	       '\'Mismatch': MismatchException
	       'Eval': fun {$ Exn} EvalException(Exn) end
	       '\'Eval': EvalException
	       'Failure': fun {$ Url Exn} FailureException(Url Exn) end
	       '\'Failure': FailureException
	       'Native': fun {$ Msg} NativeException(Msg) end
	       '\'Native': NativeException
	       'extension': Extension
	       'getInitialTable':
		  fun {$ unit} Entries N V in
		     Entries = {Dictionary.entries
				{Property.get 'alice.initialComponentTable'}}
		     N = {Length Entries}
		     V = {MakeTuple '#[]' N}
		     {List.forAllInd Entries
		      proc {$ I Key#(Inf#Mod)}
			 V.I = {ByteString.make Key}#Inf#Mod
		      end}
		     V
		  end
	       'save':
		  fun {$ Filename Component}
		     {Trace 'component' 'save ' Filename}
		     try
			{Save {ComponentToFunctor Component} Filename}
		     catch error(dp(generic 'pickle:nogoods' ...) ...)
		     then {Exception.raiseError alice(SitedException)}
		     end
		     unit
		  end
	       'load':
		  fun {$ U} Component in
		     {Trace 'component' 'load ' U}
		     try
			Component = {Load U}
		     catch error(url(load _) ...) then
			{Exception.raiseError alice(NotFoundException)}
		     [] error(foreign(dlOpen _)) then
			{Exception.raiseError alice(NotFoundException)}
		     [] error(foreign(dlOpen _ _)) then
			{Exception.raiseError alice(CorruptException)}
		     [] error(foreign(cannotFindOzInitModule _)) then
			{Exception.raiseError alice(CorruptException)}
		     [] error(dp(generic ...) ...) then
			{Exception.raiseError alice(CorruptException)}
		     end
		     case Component of unit then
			{Exception.raiseError alice(CorruptException)} unit
		     else Component
		     end
		  end
	       'linkNative':
		  fun {$ U} Component in
		     {Trace 'component' 'linkNative ' U}
		     try
			Component = {Load U#'{native}'}
		     catch error(url(load _) ...) then
			{RaiseNative 'native component not found'}
		     [] error(dp(generic ...) ...) then
			{RaiseNative 'not a native component'}
		     [] error(foreign(dlOpen _)) then
			{RaiseNative 'linking of native component failed'}
		     [] error(foreign(dlOpen _ Msg)) then
			{RaiseNative Msg}
		     [] error(foreign(cannotFindOzInitModule _)) then
			{Exception.raiseError alice(CorruptException)}
		     [] _ then
			{RaiseNative 'unknown error'}
		     end
		     case Component of unit then
			{RaiseNative 'not a native component'} unit
		     else Component
		     end
		  end
	       'pack_':
		  fun {$ Component}
		     try
			{Pickle.packWithCells {ComponentToFunctor Component}}
		     catch error(dp(generic 'pickle:nogoods' ...) ...)
		     then {Exception.raiseError alice(SitedException)} unit
		     end
		  end
	       'unpack_':
		  fun {$ S}
		     case {FunctorToComponent {Pickle.unpack S}} of unit then
			{Exception.raiseError alice(CorruptException)} unit
		     elseof Component then Component
		     end
		  end)
end
