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
   Pickle(load saveWithCells)
   DefaultURL(functorExt)
   Property(get)
export
   'UnsafePickle$': UnsafePickle_Module
define
   IoException = {NewUniqueName 'IO.Io'}
   CorruptException = {NewUniqueName 'Pickle.Corrupt'}
   SitedException = {NewUniqueName 'Pickle.Sited'}

   Extension = {ByteString.make
		case {VirtualString.toString DefaultURL.functorExt}
		of &.|Rest then Rest
		[] S then S
		end}

   fun {LoadPickleAndSign U}
      try F in
	 F = {Pickle.load U}
	 case F.'export'
	 of sig(unit) then
	    %% component produced by the hybrid compiler
	    'NONE'
	 [] sig(Sig) then
	    %% Stockhausen component
	    'SOME'(F#Sig)
	 else
	    %% non-Stockhausen component
	    'NONE'
	 end
      catch E=error(url(load _) ...) then
	 %% load error
	 {Exception.raiseError
	  alice(IoException(name: U
			    function: {ByteString.make 'loadSign'}
			    cause: E))}   %--** cause not of type exn
	 unit
      end
   end

   UnsafePickle_Module =
   'UnsafePickle'('Corrupt': CorruptException
		  '\'Corrupt': CorruptException
		  'Sited': SitedException
		  '\'Sited': SitedException
		  'extension': Extension
		  'load':
		     fun {$ URL}
			case {LoadPickleAndSign URL} of 'SOME'(F#(_#Type)) then
			   Type#{{Property.get 'alice.modulemanager'}
				 apply(url: URL F $)}
			[] 'NONE' then
			   {Exception.raiseError alice(CorruptException)} unit
			end
		     end
		  'save':
		     fun {$ Filename Type Value}
			try
			   {Pickle.saveWithCells
			    {Functor.new 'import' sig({NewName}#Type)
			     fun {$ _} Value end} Filename '' 9}
			catch error(dp(generic 'pickle:nogoods' ...) ...) then
			   {Exception.raiseError alice(SitedException)}
			end
			unit
		     end
		  'loadSign':
		     fun {$ URL}
			case {LoadPickleAndSign URL}
			of 'SOME'(_#Sign) then 'SOME'(Sign)
			[] 'NONE' then 'NONE'
			end
		     end
		  'replaceSign':
		     fun {$ U Sig Filename} F1 F2 in
			F1 = {Pickle.load U}
			F2 = {Functor.new F1.'import' sig(Sig) F1.'apply'}
			{Pickle.saveWithCells F2 Filename '' 9}
			unit
		     end)
end
