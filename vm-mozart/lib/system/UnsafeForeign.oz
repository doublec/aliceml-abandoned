%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2003
%%%   Leif Kornstaedt, 2003
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Error
export
   'UnsafeForeign$' : Foreign
define
   fun {Catch F H}
      try {F unit} catch E then {H E} end
   end

   fun {ExnMessage E}
      {ByteString.make
       {Error.messageToVirtualString {Error.exceptionToMessage E}}}
   end
   
   Foreign = 'Foreign'(
		'catch' : Catch
		'exnMessage' : ExnMessage) 
end
