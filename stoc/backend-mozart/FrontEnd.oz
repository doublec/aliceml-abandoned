%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(text pipe)
   Compiler(engine interface)
export
   TranslateFile
define
   class TextPipe from Open.pipe Open.text end

   fun {StockhausenToIntermediate File} Pipe S in
      Pipe = {New TextPipe
	      init(cmd: 'sml-cm'
		   args: ['@SMLload=../frontend/stoc-frontend' File])}
      {Pipe getS(?S)}
      {Pipe close()}
      S
   end

   fun {TranslateFile File} C VS in
      C = {New Compiler.engine init()}
      _ = {New Compiler.interface init(C auto)}
      {C enqueue(setSwitch(expression true))}
      VS = {StockhausenToIntermediate File}
      {C enqueue(feedVirtualString(VS return(result: $)))}
   end
end
