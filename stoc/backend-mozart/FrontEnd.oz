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
   Compiler(engine interface)
export
   Translate
define
   fun {Translate File} C in
      C = {New Compiler.engine init()}
      _ = {New Compiler.interface init(C auto)}
      {C enqueue(setSwitch(expression true))}
      {C enqueue(feedFile(File return(result: $)))}
   end
end
