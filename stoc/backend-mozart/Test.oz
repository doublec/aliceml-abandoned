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

local
   [FrontEnd Annotate] = {Module.link ['FrontEnd.ozf' 'Annotate.ozf']}
   AST = {FrontEnd.translate 'examples/Test5'}
in
   {Browse AST}
   {Annotate.annotate AST}
end
