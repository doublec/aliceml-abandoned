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

declare [Frontend CodeGen] = {Module.link ['Frontend.ozf' 'CodeGen.ozf']}

{Browse {Frontend.translateFile '../../test/bug.aus'}}

{Browse {CodeGen.translate {Frontend.translateFile '../../test/bug.aus'}}}

{System.showInfo 'parsing and simplifying ...'}
case {Frontend.translateFile '../../test/infer.aus'}
%      '/home/kornstae/stockhausen/lib/bootstrap/Bootstrap.aus'}
of unit then skip
elseof AST then F in
   {System.showInfo 'generating code ...'}
   F = {CodeGen.translate AST}
   {System.showInfo 'executing ...'}
   {Module.apply [F] _}
   {System.showInfo 'done'}
end
