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

declare
[Frontend CodeGen Assembler] =
{Module.link ['Frontend.ozf' 'CodeGen.ozf' 'Assembler.ozf']}

{Browse {Frontend.translateFile '../../test/bug.aus'}}

{Browse {CodeGen.translate {Frontend.translateFile '../../test/bug.aus'}}}

{System.showInfo 'parsing and simplifying ...'}
case {Frontend.translateFile '../../test/infer.aus'}
%      '/home/kornstae/stockhausen/lib/bootstrap/Bootstrap.aus'}
of unit then skip
elseof AST then
   {System.showInfo 'generating code ...'}
   case {CodeGen.translate AST} of Globals#Code then P in
      {System.showInfo 'assembling ...'}
      P = {Assembler.assemble Code Globals switches}
      {System.showInfo 'executing ...'}
      {P}
      {System.showInfo 'done'}
   end
end
