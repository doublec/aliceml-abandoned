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

%%
%% <dec> ::= conDec(<info> <id> <bool>)
%%        |  valDec(<info> [<id>] <exp>)
%%
%% <exp> ::= litExp(<info> <lit>)
%%        |  varExp(<info> <long id>)
%%        |  conExp(<info> <long id> <optional exp>)
%%        |  tupExp(<info> [<exp>])
%%        |  recExp(<info> [<field exp>])
%%        |  selExp(<info> <lab>)
%%        |  funExp(<info> <id> <exp>)
%%        |  appExp(<info> <exp> <exp>)
%%        |  adjExp(<info> <exp> <exp>)
%%        |  andExp(<info> <exp> <exp>)
%%        |  orExp(<info> <exp> <exp>)
%%        |  ifExp(<info> <exp> <exp> <exp>)
%%        |  whileExp(<info> <exp> <exp>)
%%        |  seqExp(<info> [<exp>])
%%        |  caseExp(<info> <exp> [<match>] <long id>)   % exception to raise
%%        |  raiseExp(<info> <exp>)
%%        |  handleExp(<info> <exp> <id> <exp>)
%%        |  letExp(<info> [<dec>] <exp>)
%% <optional exp> ::= none
%%                 |  some(<exp>)
%% <field exp> ::= field(<info> <lab> <exp>)
%% <match> ::= match(<info> <pat> <exp>)
%%
%% <pat> ::= litPat(<info> <lit>)
%%        |  varPat(<info> <id>)
%%        |  conPat(<info> <long id> <optional pat>)
%%        |  tupPat(<info> [<pat>])
%%        |  recPat(<info> [<field pat>] <has dots>)
%%        |  asPat(<info> <id> <pat>)
%%        |  altPat(<info> [<pat>])
%%        |  negPat(<info> <pat>)
%%        |  guardPat(<info> <pat> <exp>)
%%        |  withPat(<info> <pat> [<dec>])
%% <optional pat> ::= none
%%                 |  some(<pat>)
%% <field pat> ::= field(<info> <lab> <pat>)
%% <has dots> ::= <bool>
%%
%% <lit> ::= wordLit(<int>)
%%        |  intLit(<int>)
%%        |  charLit(<char>)
%%        |  stringLit(<string>)
%%        |  realLit(<string>)
%%
%% <long id> ::= shortId(<info> <id>)
%%            |  longId(<info> <long id> <id>)
%% <id> ::= id(<info> <stamp> <print name>)
%% <stamp> ::= <int>
%% <print name> ::= exId(<string>) | inId
%%
%% <lab> ::= lab(<info> <string>)
%%
%% <info> ::= ...
%%

functor
export
   GetPrintName
   InfoOf
   LabToFeature
define
   fun {GetPrintName Id}
      case Id of id(_ _ exId(S)) then {String.toAtom S}
      [] id(_ _ inId) then unit
      end
   end

   fun {InfoOf X}
      case X of conDec(I _ _) then I
      [] valDec(I _ _) then I
      [] litExp(I _) then I
      [] varExp(I _) then I
      [] conExp(I _ _) then I
      [] tupExp(I _) then I
      [] recExp(I _) then I
      [] selExp(I _) then I
      [] funExp(I _ _) then I
      [] appExp(I _ _) then I
      [] adjExp(I _ _) then I
      [] andExp(I _ _) then I
      [] orExp(I _ _) then I
      [] ifExp(I _ _ _) then I
      [] whileExp(I _ _) then I
      [] seqExp(I _) then I
      [] caseExp(I _ _ _) then I
      [] raiseExp(I _) then I
      [] handleExp(I _ _ _) then I
      [] letExp(I _ _) then I
      [] litPat(I _) then I
      [] varPat(I _) then I
      [] conPat(I _ _) then I
      [] tupPat(I _) then I
      [] recPat(I _ _) then I
      [] asPat(I _ _) then I
      [] altPat(I _) then I
      [] negPat(I _) then I
      [] guardPat(I _ _) then I
      [] withPat(I _ _) then I
      [] shortId(I _ _) then I
      [] longId(I _ _) then I
      [] id(I _ _) then I
      [] lab(I _) then I
      end
   end

   fun {LabToFeature S}
      if {String.isInt S} then {String.toInt S}
      else {String.toAtom S}
      end
   end
end
