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
   Parse
define
   C = {New Compiler.engine init()}
   _ = {New Compiler.interface init(C auto)}
   {C enqueue(mergeEnv(env('NONE': none
			   'SOME': some
			   'WordLit': wordLit
			   'IntLit': intLit
			   'CharLit': charLit
			   'StringLit': stringLit
			   'RealLit': realLit
			   'ExId': exId
			   'InId': inId
			   'Lab': lab
			   'Id': id
			   'ShortId': shortId
			   'LongId': longId
			   'Field': field
			   'LitExp': litExp
			   'VarExp': varExp
			   'ConExp': conExp
			   'TupExp': tupExp
			   'RecExp': recExp
			   'SelExp': selExp
			   'FunExp': funExp
			   'AppExp': appExp
			   'AdjExp': adjExp
			   'AndExp': andExp
			   'OrExp': orExp
			   'IfExp': ifExp
			   'WhileExp': whileExp
			   'SeqExp': seqExp
			   'CaseExp': caseExp
			   'RaiseExp': raiseExp
			   'HandleExp': handleExp
			   'LetExp': letExp
			   'Match': match
			   'LitPat': litPat
			   'VarPat': varPat
			   'ConPat': conPat
			   'TupPat': tupPat
			   'RecPat': recPat
			   'AsPat': asPat
			   'AltPat': altPat
			   'NegPat': negPat
			   'GuardPat': guardPat
			   'WithPat': withPat
			   'ValDec': valDec
			   'ConDec': conDec)))}
   {C enqueue(setSwitch(expression true))}

   fun {Parse File}
      {C enqueue(feedFile(File return(result: $)))}
   end
end
