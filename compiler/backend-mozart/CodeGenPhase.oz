%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Pickle(save)
   Word(toInt) at 'x-oz://boot/Word'
   PreboundComponent('$Prebound': Prebound) at 'x-alice:/common/Prebound.ozf'
   CodeGen(translate) at '../../../stoc/backend-mozart/CodeGen.ozf'
   UrlComponent('$Url': Url) at 'x-alice:/misc/Url.ozf'
export
   '$CodeGenPhase': CodeGenPhase
define
   SOME = 'SOME'
   NUM = 'NUM'
   ALPHA = 'ALPHA'
   Nullary = 'Nullary'
   Unary = 'Unary'
   Tuple = 'Tuple'
   Record = 'Record'
   WordLit = 'WordLit'
   IntLit = 'IntLit'
   CharLit = 'CharLit'
   StringLit = 'StringLit'
   RealLit = 'RealLit'
   ExId = 'ExId'
   InId = 'InId'
   Id = 'Id'
   LitTest = 'LitTest'
   TagTest = 'TagTest'
   TagAppTest = 'TagAppTest'
   ConTest = 'ConTest'
   ConAppTest = 'ConAppTest'
   RefAppTest = 'RefAppTest'
   TupTest = 'TupTest'
   RecTest = 'RecTest'
   LabTest = 'LabTest'
   VecTest = 'VecTest'
   PrintName = 'PrintName'
   AuxiliaryOf = 'AuxiliaryOf'
   OneArg = 'OneArg'
   TupArgs = 'TupArgs'
   RecArgs = 'RecArgs'
   ValDec = 'ValDec'
   RecDec = 'RecDec'
   EvalStm = 'EvalStm'
   RaiseStm = 'RaiseStm'
   ReraiseStm = 'ReraiseStm'
   HandleStm = 'HandleStm'
   EndHandleStm = 'EndHandleStm'
   TestStm = 'TestStm'
   SharedStm = 'SharedStm'
   ReturnStm = 'ReturnStm'
   IndirectStm = 'IndirectStm'
   ExportStm = 'ExportStm'
   LitExp = 'LitExp'
   PrimExp = 'PrimExp'
   NewExp = 'NewExp'
   VarExp = 'VarExp'
   TagExp = 'TagExp'
   ConExp = 'ConExp'
   RefExp = 'RefExp'
   TupExp = 'TupExp'
   RecExp = 'RecExp'
   SelExp = 'SelExp'
   VecExp = 'VecExp'
   FunExp = 'FunExp'
   AppExp = 'AppExp'
   SelAppExp = 'SelAppExp'
   TagAppExp = 'TagAppExp'
   ConAppExp = 'ConAppExp'
   RefAppExp = 'RefAppExp'
   PrimAppExp = 'PrimAppExp'
   AdjExp = 'AdjExp'

   local
      C = {NewCell 0}
   in
      proc {Gen ?N}
	 N = {Access C} + 1
	 {Assign C N}
      end
   end

   fun {TrAtom S}
      {String.toAtom {ByteString.toString S}}
   end

   fun {TrCoord (LL#LC)#(RL#RC)}
      LL#LC#RL#RC
   end

   fun {TrInfo '#'(region: Coord ...)}
      {TrCoord Coord}
   end

   fun {TrLit Lit}
      case Lit of WordLit(W) then wordLit({Word.toInt W})
      [] IntLit(I) then intLit(I)
      [] CharLit(C) then charLit(C)
      [] StringLit(S) then stringLit({ByteString.toString S})
      [] RealLit(S) then realLit({String.toFloat {ByteString.toString S}})
      end
   end

   fun {TrStamp Stamp}
      if Stamp == Prebound.valstamp_false then 'false'
      elseif Stamp == Prebound.valstamp_true then 'true'
      elseif Stamp == Prebound.valstamp_nil then 'nil'
      elseif Stamp == Prebound.valstamp_cons then 'cons'
      elseif Stamp == Prebound.valstamp_ref then 'ref'
      elseif Stamp == Prebound.valstamp_match then 'Match'
      elseif Stamp == Prebound.valstamp_bind then 'Bind'
      else Stamp
      end
   end

   fun {TrName Name}
      case Name of ExId(S) then exId({TrAtom S})
      [] !InId then inId
      end
   end

   fun {TrId Id(Info Stamp Name)}
      id({TrInfo Info} {TrStamp Stamp} {TrName Name})
   end

   fun {TrLab Lab}
      case Lab of NUM(I) then I
      [] ALPHA(S) then {String.toAtom {ByteString.toString S}}
      end
   end

   fun {TrConArity ConArity}
      case ConArity of !Nullary then nullary
      [] !Unary then unary
      [] Tuple(I) then tuple(I)
      [] Record(Labs) then record({Map Labs TrLab})
      end
   end

   fun {TrFunFlag FunFlag}
      case FunFlag of PrintName(String) then printName({TrAtom String})
      [] AuxiliaryOf(Stamp) then auxiliaryOf({TrStamp Stamp})
      end
   end

   fun {TrArgs Args}
      case Args of OneArg(Id) then oneArg({TrId Id})
      [] TupArgs(Ids) then tupArgs({Map Ids TrId})
      [] RecArgs(LabIdList) then
	 recArgs({Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      end
   end

   fun {TrTest Test}
      case Test of LitTest(Lit) then litTest({TrLit Lit})
      [] TagTest(Lab) then tagTest({TrLab Lab})
      [] TagAppTest(Lab Args ConArity) then
	 tagAppTest({TrLab Lab} {TrArgs Args} {TrConArity ConArity})
      [] ConTest(Id) then conTest({TrId Id})
      [] ConAppTest(Id Args ConArity) then
	 conAppTest({TrId Id} {TrArgs Args} {TrConArity ConArity})
      [] RefAppTest(Id) then refAppTest({TrId Id})
      [] TupTest(Ids) then tupTest({Map Ids TrId})
      [] RecTest(LabIdList) then
	 recTest({Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] LabTest(Lab Id) then labTest({TrLab Lab} {TrId Id})
      [] VecTest(Ids) then vecTest({Map Ids TrId})
      end
   end

   fun {TrStm Stm}
      case Stm of ValDec(Info Id Exp IsToplevel) then
	 valDec({TrInfo Info} {TrId Id} {TrExp Exp} IsToplevel)
      [] RecDec(Info IdExpList IsToplevel) then
	 recDec({TrInfo Info}
		{Map IdExpList fun {$ Id#Exp} {TrId Id}#{TrExp Exp} end}
		IsToplevel)
      [] EvalStm(Info Exp) then evalStm({TrInfo Info} {TrExp Exp})
      [] RaiseStm(Info Id) then raiseStm({TrInfo Info} {TrId Id})
      [] ReraiseStm(Info Id) then reraiseStm({TrInfo Info} {TrId Id})
      [] HandleStm(Info Body1 Id Body2 Body3 Shared) then
	 {Assign Shared true}
	 handleStm({TrInfo Info} {TrBody Body1} {TrId Id}
		   {TrBody Body2} {TrBody Body3} Shared)
      [] EndHandleStm(Info Shared) then endHandleStm({TrInfo Info} Shared)
      [] TestStm(Info Id Test Body1 Body2) then
	 testStm({TrInfo Info} {TrId Id} {TrTest Test}
		 {TrBody Body1} {TrBody Body2})
      [] SharedStm(Info Body Shared) then X in
	 X = {Access Shared}
	 if {IsInt X} then NewStm NewBody in
	    NewStm = sharedStm({TrInfo Info} NewBody {Gen})
	    {Assign Shared NewStm}
	    NewBody = {TrBody Body}
	    NewStm
	 else X
	 end
      [] ReturnStm(Info Exp) then returnStm({TrInfo Info} {TrExp Exp})
      [] IndirectStm(_ BodyOptRef) then
	 case {Access BodyOptRef} of SOME(Body) then {TrBody Body} end
      [] ExportStm(Info Exp) then exportStm({TrInfo Info} {TrExp Exp})
      end
   end

   fun {TrExp Exp}
      case Exp of LitExp(Info Lit) then litExp({TrInfo Info} {TrLit Lit})
      [] PrimExp(Info String) then primExp({TrInfo Info} {TrAtom String})
      [] NewExp(Info ConArity) then newExp({TrInfo Info} {TrConArity ConArity})
      [] VarExp(Info Id) then varExp({TrInfo Info} {TrId Id})
      [] TagExp(Info Lab ConArity) then
	 tagExp({TrInfo Info} {TrLab Lab} {TrConArity ConArity})
      [] ConExp(Info Id ConArity) then
	 conExp({TrInfo Info} {TrId Id} {TrConArity ConArity})
      [] RefExp(Info) then refExp({TrInfo Info})
      [] TupExp(Info Ids) then tupExp({TrInfo Info} {Map Ids TrId})
      [] RecExp(Info LabIdList) then
	 recExp({TrInfo Info}
		{Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] SelExp(Info Lab) then selExp({TrInfo Info} {TrLab Lab})
      [] VecExp(Info Ids) then vecExp({TrInfo Info} {Map Ids TrId})
      [] FunExp(Info Stamp Flags Args Body) then
	 funExp({TrInfo Info} {TrStamp Stamp} {Map Flags TrFunFlag}
		{TrArgs Args} {TrBody Body})
      [] AppExp(Info Id Args) then
	 appExp({TrInfo Info} {TrId Id} {TrArgs Args})
      [] SelAppExp(Info Lab Id) then
	 selAppExp({TrInfo Info} {TrLab Lab} {TrId Id})
      [] TagAppExp(Info Lab Args ConArity) then
	 tagAppExp({TrInfo Info} {TrLab Lab} {TrArgs Args}
		   {TrConArity ConArity})
      [] ConAppExp(Info Id Args ConArity) then
	 conAppExp({TrInfo Info} {TrId Id} {TrArgs Args} {TrConArity ConArity})
      [] RefAppExp(Info Id) then refAppExp({TrInfo Info} {TrId Id})
      [] PrimAppExp(Info String Ids) then
	 primAppExp({TrInfo Info} {TrAtom String} {Map Ids TrId})
      [] AdjExp(Info Id1 Id2) then
	 adjExp({TrInfo Info} {TrId Id1} {TrId Id2})
      end
   end

   fun {TrBody Stms}
      {Flatten {Map Stms TrStm}}
   end

   fun {TrComponent Import#(Body#_)}
      {Map Import fun {$ Id#_#U} {TrId Id}#{TrAtom {Url.toString U}} end}#
      {TrBody Body}
   end

   fun {Translate InFilename Component OutFilename} F in
      F = {CodeGen.translate InFilename {TrComponent Component}
	   InFilename#'.ozm'}
      {Pickle.save F OutFilename}
      unit
   end

   CodeGenPhase =
   'CodeGenPhase'(translate: Translate)
end
