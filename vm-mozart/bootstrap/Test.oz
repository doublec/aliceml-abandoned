functor
import
   Application(getCmdArgs exit)
   System(printError show)
   Property(get)
   Pickle(save)
   NativeWord(toInt) at '../Word.so{native}'
   Base('$Option') at 'Base.ozf'
   BackendCommon('$ImperativeGrammar') at 'BackendCommon.ozf'
   Common('$Prebound': Prebound) at 'Common.ozf'
   Main(imperatifyFile) at 'Main.ozf'
   CodeGen(translate) at '../../stoc/backend-mozart/CodeGen.ozf'
prepare
   Spec = record('in'(single char: &i type: atom optional: false)
		 'out'(single char: &o type: string optional: false))
define
   Args

   try
      Args = {Application.getCmdArgs Spec}
   catch error(ap(usage M) ...) then
      {System.printError
       'Command line option error: '#M#'\n'#
       'Usage: '#{Property.get 'application.url'}#' [options]\n'#
       '--in=<File>         File containing component to translate.\n'#
       '--out=<File>        Name of pickle to write.\n'}
      {Application.exit 2}
   end

   NONE = 'NONE'
   SOME = 'SOME'
   WordLit = 'WordLit'
   IntLit = 'IntLit'
   CharLit = 'CharLit'
   StringLit = 'StringLit'
   RealLit = 'RealLit'
   ExId = 'ExId'
   InId = 'InId'
   Id = 'Id'
   LitTest = 'LitTest'
   ConTest = 'ConTest'
   RefTest = 'RefTest'
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
   ConExp = 'ConExp'
   RefExp = 'RefExp'
   TupExp = 'TupExp'
   RecExp = 'RecExp'
   SelExp = 'SelExp'
   VecExp = 'VecExp'
   FunExp = 'FunExp'
   AppExp = 'AppExp'
   SelAppExp = 'SelAppExp'
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

   fun {TrOption Opt Tr}
      case Opt of !NONE then none
      [] SOME(X) then some({Tr X})
      end
   end

   fun {TrList Xs Tr}
      case Xs of '::'(X#Xr) then {Tr X}|{TrList Xr Tr}
      [] nil then nil
      end
   end

   fun {TrAtom S}
      {String.toAtom {ByteString.toString S}}
   end

   fun {TrCoord (LL#LC)#(RL#RC)}
      LL#LC#RL#RC
   end

   fun {TrLit Lit}
      case Lit of WordLit(W) then wordLit({NativeWord.toInt W})
      [] IntLit(I) then intLit(I)
      [] CharLit(C) then charLit(C)
      [] StringLit(S) then stringLit({ByteString.toString S})
      [] RealLit(S) then realLit({String.toFloat {ByteString.toString S}})
      end
   end

   fun {TrStamp Stamp}
      if Stamp == Prebound.stamp_false then 'false'
      elseif Stamp == Prebound.stamp_true then 'true'
      elseif Stamp == Prebound.stamp_nil then 'nil'
      elseif Stamp == Prebound.stamp_cons then 'cons'
      elseif Stamp == Prebound.stamp_ref then 'ref'
      elseif Stamp == Prebound.stamp_Match then 'Match'
      elseif Stamp == Prebound.stamp_Bind then 'Bind'
      else Stamp
      end
   end

   fun {TrName Name}
      case Name of ExId(S) then exId({TrAtom S})
      [] !InId then inId
      end
   end

   fun {TrId Id(Coord#Stamp#Name)}
      id({TrCoord Coord} {TrStamp Stamp} {TrName Name})
   end

   fun {TrLab S} S2 in
      S2 = {ByteString.toString S}
      if {String.isInt S2} then {String.toInt S2}
      else {String.toAtom S2}
      end
   end

   fun {TrTest Test}
      case Test of LitTest(Lit) then litTest({TrLit Lit})
      [] ConTest(Id#IdOpt) then conTest({TrId Id} {TrOption IdOpt TrId})
      [] RefTest(Id) then refTest({TrId Id})
      [] TupTest(Ids) then tupTest({TrList Ids TrId})
      [] RecTest(LabIdList) then
	 recTest({TrList LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] LabTest(Lab#Id) then labTest({TrLab Lab} {TrId Id})
      [] VecTest(Ids) then vecTest({TrList Ids TrId})
      end
   end

   fun {TrFunFlag FunFlag}
      case FunFlag of PrintName(String) then printName({TrAtom String})
      [] AuxiliaryOf(Stamp) then auxiliaryOf({TrStamp Stamp})
      end
   end

   fun {TrArgs Args}
      case Args of OneArg(Id) then oneArg({TrId Id})
      [] TupArgs(Ids) then tupArgs({TrList Ids TrId})
      [] RecArgs(LabIdList) then
	 recArgs({TrList LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      end
   end

   fun {TrInfo Coord#_}
      {TrCoord Coord}
   end

   fun {TrStm Stm}
      case Stm of ValDec(Info#Id#Exp#IsToplevel) then
	 valDec({TrInfo Info} {TrId Id} {TrExp Exp} IsToplevel)
      [] RecDec(Info#IdExpList#IsToplevel) then
	 recDec({TrInfo Info}
		{TrList IdExpList fun {$ Id#Exp} {TrId Id}#{TrExp Exp} end}
		IsToplevel)
      [] EvalStm(Info#Exp) then evalStm({TrInfo Info} {TrExp Exp})
      [] RaiseStm(Info#Id) then raiseStm({TrInfo Info} {TrId Id})
      [] HandleStm(Info#Body1#Id#Body2#Body3#Shared) then
	 {Assign Shared true}
	 handleStm({TrInfo Info} {TrBody Body1} {TrId Id}
		   {TrBody Body2} {TrBody Body3} Shared)
      [] EndHandleStm(Info#Shared) then endHandleStm({TrInfo Info} Shared)
      [] TestStm(Info#Id#Test#Body1#Body2) then
	 testStm({TrInfo Info} {TrId Id} {TrTest Test}
		 {TrBody Body1} {TrBody Body2})
      [] SharedStm(Info#Body#Shared) then X in
	 X = {Access Shared}
	 if {IsInt X} then NewStm NewBody in
	    NewStm = sharedStm({TrInfo Info} NewBody {Gen})
	    {Assign Shared NewStm}
	    NewBody = {TrBody Body}
	    NewStm
	 else X
	 end
      [] ReturnStm(Info#Exp) then returnStm({TrInfo Info} {TrExp Exp})
      [] IndirectStm(_#BodyOptRef) then
	 case {Access BodyOptRef} of SOME(Body) then {TrBody Body} end
      [] ExportStm(Info#Exp) then exportStm({TrInfo Info} {TrExp Exp})
      end
   end

   fun {TrExp Exp}
      case Exp of LitExp(Coord#Lit) then litExp({TrCoord Coord} {TrLit Lit})
      [] PrimExp(Coord#String) then primExp({TrCoord Coord} {TrAtom String})
      [] NewExp(Coord#StringOpt#HasArgs) then
	 newExp({TrCoord Coord} {TrOption StringOpt TrAtom} HasArgs)
      [] VarExp(Coord#Id) then varExp({TrCoord Coord} {TrId Id})
      [] ConExp(Coord#Id#HasArgs) then
	 conExp({TrCoord Coord} {TrId Id} HasArgs)
      [] RefExp(Coord) then refExp({TrCoord Coord})
      [] TupExp(Coord#Ids) then tupExp({TrCoord Coord} {TrList Ids TrId})
      [] RecExp(Coord#LabIdList) then
	 recExp({TrCoord Coord}
		{TrList LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] SelExp(Coord#Lab) then selExp({TrCoord Coord} {TrLab Lab})
      [] VecExp(Coord#Ids) then vecExp({TrCoord Coord} {TrList Ids TrId})
      [] FunExp(Coord#Stamp#Flags#ArgsBodyList) then
	 funExp({TrCoord Coord} {TrStamp Stamp} {TrList Flags TrFunFlag}
		{TrList ArgsBodyList
		 fun {$ Args#Body} {TrArgs Args}#{TrBody Body} end})
      [] AppExp(Coord#Id#Args) then
	 appExp({TrCoord Coord} {TrId Id} {TrArgs Args})
      [] SelAppExp(Coord#Lab#Id) then
	 selAppExp({TrCoord Coord} {TrLab Lab} {TrId Id})
      [] ConAppExp(Coord#Id#Args) then
	 conAppExp({TrCoord Coord} {TrId Id} {TrArgs Args})
      [] RefAppExp(Coord#Args) then refAppExp({TrCoord Coord} {TrArgs Args})
      [] PrimAppExp(Coord#String#Ids) then
	 primAppExp({TrCoord Coord} {TrAtom String} {TrList Ids TrId})
      [] AdjExp(Coord#Id1#Id2) then
	 adjExp({TrCoord Coord} {TrId Id1} {TrId Id2})
      end
   end

   fun {TrBody Stms}
      {Flatten {TrList Stms TrStm}}
   end

   fun {TrComponent IdStringList#Ids#Body}
      {TrList IdStringList fun {$ Id#String} {TrId Id}#{TrAtom String} end}#
      {TrList Ids TrId}#{TrBody Body}
   end

   Component = {TrComponent {Main.imperatifyFile Args.'in'}}
   {Pickle.save {CodeGen.translate Args.'in' Component} Args.'out'}

   {Application.exit 0}
end
