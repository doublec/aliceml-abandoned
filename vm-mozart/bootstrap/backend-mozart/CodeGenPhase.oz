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
   Word(toInt) at 'x-oz://boot/Word'
   Property(get)
   Pickle(saveWithCells)
   Open(file)
   PreboundComponent('$Prebound': Prebound) at 'x-alice:/common/Prebound.ozf'
   CodeGen(translate) at '../../../stoc/backend-mozart/CodeGen.ozf'
   UrlComponent('$Url': Url) at 'x-alice:/misc/Url.ozf'
export
   '$CodeGenPhase': CodeGenPhase
define
   fun {StringToAtom S}
      {String.toAtom {ByteString.toString S}}
   end

   fun {TrInfo '#'(region: Region ...)}
      Region
   end

   fun {TrLit Lit}
      case Lit of 'WordLit'(W) then wordLit({Word.toInt W})
      [] 'IntLit'(I) then intLit(I)
      [] 'CharLit'(C) then charLit(C)
      [] 'StringLit'(S) then stringLit({ByteString.toString S})
      [] 'RealLit'(S) then realLit({String.toFloat {VirtualString.toString S}})
      end
   end

   fun {TrName Name}
      case Name of 'ExId'(S) then exId({StringToAtom S})
      [] 'InId' then inId
      end
   end

   fun {TrId 'Id'(Info Stamp Name)}
      id({TrInfo Info}
	 if Stamp == Prebound.valstamp_false then 'false'
	 elseif Stamp == Prebound.valstamp_true then 'true'
	 elseif Stamp == Prebound.valstamp_nil then 'nil'
	 elseif Stamp == Prebound.valstamp_cons then 'cons'
	 elseif Stamp == Prebound.valstamp_ref then 'ref'
	 elseif Stamp == Prebound.valstamp_match then 'Match'
	 elseif Stamp == Prebound.valstamp_bind then 'Bind'
	 else Stamp
	 end
	 {TrName Name})
   end

   fun {TrLab Lab}
      case Lab of 'NUM'(I) then I
      [] 'ALPHA'(S) then
	 case {StringToAtom S} of 'true' then true
	 [] 'false' then false
	 [] '::' then '|'
	 elseof A then A
	 end
      end
   end

   fun {TrOption Option Tr}
      case Option of 'NONE' then non
      [] 'SOME'(X) then some({Tr X})
      end
   end

   fun {TrArity Arity}
      case Arity of 'Unary' then unary
      [] 'Tuple'(I) then tuple(I)
      [] 'Record'(Labs) then record({Map Labs TrLab})
      end
   end

   fun {TrConArity ConArity}
      {TrOption ConArity TrArity}
   end

   fun {TrFunFlag FunFlag}
      case FunFlag of 'PrintName'(String) then printName({StringToAtom String})
      [] 'AuxiliaryOf'(Stamp) then auxiliaryOf(Stamp)
      [] 'IsToplevel' then isToplevel
      end
   end

   fun {TrArgs Args}
      case Args of 'OneArg'(Id) then oneArg({TrId Id})
      [] 'TupArgs'(Ids) then tupArgs({Map Ids TrId})
      [] 'RecArgs'(LabIdList) then
	 recArgs({Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      end
   end

   fun {TrTest Test}
      case Test of 'LitTest'(Lit) then litTest({TrLit Lit})
      [] 'TagTest'(Lab N) then tagTest({TrLab Lab} N)
      [] 'TagAppTest'(Lab N Args) then
	 tagAppTest({TrLab Lab} N {TrArgs Args})
      [] 'ConTest'(Id) then conTest({TrId Id})
      [] 'ConAppTest'(Id Args) then
	 conAppTest({TrId Id} {TrArgs Args})
      [] 'RefAppTest'(Id) then refAppTest({TrId Id})
      [] 'TupTest'(Ids) then tupTest({Map Ids TrId})
      [] 'RecTest'(LabIdList) then
	 recTest({Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] 'LabTest'(Lab Id) then labTest({TrLab Lab} {TrId Id})
      [] 'VecTest'(Ids) then vecTest({Map Ids TrId})
      end
   end

   proc {TrStm Stm Hd Tl ShareDict}
      case Stm of 'ValDec'(Info Id Exp) then
	 Hd = valDec({TrInfo Info} {TrId Id} {TrExp Exp ShareDict})|Tl
      [] 'RecDec'(Info IdExpList) then
	 Hd = recDec({TrInfo Info}
		     {Map IdExpList
		      fun {$ Id#Exp} {TrId Id}#{TrExp Exp ShareDict} end})|Tl
      [] 'EvalStm'(Info Exp) then
	 Hd = evalStm({TrInfo Info} {TrExp Exp ShareDict})|Tl
      [] 'RaiseStm'(Info Id) then
	 Hd = raiseStm({TrInfo Info} {TrId Id})|Tl
      [] 'ReraiseStm'(Info Id) then
	 Hd = reraiseStm({TrInfo Info} {TrId Id})|Tl
      [] 'HandleStm'(Info Body1 Id Body2 Body3 Stamp) then
	 Hd = handleStm({TrInfo Info} {TrBody Body1 $ nil ShareDict} {TrId Id}
			{TrBody Body2 $ nil ShareDict}
			{TrBody Body3 $ nil ShareDict} Stamp)|Tl
      [] 'EndHandleStm'(Info Stamp) then
	 Hd = endHandleStm({TrInfo Info} Stamp)|Tl
      [] 'TestStm'(Info Id TestBodyList Body) then
	 Hd = testStm({TrInfo Info} {TrId Id}
		      {Map TestBodyList
		       fun {$ Test#Body}
			  {TrTest Test}#{TrBody Body $ nil ShareDict}
		       end}
		      {TrBody Body $ nil ShareDict})|Tl
      [] 'SharedStm'(Info Body Stamp) then
	 case {Dictionary.condGet ShareDict Stamp unit} of unit then NewStm in
	    {Dictionary.put ShareDict Stamp NewStm}
	    NewStm = sharedStm({TrInfo Info} {TrBody Body $ nil ShareDict}
			       Stamp)
	    Hd = NewStm|Tl
	 elseof Stm then
	    Hd = Stm|Tl
	 end
      [] 'ReturnStm'(Info Exp) then
	 Hd = returnStm({TrInfo Info} {TrExp Exp ShareDict})|Tl
      [] 'IndirectStm'(_ BodyOptRef) then
	 case {Access BodyOptRef} of 'SOME'(Body) then
	    {TrBody Body Hd Tl ShareDict}
	 end
      [] 'ExportStm'(Info Exp) then
	 Hd = exportStm({TrInfo Info} {TrExp Exp ShareDict})|Tl
      end
   end

   fun {TrExp Exp ShareDict}
      case Exp of 'LitExp'(Info Lit) then litExp({TrInfo Info} {TrLit Lit})
      [] 'PrimExp'(Info String) then
	 primExp({TrInfo Info} {StringToAtom String})
      [] 'NewExp'(Info ConArity) then
	 newExp({TrInfo Info} {TrConArity ConArity})
      [] 'VarExp'(Info Id) then varExp({TrInfo Info} {TrId Id})
      [] 'TagExp'(Info Lab N ConArity) then
	 tagExp({TrInfo Info} {TrLab Lab} N {TrConArity ConArity})
      [] 'ConExp'(Info Id ConArity) then
	 conExp({TrInfo Info} {TrId Id} {TrConArity ConArity})
      [] 'RefExp'(Info) then refExp({TrInfo Info})
      [] 'TupExp'(Info Ids) then tupExp({TrInfo Info} {Map Ids TrId})
      [] 'RecExp'(Info LabIdList) then
	 recExp({TrInfo Info}
		{Map LabIdList fun {$ Lab#Id} {TrLab Lab}#{TrId Id} end})
      [] 'SelExp'(Info Lab N) then selExp({TrInfo Info} {TrLab Lab} N)
      [] 'VecExp'(Info Ids) then vecExp({TrInfo Info} {Map Ids TrId})
      [] 'FunExp'(Info Stamp Flags Args Body) then
	 funExp({TrInfo Info} Stamp {Map Flags TrFunFlag}
		{TrArgs Args} {TrBody Body $ nil ShareDict})
      [] 'PrimAppExp'(Info String Ids) then
	 primAppExp({TrInfo Info} {StringToAtom String} {Map Ids TrId})
      [] 'VarAppExp'(Info Id Args) then
	 varAppExp({TrInfo Info} {TrId Id} {TrArgs Args})
      [] 'TagAppExp'(Info Lab N Args) then
	 tagAppExp({TrInfo Info} {TrLab Lab} N {TrArgs Args})
      [] 'ConAppExp'(Info Id Args) then
	 conAppExp({TrInfo Info} {TrId Id} {TrArgs Args})
      [] 'RefAppExp'(Info Id) then refAppExp({TrInfo Info} {TrId Id})
      [] 'SelAppExp'(Info Lab N Id) then
	 selAppExp({TrInfo Info} {TrLab Lab} N {TrId Id})
      [] 'FunAppExp'(Info Id Stamp Args) then
	 funAppExp({TrInfo Info} {TrId Id} Stamp {TrArgs Args})
      end
   end

   proc {TrBody Stms Hd Tl ShareDict}
      {FoldL Stms
       proc {$ Hd Stm Tl}
	  {TrStm Stm Hd Tl ShareDict}
       end Hd Tl}
   end

   fun {TrComponent Import#(Body#Sign)}
      {Map Import
       fun {$ Id#Sign#U}
	  {TrId Id}#Sign#{StringToAtom {Url.toString U}}
       end}#
      ({TrBody Body $ nil {NewDictionary}}#Sign)
   end

   fun {Translate Desc Component} InFilename in
      InFilename = case Desc of 'SOME'(U) then {Url.toString U}
		   [] 'NONE' then ''
		   end
      {CodeGen.translate InFilename {TrComponent Component}}
   end

   fun {Sign _#_#Sign}
      Sign
   end

   fun {Apply F#_#_}
      {{Property.get 'alice.modulemanager'} link(F)}
      unit
   end

   proc {WriteFile VS File} F in
      F = {New Open.file init(name: File flags: [write create truncate])}
      {F write(vs: VS)}
      {F close()}
   end

   fun {Save OutFilename F#VS#_}
      {Pickle.saveWithCells F OutFilename '' 0}
%      if {Access Switches.outputAssembly} then
%	 {WriteFile VS OutFilename#'.ozm'}
%      end
      unit
   end

   CodeGenPhase =
   'CodeGenPhase'(translate: Translate
		  sign: Sign
		  apply: Apply
		  save: Save)
end
