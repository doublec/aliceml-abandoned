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
   CompilerSupport(isBuiltin) at 'x-oz://boot/CompilerSupport'
   System(printName)
   Narrator('class')
   ErrorListener('class')
   CodeStore('class')
   Prebound(builtinTable env)
   Assembler(assemble)
   Word at '../../vm-mozart/Word.so{native}'
export
   Translate
define
   fun {MakeRegDict CS ?RegDict}
      RegDict = {NewDictionary}
      {List.toRecord prebound
       {Map {Arity Prebound.env}
	fun {$ X} Reg in
	   {CS newReg(Reg)}
	   {Dictionary.put RegDict X Reg}
	   Reg#Prebound.env.X
	end}}
   end

   proc {MakeReg id(_ Stamp _) State ?Reg}
      {State.cs newReg(?Reg)}
      {Dictionary.put State.regDict Stamp Reg}
   end

   proc {CondMakeReg id(_ Stamp _) State ?Reg} RegDict in
      RegDict = State.regDict
      case {Dictionary.condGet RegDict Stamp unit} of unit then
	 {State.cs newReg(?Reg)}
	 {Dictionary.put RegDict Stamp Reg}
      elseof Reg0 then
	 %% This test is needed because of shared statements:
	 %% The same variable may be declared on two different paths.
	 Reg = Reg0
      end
   end

   fun {GetReg id(_ Stamp _) State}
      {Dictionary.get State.regDict Stamp}
   end

   fun {GetPrintName id(L#C#_#_ _ Name) State}
      case Name of inId then
	 {VirtualString.toAtom
	  'File '#State.filename#', line '#L#', column '#C}
      [] exId(PrintName) then PrintName
      end
   end

   fun {TranslateCoord I#J#_#_ State}
      pos(State.filename I J)
   end

   fun {TranslateLit Lit}
      case Lit of wordLit(W) then {Word.make 31 W}
      [] intLit(I) then I
      [] charLit(C) then C
      [] stringLit(S) then {ByteString.make S}
      [] realLit(F) then F
      end
   end

   proc {TranslateStm Stm VHd VTl State ReturnReg}
      case Stm of valDec(_ Id Exp _) then
	 {TranslateExp Exp {CondMakeReg Id State} VHd VTl State}
      [] recDec(_ IdExpList _) then
	 {ForAll IdExpList proc {$ Id#_} {MakeReg Id State _} end}
	 {FoldL IdExpList
	  proc {$ VHd Id#Exp VTl}
	     {TranslateExp Exp {GetReg Id State} VHd VTl State}
	  end VHd VTl}
      [] evalStm(_ Exp) then
	 {TranslateExp Exp {State.cs newReg($)} VHd VTl State}
      [] handleStm(Coord Body1 Id Body2 Body3 Shared) then
	 Reg1 Reg2 TryVInstr CatchVInstr CatchVInter VInter
      in
	 {State.cs newReg(?Reg1)}
	 Reg2 = {MakeReg Id State}
	 VHd = vExHandler(_ TryVInstr Reg1 CatchVInstr
			  {TranslateCoord Coord State} VInter _)
	 {TranslateBody Body1 ?TryVInstr nil State ReturnReg}
	 {Assign Shared false}
	 CatchVInstr = vInlineDot(_ Reg1 1 Reg2 false unit CatchVInter)
	 {TranslateBody Body2 ?CatchVInter nil State ReturnReg}
	 {TranslateBody Body3 ?VInter nil State ReturnReg}
      [] endHandleStm(Coord Shared) then
	 if {Access Shared} then
	    VHd = vPopEx(_ {TranslateCoord Coord State} VTl)
	 else
	    VHd = VTl
	 end
	 VTl = nil
      [] testStm(Coord Id Test Body1 Body2) then Reg0 ThenVInstr ElseVInstr in
	 %--** assemble several testStm into a single vMatch
	 Reg0 = {GetReg Id State}
	 case Test of litTest(Lit) then Constant in
	    Constant = {TranslateLit Lit}
	    if {IsNumber Constant} orelse {IsLiteral Constant} then
	       VHd = vTestConstant(_ Reg0 {TranslateLit Lit}
				   ThenVInstr ElseVInstr
				   {TranslateCoord Coord State} VTl)
	    else TmpReg VInter in
	       {State.cs newReg(?TmpReg)}
	       VHd = vEquateConstant(_ Constant TmpReg VInter)
	       VInter = vTestBuiltin(_ 'Value.\'==\''
				     [Reg0 TmpReg {State.cs newReg($)}]
				     ThenVInstr ElseVInstr VTl)
	    end
	 [] conTest(Id none) then
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg0 {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl)
	 [] conTest(Id1 some(Id2)) then NameReg ThenVInstr0 in
	    NameReg = {GetReg Id1 State}
	    VHd = vTestBuiltin(_ 'Record.testLabel'
			       [Reg0 NameReg {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl)
	    ThenVInstr0 = vInlineDot(_ Reg0 1 {MakeReg Id2 State} false
				     unit ThenVInstr)
	 [] refTest(Id) then ThenVInstr0 in
	    VHd = vTestBuiltin(_ 'Cell.is' [Reg0 {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl)
	    ThenVInstr0 = vCallBuiltin(_ 'Cell.access'
				       [Reg0 {MakeReg Id State}]
				       {TranslateCoord Coord State} ThenVInstr)
	 [] tupTest(nil) then
	    VHd = vTestConstant(_ Reg0 '#'
				ThenVInstr ElseVInstr
				{TranslateCoord Coord State} VTl)
	 [] tupTest(Ids) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord('#' {Length Ids} ThenVInstr0)]
			 {TranslateCoord Coord State} VTl)
	    {FoldL Ids
	     proc {$ VHd Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] recTest(FeatureIdList) then Arity ThenVInstr0 in
	    Arity = {Map FeatureIdList fun {$ F#_} F end}
	    VHd = vMatch(_ Reg0 ElseVInstr [onRecord('#' Arity ThenVInstr0)]
			 {TranslateCoord Coord State} VTl)
	    {FoldL FeatureIdList
	     proc {$ VHd _#Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] labTest(Feature Id) then FeatureReg VInter in
	    {State.cs newReg(?FeatureReg)}
	    VHd = vEquateConstant(_ Feature FeatureReg VInter)
	    VInter = vTestBuiltin(_ 'Record.testFeature'
				  [Reg0 FeatureReg {State.cs newReg($)}
				   {MakeReg Id State}]
				  ThenVInstr ElseVInstr VTl)
	 [] vecTest(Ids) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord('#' {Length Ids} ThenVInstr0)]
			 {TranslateCoord Coord State} VTl)
	    {FoldL Ids
	     proc {$ VHd Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 end
	 {TranslateBody Body1 ?ThenVInstr nil State ReturnReg}
	 {TranslateBody Body2 ?ElseVInstr nil State ReturnReg}
      [] raiseStm(Coord Id) then
	 VHd = vCallBuiltin(_ 'Exception.raiseError' [{GetReg Id State}]
			    {TranslateCoord Coord State} VTl)
      [] reraiseStm(Coord Id) then
	 VHd = vCallBuiltin(_ 'Exception.raiseError' [{GetReg Id State}]
			    {TranslateCoord Coord State} VTl)
      [] sharedStm(_ Body I) then
	 if {Dictionary.member State.shareDict I} then
	    VHd = {Dictionary.get State.shareDict I}
	 else
	    {Dictionary.put State.shareDict I VHd}
	    case {TranslateBody Body $ nil State ReturnReg} of nil then
	       VHd = nil
	    elseof VBody then
	       VHd = vShared(_ _ {State.cs newLabel($)} VBody)
	    end
	 end
	 VTl = nil
      [] returnStm(_ Exp) then {TranslateExp Exp ReturnReg VHd VTl State}
      [] exportStm(_ Exp) then {TranslateExp Exp ReturnReg VHd VTl State}
      end
   end

   proc {TranslateArgs Args Reg VHd VTl State}
      case Args of oneArg(Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] tupArgs(nil) then
	 VHd = vEquateConstant(_ '#' Reg VTl)
      [] tupArgs(Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] recArgs(LabIdList) then Arity in
	 Arity = {Map LabIdList fun {$ Lab#_} Lab end}
	 VHd = vEquateRecord(_ '#' Arity Reg
			     {Map LabIdList
			      fun {$ _#Id} value({GetReg Id State}) end} VTl)
      end
   end

   proc {TranslateExp Exp Reg VHd VTl State}
      case Exp of litExp(_ Lit) then
	 VHd = vEquateConstant(_ {TranslateLit Lit} Reg VTl)
      [] primExp(_ Builtinname) then
	 VHd = vEquateConstant(_ Prebound.builtinTable.Builtinname Reg VTl)
      [] newExp(Coord none _) then
	 VHd = vCallBuiltin(_ 'Name.new' [Reg]
			    {TranslateCoord Coord State} VTl)
      [] newExp(_ some(A) _) then
	 VHd = vEquateConstant(_ A Reg VTl)
      [] varExp(_ Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conExp(_ Id false) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conExp(Coord Id true) then
	 Pos PredId NLiveRegs ArgReg TmpReg ResReg
	 VInstr NameReg VInter1 VInter2 GRegs Code
      in
	 Pos = {TranslateCoord Coord State}
	 PredId = pid({GetPrintName Id State} 2 Pos nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?TmpReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vEquateConstant(_ 1 TmpReg VInter1)
	 NameReg = {GetReg Id State}
	 VInter1 = vCallBuiltin(_ 'Tuple.make' [NameReg TmpReg ResReg]
				Pos VInter2)
	 VInter2 = vInlineDot(_ ResReg 1 ArgReg false Pos nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] refExp(Coord) then
	 Pos PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Pos = {TranslateCoord Coord State}
	 PredId = pid('ref' 2 Pos nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vCallBuiltin(_ 'Cell.new' [ArgReg ResReg] Pos nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] tupExp(_ nil) then
	 VHd = vEquateConstant(_ '#' Reg VTl)
      [] tupExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] recExp(_ LabIdList) then Rec in
	 %--** workaround for duplicate features
	 Rec = {FoldL LabIdList
		fun {$ Rec Lab#Id}
		   {AdjoinAt Rec Lab value({GetReg Id State})}
		end '#'}
	 VHd = vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] selExp(_ Lab) then
	 VHd = vEquateConstant(_ fun {$ X} X.Lab end Reg VTl)
      [] vecExp(_ nil) then
	 VHd = vEquateConstant(_ '#' Reg VTl)
      [] vecExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] funExp(Coord _ _ oneArg(Id)#Body|ArgsBodyList) then
	 Body2 PredId NLiveRegs ResReg FormalRegs VInstr GRegs Code
      in
	 Body2 = {FoldL ArgsBodyList
		  fun {$ In Args#Body} Test in
		     Test = case Args of tupArgs(Ids) then tupTest(Ids)
			    [] recArgs(LabIdList) then recTest(LabIdList)
			    end
		     [testStm(Coord Id Test Body In)]
		  end Body}
	 PredId = pid('' 2 {TranslateCoord Coord State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = [{MakeReg Id State} ResReg]
	 {TranslateBody Body2 ?VInstr nil State ResReg}
	 {State.cs
	  endDefinition(VInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] appExp(Coord Id Args) then ArgReg VInter in
	 {State.cs newReg(?ArgReg)}
	 {TranslateArgs Args ArgReg VHd VInter State}
	 VInter = vCall(_ {GetReg Id State} [ArgReg Reg]
			{TranslateCoord Coord State} VTl)
      [] selAppExp(Coord Lab Id) then
	 VHd = vInlineDot(_ {GetReg Id State} Lab Reg false
			  {TranslateCoord Coord State} VTl)
      [] conAppExp(Coord Id Args) then
	 Pos TmpReg VInter1 NameReg ResReg VInter2 ArgReg VInter3 VInter4
      in
	 Pos = {TranslateCoord Coord State}
	 {State.cs newReg(?TmpReg)}
	 VHd = vEquateConstant(_ 1 TmpReg VInter1)
	 NameReg = {GetReg Id State}
	 {State.cs newReg(?ResReg)}
	 VInter1 = vCallBuiltin(_ 'Tuple.make' [NameReg TmpReg ResReg]
				Pos VInter2)
	 {State.cs newReg(?ArgReg)}
	 {TranslateArgs Args ArgReg VInter2 VInter3 State}
	 VInter3 = vInlineDot(_ ResReg 1 ArgReg false Pos VInter4)
	 VInter4 = vUnify(_ Reg ResReg VTl)
      [] refAppExp(Coord Args) then ArgReg VInter in
	 {State.cs newReg(?ArgReg)}
	 {TranslateArgs Args ArgReg VHd VInter State}
	 VInter = vCallBuiltin(_ 'Cell.new' [ArgReg Reg]
			       {TranslateCoord Coord State} VTl)
      [] primAppExp(Coord Builtinname Ids) then Value Regs in
	 Value = Prebound.builtinTable.Builtinname
	 Regs = {FoldR Ids fun {$ Id Regs} {GetReg Id State}|Regs end [Reg]}
	 if {CompilerSupport.isBuiltin Value} then
	    VHd = vCallBuiltin(_ {System.printName Value}
			       Regs {TranslateCoord Coord State} VTl)
	 else
	    VHd = vCallConstant(_ Value Regs {TranslateCoord Coord State} VTl)
	 end
      [] adjExp(Coord Id1 Id2) then
	 VHd = vCallBuiltin(_ 'Record.adjoin' [{GetReg Id1 State}
					       {GetReg Id2 State} Reg]
			    {TranslateCoord Coord State} VTl)
      end
   end

   proc {TranslateBody Stms VHd VTl State ReturnReg}
      {FoldL Stms
       proc {$ VHd Stm VTl}
	  {TranslateStm Stm VHd VTl State ReturnReg}
       end VHd VTl}
   end

/*
   F = {Functor.new
	'import'('A': info('from': 'A.ozf') 'B': info('from': 'B.ozf'))
	'export'(c: value d: value)
	fun {$ IMPORT} A B C D in
	   A = IMPORT.'A'
	   B = IMPORT.'B'
	   'export'(c: C d: D)
	end}
*/

   fun {Translate Filename Import#Body}
      NarratorObject Reporter CS RegDict Prebound ImportReg ExportReg
      State VInstr VInter GRegs Code NLiveRegs
   in
      NarratorObject = {New Narrator.'class' init(?Reporter)}
      _ = {New ErrorListener.'class' init(NarratorObject)}
      CS = {New CodeStore.'class'
	    init(proc {$ getSwitch(_ X)} X = false end Reporter)}
      {MakeRegDict CS ?RegDict ?Prebound}
      {CS startDefinition()}
      {CS newReg(?ImportReg)}
      {CS newReg(?ExportReg)}
      State = state(regDict: RegDict shareDict: {NewDictionary} cs: CS
		    filename: Filename)
      {FoldL Import
       proc {$ VHd (Id=id(_ Stamp _))#_ VTl}
	  VHd = vInlineDot(_ ImportReg {VirtualString.toAtom Stamp}
			   {MakeReg Id State} false unit VTl)
       end VInstr VInter}
      {TranslateBody Body ?VInter nil State ExportReg}
      {CS endDefinition(VInstr [ImportReg ExportReg] nil
			?GRegs ?Code ?NLiveRegs)}
      case Code of Code1#Code2 then StartLabel EndLabel Res in
	 StartLabel = {NewName}
	 EndLabel = {NewName}
	 {{Assembler.assemble
	   (lbl(StartLabel)|
	    definition(x(0) EndLabel
		       pid('Component' 2 pos(Filename 1 0) nil NLiveRegs)
		       unit {List.mapInd GRegs fun {$ I _} g(I) end}
		       Code1)|
	    endDefinition(StartLabel)|
	    {Append Code2 [lbl(EndLabel) unify(x(0) g(0)) return]})
	   Res|{Map GRegs fun {$ Reg} Prebound.Reg end}
	   switches}}
	 {Functor.new
	  {List.toRecord 'import' {Map Import
				   fun {$ id(_ Stamp _)#URL}
				      {VirtualString.toAtom Stamp}#
				      info('from': URL)
				   end}}
	  'export' Res}
      end
   end
end
