%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   CompilerSupport(isBuiltin) at 'x-oz://boot/CompilerSupport'
   Word(make) at 'x-oz://boot/Word'
   System(printName)
   Narrator('class')
   ErrorListener('class')
   CodeStore('class')
   Prebound(builtinTable env)
   Assembler(assemble)
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

   fun {GetPrintName id((L#C)#_ _ Name) State}
      case Name of inId then
	 {VirtualString.toAtom
	  'File '#State.filename#', line '#L#', column '#C}
      [] exId(PrintName) then PrintName
      end
   end

   fun {TranslateRegion (I#J)#(_#_) State}
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

   fun {TranslateMatch Test Reg ThenVInstr State}
      case Test of litTest(Lit) then Constant in
	 Constant = {TranslateLit Lit}
	 if {IsNumber Constant} orelse {IsLiteral Constant} then
	    onScalar(Constant ThenVInstr)
	 else unit
	 end
      [] tagTest(Label _) then
	 onScalar(Label ThenVInstr)
      [] tagAppTest(Label _ oneArg(Id)) then ThenVInstr0 in
	 ThenVInstr0 = vGetVariable(_ {MakeReg Id State} ThenVInstr)
	 onRecord(Label 1 ThenVInstr0)
      [] tagAppTest(Label _ tupArgs(nil)) then
	 onScalar(Label ThenVInstr)
      [] tagAppTest(Label _ tupArgs(Ids=_|_)) then ThenVInstr0 in
	 {FoldL Ids
	  proc {$ VHd Id VTl}
	     VHd = vGetVariable(_ {MakeReg Id State} VTl)
	  end ThenVInstr0 ThenVInstr}
	 onRecord(Label {Length Ids} ThenVInstr0)
      [] tagAppTest(Label _ recArgs(LabelIdList)) then ThenVInstr0 in
	 {FoldL LabelIdList
	  proc {$ VHd _#Id VTl}
	     VHd = vGetVariable(_ {MakeReg Id State} VTl)
	  end ThenVInstr0 ThenVInstr}
	 onRecord(Label {Map LabelIdList fun {$ Label#_} Label end}
		  ThenVInstr0)
      [] tupTest(nil) then
	 onScalar(unit ThenVInstr)
      [] tupTest(Ids) then ThenVInstr0 in
	 {FoldL Ids
	  proc {$ VHd Id VTl}
	     VHd = vGetVariable(_ {MakeReg Id State} VTl)
	  end ThenVInstr0 ThenVInstr}
	 onRecord('#' {Length Ids} ThenVInstr0)
      [] recTest(LabelIdList) then Arity ThenVInstr0 in
	 Arity = {Map LabelIdList fun {$ Label#_} Label end}
	 {FoldL LabelIdList
	  proc {$ VHd _#Id VTl}
	     VHd = vGetVariable(_ {MakeReg Id State} VTl)
	  end ThenVInstr0 ThenVInstr}
	 onRecord('#' Arity ThenVInstr0)
      [] vecTest(Ids) then ThenVInstr0 in
	 {FoldL Ids
	  proc {$ VHd Id VTl}
	     VHd = vGetVariable(_ {MakeReg Id State} VTl)
	  end ThenVInstr0 ThenVInstr}
	 onRecord('#' {Length Ids} ThenVInstr0)
      else unit
      end
   end

   fun {GetTestPrefix TestBodyList Reg Matches State ReturnReg}
      case TestBodyList of Test#Body|Rest then ThenVInstr in
	 case {TranslateMatch Test Reg ThenVInstr State} of unit then
	    case Matches of nil then
	       test(Test Body Rest)
	    else
	       matches({Reverse Matches} TestBodyList)
	    end
	 elseof Match then
	    {TranslateBody Body ?ThenVInstr nil State ReturnReg}
	    {GetTestPrefix Rest Reg Match|Matches State ReturnReg}
	 end
      [] nil then
	 case Matches of nil then none
	 else matches({Reverse Matches} nil)
	 end
      end
   end

   proc {TranslateStm Stm VHd VTl State ReturnReg}
      case Stm of valDec(_ Id Exp) then
	 {TranslateExp Exp {CondMakeReg Id State} VHd VTl State}
      [] recDec(_ IdExpList) then
	 {ForAll IdExpList proc {$ Id#_} {MakeReg Id State _} end}
	 {FoldL IdExpList
	  proc {$ VHd Id#Exp VTl}
	     {TranslateExp Exp {GetReg Id State} VHd VTl State}
	  end VHd VTl}
      [] evalStm(_ Exp) then
	 {TranslateExp Exp {State.cs newReg($)} VHd VTl State}
      [] handleStm(Region Body1 Id Body2 Body3 Stamp) then
	 Reg1 Reg2 TryVInstr CatchVInstr CatchVInter VInter
      in
	 {State.cs newReg(?Reg1)}
	 Reg2 = {MakeReg Id State}
	 VHd = vExHandler(_ TryVInstr Reg1 CatchVInstr
			  {TranslateRegion Region State} VInter _)
	 {TranslateBody Body1 ?TryVInstr nil State ReturnReg}
	 {Dictionary.put State.shareDict Stamp unit}
	 CatchVInstr = vInlineDot(_ Reg1 1 Reg2 true unit CatchVInter)
	 {TranslateBody Body2 ?CatchVInter nil State ReturnReg}
	 {TranslateBody Body3 ?VInter nil State ReturnReg}
      [] endHandleStm(Region Stamp) then
	 if {Dictionary.member State.shareDict Stamp} then
	    %% This statement ends the `handle' part
	    VHd = VTl
	 else
	    %% This statement ends the `try' part
	    VHd = vPopEx(_ {TranslateRegion Region State} VTl)
	 end
	 VTl = nil
      [] testStm(Region Id TestBodyList ElseBody) then Reg in
	 Reg = {GetReg Id State}
	 case {GetTestPrefix TestBodyList Reg nil State ReturnReg}
	 of matches(Matches Rest) then ElseVInstr in
	    VHd = vMatch(_ Reg ElseVInstr Matches
			 {TranslateRegion Region State} VTl)
	    {TranslateStm testStm(Region Id Rest ElseBody)
	     ElseVInstr nil State ReturnReg}
	 [] test(Test Body Rest) then Coord ThenVInstr ElseVInstr in
	    Coord = {TranslateRegion Region State}
	    case Test of litTest(Lit) then Constant TmpReg VInter in
	       Constant = {TranslateLit Lit}
	       {State.cs newReg(?TmpReg)}
	       VHd = vEquateConstant(_ Constant TmpReg VInter)
	       VInter = vTestBuiltin(_ 'Value.\'==\''
				     [Reg TmpReg {State.cs newReg($)}]
				     ThenVInstr ElseVInstr VTl)
	    [] conTest(Id) then
	       VHd = vTestBuiltin(_ 'Value.\'==\''
				  [Reg {GetReg Id State} {State.cs newReg($)}]
				  ThenVInstr ElseVInstr VTl)
	    [] conAppTest(Id tupArgs(nil)) then
	       VHd = vTestBuiltin(_ 'Value.\'==\''
				  [Reg {GetReg Id State} {State.cs newReg($)}]
				  ThenVInstr ElseVInstr VTl)
	    [] conAppTest(Id Args) then ThenVInstr0 in
	       VHd = vTestBuiltin(_ 'Record.testLabel'
				  [Reg {GetReg Id State} {State.cs newReg($)}]
				  ThenVInstr0 ElseVInstr VTl)
	       case Args of oneArg(Id) then
		  ThenVInstr0 = vInlineDot(_ Reg 1 {MakeReg Id State} true
					   Coord ThenVInstr)
	       [] tupArgs(Ids=_|_) then
		  {List.foldLInd Ids
		   proc {$ I VHd Id VTl}
		      VHd = vInlineDot(_ Reg I {MakeReg Id State} true
				       Coord VTl)
		   end ThenVInstr0 ThenVInstr}
	       [] recArgs(LabelIdList) then
		  {FoldL LabelIdList
		   proc {$ VHd Label#Id VTl}
		      VHd = vInlineDot(_ Reg Label {MakeReg Id State} true
				       Coord VTl)
		   end ThenVInstr0 ThenVInstr}
	       end
	    [] refAppTest(Id) then ThenVInstr0 in
	       VHd = vTestBuiltin(_ 'Cell.is' [Reg {State.cs newReg($)}]
				  ThenVInstr0 ElseVInstr VTl)
	       ThenVInstr0 = vCallBuiltin(_ 'Cell.access'
					  [Reg {MakeReg Id State}]
					  Coord ThenVInstr)
	    [] labTest(Label Id) then
	       VHd = vInlineDot(_ Reg Label {MakeReg Id State} true
				Coord ThenVInstr)
	    end
	    {TranslateBody Body ?ThenVInstr nil State ReturnReg}
	    {TranslateStm testStm(Region Id Rest ElseBody)
	     ElseVInstr nil State ReturnReg}
	 [] none then
	    {TranslateBody ElseBody VHd VTl State ReturnReg}
	 end
      [] raiseStm(Region Id) then
	 VHd = vCallBuiltin(_ 'Exception.raiseError' [{GetReg Id State}]
			    {TranslateRegion Region State} VTl)
      [] reraiseStm(Region Id) then
	 VHd = vCallBuiltin(_ 'Exception.raiseError' [{GetReg Id State}]
			    {TranslateRegion Region State} VTl)
      [] sharedStm(_ Body Stamp) then
	 if {Dictionary.member State.shareDict Stamp} then
	    VHd = {Dictionary.get State.shareDict Stamp}
	 else
	    {Dictionary.put State.shareDict Stamp VHd}
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

   proc {TranslateExp Exp Reg VHd VTl State}
      case Exp of litExp(_ Lit) then
	 VHd = vEquateConstant(_ {TranslateLit Lit} Reg VTl)
      [] primExp(_ Builtinname) then
	 VHd = vEquateConstant(_ Prebound.builtinTable.Builtinname Reg VTl)
      [] newExp(Region _) then
	 VHd = vCallBuiltin(_ 'Name.new' [Reg]
			    {TranslateRegion Region State} VTl)
      [] varExp(_ Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] tagExp(_ Label _ nullary) then
	 VHd = vEquateConstant(_ Label Reg VTl)
      [] tagExp(Region Label _ unary) then
	 PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 PredId = pid(Label 2 {TranslateRegion Region State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vEquateRecord(_ Label 1 ResReg [value(ArgReg)] nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] tagExp(_ Label _ tuple(0)) then
	 VHd = vEquateConstant(_ fun {$ unit} Label end Reg VTl)
      [] tagExp(_ Label _ _) then
	 VHd = vEquateConstant(_ fun {$ X} {Adjoin X Label} end Reg VTl)
      [] conExp(_ Id nullary) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conExp(Region Id ConArity) then
	 Coord PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Coord = {TranslateRegion Region State}
	 PredId = pid({GetPrintName Id State} 2 Coord nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 case ConArity of unary then WidthReg VInter1 VInter2 in
	    {State.cs newReg(?WidthReg)}
	    VInstr = vEquateConstant(_ 1 WidthReg VInter1)
	    VInter1 = vCallBuiltin(_ 'Tuple.make'
				   [{GetReg Id State} WidthReg ResReg]
				   Coord VInter2)
	    VInter2 = vInlineDot(_ ResReg 1 ArgReg true Coord nil)
	 [] tuple(0) then VInter in
	    VInstr = vCallBuiltin(_ 'Value.wait' [ArgReg] Coord VInter)
	    VInter = vUnify(_ ResReg {GetReg Id State} nil)
	 else
	    VInstr = vCallBuiltin(_ 'Record.adjoin'
				  [ArgReg {GetReg Id State} ResReg] Coord nil)
	 end
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] refExp(Region) then
	 Coord PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Coord = {TranslateRegion Region State}
	 PredId = pid('ref' 2 Coord nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vCallBuiltin(_ 'Cell.new' [ArgReg ResReg] Coord nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] tupExp(_ nil) then
	 VHd = vEquateConstant(_ unit Reg VTl)
      [] tupExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] recExp(_ LabelIdList) then Rec in
	 %--** this is a workaround for duplicate features (due to structs)
	 Rec = {FoldL LabelIdList
		fun {$ Rec Label#Id}
		   {AdjoinAt Rec Label value({GetReg Id State})}
		end '#'}
	 VHd = vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] selExp(_ _ Label) then
	 VHd = vEquateConstant(_ fun {$ X} X.Label end Reg VTl)
      [] vecExp(_ nil) then
	 VHd = vEquateConstant(_ '#' Reg VTl)
      [] vecExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] funExp(Region _ _ tupArgs(Ids=_|_) Body) then
	 PredId NLiveRegs ResReg FormalRegs BodyVInstr GRegs Code
      in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Region.1.1#'.'#Region.1.2#'/'#
		       {Length Ids}#'-ary'}
		      {Length Ids} + 1 {TranslateRegion Region State}
		      nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = {FoldR Ids
		       fun {$ Id Rest} {MakeReg Id State}|Rest end [ResReg]}
	 {TranslateBody Body ?BodyVInstr nil State ResReg}
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] funExp(Region _ _ Args Body) then
	 PredId NLiveRegs ResReg FormalRegs ArgReg
	 BodyVInstr ThenVInstr ElseVInstr MatchReg ElseVInter GRegs Code
      in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Region.1.1#'.'#Region.1.2}
		      2 {TranslateRegion Region State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = [ArgReg ResReg]
	 case Args of oneArg(Id) then
	    ArgReg = {MakeReg Id State}
	    BodyVInstr = ThenVInstr
	 [] tupArgs(nil) then
	    {State.cs newReg(?ArgReg)}
	    BodyVInstr = vTestConstant(_ ArgReg unit
				       ThenVInstr ElseVInstr
				       {TranslateRegion Region State} nil)
	 [] recArgs(LabelIdList) then Arity ThenVInstr0 in
	    {State.cs newReg(?ArgReg)}
	    Arity = {Map LabelIdList fun {$ Label#_} Label end}
	    BodyVInstr = vMatch(_ ArgReg ElseVInstr
				[onRecord('#' Arity ThenVInstr0)]
				{TranslateRegion Region State} nil)
	    {FoldL LabelIdList
	     proc {$ VHd _#Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 end
	 {TranslateBody Body ?ThenVInstr nil State ResReg}
	 {State.cs newReg(?MatchReg)}
	 ElseVInstr = vEquateConstant(_ Prebound.env.'Match'
				      MatchReg ElseVInter)
	 ElseVInter = vCallBuiltin(_ 'Exception.raiseError' [MatchReg]
				   {TranslateRegion Region State} nil)
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] primAppExp(Region Builtinname Ids) then Value Regs in
	 Value = Prebound.builtinTable.Builtinname
	 Regs = {FoldR Ids fun {$ Id Regs} {GetReg Id State}|Regs end [Reg]}
	 if {CompilerSupport.isBuiltin Value} then
	    VHd = vCallBuiltin(_ {System.printName Value}
			       Regs {TranslateRegion Region State} VTl)
	 else
	    VHd = vCallConstant(_ Value
				Regs {TranslateRegion Region State} VTl)
	 end
      [] varAppExp(Region Id tupArgs(Ids=_|_)) then
	 VHd = vConsCall(_ {GetReg Id State}
			 {FoldR Ids
			  fun {$ Id Rest} {GetReg Id State}|Rest end [Reg]}
			 {TranslateRegion Region State} VTl)
      [] varAppExp(Region Id Args) then ArgReg VInter in
	 case Args of oneArg(Id) then
	    VHd = VInter
	    ArgReg = {GetReg Id State}
	 [] tupArgs(nil) then
	    {State.cs newReg(?ArgReg)}
	    VHd = vEquateConstant(_ unit ArgReg VInter)
	 [] tupArgs(Ids=_|_) then
	    {State.cs newReg(?ArgReg)}
	    VHd = vEquateRecord(_ '#' {Length Ids} ArgReg
				{Map Ids
				 fun {$ Id} value({GetReg Id State}) end}
				VInter)
	 [] recArgs(LabelIdList) then
	    {State.cs newReg(?ArgReg)}
	    VHd = vEquateRecord(_ '#'
				{Map LabelIdList fun {$ Label#_} Label end}
				ArgReg
				{Map LabelIdList
				 fun {$ _#Id} value({GetReg Id State}) end}
				VInter)
	 end
	 VInter = vDeconsCall(_ {GetReg Id State} ArgReg Reg
			      {TranslateRegion Region State} VTl)
      [] tagAppExp(_ Label _ oneArg(Id)) then
	 VHd = vEquateRecord(_ Label 1 Reg [value({GetReg Id State})] VTl)
      [] tagAppExp(_ Label _ tupArgs(nil)) then
	 VHd = vEquateConstant(_ Label Reg VTl)
      [] tagAppExp(_ Label _ tupArgs(Ids=_|_)) then
	 VHd = vEquateRecord(_ Label {Length Ids} Reg
			     {Map Ids fun {$ Id} value({GetReg Id State}) end}
			     VTl)
      [] tagAppExp(_ Label _ recArgs(LabelIdList)) then
	 VHd = vEquateRecord(_ Label
			     {Map LabelIdList fun {$ Label#_} Label end} Reg
			     {Map LabelIdList
			      fun {$ _#Id} value({GetReg Id State}) end}
			     VTl)
      [] conAppExp(Region Id1 oneArg(Id2)) then
	 Coord WidthReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 VHd = vEquateConstant(_ 1 WidthReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg Id1 State} WidthReg Reg]
				Coord VInter2)
	 VInter2 = vInlineDot(_ Reg 1 {GetReg Id2 State} true Coord VTl)
      [] conAppExp(_ Id tupArgs(nil)) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conAppExp(Region Id tupArgs(Ids=_|_)) then
	 Coord WidthReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 VHd = vEquateConstant(_ {Length Ids} WidthReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg Id State} WidthReg Reg] Coord VInter2)
	 {List.foldLInd Ids
	  proc {$ I VHd Id VTl}
	     VHd = vInlineDot(_ Reg I {GetReg Id State} true Coord VTl)
	  end VInter2 VTl}
      [] conAppExp(Region Id recArgs(LabelIdList)) then
	 Coord ArityReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?ArityReg)}
	 VHd = vEquateConstant(_ {Map LabelIdList fun {$ Label#_} Label end}
			       ArityReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Record.make'
				[{GetReg Id State} ArityReg Reg] Coord VInter2)
	 {List.foldL LabelIdList
	  proc {$ VHd Label#Id VTl}
	     VHd = vInlineDot(_ Reg Label {GetReg Id State} true Coord VTl)
	  end VInter2 VTl}
      [] refAppExp(Region Id) then
	 VHd = vCallBuiltin(_ 'Cell.new' [{GetReg Id State} Reg]
			    {TranslateRegion Region State} VTl)
      [] selAppExp(Region Label _ Id) then
	 VHd = vInlineDot(_ {GetReg Id State} Label Reg false
			  {TranslateRegion Region State} VTl)
      [] funAppExp(Region Id _ Args) then
	 {TranslateExp varAppExp(Region Id Args) Reg VHd VTl State}
      [] adjExp(Region Id1 Id2) then
	 VHd = vCallBuiltin(_ 'Record.adjoin' [{GetReg Id1 State}
					       {GetReg Id2 State} Reg]
			    {TranslateRegion Region State} VTl)
      end
   end

   proc {TranslateBody Stms VHd VTl State ReturnReg}
      {FoldL Stms
       proc {$ VHd Stm VTl}
	  {TranslateStm Stm VHd VTl State ReturnReg}
       end VHd VTl}
   end

   fun {Translate Filename Import#(Body#Sign)}
      NarratorObject Reporter CS RegDict Prebound ImportReg ExportReg
      State VInstr VInter GRegs Code NLiveRegs
   in
      NarratorObject = {New Narrator.'class' init(?Reporter)}
      _ = {New ErrorListener.'class' init(NarratorObject)}
      CS = {New CodeStore.'class'
	    init(proc {$ getSwitch(_ $)} false end Reporter)}
      {MakeRegDict CS ?RegDict ?Prebound}
      {CS startDefinition()}
      {CS newReg(?ImportReg)}
      {CS newReg(?ExportReg)}
      State = state(regDict: RegDict shareDict: {NewDictionary} cs: CS
		    filename: {VirtualString.toAtom Filename})
      {FoldL Import
       proc {$ VHd (Id=id(_ Stamp _))#_#_ VTl}
	  VHd = vInlineDot(_ ImportReg {VirtualString.toAtom Stamp}
			   {MakeReg Id State} false unit VTl)
       end VInstr VInter}
      {TranslateBody Body ?VInter nil State ExportReg}
      {CS endDefinition(VInstr [ImportReg ExportReg] nil
			?GRegs ?Code ?NLiveRegs)}
      case Code of Code1#Code2 then StartLabel EndLabel Res P VS in
	 StartLabel = {NewName}
	 EndLabel = {NewName}
	 {Assembler.assemble
	  (lbl(StartLabel)|
	   definition(x(0) EndLabel
		      pid({VirtualString.toAtom 'Component '#Filename} 2
			  pos(Filename 1 0) nil NLiveRegs)
		      unit {List.mapInd GRegs fun {$ I _} g(I) end}
		      Code1)|
	   endDefinition(StartLabel)|
	   {Append Code2 [lbl(EndLabel) unify(x(0) g(0)) return]})
	  Res|{Map GRegs fun {$ Reg} Prebound.Reg end}
	  switches ?P ?VS}
	 {P}
	 {Functor.new
	  {List.toRecord 'import'
	   {Map Import
	    fun {$ id(_ Stamp _)#Sign#URL}
	       {VirtualString.toAtom Stamp}#info('from': URL 'type': sig(Sign))
	    end}}
	  sig(Sign) Res}#VS#Sign
      end
   end
end
