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
   System(printName printError)
   Narrator('class')
   ErrorListener('class')
   Open(file)
   CodeStore('class')
   Prebound(builtinTable env)
   Assembler(assemble)
   Word at '/home/kornstae/stockhausen/vm-mozart/Word.so{native}'
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

   fun {TranslateLabel Label}
      case Label of 'true' then true
      [] 'false' then false
      [] '::' then '|'
      else Label
      end
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
	 CatchVInstr = vInlineDot(_ Reg1 1 Reg2 true unit CatchVInter)
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
	 [] tagTest(Label) then
	    VHd = vTestConstant(_ Reg0 {TranslateLabel Label}
				ThenVInstr ElseVInstr unit VTl)
	 [] tagAppTest(Label oneArg(Id) unary) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord({TranslateLabel Label} 1 ThenVInstr0)]
			 unit VTl)
	    ThenVInstr0 = vGetVariable(_ {MakeReg Id State} ThenVInstr)
	 [] tagAppTest(Label oneArg(Id) tuple(0)) then ThenVInstr0 in
	    VHd = vTestConstant(_ Reg0 {TranslateLabel Label}
				ThenVInstr0 ElseVInstr unit VTl)
	    ThenVInstr0 = vEquateConstant(_ unit {MakeReg Id State}
					  ThenVInstr)
	 [] tagAppTest(Label oneArg(Id) ConArity) then
	    ThenVInstr0 LabelReg ThenVInstr1
	 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord({TranslateLabel Label} ConArity.1
				   ThenVInstr0)]
			 unit VTl)
	    {State.cs newReg(?LabelReg)}
	    ThenVInstr0 = vEquateConstant(_ '#' LabelReg ThenVInstr1)
	    ThenVInstr1 = vCallBuiltin(_ 'Record.adjoin'
				       [Reg0 LabelReg {MakeReg Id State}]
				       unit ThenVInstr)
	 [] tagAppTest(Label tupArgs(nil) _) then
	    VHd = vTestConstant(_ Reg0 {TranslateLabel Label}
				ThenVInstr ElseVInstr unit VTl)
	 [] tagAppTest(Label tupArgs(Ids=_|_) _) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord({TranslateLabel Label} {Length Ids}
				   ThenVInstr0)]
			 unit VTl)
	    {FoldL Ids
	     proc {$ VHd Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] tagAppTest(Label recArgs(LabelIdList) _) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord({TranslateLabel Label}
				   {Map LabelIdList fun {$ Label#_} Label end}
				   ThenVInstr0)]
			 unit VTl)
	    {FoldL LabelIdList
	     proc {$ VHd _#Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] conTest(Id) then
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg0 {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl)
	 [] conAppTest(Id1 oneArg(Id2) tuple(0)) then ThenVInstr0 in
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg0 {GetReg Id1 State} {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl)
	    ThenVInstr0 = vEquateConstant(_ unit {MakeReg Id2 State}
					  ThenVInstr)
	 [] conAppTest(Id tupArgs(nil) _) then
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg0 {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl)
	 [] conAppTest(Id Args ConArity) then ThenVInstr0 in
	    VHd = vTestBuiltin(_ 'Record.testLabel'
			       [Reg0 {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl)
	    case Args#ConArity of oneArg(Id)#unary then
	       ThenVInstr0 = vInlineDot(_ Reg0 1 {MakeReg Id State} true
					unit ThenVInstr)
	    [] oneArg(Id)#_ then LabelReg ThenVInstr1 in
	       {State.cs newReg(?LabelReg)}
	       ThenVInstr0 = vEquateConstant(_ '#' LabelReg ThenVInstr1)
	       ThenVInstr1 = vCallBuiltin(_ 'Record.adjoin'
					  [Reg0 LabelReg {MakeReg Id State}]
					  unit ThenVInstr)
	    [] tupArgs(Ids=_|_)#_ then
	       {List.foldLInd Ids
		proc {$ I VHd Id VTl}
		   VHd = vInlineDot(_ Reg0 I {MakeReg Id State} true unit VTl)
		end ThenVInstr0 ThenVInstr}
	    [] recArgs(LabelIdList)#_ then
	       {FoldL LabelIdList
		proc {$ VHd Label#Id VTl}
		   VHd = vInlineDot(_ Reg0 Label {MakeReg Id State} true
				    unit VTl)
		end ThenVInstr0 ThenVInstr}
	    end
	 [] refAppTest(Id) then ThenVInstr0 in
	    VHd = vTestBuiltin(_ 'Cell.is' [Reg0 {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl)
	    ThenVInstr0 = vCallBuiltin(_ 'Cell.access'
				       [Reg0 {MakeReg Id State}]
				       {TranslateCoord Coord State} ThenVInstr)
	 [] tupTest(nil) then
	    VHd = vTestConstant(_ Reg0 unit
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
	 [] recTest(LabelIdList) then Arity ThenVInstr0 in
	    Arity = {Map LabelIdList
		     fun {$ Label#_} {TranslateLabel Label} end}
	    VHd = vMatch(_ Reg0 ElseVInstr [onRecord('#' Arity ThenVInstr0)]
			 {TranslateCoord Coord State} VTl)
	    {FoldL LabelIdList
	     proc {$ VHd _#Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] labTest(Label Id) then
	    VHd = vInlineDot(_ Reg0 {TranslateLabel Label}
			     {MakeReg Id State} true unit ThenVInstr)
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

   proc {TranslateExp Exp Reg VHd VTl State}
      case Exp of litExp(_ Lit) then
	 VHd = vEquateConstant(_ {TranslateLit Lit} Reg VTl)
      [] primExp(_ Builtinname) then
	 VHd = vEquateConstant(_ Prebound.builtinTable.Builtinname Reg VTl)
      [] newExp(Coord _) then
	 VHd = vCallBuiltin(_ 'Name.new' [Reg]
			    {TranslateCoord Coord State} VTl)
      [] varExp(_ Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] tagExp(_ Label nullary) then
	 VHd = vEquateConstant(_ {TranslateLabel Label} Reg VTl)
      [] tagExp(Coord Label unary) then
	 Label2 PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Label2 = {TranslateLabel Label}
	 PredId = pid(Label2 2 {TranslateCoord Coord State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vEquateRecord(_ Label2 1 ResReg [value(ArgReg)] nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] tagExp(_ Label tuple(0)) then Label2 in
	 Label2 = {TranslateLabel Label}
	 VHd = vEquateConstant(_ fun {$ unit} Label2 end Reg VTl)
      [] tagExp(_ Label _) then Label2 in
	 Label2 = {TranslateLabel Label}
	 VHd = vEquateConstant(_ fun {$ X} {Adjoin X Label2} end Reg VTl)
      [] conExp(_ Id nullary) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conExp(Coord Id ConArity) then
	 Pos PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Pos = {TranslateCoord Coord State}
	 PredId = pid({GetPrintName Id State} 2 Pos nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 case ConArity of unary then WidthReg VInter1 VInter2 in
	    {State.cs newReg(?WidthReg)}
	    VInstr = vEquateConstant(_ 1 WidthReg VInter1)
	    VInter1 = vCallBuiltin(_ 'Tuple.make'
				   [{GetReg Id State} WidthReg ResReg]
				   Pos VInter2)
	    VInter2 = vInlineDot(_ ResReg 1 ArgReg true Pos nil)
	 [] tuple(0) then VInter in
	    VInstr = vCallBuiltin(_ 'Value.wait' [ArgReg] Pos VInter)
	    VInter = vUnify(_ ResReg {GetReg Id State} nil)
	 else
	    VInstr = vCallBuiltin(_ 'Record.adjoin'
				  [ArgReg {GetReg Id State} ResReg] Pos nil)
	 end
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
	 VHd = vEquateConstant(_ unit Reg VTl)
      [] tupExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] recExp(_ LabelIdList) then Rec in
	 %--** this is a workaround for duplicate features (due to structs)
	 Rec = {FoldL LabelIdList
		fun {$ Rec Label#Id}
		   {AdjoinAt Rec {TranslateLabel Label}
		    value({GetReg Id State})}
		end '#'}
	 VHd = vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] selExp(_ Label) then Label2 in
	 Label2 = {TranslateLabel Label}
	 VHd = vEquateConstant(_ fun {$ X} X.Label2 end Reg VTl)
      [] vecExp(_ nil) then
	 VHd = vEquateConstant(_ '#' Reg VTl)
      [] vecExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] funExp(Coord _ _ tupArgs(Ids=_|_) Body) then
	 PredId NLiveRegs ResReg FormalRegs BodyVInstr GRegs Code
      in
	 PredId = pid({VirtualString.toAtom {Length Ids}#'-ary line '#Coord.1}
		      {Length Ids} + 1 {TranslateCoord Coord State}
		      nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = {FoldR Ids
		       fun {$ Id Rest} {MakeReg Id State}|Rest end [ResReg]}
	 {TranslateBody Body ?BodyVInstr nil State ResReg}
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] funExp(Coord _ _ Args Body) then
	 PredId NLiveRegs ResReg FormalRegs ArgReg
	 BodyVInstr ThenVInstr ElseVInstr MatchReg ElseVInter GRegs Code
      in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Coord.1#'.'#Coord.2}
		      2 {TranslateCoord Coord State} nil NLiveRegs)
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
				       {TranslateCoord Coord State} nil)
	 [] recArgs(LabelIdList) then Arity ThenVInstr0 in
	    {State.cs newReg(?ArgReg)}
	    Arity = {Map LabelIdList
		     fun {$ Label#_} {TranslateLabel Label} end}
	    BodyVInstr = vMatch(_ ArgReg ElseVInstr
				[onRecord('#' Arity ThenVInstr0)]
				{TranslateCoord Coord State} nil)
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
				   {TranslateCoord Coord State} nil)
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] appExp(Coord Id tupArgs(Ids=_|_)) then
	 VHd = vConsCall(_ {GetReg Id State}
			 {FoldR Ids
			  fun {$ Id Rest} {GetReg Id State}|Rest end [Reg]}
			 {TranslateCoord Coord State} VTl)
      [] appExp(Coord Id Args) then ArgReg VInter in
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
				{Map LabelIdList
				 fun {$ Label#_} {TranslateLabel Label} end}
				ArgReg
				{Map LabelIdList
				 fun {$ _#Id} value({GetReg Id State}) end}
				VInter)
	 end
	 VInter = vDeconsCall(_ {GetReg Id State} ArgReg Reg
			      {TranslateCoord Coord State} VTl)
      [] selAppExp(Coord Label Id) then
	 VHd = vInlineDot(_ {GetReg Id State} {TranslateLabel Label} Reg false
			  {TranslateCoord Coord State} VTl)
      [] tagAppExp(_ Label oneArg(Id) unary) then
	 VHd = vEquateRecord(_ {TranslateLabel Label} 1 Reg
			     [value({GetReg Id State})] VTl)
      [] tagAppExp(Coord Label oneArg(Id) tuple(0)) then VInter in
	 VHd = vCallBuiltin(_ 'Value.wait' [{GetReg Id State}]
			    {TranslateCoord Coord State} VInter)
	 VInter = vEquateConstant(_ {TranslateLabel Label} Reg VTl)
      [] tagAppExp(Coord Label oneArg(Id) _) then LabelReg VInter in
	 {State.cs newReg(?LabelReg)}
	 VHd = vEquateConstant(_ {TranslateLabel Label} LabelReg VInter)
	 VInter = vCallBuiltin(_ 'Record.adjoin'
			       [{GetReg Id State} LabelReg Reg]
			       {TranslateCoord Coord State} VTl)
      [] tagAppExp(_ Label tupArgs(nil) _) then
	 VHd = vEquateConstant(_ {TranslateLabel Label} Reg VTl)
      [] tagAppExp(_ Label tupArgs(Ids=_|_) _) then
	 VHd = vEquateRecord(_ {TranslateLabel Label} {Length Ids} Reg
			     {Map Ids fun {$ Id} value({GetReg Id State}) end}
			     VTl)
      [] tagAppExp(_ Label recArgs(LabelIdList) _) then
	 VHd = vEquateRecord(_ {TranslateLabel Label}
			     {Map LabelIdList
			      fun {$ Label#_} {TranslateLabel Label} end} Reg
			     {Map LabelIdList
			      fun {$ _#Id} value({GetReg Id State}) end}
			     VTl)
      [] conAppExp(Coord Id1 oneArg(Id2) unary) then
	 Pos WidthReg VInter1 VInter2
      in
	 Pos = {TranslateCoord Coord State}
	 {State.cs newReg(?WidthReg)}
	 VHd = vEquateConstant(_ 1 WidthReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg Id1 State} WidthReg Reg] Pos VInter2)
	 VInter2 = vInlineDot(_ Reg 1 {GetReg Id2 State} true Pos VTl)
      [] conAppExp(Coord Id1 oneArg(Id2) tuple(0)) then VInter in
	 VHd = vCallBuiltin(_ 'Value.wait' [{GetReg Id2 State}]
			    {TranslateCoord Coord State} VInter)
	 VInter = vUnify(_ Reg {GetReg Id1 State} VTl)
      [] conAppExp(Coord Id1 oneArg(Id2) _) then
	 VHd = vCallBuiltin(_ 'Record.adjoin'
			    [{GetReg Id2 State} {GetReg Id1 State} Reg]
			    {TranslateCoord Coord State} VTl)
      [] conAppExp(_ Id tupArgs(nil) _) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conAppExp(Coord Id tupArgs(Ids=_|_) _) then
	 Pos WidthReg VInter1 VInter2
      in
	 Pos = {TranslateCoord Coord State}
	 {State.cs newReg(?WidthReg)}
	 VHd = vEquateConstant(_ {Length Ids} WidthReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg Id State} WidthReg Reg] Pos VInter2)
	 {List.foldLInd Ids
	  proc {$ I VHd Id VTl}
	     VHd = vInlineDot(_ Reg I {GetReg Id State} true Pos VTl)
	  end VInter2 VTl}
      [] conAppExp(Coord Id recArgs(LabelIdList) _) then
	 Pos ArityReg VInter1 VInter2
      in
	 Pos = {TranslateCoord Coord State}
	 {State.cs newReg(?ArityReg)}
	 VHd = vEquateConstant(_ {Map LabelIdList
				  fun {$ Label#_} {TranslateLabel Label} end}
			       ArityReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Record.make'
				[{GetReg Id State} ArityReg Reg] Pos VInter2)
	 {List.foldL LabelIdList
	  proc {$ VHd Label#Id VTl}
	     VHd = vInlineDot(_ Reg {TranslateLabel Label}
			      {GetReg Id State} true Pos VTl)
	  end VInter2 VTl}
      [] refAppExp(Coord Id) then
	 VHd = vCallBuiltin(_ 'Cell.new' [{GetReg Id State} Reg]
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

   proc {WriteFile VS File} F in
      F = {New Open.file init(name: File flags: [write create truncate])}
      {F write(vs: VS)}
      {F close()}
   end

   fun {Translate Filename#Import#Body AssemblyFilename}
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
       proc {$ VHd (Id=id(_ Stamp _))#_ VTl}
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
	 case AssemblyFilename of unit then skip
	 else {WriteFile VS AssemblyFilename}
	 end
	 {P}
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
