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
   Narrator('class')
   ErrorListener('class')
   CodeStore('class')
   Prebound(env)
export
   Translate
define
   BuiltinTable = unit   %--** table mapping Stockhausen builtins to Mozart

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

   proc {SetReg id(_ Stamp _) State Reg}
      {Dictionary.put State.regDict Stamp Reg}
   end

   fun {GetReg id(_ Stamp _) State}
      {Dictionary.get State.regDict Stamp}
   end

   fun {GetPrintName id(_ _ Name)}
      case Name of inId then ''
      [] exId(PrintName) then PrintName
      end
   end

   fun {TranslateCoord I#J}
      pos(bogus I J)   %--**
   end

   fun {TranslateLab lab(_ S)}
      {String.toAtom S}
   end

   fun {TranslateLit Lit}
      case Lit of wordLit(W) then W   %--** {Word.make ...}
      [] intLit(I) then I
      [] charLit(C) then C
      [] stringLit(S) then {ByteString.make S}
      [] realLit(S) then {String.toFloat S}
      end
   end

   proc {TranslateStm Stm VHd VTl State ReturnReg}
      case Stm of valDec(_ Id Exp _) then
	 {TranslateExp Exp {MakeReg Id State} VHd VTl State}
      [] recDec(_ IdsExpList _) then
	 {ForAll IdsExpList
	  proc {$ Ids#_}
	     case Ids of Id|Idr then Reg in
		Reg = {MakeReg Id State}
		{ForAll Idr proc {$ Id} {SetReg Id State Reg} end}
	     [] nil then skip
	     end
	  end}
	 {FoldL IdsExpList
	  proc {$ VHd Ids#Exp VTl} Reg in
	     Reg = case Ids of Id|_ then {GetReg Id State}
		   [] nil then {State.cs newReg($)}
		   end
	     {TranslateExp Exp Reg VHd VTl State}
	  end VHd VTl}
      [] conDec(Coord Id false _) then
	 VHd = vCallBuiltin(_ 'Name.new' [{MakeReg Id State}]
			    {TranslateCoord Coord} VTl)
      [] conDec(Coord Id true _) then
	 Reg Pos NameReg VInter PredId NLiveRegs ArgReg TmpReg ResReg
	 VInstr VInter1 VInter2 GRegs Code
      in
	 Reg = {MakeReg Id State}
	 Pos = {TranslateCoord Coord}
	 {State.cs newReg(?NameReg)}
	 VHd = vCallBuiltin(_ 'Name.new' [NameReg] Pos VInter)
	 PredId = pid({GetPrintName Id} 2 Pos nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?TmpReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vEquateConstant(_ 1 TmpReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make' [NameReg TmpReg ResReg]
				Pos VInter2)
	 VInter2 = vInlineDot(_ ResReg 1 ArgReg false Pos nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VInter = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] evalStm(_ Exp) then
	 {TranslateExp Exp {State.cs newReg($)} VHd VTl State}
      [] handleStm(Coord Body1 Id Body2) then Reg TryVInstr CatchVInstr in
	 Reg = {MakeReg Id State}
	 VHd = vExHandler(_ TryVInstr Reg CatchVInstr
			  {TranslateCoord Coord} VTl _)
	 {TranslateBody Body1 ?TryVInstr nil State ReturnReg}
	 {TranslateBody Body2 ?CatchVInstr nil State ReturnReg}
      [] endHandleStm(Coord Body) then
	 VHd = vPopEx(_ {TranslateCoord Coord}
		      {TranslateBody Body $ VTl State ReturnReg})
      [] testStm(Coord Id Test Body1 Body2) then Reg0 ThenVInstr ElseVInstr in
	 %--** assemble several testStm into a single vMatch
	 Reg0 = {GetReg Id State}
	 case Test of litTest(Lit) then Constant in
	    Constant = {TranslateLit Lit}
	    if {IsNumber Constant} orelse {IsLiteral Constant} then
	       VHd = vTestConstant(_ Reg0 {TranslateLit Lit}
				   ThenVInstr ElseVInstr {TranslateCoord Coord}
				   VTl)
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
	 [] conTest(_ some(_)) then
	    VHd = VTl   %--**
	 [] tupTest(Ids) then ThenVInstr0 in
	    VHd = vMatch(_ Reg0 ElseVInstr
			 [onRecord('#' {Length Ids} ThenVInstr0)]
			 {TranslateCoord Coord} VTl)
	    {FoldL Ids
	     proc {$ VHd Id VTl}
		VHd = vGetVariable(_ {MakeReg Id State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 [] recTest(FeatureIdList) then Arity ThenVInstr0 in
	    Arity = {Map FeatureIdList fun {$ F#_} F end}
	    VHd = vMatch(_ Reg0 ElseVInstr [onRecord('#' Arity ThenVInstr0)]
			 {TranslateCoord Coord} VTl)
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
	 end
	 {TranslateBody Body1 ?ThenVInstr nil State ReturnReg}
	 {TranslateBody Body2 ?ElseVInstr nil State ReturnReg}
      [] raiseStm(Coord Id) then
	 VHd = vCallBuiltin(_ 'Exception.raise' [{GetReg Id State}]
			    {TranslateCoord Coord} VTl)
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
      [] returnStm(_ Exp) then
	 case ReturnReg of unit then
	    {Exception.raiseError stoc(internal translateStm Stm)}
	 else
	    {TranslateExp Exp ReturnReg VHd VTl State}
	 end
      [] exportStm(_ _) then
	 VHd = VTl   %--**
      end
   end

   proc {TranslateExp Exp Reg VHd VTl State}
      case Exp of litExp(_ Lit) then
	 VHd = vEquateConstant(_ {TranslateLit Lit} Reg VTl)
      [] varExp(_ Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] tupExp(_ Ids) then
	 VHd = vEquateRecord(_ '#' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] recExp(_ LabIdList) then Arity in
	 Arity = {Map LabIdList fun {$ Lab#_} {TranslateLab Lab} end}
	 VHd = vEquateRecord(_ '#' Arity Reg
			     {Map LabIdList
			      fun {$ _#Id} value({GetReg Id State}) end} VTl)
      [] selExp(_ Lab) then
	 VHd = vEquateConstant(_ fun {$ X} X.{TranslateLab Lab} end Reg VTl)
      [] funExp(Coord PrintName ArgsBodyList) then
	 case ArgsBodyList of [oneArg(Id)#Body] then   %--** support others
	    PredId NLiveRegs ResReg FormalRegs VInstr GRegs Code
	 in
	    PredId = pid(PrintName 2 {TranslateCoord Coord} nil NLiveRegs)
	    {State.cs startDefinition()}
	    {State.cs newReg(?ResReg)}
	    FormalRegs = [{MakeReg Id State} ResReg]
	    {TranslateBody Body ?VInstr nil State ResReg}
	    {State.cs
	     endDefinition(VInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	    VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
	 end
      [] appExp(Coord Id1 Args) then
	 case Args of oneArg(Id2) then   %--** support others
	    VHd = vCall(_ {GetReg Id1 State} [{GetReg Id2 State} Reg]
			{TranslateCoord Coord} VTl)
	 end
      [] selAppExp(Coord Lab Id) then
	 VHd = vInlineDot(_ {GetReg Id State} {TranslateLab Lab} Reg false
			  {TranslateCoord Coord} VTl)
      [] conAppExp(_ _ _) then
	 VHd = VTl   %--**
      [] buitinAppExp(Coord Builtinname Ids) then
	 VHd = vCallBuiltin(_ BuiltinTable.Builtinname
			    {FoldR Ids
			     fun {$ Id Regs} {GetReg Id State}|Regs end [Reg]}
			    {TranslateCoord Coord} VTl)
      [] adjExp(Coord Id1 Id2) then
	 VHd = vCallBuiltin(_ 'Record.adjoin' [{GetReg Id1 State}
					       {GetReg Id2 State} Reg]
			    {TranslateCoord Coord} VTl)
      end
   end

   proc {TranslateBody Stms VHd VTl State ReturnReg}
      {FoldL Stms
       proc {$ VHd Stm VTl}
	  {TranslateStm Stm VHd VTl State ReturnReg}
       end VHd VTl}
   end

   fun {Translate Program}
      NarratorObject Reporter CS RegDict Prebound VInstr GRegs Code NLiveRegs
   in
      NarratorObject = {New Narrator.'class' init(?Reporter)}
      _ = {New ErrorListener.'class' init(NarratorObject)}
      CS = {New CodeStore.'class'
	    init(proc {$ getSwitch(_ X)} X = false end Reporter)}
      {MakeRegDict CS ?RegDict ?Prebound}
      {CS startDefinition()}
      {TranslateBody Program ?VInstr nil
       state(regDict: RegDict shareDict: {NewDictionary} cs: CS) unit}
      {CS endDefinition(VInstr nil nil ?GRegs ?Code ?NLiveRegs)}
      case Code of Code1#Code2 then StartLabel EndLabel in
	 StartLabel = {NewName}
	 EndLabel = {NewName}
	 {Map GRegs fun {$ Reg} Prebound.Reg end}#
	 (lbl(StartLabel)|
	  definition(x(0) EndLabel
		     pid('Toplevel abstraction' 0 pos('' 1 0) [sited]
			 NLiveRegs)
		     unit {List.mapInd GRegs fun {$ I _} g(I - 1) end}
		     Code1)|
	  endDefinition(StartLabel)|
	  {Append Code2 [lbl(EndLabel) tailCall(x(0) 0)]})
      end
   end
end
