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
   Word(make toInt) at 'x-oz://boot/Word'
   System(printName)
   Narrator('class')
   ErrorListener('class')
   CodeStore('class')
   Prebound(builtinTable: BuiltinTable
	    raiseAliceException: RaiseAliceException
	    unwrapAliceException: UnwrapAliceException)
   Assembler(assemble)
export
   Translate
define
   proc {MakeReg IdDef State ?Reg}
      case IdDef of idDef(id(_ Stamp _)) then
	 case {Dictionary.condGet State.regDict Stamp unit} of unit then
	    {State.cs newReg(?Reg)}
	    {Dictionary.put State.regDict Stamp Reg}
	 elseof Reg0 then
	    %% This test is needed because of shared statements:
	    %% The same variable may be declared on two different paths.
	    Reg = Reg0
	 end
      [] wildcard then
	 {State.cs newReg(?Reg)}
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

   fun {GetStaticCon Stamp State} Dict in
      Dict = State.shareDict
      if {Dictionary.member Dict Stamp} then
	 {Dictionary.get Dict Stamp}
      else N = {NewName} in
	 {Dictionary.put Dict Stamp N}
	 N
      end
   end

   fun {TranslateRegion (I#J)#(_#_) State}
      pos(State.filename I J)
   end

   proc {NoElse Region ElseVInstr nil State} Coord in
      Coord = {TranslateRegion Region State}
      case Coord of pos(Filename I J) then ExnReg VInter in
	 {State.cs newReg(?ExnReg)}
	 ElseVInstr = vEquateRecord(_ kernel 4 ExnReg
				    [constant(noElse) constant(Filename)
				     constant(I) constant(J)] VInter)
	 VInter = vCallBuiltin(_ 'Exception.raiseError' [ExnReg] Coord nil)
      end
   end

   proc {TranslateStm Stm VHd VTl State ReturnReg}
      case Stm of valDec(_ IdDef Exp) then
	 {TranslateExp Exp {MakeReg IdDef State} VHd VTl State}
      [] recDec(_ IdDefExpList) then RegExpList in
	 RegExpList = {Map IdDefExpList
		       fun {$ IdDef#Exp} {MakeReg IdDef State}#Exp end}
	 {FoldL RegExpList
	  proc {$ VHd Reg#Exp VTl}
	     {TranslateExp Exp Reg VHd VTl State}
	  end VHd VTl}
      [] refAppDec(Region IdDef Id) then
	 VHd = vCallBuiltin(_ 'Cell.access'
			    [{GetReg Id State} {MakeReg IdDef State}]
			    {TranslateRegion Region State} VTl)
      [] tupDec(Region nil Id) then
	 VHd = vMatch(_ {GetReg Id State} {NoElse Region $ nil State}
		      [onScalar(unit VTl)]
		      {TranslateRegion Region State} nil)
      [] tupDec(Region IdDefs Id) then ThenVInstr in
	 {FoldL IdDefs
	  proc {$ VHd IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VTl}
	 VHd = vMatch(_ {GetReg Id State} {NoElse Region $ nil State}
		      [onRecord('#' {Length IdDefs} ThenVInstr)]
		      {TranslateRegion Region State} nil)
      [] prodDec(Region LabelIdDefList Id) then Arity ThenVInstr in
	 Arity = {Map LabelIdDefList fun {$ Label#_} Label end}
	 {FoldL LabelIdDefList
	  proc {$ VHd _#IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VTl}
	 VHd = vMatch(_ {GetReg Id State} {NoElse Region $ nil State}
		      [onRecord('#' Arity ThenVInstr)]
		      {TranslateRegion Region State} nil)
      [] handleStm(Region Body1 IdDef Body2 Body3 Stamp) then
	 Reg1 Reg2 Coord TryVInstr CatchVInstr CatchVInter VInter
      in
	 {State.cs newReg(?Reg1)}
	 Reg2 = {MakeReg IdDef State}
	 Coord = {TranslateRegion Region State}
	 VHd = vExHandler(_ TryVInstr Reg1 CatchVInstr Coord VInter _)
	 {TranslateBody Body1 ?TryVInstr nil State ReturnReg}
	 {Dictionary.put State.shareDict Stamp unit}
	 CatchVInstr = vCallConstant(_ UnwrapAliceException
				     [Reg1 Reg2] Coord CatchVInter)
	 {TranslateBody Body2 ?CatchVInter nil State ReturnReg}
	 {TranslateBody Body3 ?VInter VTl=nil State ReturnReg}
      [] endHandleStm(Region Stamp) then
	 if {Dictionary.member State.shareDict Stamp} then
	    %% This statement ends the `handle' part
	    VHd = VTl
	 else
	    %% This statement ends the `try' part
	    VHd = vPopEx(_ {TranslateRegion Region State} VTl)
	 end
	 VTl = nil
      [] testStm(Region Id litTests(LitBodyList=wordLit(_)#_|_) ElseBody) then
	 IntReg Coord Matches VInter ElseVInstr
      in
	 {State.cs newReg(?IntReg)}
	 Coord = {TranslateRegion Region State}
	 VHd = vCallBuiltin(_ 'Word.toInt' [{GetReg Id State} IntReg]
			    Coord VInter)
	 Matches = {Map LitBodyList
		    fun {$ wordLit(W)#Body} ThenVInstr in
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		       onScalar({Word.toInt {Word.make 31 W}} ThenVInstr)
		    end}
	 VInter = vMatch(_ IntReg ElseVInstr Matches Coord VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] testStm(Region Id litTests(LitBodyList=intLit(_)#_|_) ElseBody) then
	 Matches ElseVInstr
      in
	 Matches = {Map LitBodyList
		    fun {$ intLit(I)#Body} ThenVInstr in
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		       onScalar(I ThenVInstr)
		    end}
	 VHd = vMatch(_ {GetReg Id State} ElseVInstr Matches
		      {TranslateRegion Region State} VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] testStm(Region Id litTests(LitBodyList=charLit(_)#_|_) ElseBody) then
	 Matches ElseVInstr
      in
	 Matches = {Map LitBodyList
		    fun {$ charLit(C)#Body} ThenVInstr in
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		       onScalar(C ThenVInstr)
		    end}
	 VHd = vMatch(_ {GetReg Id State} ElseVInstr Matches
		      {TranslateRegion Region State} VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] testStm(Region Id litTests(stringLit(S)#Body|Rest) ElseBody) then
	 TmpReg VInter ThenVInstr ElseVInstr
      in
	 {State.cs newReg(?TmpReg)}
	 VHd = vEquateConstant(_ {ByteString.make S} TmpReg VInter)
	 VInter = vTestBuiltin(_ 'Value.\'==\''
			       [{GetReg Id State} TmpReg {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl=nil)
	 {TranslateBody Body ?ThenVInstr nil State ReturnReg}
	 {TranslateStm testStm(Region Id litTests(Rest) ElseBody)
	  ElseVInstr nil State ReturnReg}
      [] testStm(_ _ litTests(nil) ElseBody) then
	 {TranslateBody ElseBody VHd VTl=nil State ReturnReg}
      [] testStm(Region Id litTests(LitBodyList=realLit(_)#_|_) ElseBody) then
	 Matches ElseVInstr
      in
	 Matches = {Map LitBodyList
		    fun {$ realLit(F)#Body} ThenVInstr in
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		       onScalar(F ThenVInstr)
		    end}
	 VHd = vMatch(_ {GetReg Id State} ElseVInstr Matches
		      {TranslateRegion Region State} VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] testStm(Region Id tagTests(TagBodyList) ElseBody) then
	 Matches ElseVInstr
      in
	 Matches = {Map TagBodyList
		    proc {$ Label#_#ConArgs#Body ?Match} ThenVInstr in
		       Match =
		       case ConArgs of none then
			  onScalar(Label ThenVInstr)
		       [] some(oneArg(IdDef)) then ThenVInstr0 in
			  ThenVInstr0 = vGetVariable(_ {MakeReg IdDef State}
						     ThenVInstr)
			  onRecord(Label 1 ThenVInstr0)
		       [] some(tupArgs(nil)) then
			  onScalar(Label ThenVInstr)
		       [] some(tupArgs(IdDefs=_|_)) then ThenVInstr0 in
			  {FoldL IdDefs
			   proc {$ VHd IdDef VTl}
			      VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			   end ThenVInstr0 ThenVInstr}
			  onRecord(Label {Length IdDefs} ThenVInstr0)
		       [] some(prodArgs(LabelIdDefList)) then ThenVInstr0 in
			  {FoldL LabelIdDefList
			   proc {$ VHd _#IdDef VTl}
			      VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			   end ThenVInstr0 ThenVInstr}
			  onRecord(Label {Map LabelIdDefList
					  fun {$ Label#_} Label end}
				   ThenVInstr0)
		       end
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		    end}
	 VHd = vMatch(_ {GetReg Id State} ElseVInstr Matches
		      {TranslateRegion Region State} VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] testStm(Region Id conTests(Con#ConArgs#Body|Rest) ElseBody) then
	 Reg ThenVInstr ElseVInstr
      in
	 Reg = {GetReg Id State}
	 case Con#ConArgs of con(Id)#none then
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl=nil)
	 [] con(Id)#some(tupArgs(nil)) then
	    VHd = vTestBuiltin(_ 'Value.\'==\''
			       [Reg {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr ElseVInstr VTl=nil)
	 [] con(Id)#some(Args) then ThenVInstr0 Coord in
	    VHd = vTestBuiltin(_ 'Record.testLabel'
			       [Reg {GetReg Id State} {State.cs newReg($)}]
			       ThenVInstr0 ElseVInstr VTl=nil)
	    Coord = {TranslateRegion Region State}
	    case Args of oneArg(IdDef) then
	       ThenVInstr0 = vInlineDot(_ Reg 1 {MakeReg IdDef State} true
					Coord ThenVInstr)
	    [] tupArgs(IdDefs=_|_) then
	       {List.foldLInd IdDefs
		proc {$ I VHd IdDef VTl}
		   VHd = vInlineDot(_ Reg I {MakeReg IdDef State} true
				    Coord VTl)
		end ThenVInstr0 ThenVInstr}
	    [] prodArgs(LabelIdDefList) then
	       {FoldL LabelIdDefList
		proc {$ VHd Label#IdDef VTl}
		   VHd = vInlineDot(_ Reg Label {MakeReg IdDef State} true
				    Coord VTl)
		end ThenVInstr0 ThenVInstr}
	    end
/*--**
	 [] staticCon(Stamp)#none then
	    {TranslateMatch tagTest({GetStaticCon Stamp State} unit)
	     Reg ThenVInstr State}
	 [] staticCon(Stamp)#some(tupArgs(nil)) then
	    {TranslateMatch tagTest({GetStaticCon Stamp State} unit)
	     Reg ThenVInstr State}
	 [] staticCon(Stamp)#some(Args) then
	    {TranslateMatch tagAppTest({GetStaticCon Stamp State} unit Args)
	     Reg ThenVInstr State}
*/
	 end
	 {TranslateBody Body ?ThenVInstr nil State ReturnReg}
	 {TranslateStm testStm(Region Id conTests(Rest) ElseBody)
	  ElseVInstr nil State ReturnReg}
      [] testStm(_ _ conTests(nil) ElseBody) then
	 {TranslateBody ElseBody VHd VTl=nil State ReturnReg}
      [] testStm(Region Id vecTests(VecBodyList) ElseBody) then
	 Matches ElseVInstr
      in
	 Matches = {Map VecBodyList
		    fun {$ IdDefs#Body} ThenVInstr0 ThenVInstr in
		       {FoldL IdDefs
			proc {$ VHd IdDef VTl}
			   VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			end ThenVInstr0 ThenVInstr}
		       {TranslateBody Body ?ThenVInstr nil State ReturnReg}
		       onRecord('#[]' {Length IdDefs} ThenVInstr0)
		    end}
	 VHd = vMatch(_ {GetReg Id State} ElseVInstr Matches
		      {TranslateRegion Region State} VTl=nil)
	 {TranslateBody ElseBody ?ElseVInstr nil State ReturnReg}
      [] raiseStm(Region Id) then
	 VHd = vCallConstant(_ RaiseAliceException [{GetReg Id State}]
			     {TranslateRegion Region State} VTl=nil)
      [] reraiseStm(Region Id) then
	 VHd = vCallConstant(_ RaiseAliceException [{GetReg Id State}]
			     {TranslateRegion Region State} VTl=nil)
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
      [] returnStm(_ Exp) then {TranslateExp Exp ReturnReg VHd VTl=nil State}
      [] exportStm(_ Exp) then {TranslateExp Exp ReturnReg VHd VTl=nil State}
      end
   end

   proc {TranslateExp Exp Reg VHd VTl State}
      case Exp of litExp(_ Lit) then Constant in
	 Constant = case Lit of wordLit(W) then {Word.make 31 W}
		    [] intLit(I) then I
		    [] charLit(C) then C
		    [] stringLit(S) then {ByteString.make S}
		    [] realLit(F) then F
		    end
	 VHd = vEquateConstant(_ Constant Reg VTl)
      [] primExp(_ Builtinname) then
	 VHd = vEquateConstant(_ BuiltinTable.Builtinname Reg VTl)
      [] newExp(Region) then
	 VHd = vCallBuiltin(_ 'Name.new' [Reg]
			    {TranslateRegion Region State} VTl)
      [] varExp(_ Id) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] tagExp(_ Label _ none) then
	 VHd = vEquateConstant(_ Label Reg VTl)
      [] tagExp(Region Label _ some(unary)) then
	 PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 PredId = pid(if {IsAtom Label} then Label else '' end
		      2 {TranslateRegion Region State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 VInstr = vEquateRecord(_ Label 1 ResReg [value(ArgReg)] nil)
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] tagExp(_ Label _ some(_)) then
	 VHd = vEquateConstant(_ fun {$ X} {Adjoin X Label} end Reg VTl)
      [] conExp(_ con(Id) none) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conExp(Region con(Id) some(Arity)) then
	 Coord PredId NLiveRegs ArgReg ResReg VInstr GRegs Code
      in
	 Coord = {TranslateRegion Region State}
	 PredId = pid({GetPrintName Id State} 2 Coord nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ArgReg)}
	 {State.cs newReg(?ResReg)}
	 case Arity of unary then WidthReg VInter1 VInter2 in
	    {State.cs newReg(?WidthReg)}
	    VInstr = vEquateConstant(_ 1 WidthReg VInter1)
	    VInter1 = vCallBuiltin(_ 'Tuple.make'
				   [{GetReg Id State} WidthReg ResReg]
				   Coord VInter2)
	    VInter2 = vInlineDot(_ ResReg 1 ArgReg true Coord nil)
	 [] tupArity(0) then VInter in
	    VInstr = vCallBuiltin(_ 'Value.wait' [ArgReg] Coord VInter)
	    VInter = vUnify(_ ResReg {GetReg Id State} nil)
	 else
	    VInstr = vCallBuiltin(_ 'Record.adjoin'
				  [ArgReg {GetReg Id State} ResReg] Coord nil)
	 end
	 {State.cs
	  endDefinition(VInstr [ArgReg ResReg] nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] conExp(Region staticCon(Stamp) ConArity) then
	 {TranslateExp
	  tagExp(Region {GetStaticCon Stamp State} unit ConArity)
	  Reg VHd VTl State}
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
      [] prodExp(_ LabelIdList) then Rec in
	 Rec = {List.toRecord '#'
		{Map LabelIdList
		 fun {$ Label#Id} Label#value({GetReg Id State}) end}}
	 VHd = vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] selExp(_ Label _) then
	 VHd = vEquateConstant(_ fun {$ X} X.Label end Reg VTl)
      [] vecExp(_ nil) then
	 VHd = vEquateConstant(_ '#[]' Reg VTl)
      [] vecExp(_ Ids) then
	 VHd = vEquateRecord(_ '#[]' {Length Ids} Reg
			     {Map Ids
			      fun {$ Id} value({GetReg Id State}) end} VTl)
      [] funExp(Region _ _ tupArgs(IdDefs=_|_) Body) then
	 PredId NLiveRegs ResReg FormalRegs BodyVInstr GRegs Code
      in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Region.1.1#'.'#Region.1.2#'/'#
		       {Length IdDefs}#'-ary'}
		      {Length IdDefs} + 1 {TranslateRegion Region State}
		      nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = {FoldR IdDefs
		       fun {$ IdDef Rest} {MakeReg IdDef State}|Rest end
		       [ResReg]}
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
	 case Args of oneArg(IdDef) then
	    ArgReg = {MakeReg IdDef State}
	    BodyVInstr = ThenVInstr
	 [] tupArgs(nil) then
	    {State.cs newReg(?ArgReg)}
	    BodyVInstr = vTestConstant(_ ArgReg unit
				       ThenVInstr ElseVInstr
				       {TranslateRegion Region State} nil)
	 [] prodArgs(LabelIdDefList) then Arity ThenVInstr0 in
	    {State.cs newReg(?ArgReg)}
	    Arity = {Map LabelIdDefList fun {$ Label#_} Label end}
	    BodyVInstr = vMatch(_ ArgReg ElseVInstr
				[onRecord('#' Arity ThenVInstr0)]
				{TranslateRegion Region State} nil)
	    {FoldL LabelIdDefList
	     proc {$ VHd _#IdDef VTl}
		VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 end
	 {TranslateBody Body ?ThenVInstr nil State ResReg}
	 {State.cs newReg(?MatchReg)}
	 ElseVInstr = vEquateConstant(_ BuiltinTable.'General.Match'
				      MatchReg ElseVInter)
	 ElseVInter = vCallBuiltin(_ 'Exception.raiseError' [MatchReg]
				   {TranslateRegion Region State} nil)
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 VHd = vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] primAppExp(Region Builtinname Ids) then Value Regs in
	 Value = BuiltinTable.Builtinname
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
	 [] prodArgs(LabelIdList) then
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
      [] tagAppExp(_ Label _ prodArgs(LabelIdList)) then
	 VHd = vEquateRecord(_ Label
			     {Map LabelIdList fun {$ Label#_} Label end} Reg
			     {Map LabelIdList
			      fun {$ _#Id} value({GetReg Id State}) end}
			     VTl)
      [] conAppExp(Region con(Id1) oneArg(Id2)) then
	 Coord WidthReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 VHd = vEquateConstant(_ 1 WidthReg VInter1)
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg Id1 State} WidthReg Reg]
				Coord VInter2)
	 VInter2 = vInlineDot(_ Reg 1 {GetReg Id2 State} true Coord VTl)
      [] conAppExp(_ con(Id) tupArgs(nil)) then
	 VHd = vUnify(_ Reg {GetReg Id State} VTl)
      [] conAppExp(Region con(Id) tupArgs(Ids=_|_)) then
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
      [] conAppExp(Region con(Id) prodArgs(LabelIdList)) then
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
      [] conAppExp(Region staticCon(Stamp) Arity) then
	 {TranslateExp
	  tagAppExp(Region {GetStaticCon Stamp State} unit Arity)
	  Reg VHd VTl State}
      [] refAppExp(Region Id) then
	 VHd = vCallBuiltin(_ 'Cell.new' [{GetReg Id State} Reg]
			    {TranslateRegion Region State} VTl)
      [] selAppExp(Region Label _ Id) then
	 VHd = vInlineDot(_ {GetReg Id State} Label Reg false
			  {TranslateRegion Region State} VTl)
      [] funAppExp(Region Id _ Args) then
	 {TranslateExp varAppExp(Region Id Args) Reg VHd VTl State}
      end
   end

   proc {TranslateBody Stms VHd VTl State ReturnReg}
      {FoldL Stms
       proc {$ VHd Stm VTl}
	  {TranslateStm Stm VHd VTl State ReturnReg}
       end VHd VTl}
   end

   fun {Translate Filename Import#(Body#Sign)}
      NarratorObject Reporter CS ImportReg ExportReg
      State VInstr VInter Code NLiveRegs
   in
      NarratorObject = {New Narrator.'class' init(?Reporter)}
      _ = {New ErrorListener.'class' init(NarratorObject)}
      CS = {New CodeStore.'class'
	    init(proc {$ getSwitch(_ $)} false end Reporter)}
      {CS startDefinition()}
      {CS newReg(?ImportReg)}
      {CS newReg(?ExportReg)}
      State = state(regDict: {NewDictionary} shareDict: {NewDictionary} cs: CS
		    filename: {VirtualString.toAtom Filename})
      {List.foldLInd Import
       proc {$ I VHd IdDef#_#_ VTl}
	  VHd = vInlineDot(_ ImportReg I {MakeReg IdDef State} false unit VTl)
       end VInstr VInter}
      {TranslateBody Body ?VInter nil State ExportReg}
      {CS endDefinition(VInstr [ImportReg ExportReg] nil
			nil ?Code ?NLiveRegs)}
      case Code of Code1#Code2 then StartLabel EndLabel Res P VS in
	 StartLabel = {NewName}
	 EndLabel = {NewName}
	 {Assembler.assemble
	  (lbl(StartLabel)|
	   definition(x(0) EndLabel
		      pid({VirtualString.toAtom 'Component '#Filename} 2
			  pos(Filename 1 0) nil NLiveRegs)
		      unit nil Code1)|
	   endDefinition(StartLabel)|
	   {Append Code2 [lbl(EndLabel) unify(x(0) g(0)) return]}) [Res]
	  switches ?P ?VS}
	 {P}
	 {Functor.new
	  {List.toRecord 'import'
	   {List.mapInd Import
	    fun {$ I _#Sign#URL} I#info('from': URL 'type': sig(Sign)) end}}
	  sig(Sign) Res}#VS#Sign
      end
   end
end
