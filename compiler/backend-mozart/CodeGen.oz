%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2002
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
   Compiler(codeStoreClass assemble)
   Prebound(builtinTable: BuiltinTable
	    raiseAliceException: RaiseAliceException
	    unwrapAliceException: UnwrapAliceException)
export
   Translate
define
   fun {TestBuiltin Builtinname Regs ThenVInstr ElseVInstr State} Reg VInter in
      {State.cs newReg(?Reg)}
      VInter = vTestBool(_ Reg ThenVInstr ElseVInstr nil unit nil)
      vCallBuiltin(_ Builtinname {Append Regs [Reg]} unit VInter)
   end

   proc {InitReg Stamp State ?Reg}
      {State.cs newReg(?Reg)}
      {Dictionary.put State.regDict Stamp Reg}
   end

   proc {MakeReg IdDef State ?Reg}
      case IdDef of 'IdDef'('Id'(_ Stamp _)) then
	 case {Dictionary.condGet State.regDict Stamp unit} of unit then
	    {State.cs newReg(?Reg)}
	    {Dictionary.put State.regDict Stamp Reg}
	 elseof Reg0 then
	    %% This test is needed because of shared statements:
	    %% The same variable may be declared on two different paths.
	    Reg = Reg0
	 end
      [] 'Wildcard' then
	 {State.cs newReg(?Reg)}
      end
   end

   fun {GetReg IdRef State} Id in
      Id = case IdRef of 'IdRef'(Id) then Id
	   [] 'LastIdRef'(Id) then Id
	   end
      case Id of 'Id'(_ Stamp _) then
	 {Dictionary.get State.regDict Stamp}
      end
   end

   fun {TranslateRegion (I#J)#(_#_) State}
      pos(State.filename I J)
   end

   fun {TranslateTagTests
	 Region IdRef TagBodyVec ElseVInstr State ReturnReg IsTry}
      Matches = {Record.foldR TagBodyVec
		 fun {$ Label#_#Args#Body In} Match ThenVInstr in
		    Match =
		    case Args of 'OneArg'(IdDef) then ThenVInstr0 in
		       ThenVInstr0 = vGetVariable(_ {MakeReg IdDef State}
						  ThenVInstr)
		       onRecord(Label 1 ThenVInstr0)
		    [] 'TupArgs'('#[]') then
		       onScalar(Label ThenVInstr)
		    [] 'TupArgs'(IdDefs) then ThenVInstr0 in
		       {Record.foldL IdDefs
			proc {$ VHd IdDef VTl}
			   VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			end ThenVInstr0 ThenVInstr}
		       onRecord(Label {Width IdDefs} ThenVInstr0)
		    [] 'ProdArgs'(LabelIdDefVec) then ThenVInstr0 in
		       {Record.foldL LabelIdDefVec
			proc {$ VHd _#IdDef VTl}
			   VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			end ThenVInstr0 ThenVInstr}
		       onRecord(Label {Record.foldR LabelIdDefVec
				       fun {$ Label#_ In} Label|In end nil}
				ThenVInstr0)
		    end
		    ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		    Match|In
		 end nil}
   in
      vMatch(_ {GetReg IdRef State} ElseVInstr Matches
	     {TranslateRegion Region State} nil)
   end

   fun {NoElse Region State} Coord in
      Coord = {TranslateRegion Region State}
      case Coord of pos(Filename I J) then ExnReg VInter in
	 {State.cs newReg(?ExnReg)}
	 VInter = vCallBuiltin(_ 'Exception.raiseError' [ExnReg] Coord nil)
	 vEquateConstant(_ kernel(noElse Filename I J) ExnReg VInter)
      end
   end

   proc {TranslateBody Stms State ReturnReg IsTry ?VHd}
      case Stms of 'LastUse'(_ _)|Body then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] 'ValDec'(_ IdDef Exp)|Body then VInter in
	 VHd = {TranslateExp Exp {MakeReg IdDef State} VInter State}
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'RefDec'(Region IdDef IdRef)|Body then VInter in
	 VHd = vCallBuiltin(_ 'Cell.access'
			    [{GetReg IdRef State} {MakeReg IdDef State}]
			    {TranslateRegion Region State} VInter)
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'TupDec'(Region '#[]' IdRef)|Body then VInter in
	 VHd = vMatch(_ {GetReg IdRef State} {NoElse Region State}
		      [onScalar(unit VInter)]
		      {TranslateRegion Region State} nil)
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'TupDec'(Region IdDefs IdRef)|Body then ThenVInstr VInter in
	 VHd = vMatch(_ {GetReg IdRef State} {NoElse Region State}
		      [onRecord('#' {Width IdDefs} ThenVInstr)]
		      {TranslateRegion Region State} nil)
	 {Record.foldL IdDefs
	  proc {$ VHd IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VInter}
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'ProdDec'(Region LabelIdDefVec IdRef)|Body
      then Arity ThenVInstr VInter in
	 Arity = {Record.foldR LabelIdDefVec
		  fun {$ Label#_ In} Label|In end nil}
	 VHd = vMatch(_ {GetReg IdRef State} {NoElse Region State}
		      [onRecord('#' Arity ThenVInstr)]
		      {TranslateRegion Region State} nil)
	 {Record.foldL LabelIdDefVec
	  proc {$ VHd _#IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VInter}
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] ['TryStm'(Region TryBody IdDef1 IdDef2 HandleBody)]
      then Reg1 Coord TryVInstr HandleVInstr Reg2 HandleVInter in
	 Reg1 = {MakeReg IdDef1 State}
	 Coord = {TranslateRegion Region State}
	 VHd = vExHandler(_ TryVInstr Reg1 HandleVInstr Coord nil _)
	 TryVInstr = {TranslateBody TryBody State ReturnReg true|IsTry}
	 Reg2 = {MakeReg IdDef2 State}
	 HandleVInstr = vCallConstant(_ UnwrapAliceException
				      [Reg1 Reg2] Coord HandleVInter)
	 HandleVInter = {TranslateBody HandleBody State ReturnReg IsTry}
      [] ['EndTryStm'(Region Body)] then VInter in
	 VHd = vPopEx(_ {TranslateRegion Region State} VInter)
	 VInter = {TranslateBody Body State ReturnReg IsTry.2}
      [] ['EndHandleStm'(_ Body)] then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('WordLit'(_)#_ ...)) ElseBody)]
      then IntReg Coord Matches VInter ElseVInstr in
	 {State.cs newReg(?IntReg)}
	 Coord = {TranslateRegion Region State}
	 VHd = vCallBuiltin(_ 'Word.toInt' [{GetReg IdRef State} IntReg]
			    Coord VInter)
	 VInter = vMatch(_ IntReg ElseVInstr Matches Coord nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'WordLit'(W)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar({Word.toInt W} ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('IntLit'(_)#_ ...)) ElseBody)]
      then Matches ElseVInstr in
	 VHd = vMatch(_ {GetReg IdRef State} ElseVInstr Matches
		      {TranslateRegion Region State} nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'IntLit'(I)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar(I ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('CharLit'(_)#_ ...)) ElseBody)]
      then Matches ElseVInstr in
	 VHd = vMatch(_ {GetReg IdRef State} ElseVInstr Matches
		      {TranslateRegion Region State} nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'CharLit'(C)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar(C ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(_ IdRef 'LitTests'(LitBodyVec='#[]'('StringLit'(_)#_ ...))
		    ElseBody)]
      then ElseVInstr in
	 {Record.foldL LitBodyVec
	  proc {$ VHd 'StringLit'(S)#Body ElseVInstr}
	     TmpReg VInter ThenVInstr
	  in
	     {State.cs newReg(?TmpReg)}
	     VHd = vEquateConstant(_ S TmpReg VInter)
	     VInter = {TestBuiltin 'Value.\'==\'' [{GetReg IdRef State} TmpReg]
		       ThenVInstr ElseVInstr State}
	     ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
	  end VHd ElseVInstr}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('RealLit'(_)#_ ...)) ElseBody)]
      then Matches ElseVInstr in
	 VHd = vMatch(_ {GetReg IdRef State} ElseVInstr Matches
		      {TranslateRegion Region State} nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'RealLit'(F)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar(F ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef 'TagTests'(TagBodyVec) ElseBody)]
      then ElseVInstr in
	 VHd = {TranslateTagTests Region IdRef TagBodyVec
		ElseVInstr State ReturnReg IsTry}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef 'ConTests'(ConBodyVec) ElseBody)]
      then ElseVInstr in
	 {Record.foldL ConBodyVec
	  proc {$ VHd ConIdRef#Args#Body ElseVInstr} Reg ThenVInstr in
	     Reg = {GetReg IdRef State}
	     case Args of 'TupArgs'('#[]') then
		VHd = {TestBuiltin 'Value.\'==\'' [Reg {GetReg ConIdRef State}]
		       ThenVInstr ElseVInstr State}
	     else ThenVInstr0 Coord in
		VHd = {TestBuiltin 'Record.testLabel'
		       [Reg {GetReg ConIdRef State}]
		       ThenVInstr0 ElseVInstr State}
		Coord = {TranslateRegion Region State}
		case Args of 'OneArg'(IdDef) then
		   ThenVInstr0 = vInlineDot(_ Reg 1 {MakeReg IdDef State} true
					    Coord ThenVInstr)
		[] 'TupArgs'(IdDefs) then
		   {Record.foldLInd IdDefs
		    proc {$ I VHd IdDef VTl}
		       VHd = vInlineDot(_ Reg I {MakeReg IdDef State} true
					Coord VTl)
		    end ThenVInstr0 ThenVInstr}
		[] 'ProdArgs'(LabelIdDefVec) then
		   {Record.foldL LabelIdDefVec
		    proc {$ VHd Label#IdDef VTl}
		       VHd = vInlineDot(_ Reg Label {MakeReg IdDef State} true
					Coord VTl)
		    end ThenVInstr0 ThenVInstr}
		end
	     end
	     ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
	  end VHd ElseVInstr}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef 'VecTests'(VecBodyVec) ElseBody)]
      then Matches ElseVInstr in
	 VHd = vMatch(_ {GetReg IdRef State} ElseVInstr Matches
		      {TranslateRegion Region State} nil)
	 Matches = {Record.foldR VecBodyVec
		    fun {$ IdDefs#Body In}
		       case IdDefs of '#[]' then ThenVInstr in
			  ThenVInstr = {TranslateBody Body State ReturnReg
					IsTry}
			  onScalar('#[]' ThenVInstr)|In
		       else ThenVInstr0 ThenVInstr in
			  {Record.foldL IdDefs
			   proc {$ VHd IdDef VTl}
			      VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
			   end ThenVInstr0 ThenVInstr}
			  ThenVInstr = {TranslateBody Body State ReturnReg
					IsTry}
			  onRecord('#[]' {Width IdDefs} ThenVInstr0)|In
		       end
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['RaiseStm'(Region IdRef)] then Coord CoordReg VInter1 VInter2 in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?CoordReg)}
	 VHd = vEquateConstant(_ Coord CoordReg VInter1)
	 VInter1 = vCallConstant(_ RaiseAliceException
				 [{GetReg IdRef State} CoordReg] Coord VInter2)
	 VInter2 = if IsTry.1 then vPopEx(_ Coord nil)
		   else nil
		   end
      [] ['ReraiseStm'(Region IdRef)] then Coord VInter in
	 %--** Mozart does not update the stack trace
	 Coord = {TranslateRegion Region State}
	 VHd = vCallBuiltin(_ 'Exception.raise' [{GetReg IdRef State}]
			    Coord VInter)
	 VInter = if IsTry.1 then vPopEx(_ Coord nil)
		  else nil
		  end
      [] ['SharedStm'(_ Body Stamp)] then
	 if {Dictionary.member State.shareDict Stamp} then
	    VHd = {Dictionary.get State.shareDict Stamp}
	 else
	    {Dictionary.put State.shareDict Stamp VHd}
	    case {TranslateBody Body State ReturnReg IsTry} of nil then
	       VHd = nil
	    elseof VBody then
	       VHd = vShared(_ _ {State.cs newLabel($)} VBody)
	    end
	 end
      [] ['ReturnStm'(_ Exp)] then
	 VHd = {TranslateExp Exp ReturnReg nil State}
      [] ['IndirectStm'(_ BodyOptRef)] then
	 case {Access BodyOptRef} of 'SOME'(Body) then
	    VHd = {TranslateBody Body State ReturnReg IsTry}
	 end
      [] ['ExportStm'(_ Exp)] then
	 VHd = {TranslateExp Exp ReturnReg nil State}
      end
   end

   fun {TranslateExp Exp Reg VTl State}
      case Exp of 'LitExp'(_ Lit) then Constant in
	 Constant = case Lit of 'WordLit'(W) then {Word.make 31 {Word.toInt W}}
		    [] 'IntLit'(I) then I
		    [] 'CharLit'(C) then C
		    [] 'StringLit'(S) then S
		    [] 'RealLit'(F) then F
		    end
	 vEquateConstant(_ Constant Reg VTl)
      [] 'PrimExp'(_ Builtinname) then
	 vEquateConstant(_ BuiltinTable.Builtinname Reg VTl)
      [] 'NewExp'(Region 'InId') then
	 vCallBuiltin(_ 'Name.new' [Reg] {TranslateRegion Region State} VTl)
      [] 'NewExp'(Region 'ExId'(PrintName)) then VHd VInter ArgReg in
	 {State.cs newReg(?ArgReg)}
	 VHd = vEquateConstant(_ PrintName ArgReg VInter)
	 VInter = vCallBuiltin(_ 'Name.newNamed' [ArgReg Reg]
			       {TranslateRegion Region State} VTl)
	 VHd
      [] 'VarExp'(_ IdRef) then
	 vUnify(_ Reg {GetReg IdRef State} VTl)
      [] 'TagExp'(_ Label _ 'OneArg'(IdRef)) then
	 vEquateRecord(_ Label 1 Reg [value({GetReg IdRef State})] VTl)
      [] 'TagExp'(_ Label _ 'TupArgs'('#[]')) then
	 vEquateConstant(_ Label Reg VTl)
      [] 'TagExp'(_ Label _ 'TupArgs'(IdRefs)) then
	 vEquateRecord(_ Label {Width IdRefs} Reg
		       {Record.foldR IdRefs
			fun {$ IdRef In} value({GetReg IdRef State})|In end
			nil} VTl)
      [] 'TagExp'(_ Label _ 'ProdArgs'(LabelIdRefVec)) then Rec in
	 Rec = {List.toRecord '#'
		{Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In} Label#value({GetReg IdRef State})|In
		 end nil}}
	 vEquateRecord(_ Label {Arity Rec} Reg {Record.toList Rec} VTl)
      [] 'ConExp'(Region IdRef1 'OneArg'(IdRef2)) then
	 Coord WidthReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg IdRef1 State} WidthReg Reg]
				Coord VInter2)
	 VInter2 = vInlineDot(_ Reg 1 {GetReg IdRef2 State} true Coord VTl)
	 vEquateConstant(_ 1 WidthReg VInter1)
      [] 'ConExp'(_ IdRef 'TupArgs'('#[]')) then
	 vUnify(_ Reg {GetReg IdRef State} VTl)
      [] 'ConExp'(Region IdRef 'TupArgs'(IdRefs)) then
	 Coord WidthReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 VInter1 = vCallBuiltin(_ 'Tuple.make'
				[{GetReg IdRef State} WidthReg Reg]
				Coord VInter2)
	 {Record.foldLInd IdRefs
	  proc {$ I VHd IdRef VTl}
	     VHd = vInlineDot(_ Reg I {GetReg IdRef State} true Coord VTl)
	  end VInter2 VTl}
	 vEquateConstant(_ {Width IdRefs} WidthReg VInter1)
      [] 'ConExp'(Region IdRef 'ProdArgs'(LabelIdRefVec)) then
	 Coord ArityReg VInter1 VInter2
      in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?ArityReg)}
	 {Record.foldL LabelIdRefVec
	  proc {$ VHd Label#IdRef VTl}
	     VHd = vInlineDot(_ Reg Label {GetReg IdRef State} true Coord VTl)
	  end VInter2 VTl}
	 VInter1 = vCallBuiltin(_ 'Record.make'
				[{GetReg IdRef State} ArityReg Reg]
				Coord VInter2)
	 vEquateConstant(_ {Record.foldR LabelIdRefVec
			    fun {$ Label#_ In} Label|In end nil}
			 ArityReg VInter1)
      [] 'RefExp'(Region IdRef) then
	 vCallBuiltin(_ 'Cell.new' [{GetReg IdRef State} Reg]
		      {TranslateRegion Region State} VTl)
      [] 'TupExp'(_ '#[]') then
	 vEquateConstant(_ unit Reg VTl)
      [] 'TupExp'(_ IdRefs) then
	 vEquateRecord(_ '#' {Width IdRefs} Reg
		       {Record.foldR IdRefs
			fun {$ IdRef In} value({GetReg IdRef State})|In end
			nil} VTl)
      [] 'ProdExp'(_ LabelIdRefVec) then Rec in
	 Rec = {List.toRecord '#'
		{Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In}
		    Label#value({GetReg IdRef State})|In
		 end nil}}
	 vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] 'VecExp'(_ '#[]') then
	 vEquateConstant(_ '#[]' Reg VTl)
      [] 'VecExp'(_ IdRefs) then
	 vEquateRecord(_ '#[]' {Width IdRefs} Reg
		       {Record.foldR IdRefs
			fun {$ IdRef In}
			   value({GetReg IdRef State})|In
			end nil} VTl)
      [] 'FunExp'(Region _ _ 'TupArgs'(IdDefs) Body) andthen {Width IdDefs} > 1
      then PredId NLiveRegs ResReg FormalRegs BodyVInstr GRegs Code in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Region.1.1#'.'#Region.1.2#'/'#
		       {Width IdDefs}#'-ary'}
		      {Width IdDefs} + 1 {TranslateRegion Region State}
		      nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = {Record.foldR IdDefs
		       fun {$ IdDef Rest} {MakeReg IdDef State}|Rest end
		       [ResReg]}
	 BodyVInstr = {TranslateBody Body State ResReg [false]}
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] 'FunExp'(Region _ _ Args Body) then
	 PredId NLiveRegs ResReg FormalRegs ArgReg
	 BodyVInstr ThenVInstr ElseVInstr MatchReg ElseVInter GRegs Code
      in
	 PredId = pid({VirtualString.toAtom
		       State.filename#':'#Region.1.1#'.'#Region.1.2}
		      2 {TranslateRegion Region State} nil NLiveRegs)
	 {State.cs startDefinition()}
	 {State.cs newReg(?ResReg)}
	 FormalRegs = [ArgReg ResReg]
	 case Args of 'OneArg'(IdDef) then
	    ArgReg = {MakeReg IdDef State}
	    BodyVInstr = ThenVInstr
	 [] 'TupArgs'('#[]') then
	    {State.cs newReg(?ArgReg)}
	    BodyVInstr = vTestConstant(_ ArgReg unit
				       ThenVInstr ElseVInstr
				       {TranslateRegion Region State} nil)
	 [] 'TupArgs'('#[]'(IdDef)) then ThenVInstr0 in
	    {State.cs newReg(?ArgReg)}
	    BodyVInstr = vMatch(_ ArgReg ElseVInstr
				[onRecord('#' 1 ThenVInstr0)]
				{TranslateRegion Region State} nil)
	    ThenVInstr0 = vGetVariable(_ {MakeReg IdDef State} ThenVInstr)
	 [] 'ProdArgs'(LabelIdDefVec) then Arity ThenVInstr0 in
	    {State.cs newReg(?ArgReg)}
	    Arity = {Record.foldR LabelIdDefVec
		     fun {$ Label#_ In} Label|In end nil}
	    BodyVInstr = vMatch(_ ArgReg ElseVInstr
				[onRecord('#' Arity ThenVInstr0)]
				{TranslateRegion Region State} nil)
	    {Record.foldL LabelIdDefVec
	     proc {$ VHd _#IdDef VTl}
		VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	     end ThenVInstr0 ThenVInstr}
	 end
	 ThenVInstr = {TranslateBody Body State ResReg [false]}
	 {State.cs newReg(?MatchReg)}
	 ElseVInstr = vEquateConstant(_ BuiltinTable.'General.Match'
				      MatchReg ElseVInter)
	 ElseVInter = vCallBuiltin(_ 'Exception.raiseError' [MatchReg]
				   {TranslateRegion Region State} nil)
	 {State.cs
	  endDefinition(BodyVInstr FormalRegs nil ?GRegs ?Code ?NLiveRegs)}
	 vDefinition(_ Reg PredId unit GRegs Code VTl)
      [] 'PrimAppExp'(_ 'Char.ord' '#[]'(IdRef)) then
	 vUnify(_ Reg {GetReg IdRef State} VTl)
      [] 'PrimAppExp'(Region 'Future.await' '#[]'(IdRef)) then ArgReg VInter in
	 ArgReg = {GetReg IdRef State}
	 VInter = vUnify(_ Reg ArgReg VTl)
	 vCallBuiltin(_ 'Value.wait' [ArgReg]
		      {TranslateRegion Region State} VInter)
      [] 'PrimAppExp'(Region 'General.:=' '#[]'(IdRef1 IdRef2)) then VInter in
	 VInter = vEquateConstant(_ unit Reg VTl)
	 vCallBuiltin(_ 'Cell.assign'
		      [{GetReg IdRef1 State} {GetReg IdRef2 State}]
		      {TranslateRegion Region State} VInter)
      [] 'PrimAppExp'(Region Builtinname '#[]') then ArgReg VInter Value in
	 {State.cs newReg(?ArgReg)}
	 Value = BuiltinTable.Builtinname
	 if {CompilerSupport.isBuiltin Value} then
	    VInter = vCallBuiltin(_ {System.printName Value} [ArgReg Reg]
				  {TranslateRegion Region State} VTl)
	 else
	    VInter = vCallConstant(_ Value [ArgReg Reg]
				   {TranslateRegion Region State} VTl)
	 end
	 vEquateConstant(_ unit ArgReg VInter)
      [] 'PrimAppExp'(Region Builtinname IdRefs) then Value Regs in
	 Value = BuiltinTable.Builtinname
	 Regs = {Record.foldR IdRefs
		 fun {$ IdRef Regs} {GetReg IdRef State}|Regs end [Reg]}
	 if {CompilerSupport.isBuiltin Value} then
	    vCallBuiltin(_ {System.printName Value} Regs
			 {TranslateRegion Region State} VTl)
	 else
	    vCallConstant(_ Value Regs {TranslateRegion Region State} VTl)
	 end
      [] 'VarAppExp'(Region IdRef1 'OneArg'(IdRef2)) then
	 vDeconsCall(_ {GetReg IdRef1 State} {GetReg IdRef2 State} Reg
		     {TranslateRegion Region State} VTl)
      [] 'VarAppExp'(Region IdRef 'TupArgs'('#[]')) then ArgReg VInter in
	 {State.cs newReg(?ArgReg)}
	 VInter = vCall(_ {GetReg IdRef State} [ArgReg Reg]
			{TranslateRegion Region State} VTl)
	 vEquateConstant(_ unit ArgReg VInter)
      [] 'VarAppExp'(Region IdRef1 'TupArgs'('#[]'(IdRef2)))
      then ArgReg VInter in
	 {State.cs newReg(?ArgReg)}
	 VInter = vCall(_ {GetReg IdRef1 State} [ArgReg Reg]
			{TranslateRegion Region State} VTl)
	 vEquateRecord(_ '#' 1 ArgReg [value({GetReg IdRef2 State})] VInter)
      [] 'VarAppExp'(Region IdRef 'TupArgs'(IdRefs)) then
	 vConsCall(_ {GetReg IdRef State}
		   {Record.foldR IdRefs
		    fun {$ IdRef Rest} {GetReg IdRef State}|Rest end [Reg]}
		   {TranslateRegion Region State} VTl)
      [] 'VarAppExp'(Region IdRef 'ProdArgs'(LabelIdRefVec)) then
	 Rec ArgReg VInter
      in
	 Rec = {List.toRecord '#'
		{Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In}
		    Label#value({GetReg IdRef State})|In
		 end nil}}
	 {State.cs newReg(?ArgReg)}
	 VInter = vCall(_ {GetReg IdRef State} [ArgReg Reg]
			{TranslateRegion Region State} VTl)
	 vEquateRecord(_ '#' {Arity Rec} ArgReg {Record.toList Rec} VInter)
      [] 'SelAppExp'(Region _ Label _ IdRef) then
	 vInlineDot(_ {GetReg IdRef State} Label Reg false
		    {TranslateRegion Region State} VTl)
      [] 'LazySelAppExp'(Region _ Label _ IdRef) then LabelReg VInter in
	 {State.cs newReg(?LabelReg)}
	 VInter = vCallBuiltin(_ 'Value.byNeedDot'
			       [{GetReg IdRef State} LabelReg Reg]
			       {TranslateRegion Region State} VTl)
	 vEquateConstant(_ Label LabelReg VInter)
      [] 'FunAppExp'(Region IdRef1 _ 'OneArg'(IdRef2)) then
	 vCall(_ {GetReg IdRef1 State} [{GetReg IdRef2 State} Reg]
	       {TranslateRegion Region State} VTl)
      [] 'FunAppExp'(Region IdRef _ 'TupArgs'(IdRefs))
	 andthen {Width IdRefs} > 0
      then
	 vCall(_ {GetReg IdRef State}
	       {Record.foldR IdRefs
		fun {$ IdRef Rest} {GetReg IdRef State}|Rest end [Reg]}
	       {TranslateRegion Region State} VTl)
      [] 'FunAppExp'(Region IdRef _ Args) then
	 {TranslateExp 'VarAppExp'(Region IdRef Args) Reg VTl State}
      [] 'FailExp'(Region) then Coord in
	 Coord = {TranslateRegion Region State}
	 case Coord of pos(Filename I J) then ExnReg VInter in
	    {State.cs newReg(?ExnReg)}
	    VInter = vCallBuiltin(_ 'Value.byNeedFail' [ExnReg Reg] Coord VTl)
	    vEquateConstant(_ alice(failed Filename I J) ExnReg VInter)
	 end
      end
   end

   fun {Translate Filename Import#Body#_#Sign StampValueList}
      NarratorObject Reporter CS ImportReg ExportReg
      State RegToValueMapping VInstr VInter GRegs Code NLiveRegs
   in
      NarratorObject = {New Narrator.'class' init(?Reporter)}
      _ = {New ErrorListener.'class' init(NarratorObject)}
      CS = {New Compiler.codeStoreClass
	    init(proc {$ getSwitch(_ $)} false end Reporter)}
      State = state(regDict: {NewDictionary} shareDict: {NewDictionary} cs: CS
		    filename: {VirtualString.toAtom Filename})
      RegToValueMapping = {Dictionary.new}
      {ForAll StampValueList
       proc {$ Stamp#Value} Reg in
	  Reg = {InitReg Stamp State}
	  {Dictionary.put RegToValueMapping Reg Value}
       end}
      {CS startDefinition()}
      {CS newReg(?ImportReg)}
      {CS newReg(?ExportReg)}
      {Record.foldLInd Import
       proc {$ I VHd Id#_#_ VTl}
	  VHd = vInlineDot(_ ImportReg I {MakeReg 'IdDef'(Id) State}
			   false unit VTl)
       end VInstr VInter}
      VInter = {TranslateBody Body State ExportReg [false]}
      {CS endDefinition(VInstr [ImportReg ExportReg] nil
			?GRegs ?Code ?NLiveRegs)}
      case Code of Code1#Code2 then StartLabel EndLabel Res P VS in
	 StartLabel = {NewName}
	 EndLabel = {NewName}
	 {Compiler.assemble
	  (lbl(StartLabel)|
	   definition(x(0) EndLabel
		      pid({VirtualString.toAtom 'Component '#Filename} 2
			  pos({VirtualString.toAtom Filename} 1 0) nil
			  NLiveRegs)
		      unit {List.mapInd GRegs fun {$ I _} g(I - 1) end} Code1)|
	   endDefinition(StartLabel)|
	   {Append Code2 [lbl(EndLabel) unify(x(0) g({Length GRegs})) return]})
	  {Append {Map GRegs
		   fun {$ Reg} {Dictionary.get RegToValueMapping Reg} end}
	   [Res]}
	  switches/*--**(profile: true)*/ ?P ?VS}
	 {P}
	 {Functor.new
	  {List.toRecord 'import'
	   {Record.foldRInd Import
	    fun {$ I _#Sign#URL In}
	       I#info('from': URL 'type': sig(Sign))|In
	    end nil}}
	  sig(Sign) Res}#VS
      end
   end
end
