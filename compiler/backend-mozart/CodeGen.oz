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
   Debug(setRaiseOnBlock) at 'x-oz://boot/Debug'
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

   fun {GetArg IdRef State}
      case IdRef of 'IdRef'('Id'(_ Stamp _)) then
	 value({Dictionary.get State.regDict Stamp})
      [] 'LastIdRef'('Id'(_ Stamp _)) then
	 value({Dictionary.get State.regDict Stamp})
      [] 'Lit'('WordLit'(W)) then constant({Word.make 31 {Word.toInt W}})
      [] 'Lit'('IntLit'(I)) then constant(I)
      [] 'Lit'('CharLit'(C)) then constant(C)
      [] 'Lit'('StringLit'(S)) then constant(S)
      [] 'Lit'('RealLit'(F)) then constant(F)
      [] 'Prim'(Builtinname) then constant(BuiltinTable.Builtinname)
      [] 'Value'(X _) then constant(X)
      end
   end

   fun {GetReg IdRef VHd VTl State}
      case {GetArg IdRef State} of value(Reg) then
	 VHd = VTl
	 Reg
      [] constant(X) then Reg in
	 {State.cs newReg(?Reg)}
	 VHd = vEquateConstant(_ X Reg VTl)
	 Reg
      end
   end

   fun {TranslateRegion (I#J)#(_#_) State}
      pos(State.filename I J)
   end

   fun {TranslateTagTests
	Region IdRef Labels TagBodyVec ElseVInstr State ReturnReg IsTry}
      Matches = {Record.foldR TagBodyVec
		 fun {$ N#Args#Body In} Label Match ThenVInstr in
		    Label = Labels.(N + 1)
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
      Reg = {GetReg IdRef VHd VTl State}
      VTl = vMatch(_ Reg ElseVInstr Matches
		   {TranslateRegion Region State} nil)
      VHd
   in
      VHd
   end

   fun {NoElse Region State} Coord in
      Coord = {TranslateRegion Region State}
      case Coord of pos(Filename I J) then ExnReg VInter in
	 {State.cs newReg(?ExnReg)}
	 VInter = vCallBuiltin(_ 'Exception.raiseError' [ExnReg] Coord nil)
	 vEquateConstant(_ kernel(noElse Filename I J) ExnReg VInter)
      end
   end

   fun lazy {LazyPolySel X F}
      try X.F
      catch error(InnerE ...) then
	 {Value.byNeedFail error(InnerE)}
      end
   end

   fun {CallConstant Region X ArgRegs VTl State}
      if {IsDet X} andthen {CompilerSupport.isBuiltin X} then
	 vCallBuiltin(_ {System.printName X} ArgRegs
		      {TranslateRegion Region State} VTl)
      else
	 vCallConstant(_ X ArgRegs {TranslateRegion Region State} VTl)
      end
   end

   proc {TranslateBody Stms State ReturnReg IsTry ?VHd}
      case Stms of 'Entry'(_ _)|Body then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] 'Exit'(_ _ _)|Body then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] 'LastUse'(_ _)|Body then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] 'ValDec'(_ IdDef Exp)|Body then VInter in
	 VHd = {TranslateExp Exp {MakeReg IdDef State} VInter State}
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'RecDec'(_ IdDefExpVec)|Body then VInter in
	 {Record.forAll IdDefExpVec
	  proc {$ IdDef#_} _ = {MakeReg IdDef State} end}
	 {Record.foldL IdDefExpVec
	  proc {$ VHd IdDef#Exp VTl}
	     VHd = {TranslateExp Exp {MakeReg IdDef State} VTl State}
	  end VHd VInter}
	 VInter = {TranslateBody Body State ReturnReg IsTry}
      [] 'RefDec'(Region IdDef IdRef)|Body then Reg VInter1 VInter2 in
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vCallBuiltin(_ 'Cell.access'
				[Reg {MakeReg IdDef State}]
				{TranslateRegion Region State} VInter2)
	 VInter2 = {TranslateBody Body State ReturnReg IsTry}
      [] 'TupDec'(Region '#[]' IdRef)|Body then Reg VInter1 VInter2 in
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vMatch(_ Reg {NoElse Region State}
			  [onScalar(unit VInter2)]
			  {TranslateRegion Region State} nil)
	 VInter2 = {TranslateBody Body State ReturnReg IsTry}
      [] 'TupDec'(Region IdDefs IdRef)|Body
      then Reg ThenVInstr VInter1 VInter2 in
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vMatch(_ Reg {NoElse Region State}
			  [onRecord('#' {Width IdDefs} ThenVInstr)]
			  {TranslateRegion Region State} nil)
	 {Record.foldL IdDefs
	  proc {$ VHd IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VInter2}
	 VInter2 = {TranslateBody Body State ReturnReg IsTry}
      [] 'ProdDec'(Region LabelIdDefVec IdRef)|Body
      then Arity Reg ThenVInstr VInter1 VInter2 in
	 Arity = {Record.foldR LabelIdDefVec
		  fun {$ Label#_ In} Label|In end nil}
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vMatch(_ Reg {NoElse Region State}
			  [onRecord('#' Arity ThenVInstr)]
			  {TranslateRegion Region State} nil)
	 {Record.foldL LabelIdDefVec
	  proc {$ VHd _#IdDef VTl}
	     VHd = vGetVariable(_ {MakeReg IdDef State} VTl)
	  end ThenVInstr VInter2}
	 VInter2 = {TranslateBody Body State ReturnReg IsTry}
      [] ['TryStm'(Region TryBody IdDef1 IdDef2 HandleBody)]
      then Reg1 Coord TryVInstr HandleVInstr Reg2 HandleVInter in
	 Reg1 = {MakeReg IdDef1 State}
	 Coord = {TranslateRegion Region State}
	 VHd = vExHandler(_ TryVInstr Reg1 HandleVInstr Coord nil _)
	 TryVInstr = {TranslateBody TryBody State ReturnReg true|IsTry}
	 Reg2 = {MakeReg IdDef2 State}
	 HandleVInstr = {CallConstant Region UnwrapAliceException [Reg1 Reg2]
			 HandleVInter State}
	 HandleVInter = {TranslateBody HandleBody State ReturnReg IsTry}
      [] ['EndTryStm'(Region Body)] then VInter in
	 VHd = vPopEx(_ {TranslateRegion Region State} VInter)
	 VInter = {TranslateBody Body State ReturnReg IsTry.2}
      [] ['EndHandleStm'(_ Body)] then
	 VHd = {TranslateBody Body State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('WordLit'(_)#_ ...)) ElseBody)]
      then IntReg Coord Reg Matches VInter1 VInter2 ElseVInstr in
	 {State.cs newReg(?IntReg)}
	 Coord = {TranslateRegion Region State}
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vCallBuiltin(_ 'Word.toInt' [Reg IntReg]
				Coord VInter2)
	 VInter2 = vMatch(_ IntReg ElseVInstr Matches Coord nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'WordLit'(W)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar({Word.toInt W} ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('IntLit'(_)#_ ...)) ElseBody)]
      then Reg VInter Matches ElseVInstr in
	 Reg = {GetReg IdRef VHd VInter State}
	 VInter = vMatch(_ Reg ElseVInstr Matches
			 {TranslateRegion Region State} nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'IntLit'(I)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar(I ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('CharLit'(_)#_ ...)) ElseBody)]
      then Reg VInter Matches ElseVInstr in
	 Reg = {GetReg IdRef VHd VInter State}
	 VInter = vMatch(_ Reg ElseVInstr Matches
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
	     TmpReg Reg VInter1 VInter2 ThenVInstr
	  in
	     {State.cs newReg(?TmpReg)}
	     Reg = {GetReg IdRef VHd VInter1 State}
	     VInter1 = vEquateConstant(_ S TmpReg VInter2)
	     VInter2 = {TestBuiltin 'Value.\'==\'' [Reg TmpReg]
			ThenVInstr ElseVInstr State}
	     ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
	  end VHd ElseVInstr}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef
		    'LitTests'(LitBodyVec='#[]'('RealLit'(_)#_ ...)) ElseBody)]
      then Reg VInter Matches ElseVInstr in
	 Reg = {GetReg IdRef VHd VInter State}
	 VInter = vMatch(_ Reg ElseVInstr Matches
			 {TranslateRegion Region State} nil)
	 Matches = {Record.foldR LitBodyVec
		    fun {$ 'RealLit'(F)#Body In} ThenVInstr in
		       ThenVInstr = {TranslateBody Body State ReturnReg IsTry}
		       onScalar(F ThenVInstr)|In
		    end nil}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef 'TagTests'(Labels TagBodyVec) ElseBody)]
      then ElseVInstr in
	 VHd = {TranslateTagTests Region IdRef Labels TagBodyVec
		ElseVInstr State ReturnReg IsTry}
	 ElseVInstr = {TranslateBody ElseBody State ReturnReg IsTry}
      [] ['TestStm'(Region IdRef 'ConTests'(ConBodyVec) ElseBody)]
      then ElseVInstr in
	 {Record.foldL ConBodyVec
	  proc {$ VHd ConIdRef#Args#Body ElseVInstr}
	     Reg ConReg VInter1 VInter2 ThenVInstr
	  in
	     Reg = {GetReg IdRef VHd VInter1 State}
	     ConReg = {GetReg ConIdRef VInter1 VInter2 State}
	     case Args of 'TupArgs'('#[]') then
		VInter2 = {TestBuiltin 'Value.\'==\'' [Reg ConReg]
			   ThenVInstr ElseVInstr State}
	     else ThenVInstr0 Coord in
		VInter2 = {TestBuiltin 'Record.testLabel' [Reg ConReg]
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
      then Reg VInter Matches ElseVInstr in
	 Reg = {GetReg IdRef VHd VInter State}
	 VInter = vMatch(_ Reg ElseVInstr Matches
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
      [] ['RaiseStm'(Region IdRef)]
      then Coord CoordReg Reg VInter1 VInter2 VInter3 in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?CoordReg)}
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vEquateConstant(_ Coord CoordReg VInter2)
	 VInter2 = {CallConstant Region RaiseAliceException [Reg CoordReg]
		    VInter3 State}
	 VInter3 = if IsTry.1 then vPopEx(_ Coord nil)
		   else nil
		   end
      [] ['ReraiseStm'(Region IdRef)] then Coord Reg VInter1 VInter2 in
	 %--** Mozart does not update the stack trace
	 Coord = {TranslateRegion Region State}
	 Reg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vCallBuiltin(_ 'Exception.raise' [Reg] Coord VInter2)
	 VInter2 = if IsTry.1 then vPopEx(_ Coord nil)
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

   fun {VarApp Region IdRef ArgRegs VTl State}
      case {GetArg IdRef State} of value(Reg0) then
	 vCall(_ Reg0 ArgRegs {TranslateRegion Region State} VTl)
      [] constant(X) then
	 {CallConstant Region X ArgRegs VTl State}
      end
   end

   fun {TranslateVarAppExp Region IdRef Args IsDirect Reg VTl State}
      case Args of 'OneArg'(ArgIdRef) then ArgReg VHd VInter1 in
	 ArgReg = {GetReg ArgIdRef VHd VInter1 State}
	 if IsDirect then
	    VInter1 = {VarApp Region IdRef [ArgReg Reg] VTl State}
	 else Reg0 VInter2 in
	    Reg0 = {GetReg IdRef VInter1 VInter2 State}
	    VInter2 = vDeconsCall(_ Reg0 ArgReg Reg
				  {TranslateRegion Region State} VTl)
	 end
	 VHd
      [] 'TupArgs'('#[]') then ArgReg VHd VInter in
	 {State.cs newReg(?ArgReg)}
	 VHd = vEquateConstant(_ unit ArgReg VInter)
	 VInter = {VarApp Region IdRef [ArgReg Reg] VTl State}
	 VHd
      [] 'TupArgs'('#[]'(ArgIdRef)) then ArgReg VHd VInter in
	 {State.cs newReg(?ArgReg)}
	 VHd = vEquateRecord(_ '#' 1 ArgReg [{GetArg ArgIdRef State}] VInter)
	 VInter = {VarApp Region IdRef [ArgReg Reg] VTl State}
	 VHd
      [] 'TupArgs'(IdRefs) then ArgRegs VHd VInter1 in
	 VInter1#ArgRegs = {Record.foldR IdRefs
			    fun {$ IdRef VHd#Rest} Reg VTl in
			       Reg = {GetReg IdRef VHd VTl State}
			       VTl#(Reg|Rest)
			    end VHd#[Reg]}
	 if IsDirect then
	    VInter1 = {VarApp Region IdRef ArgRegs VTl State}
	 else Reg0 VInter2 in
	    Reg0 = {GetReg IdRef VInter1 VInter2 State}
	    VInter2 = vConsCall(_ Reg0 ArgRegs
				{TranslateRegion Region State} VTl)
	 end
	 VHd
      [] 'ProdArgs'(LabelIdRefVec) then Args Rec ArgReg VHd VInter in
	 {State.cs newReg(?ArgReg)}
	 Args = {Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In}
		    Label#{GetArg IdRef State}|In
		 end nil}
	 Rec = {List.toRecord '#' Args}
	 VHd = vEquateRecord(_ '#' {Arity Rec} ArgReg
			     {Record.toList Rec} VInter)
	 VInter = {VarApp Region IdRef [ArgReg Reg] VTl State}
	 VHd
      end
   end

   fun {TranslateExp Exp Reg VTl State}
      case Exp of 'NewExp'(Region 'InId') then
	 vCallBuiltin(_ 'Name.new' [Reg] {TranslateRegion Region State} VTl)
      [] 'NewExp'(Region 'ExId'(PrintName0))
      then PrintName VHd VInter ArgReg in
	 PrintName = case {Atom.toString PrintName0} of &'|Rest then
			{String.toAtom Rest}
		     else PrintName0
		     end
	 {State.cs newReg(?ArgReg)}
	 VHd = vEquateConstant(_ PrintName ArgReg VInter)
	 VInter = vCallBuiltin(_ 'Name.newNamed' [ArgReg Reg]
			       {TranslateRegion Region State} VTl)
	 VHd
      [] 'VarExp'(_ IdRef) then Reg0 VHd VInter in
	 Reg0 = {GetReg IdRef VHd VInter State}
	 VInter = vUnify(_ Reg Reg0 VTl)
	 VHd
      [] 'TagExp'(_ Labels N 'OneArg'(IdRef)) then
	 vEquateRecord(_ Labels.(N + 1) 1 Reg [{GetArg IdRef State}] VTl)
      [] 'TagExp'(_ Labels N 'TupArgs'('#[]')) then
	 vEquateConstant(_ Labels.(N + 1) Reg VTl)
      [] 'TagExp'(_ Labels N 'TupArgs'(IdRefs)) then Args in
	 Args = {Record.foldR IdRefs
		 fun {$ IdRef In}
		    {GetArg IdRef State}|In
		 end nil}
	 vEquateRecord(_ Labels.(N + 1) {Width IdRefs} Reg Args VTl)
      [] 'TagExp'(_ Labels N 'ProdArgs'(LabelIdRefVec)) then Args Rec in
	 Args = {Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In}
		    Label#{GetArg IdRef State}|In
		 end nil}
	 Rec = {List.toRecord '#' Args}
	 vEquateRecord(_ Labels.(N + 1) {Arity Rec} Reg
		       {Record.toList Rec} VTl)
      [] 'ConExp'(Region IdRef1 'OneArg'(IdRef2))
      then Coord WidthReg Reg1 Reg2 VHd VInter1 VInter2 VInter3 VInter4 in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 Reg1 = {GetReg IdRef1 VHd VInter1 State}
	 VInter1 = vEquateConstant(_ 1 WidthReg VInter2)
	 VInter2 = vCallBuiltin(_ 'Tuple.make' [Reg1 WidthReg Reg]
				Coord VInter3)
	 Reg2 = {GetReg IdRef2 VInter3 VInter4 State}
	 VInter4 = vInlineDot(_ Reg 1 Reg2 true Coord VTl)
	 VHd
      [] 'ConExp'(_ IdRef 'TupArgs'('#[]')) then Reg0 VHd VInter in
	 Reg0 = {GetReg IdRef VHd VInter State}
	 VInter = vUnify(_ Reg Reg0 VTl)
	 VHd
      [] 'ConExp'(Region IdRef 'TupArgs'(IdRefs))
      then Coord WidthReg Reg0 VHd VInter1 VInter2 VInter3 in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?WidthReg)}
	 Reg0 = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vEquateConstant(_ {Width IdRefs} WidthReg VInter2)
	 VInter2 = vCallBuiltin(_ 'Tuple.make' [Reg0 WidthReg Reg]
				Coord VInter3)
	 {Record.foldLInd IdRefs
	  proc {$ I VHd IdRef VTl} Reg0 VInter in
	     Reg0 = {GetReg IdRef VHd VInter State}
	     VInter = vInlineDot(_ Reg I Reg0 true Coord VTl)
	  end VInter3 VTl}
	 VHd
      [] 'ConExp'(Region IdRef 'ProdArgs'(LabelIdRefVec))
      then Coord ArityReg Reg0 VHd VInter1 VInter2 VInter3 in
	 Coord = {TranslateRegion Region State}
	 {State.cs newReg(?ArityReg)}
	 Reg0 = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vEquateConstant(_ {Record.foldR LabelIdRefVec
				      fun {$ Label#_ In} Label|In end nil}
				   ArityReg VInter2)
	 VInter2 = vCallBuiltin(_ 'Record.make' [Reg0 ArityReg Reg]
				Coord VInter3)
	 {Record.foldL LabelIdRefVec
	  proc {$ VHd Label#IdRef VTl} Reg0 VInter in
	     Reg0 = {GetReg IdRef VHd VInter State}
	     VInter = vInlineDot(_ Reg Label Reg0 true Coord VTl)
	  end VInter3 VTl}
	 VHd
      [] 'RefExp'(Region IdRef) then Reg0 VHd VInter in
	 Reg0 = {GetReg IdRef VHd VInter State}
	 VInter = vCallBuiltin(_ 'Cell.new' [Reg0 Reg]
			       {TranslateRegion Region State} VTl)
	 VHd
      [] 'TupExp'(_ '#[]') then
	 vEquateConstant(_ unit Reg VTl)
      [] 'TupExp'(_ IdRefs) then Args in
	 Args = {Record.foldR IdRefs
		 fun {$ IdRef In}
		    {GetArg IdRef State}|In
		 end nil}
	 vEquateRecord(_ '#' {Width IdRefs} Reg Args VTl)
      [] 'ProdExp'(_ LabelIdRefVec) then Args Rec in
	 Args = {Record.foldR LabelIdRefVec
		 fun {$ Label#IdRef In}
		    Label#{GetArg IdRef State}|In
		 end nil}
	 Rec = {List.toRecord '#' Args}
	 vEquateRecord(_ '#' {Arity Rec} Reg {Record.toList Rec} VTl)
      [] 'PolyProdExp'(Info LabelIdRefVec) then
	 {TranslateExp 'ProdExp'(Info LabelIdRefVec) Reg VTl State}
      [] 'VecExp'(_ '#[]') then
	 vEquateConstant(_ '#[]' Reg VTl)
      [] 'VecExp'(_ IdRefs) then Args in
	 Args = {Record.foldR IdRefs
		 fun {$ IdRef In}
		    {GetArg IdRef State}|In
		 end nil}
	 vEquateRecord(_ '#[]' {Width IdRefs} Reg Args VTl)
      [] 'FunExp'(Region _ _ 'TupArgs'(IdDefs) _ Body)
	 andthen {Width IdDefs} > 1
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
      [] 'FunExp'(Region _ _ Args _ Body) then
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
      [] 'PrimAppExp'(_ 'Char.ord' '#[]'(IdRef)) then Reg0 VHd VInter in
	 Reg0 = {GetReg IdRef VHd VInter State}
	 VInter = vUnify(_ Reg Reg0 VTl)
	 VHd
      [] 'PrimAppExp'(Region 'Future.await' '#[]'(IdRef))
      then ArgReg VHd VInter1 VInter2 in
	 ArgReg = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vCallBuiltin(_ 'Value.wait' [ArgReg]
				{TranslateRegion Region State} VInter2)
	 VInter2 = vUnify(_ Reg ArgReg VTl)
	 VHd
      [] 'PrimAppExp'(Region 'General.:=' '#[]'(IdRef1 IdRef2))
      then Reg1 Reg2 VHd VInter1 VInter2 VInter3 in
	 Reg1 = {GetReg IdRef1 VHd VInter1 State}
	 Reg2 = {GetReg IdRef2 VInter1 VInter2 State}
	 VInter2 = vCallBuiltin(_ 'Cell.assign' [Reg1 Reg2]
				{TranslateRegion Region State} VInter3)
	 VInter3 = vEquateConstant(_ unit Reg VTl)
	 VHd
      [] 'PrimAppExp'(Region Builtinname '#[]') then ArgReg VInter Value in
	 {State.cs newReg(?ArgReg)}
	 Value = BuiltinTable.Builtinname
	 if {CompilerSupport.isBuiltin Value} then
	    VInter = vCallBuiltin(_ {System.printName Value} [ArgReg Reg]
				  {TranslateRegion Region State} VTl)
	 else
	    VInter = {CallConstant Region Value [ArgReg Reg] VTl State}
	 end
	 vEquateConstant(_ unit ArgReg VInter)
      [] 'PrimAppExp'(Region Builtinname IdRefs)
      then Value Regs VHd VInter in
	 Value = BuiltinTable.Builtinname
	 VInter#Regs = {Record.foldR IdRefs
			fun {$ IdRef VHd#Regs} Reg VTl in
			   Reg = {GetReg IdRef VHd VTl State}
			   VTl#(Reg|Regs)
			end VHd#[Reg]}
	 if {CompilerSupport.isBuiltin Value} then
	    VInter = vCallBuiltin(_ {System.printName Value} Regs
				  {TranslateRegion Region State} VTl)
	 else
	    VInter = {CallConstant Region Value Regs VTl State}
	 end
	 VHd
      [] 'VarAppExp'(Region IdRef Args) then
	 {TranslateVarAppExp Region IdRef Args false Reg VTl State}
      [] 'DirectAppExp'(Region IdRef Args) then
	 {TranslateVarAppExp Region IdRef Args true Reg VTl State}
      [] 'SelExp'(Region _ Label _ IdRef) then Reg0 VHd VInter in
	 Reg0 = {GetReg IdRef VHd VInter State}
	 VInter = vInlineDot(_ Reg0 Label Reg false
			     {TranslateRegion Region State} VTl)
	 VHd
      [] 'LazyPolySelExp'(Region Label IdRef)
      then LabelReg Reg0 VHd VInter1 VInter2 in
	 {State.cs newReg(?LabelReg)}
	 Reg0 = {GetReg IdRef VHd VInter1 State}
	 VInter1 = vEquateConstant(_ Label LabelReg VInter2)
	 VInter2 = {CallConstant Region LazyPolySel [Reg0 LabelReg Reg]
		    VTl State}
	 VHd
      [] 'FunAppExp'(Region IdRef1 _ 'OneArg'(IdRef2))
      then Reg1 Reg2 VHd VInter1 VInter2 in
	 Reg1 = {GetReg IdRef1 VHd VInter1 State}
	 Reg2 = {GetReg IdRef2 VInter1 VInter2 State}
	 VInter2 = vCall(_ Reg1 [Reg2 Reg] {TranslateRegion Region State} VTl)
	 VHd
      [] 'FunAppExp'(Region IdRef _ 'TupArgs'(IdRefs))
	 andthen {Width IdRefs} > 0
      then Reg0 VHd Args VInter1 VInter2 in
	 Reg0 = {GetReg IdRef VHd VInter1 State}
	 VInter2#Args = {Record.foldR IdRefs
			 fun {$ IdRef VHd#Rest} Reg VTl in
			    Reg = {GetReg IdRef VHd VTl State}
			    VTl#(Reg|Rest)
			 end VInter1#[Reg]}
	 VInter2 = vCall(_ Reg0 Args {TranslateRegion Region State} VTl)
	 VHd
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

   fun {Translate Filename Imports#Body#_#Sign StampValueList}
      NarratorObject Reporter CS ImportReg ExportReg
      State RegToValueMapping VInstr VInter GRegs Code NLiveRegs
   in
      {Debug.setRaiseOnBlock {Thread.this} true}
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
      {Record.foldLInd Imports
       proc {$ I VHd Id#_#_#_ VTl}
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
	 {Debug.setRaiseOnBlock {Thread.this} false}
	 {P}
	 {Functor.new
	  {List.toRecord 'import'
	   {Record.foldRInd Imports
	    fun {$ I _#Sign#URL#_ In}
	       I#info('from': URL 'type': sig(Sign))|In
	    end nil}}
	  sig(Sign) Res}#VS
      end
   end
end
