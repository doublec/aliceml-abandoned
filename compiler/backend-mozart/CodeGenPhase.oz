%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2002
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
   CodeGen(translate) at 'CodeGen'
   UrlComponent('Url$': Url) at '../../lib/system/Url'
   UnsafeComponent(functorToComponent) at '../../lib/system/UnsafeComponent'
export
   'CodeGenPhase$': CodeGenPhase
define
   fun {TrName Name}
      {Record.map Name VirtualString.toAtom}
   end

   fun {TrLabel Label}
      case Label of 'NUM'(I) then I
      [] 'ALPHA'(S) then
	 case {VirtualString.toAtom S} of 'true' then true
	 [] 'false' then false
	 [] '::' then '|'
	 elseof A then A
	 end
      end
   end

   fun {TrId 'Id'(Info Stamp Name)}
      'Id'(Info Stamp {TrName Name})
   end

   fun {TrIdDef IdDef}
      case IdDef of 'IdDef'(Id) then 'IdDef'({TrId Id})
      [] 'Wildcard' then 'Wildcard'
      end
   end

   fun {TrIdRef IdRef}
      case IdRef of 'IdRef'(Id) then 'IdRef'({TrId Id})
      [] 'LastIdRef'(Id) then 'LastIdRef'({TrId Id})
      [] 'Lit'(_) then IdRef
      [] 'Prim'(String) then 'Prim'({VirtualString.toAtom String})
      [] 'Value'(_ _) then IdRef
      end
   end

   fun {TrFunFlag FunFlag}
      case FunFlag of 'PrintName'(String) then
	 'PrintName'({VirtualString.toAtom String})
      else FunFlag
      end
   end

   fun {TrArgs Args TrX}
      case Args of 'OneArg'(X) then 'OneArg'({TrX X})
      [] 'TupArgs'(Xs) then 'TupArgs'({Record.map Xs TrX})
      [] 'ProdArgs'(LabelXVec) then
	 'ProdArgs'({Record.map LabelXVec
		     fun {$ Label#X} {TrLabel Label}#{TrX X} end})
      end
   end

   fun {TrProd Prod}
      case Prod of 'Tuple'(N) then 'Tuple'(N)
      [] 'Product'(Labels) then 'Product'({Record.map Labels TrLabel})
      end
   end

   proc {TrStm Stm Hd Tl ShareDict}
      case Stm of 'LastUse'(Info Ids) then
	 Hd = 'LastUse'(Info {Record.map Ids TrId})|Tl
      [] 'ValDec'(Info IdDef Exp) then
	 Hd = 'ValDec'(Info {TrIdDef IdDef} {TrExp Exp ShareDict})|Tl
      [] 'RecDec'(Info IdDefExpVec) then
	 Hd = 'RecDec'(Info {Record.map IdDefExpVec
			     fun {$ IdDef#Exp}
				{TrIdDef IdDef}#{TrExp Exp ShareDict}
			     end})|Tl
      [] 'RefDec'(Info IdDef IdRef) then
	 Hd = 'RefDec'(Info {TrIdDef IdDef} {TrIdRef IdRef})|Tl
      [] 'TupDec'(Info IdDefs IdRef) then
	 Hd = 'TupDec'(Info {Record.map IdDefs TrIdDef} {TrIdRef IdRef})|Tl
      [] 'ProdDec'(Info LabelIdDefVec IdRef) then
	 Hd = 'ProdDec'(Info
			{Record.map LabelIdDefVec
			 fun {$ Label#IdDef}
			    {TrLabel Label}#{TrIdDef IdDef}
			 end} {TrIdRef IdRef})|Tl
      [] 'RaiseStm'(Info IdRef) then
	 Hd = 'RaiseStm'(Info {TrIdRef IdRef})|Tl
      [] 'ReraiseStm'(Info IdRef) then
	 Hd = 'ReraiseStm'(Info {TrIdRef IdRef})|Tl
      [] 'TryStm'(Info TryBody IdDef1 IdDef2 HandleBody) then
	 Hd = 'TryStm'(Info {TrBody TryBody $ nil ShareDict}
		       {TrIdDef IdDef1} {TrIdDef IdDef2}
		       {TrBody HandleBody $ nil ShareDict})|Tl
      [] 'EndTryStm'(Info Body) then
	 Hd = 'EndTryStm'(Info {TrBody Body $ nil ShareDict})|Tl
      [] 'EndHandleStm'(Info Body) then
	 Hd = 'EndHandleStm'(Info {TrBody Body $ nil ShareDict})|Tl
      [] 'TestStm'(Info IdRef Tests Body) then
	 Hd = 'TestStm'(Info {TrIdRef IdRef}
			case Tests of 'LitTests'(LitBodyVec) then
			   'LitTests'({Record.map LitBodyVec
				       fun {$ Lit#Body}
					  Lit#{TrBody Body $ nil ShareDict}
				       end})
			[] 'TagTests'(Labels TagBodyVec) then
			   'TagTests'({Record.map Labels TrLabel}
				      {Record.map TagBodyVec
				       fun {$ N#Args#Body}
					  N#{TrArgs Args TrIdDef}#
					  {TrBody Body $ nil ShareDict}
				       end})
			[] 'ConTests'(ConBodyVec) then
			   'ConTests'({Record.map ConBodyVec
				       fun {$ IdRef#Args#Body}
					  {TrIdRef IdRef}#
					  {TrArgs Args TrIdDef}#
					  {TrBody Body $ nil ShareDict}
				       end})
			[] 'VecTests'(VecBodyVec) then
			   'VecTests'({Record.map VecBodyVec
				       fun {$ IdDefs#Body}
					  {Record.map IdDefs TrIdDef}#
					  {TrBody Body $ nil ShareDict}
				       end})
			end
		      {TrBody Body $ nil ShareDict})|Tl
      [] 'SharedStm'(Info Body Stamp) then
	 case {Dictionary.condGet ShareDict Stamp unit} of unit then NewStm in
	    {Dictionary.put ShareDict Stamp NewStm}
	    NewStm = 'SharedStm'(Info {TrBody Body $ nil ShareDict}
				 Stamp)
	    Hd = NewStm|Tl
	 elseof Stm then
	    Hd = Stm|Tl
	 end
      [] 'ReturnStm'(Info Exp) then
	 Hd = 'ReturnStm'(Info {TrExp Exp ShareDict})|Tl
      [] 'IndirectStm'(_ BodyOptRef) then
	 case {Access BodyOptRef} of 'SOME'(Body) then
	    {TrBody Body Hd Tl ShareDict}
	 end
      [] 'ExportStm'(Info Exp) then
	 Hd = 'ExportStm'(Info {TrExp Exp ShareDict})|Tl
      end
   end

   fun {TrExp Exp ShareDict}
      case Exp of 'NewExp'(Info Name) then 'NewExp'(Info {TrName Name})
      [] 'VarExp'(Info IdRef) then 'VarExp'(Info {TrIdRef IdRef})
      [] 'TagExp'(Info Labels N Args) then
	 'TagExp'(Info {Record.map Labels TrLabel} N {TrArgs Args TrIdRef})
      [] 'ConExp'(Info IdRef Args) then
	 'ConExp'(Info {TrIdRef IdRef} {TrArgs Args TrIdRef})
      [] 'RefExp'(Info IdRef) then 'RefExp'(Info {TrIdRef IdRef})
      [] 'TupExp'(Info IdRefs) then 'TupExp'(Info {Record.map IdRefs TrIdRef})
      [] 'ProdExp'(Info LabelIdRefVec) then
	 'ProdExp'(Info
		   {Record.map LabelIdRefVec
		    fun {$ Label#IdRef} {TrLabel Label}#{TrIdRef IdRef} end})
      [] 'PolyProdExp'(Info LabelIdRefVec) then
	 'PolyProdExp'(Info
		       {Record.map LabelIdRefVec
			fun {$ Label#IdRef}
			   {TrLabel Label}#{TrIdRef IdRef}
			end})
      [] 'VecExp'(Info IdRefs) then 'VecExp'(Info {Record.map IdRefs TrIdRef})
      [] 'FunExp'(Info Stamp Flags Args Body) then
	 'FunExp'(Info Stamp {Map Flags TrFunFlag}
		  {TrArgs Args TrIdDef} {TrBody Body $ nil ShareDict})
      [] 'PrimAppExp'(Info String IdRefs) then
	 'PrimAppExp'(Info {VirtualString.toAtom String}
		      {Record.map IdRefs TrIdRef})
      [] 'VarAppExp'(Info IdRef Args) then
	 'VarAppExp'(Info {TrIdRef IdRef} {TrArgs Args TrIdRef})
      [] 'DirectAppExp'(Info IdRef Args) then
	 'DirectAppExp'(Info {TrIdRef IdRef} {TrArgs Args TrIdRef})
      [] 'SelExp'(Info Prod Label N IdRef) then
	 'SelExp'(Info {TrProd Prod} {TrLabel Label} N {TrIdRef IdRef})
      [] 'LazyPolySelExp'(Info Label IdRef) then
	 'LazyPolySelExp'(Info {TrLabel Label} {TrIdRef IdRef})
      [] 'FunAppExp'(Info IdRef Stamp Args) then
	 'FunAppExp'(Info {TrIdRef IdRef} Stamp {TrArgs Args TrIdRef})
      [] 'FailExp'(Info) then 'FailExp'(Info)
      end
   end

   proc {TrBody Stms Hd Tl ShareDict}
      {FoldL Stms
       proc {$ Hd Stm Tl}
	  {TrStm Stm Hd Tl ShareDict}
       end Hd Tl}
   end

   fun {TrComponent '#'(imports: Imports body: Body
			exports: Exports sign: Sign)}
      {Record.map Imports
       fun {$ Id#Sign#U#B}
	  {TrId Id}#Sign#{VirtualString.toAtom {Url.toString U}}#B
       end}#{TrBody Body $ nil {NewDictionary}}#
      {Record.map Exports
       fun {$ Label#Id} {TrLabel Label}#{TrId Id} end}#Sign
   end

   fun {Translate Desc Env Component} InFilename Env2 in
      Env2 = {Dictionary.clone Env}
      InFilename = case Desc of 'SOME'(U) then {Url.toString U}
		   [] 'NONE' then ''
		   end
      case {TrComponent Component} of ComponentTr=_#_#Exports#_ then
	 case {CodeGen.translate InFilename ComponentTr
	       {Dictionary.entries Env2}}
	 of F#VS then Env2#(F#VS#Exports#Env2)
	 end
      end
   end

   fun {DumpTarget _#VS#_#_}
      {ByteString.make VS}
   end

   fun {ToComponent F#_#_#_}
      {UnsafeComponent.functorToComponent F}
   end

   fun {Eval _#_#Exports#Env M}
      {Wait M}
      {Record.forAll Exports
       proc {$ Label#'Id'(_ Stamp _)}
	  {Dictionary.put Env Stamp M.Label}
       end}
      unit
   end

   C =
   'CodeGenPhase.C'(empty: {Dictionary.new})

   CodeGenPhase =
   'CodeGenPhase'('C$': C
		  translate: Translate
		  dumpTarget: DumpTarget
		  eval: Eval
		  component: ToComponent)
end
