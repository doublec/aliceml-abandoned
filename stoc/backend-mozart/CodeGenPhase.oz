%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
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
   UrlComponent('Url$': Url) at '../../lib/utility/Url'
export
   'CodeGenPhase$': CodeGenPhase
define
   fun {TrOption Option TrX}
      {Record.map Option TrX}
   end

   fun {TrInfo '#'(region: Region ...)}
      Region
   end

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
      'Id'({TrInfo Info} Stamp {TrName Name})
   end

   fun {TrIdDef IdDef}
      case IdDef of 'IdDef'(Id) then 'IdDef'({TrId Id})
      [] 'Wildcard' then 'Wildcard'
      end
   end

   fun {TrFunFlag FunFlag}
      case FunFlag of 'PrintName'(String) then
	 'PrintName'({VirtualString.toAtom String})
      else FunFlag
      end
   end

   fun {TrCon Con}
      case Con of 'Con'(Id) then 'Con'({TrId Id})
      [] 'StaticCon'(Stamp) then 'StaticCon'(Stamp)
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

   fun {TrConArgs ConArgs TrX}
      {TrOption ConArgs fun {$ Args} {TrArgs Args TrX} end}
   end

   fun {TrProd Prod}
      case Prod of 'Tuple'(N) then 'Tuple'(N)
      [] 'Product'(Labels) then 'Product'({Record.map Labels TrLabel})
      end
   end

   proc {TrStm Stm Hd Tl ShareDict}
      case Stm of 'ValDec'(Info IdDef Exp) then
	 Hd = 'ValDec'({TrInfo Info} {TrIdDef IdDef} {TrExp Exp ShareDict})|Tl
      [] 'RefAppDec'(Info IdDef Id) then
	 Hd = 'RefAppDec'({TrInfo Info} {TrIdDef IdDef} {TrId Id})|Tl
      [] 'TupDec'(Info IdDefs Id) then
	 Hd = 'TupDec'({TrInfo Info} {Record.map IdDefs TrIdDef} {TrId Id})|Tl
      [] 'ProdDec'(Info LabelIdDefVec Id) then
	 Hd = 'ProdDec'({TrInfo Info}
			{Record.map LabelIdDefVec
			 fun {$ Label#IdDef}
			    {TrLabel Label}#{TrIdDef IdDef}
			 end} {TrId Id})|Tl
      [] 'RaiseStm'(Info Id) then
	 Hd = 'RaiseStm'({TrInfo Info} {TrId Id})|Tl
      [] 'ReraiseStm'(Info Id) then
	 Hd = 'ReraiseStm'({TrInfo Info} {TrId Id})|Tl
      [] 'TryStm'(Info TryBody IdDef1 IdDef2 HandleBody) then
	 Hd = 'TryStm'({TrInfo Info} {TrBody TryBody $ nil ShareDict}
		       {TrIdDef IdDef1} {TrIdDef IdDef2}
		       {TrBody HandleBody $ nil ShareDict})|Tl
      [] 'EndTryStm'(Info Body) then
	 Hd = 'EndTryStm'({TrInfo Info} {TrBody Body $ nil ShareDict})|Tl
      [] 'EndHandleStm'(Info Body) then
	 Hd = 'EndHandleStm'({TrInfo Info} {TrBody Body $ nil ShareDict})|Tl
      [] 'TestStm'(Info Id Tests Body) then
	 Hd = 'TestStm'({TrInfo Info} {TrId Id}
			case Tests of 'LitTests'(LitBodyVec) then
			   'LitTests'({Record.map LitBodyVec
				       fun {$ Lit#Body}
					  Lit#{TrBody Body $ nil ShareDict}
				       end})
			[] 'TagTests'(TagBodyVec) then
			   'TagTests'({Record.map TagBodyVec
				       fun {$ Label#N#ConArgs#Body}
					  {TrLabel Label}#N#
					  {TrConArgs ConArgs TrIdDef}#
					  {TrBody Body $ nil ShareDict}
				       end})
			[] 'ConTests'(ConBodyVec) then
			   'ConTests'({Record.map ConBodyVec
				       fun {$ Con#ConArgs#Body}
					  {TrCon Con}#
					  {TrConArgs ConArgs TrIdDef}#
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
	    NewStm = 'SharedStm'({TrInfo Info} {TrBody Body $ nil ShareDict}
				 Stamp)
	    Hd = NewStm|Tl
	 elseof Stm then
	    Hd = Stm|Tl
	 end
      [] 'ReturnStm'(Info Exp) then
	 Hd = 'ReturnStm'({TrInfo Info} {TrExp Exp ShareDict})|Tl
      [] 'IndirectStm'(_ BodyOptRef) then
	 case {Access BodyOptRef} of 'SOME'(Body) then
	    {TrBody Body Hd Tl ShareDict}
	 end
      [] 'ExportStm'(Info Exp) then
	 Hd = 'ExportStm'({TrInfo Info} {TrExp Exp ShareDict})|Tl
      end
   end

   fun {TrExp Exp ShareDict}
      case Exp of 'LitExp'(Info Lit) then 'LitExp'({TrInfo Info} Lit)
      [] 'PrimExp'(Info String) then
	 'PrimExp'({TrInfo Info} {VirtualString.toAtom String})
      [] 'NewExp'(Info) then 'NewExp'({TrInfo Info})
      [] 'VarExp'(Info Id) then 'VarExp'({TrInfo Info} {TrId Id})
      [] 'TagExp'(Info Label N) then 'TagExp'({TrInfo Info} {TrLabel Label} N)
      [] 'ConExp'(Info Con) then 'ConExp'({TrInfo Info} {TrCon Con})
      [] 'TupExp'(Info Ids) then 'TupExp'({TrInfo Info} {Record.map Ids TrId})
      [] 'ProdExp'(Info LabelIdVec) then
	 'ProdExp'({TrInfo Info}
		   {Record.map LabelIdVec
		    fun {$ Label#Id} {TrLabel Label}#{TrId Id} end})
      [] 'VecExp'(Info Ids) then 'VecExp'({TrInfo Info} {Record.map Ids TrId})
      [] 'FunExp'(Info Stamp Flags Args Body) then
	 'FunExp'({TrInfo Info} Stamp {Map Flags TrFunFlag}
		  {TrArgs Args TrIdDef} {TrBody Body $ nil ShareDict})
      [] 'PrimAppExp'(Info String Ids) then
	 'PrimAppExp'({TrInfo Info} {VirtualString.toAtom String}
		      {Record.map Ids TrId})
      [] 'VarAppExp'(Info Id Args) then
	 'VarAppExp'({TrInfo Info} {TrId Id} {TrArgs Args TrId})
      [] 'TagAppExp'(Info Label N Args) then
	 'TagAppExp'({TrInfo Info} {TrLabel Label} N {TrArgs Args TrId})
      [] 'ConAppExp'(Info Con Args) then
	 'ConAppExp'({TrInfo Info} {TrCon Con} {TrArgs Args TrId})
      [] 'RefAppExp'(Info Id) then 'RefAppExp'({TrInfo Info} {TrId Id})
      [] 'SelAppExp'(Info Prod Label N Id) then
	 'SelAppExp'({TrInfo Info} {TrProd Prod} {TrLabel Label} N {TrId Id})
      [] 'FunAppExp'(Info Id Stamp Args) then
	 'FunAppExp'({TrInfo Info} {TrId Id} Stamp {TrArgs Args TrId})
      end
   end

   proc {TrBody Stms Hd Tl ShareDict}
      {FoldL Stms
       proc {$ Hd Stm Tl}
	  {TrStm Stm Hd Tl ShareDict}
       end Hd Tl}
   end

   fun {TrComponent Import#Body#ExportDesc#Sign}
      {Record.map Import
       fun {$ IdDef#Sign#U}
	  {TrIdDef IdDef}#Sign#{VirtualString.toAtom {Url.toString U}}
       end}#{TrBody Body $ nil {NewDictionary}}#
      {Record.map ExportDesc fun {$ Label#Id} {TrLabel Label}#{TrId Id} end}#
      Sign
   end

   fun {Translate Env Desc Component} InFilename in
      InFilename = case Desc of 'SOME'(U) then {Url.toString U}
		   [] 'NONE' then ''
		   end
      case {TrComponent Component} of ComponentTr=_#_#ExportDesc#_ then
	 case {CodeGen.translate InFilename ComponentTr
	       {Dictionary.entries Env}}
	 of F#VS then F#VS#ExportDesc#Env
	 end
      end
   end

   fun {Sign F#_#_#_}
      case F.'export' of sig(Sign) then Sign end
   end

   proc {WriteFile VS File} F in
      F = {New Open.file init(name: File flags: [write create truncate])}
      {F write(vs: VS)}
      {F close()}
   end

   fun {Save F#VS#_#_ OutFilename OutputAssembly}
      {Pickle.saveWithCells F OutFilename '' 9}
      if OutputAssembly then
	 {WriteFile VS OutFilename#'.ozm'}
      end
      unit
   end

   fun {Apply F#_#ExportDesc#Env} M in
      {{Property.get 'alice.modulemanager'} apply(F ?M)}
      {Wait M}
      {Record.forAll ExportDesc
       proc {$ Label#'Id'(_ Stamp _)}
	  {Dictionary.put Env Stamp M.Label}
       end}
      unit
   end

   C =
   'CodeGenPhase.C'('$t': {Value.byNeedFail rttNotImplemented}
		    new: fun {$ unit} {Dictionary.new} end
		    clone: Dictionary.clone)

   CodeGenPhase =
   'CodeGenPhase'('C$': C
		  '$t': {Value.byNeedFail rttNotImplemented}
		  '$value': {Value.byNeedFail rttNotImplemented}
		  translate: Translate
		  sign: Sign
		  save: Save
		  apply: Apply)
end
