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

%%
%% <valrep> ::= <reg>#<desc>
%% <desc> ::= record(<feature>: <valrep> ... <feature>: <valrep>)
%%         |  name(<name>)
%%         |  con(<name>)
%%         |  conval(<name> <valrep>)
%%         |  word(<int>)
%%         |  int(<int>)
%%         |  char(<char>)
%%         |  string(<byteString>)
%%         |  real(<float>)
%%         |  fn(<immediate> <fast>)
%%         |  selector(<feature>)
%%         |  builtin(<atom>)
%%         |  top
%% <reg> ::= <int>
%%        |  unit
%% <immediate> ::= unit
%%              |  <procedure>
%%              |  <foreignPointer>
%% <fast> ::= unit
%%         |  <arity>#<reg>#<immediate>
%% <arity> ::= [<feature>]
%%

functor
import
   CompilerSupport(newNamedName newPredicateRef)
   at 'x-oz://boot/CompilerSupport'
   Info(setValRep: SetValRep)
   Intermediate(getPrintName infoOf labToFeature)
export
   Annotate
define
   %%
   %% Accessing the State
   %%
   %% <state> ::= state(isToplevel: <cell>(<bool>)
   %%                   counter: <cell>(<int>)
   %%                   env: <cell>([<dictionary>(<stamp> -> <valrep>)])
   %%                   regStamps: [<dictionary>(<reg> -> [<stamp>])])
   %%

   fun {NewState}
      state(isToplevel: {NewCell true}
	    counter: {NewCell 0}
	    env: {NewCell [{NewDictionary}]}
	    regStamps: {NewCell [{NewDictionary}]})
   end

   fun {IsToplevel State}
      {Access State.isToplevel}
   end

   proc {SetToplevel State B}
      {Assign State.isToplevel B}
   end

   proc {NewReg State ?Reg} C in
      C = State.counter
      Reg = {Access C}
      {Assign C Reg + 1}
   end

   local
      fun {Lookup Env|Envr Stamp}
	 case {Dictionary.condGet Env Stamp unit} of unit then
	    {Lookup Envr Stamp}
	 elseof ValRep then ValRep
	 end
      end
   in
      fun {LookupValRep State Stamp}
	 {Lookup {Access State.env} Stamp}
      end
   end

   proc {EnterValRep State Stamp ValRep}
      case {Access State.env} of Env|_ then
	 {Dictionary.put Env Stamp ValRep}
      end
   end

   proc {EnterRegStamp State Reg Stamp}
      case {Access State.regStamps} of D|_ then
	 {Dictionary.put D Reg Stamp|{Dictionary.condGet D Reg nil}}
      end
   end

   fun {GetRegStamps State Reg}
      case {Access State.regStamps} of D|_ then
	 {Dictionary.condGet D Reg nil}
      end
   end

   proc {Save State}
      {Assign State.env {NewDictionary}|{Access State.env}}
      case {Access State.regStamps} of Ds=D|_ then
	 {Assign State.regStamps {Dictionary.clone D}|Ds}
      end
   end

   proc {Restore State}
      case {Access State.env} of _|Envr then
	 {Assign State.env Envr}
      end
      case {Access State.regStamps} of _|Dr then
	 {Assign State.regStamps Dr}
      end
   end

   %%
   %% Auxiliary Functions on Value Representations
   %%

   fun {ValRepToReg Reg#_}
      Reg
   end

   fun {ValRepToValue ValRep}
      case ValRep of _#V andthen {IsDet V} then V
      [] _#_ then unit
      end
   end

   fun {LitToValue Lit}
      case Lit of wordLit(W) then word(W)
      [] intLit(I) then int(I)
      [] charLit(C) then char(C)
      [] stringLit(S) then string({ByteString.make S})
      [] realLit(S) then real({String.toFloat S})
      end
   end

   proc {LongIdValRep LongId State ?ValRep}
      case LongId of shortId(Info id(_ Stamp _)) then
	 ValRep = {LookupValRep State Stamp}
	 {SetValRep Info ValRep}
      [] longId(Info LongId id(_ _ inId(PrintName))) then V in
	 V = {ValRepToValue {LongIdValRep LongId State}}
	 ValRep = if {Label V} == record andthen {HasFeature V PrintName} then
		     V.PrintName
		  else {NewReg State}#top
		  end
	 {SetValRep Info ValRep}
      [] longId(_ _ id(_ _ exId)) then
	 raise notImplemented(longIdValRep LongId) end   %--**
      end
   end

   local
      fun {CombineValReps Reg#V1 _#V2}
	 Reg#case V1 of top then V2
	     elsecase V1 of conval(N ValRep1) then
		case V2 of conval(!N ValRep2) then
		   conval(N {CombineValReps ValRep1 ValRep2})
		else V1
		end
	     elseif {Label V1} == record andthen {Label V2} == record
		andthen {Arity V1} == {Arity V2}
	     then
		{Record.zip V1 V2 CombineValReps}
	     else V1
	     end
      end
   in
      proc {Bind Reg#V1 ValRep2=_#V2 State}
	 %% arguments: ValRep of arbiter, ValRep of pattern
	 {ForAll {GetRegStamps State Reg}
	  proc {$ Stamp}
	     {EnterValRep State Stamp
	      {CombineValReps {LookupValRep State Stamp} ValRep2}}
	  end}
	 case V1 of conval(N ValRep1) then
	    case V2 of conval(!N ValRep2) then
	       {Bind ValRep1 ValRep2 State}
	    else skip
	    end
	 elseif {Label V1} == record andthen {Label V2} == record
	    andthen {Arity V1} == {Arity V2}
	 then
	    {Record.forAllInd V1
	     proc {$ Feature ValRep1}
		{Bind ValRep1 V2.Feature State}
	     end}
	 else skip
	 end
      end
   end

   %%
   %% The Annotation Procedures
   %%

   proc {Annotate Decs} State in
      State = {NewState}
      {ForAll Decs proc {$ Dec} {AnnotateDec Dec State} end}
   end

   proc {AnnotateDec Dec State}
      case Dec of conDec(_ Id=id(Info Stamp _) HasArgs) then V ValRep Reg in
	 V = if {IsToplevel State} then N in
		N = case {Intermediate.getPrintName Id} of unit then {NewName}
		    elseof PN then {CompilerSupport.newNamedName PN}
		    end
		if HasArgs then con(N)
		else name(N)
		end
	     else top
	     end
	 Reg = {NewReg State}
	 ValRep = Reg#V
	 {SetValRep Info ValRep}
	 {EnterValRep State Stamp ValRep}
	 {EnterRegStamp State Reg Stamp}
      [] valDec(_ Ids Exp) then ValRep in
	 {ForAll Ids
	  proc {$ id(Info Stamp _)} ValRep in
	     ValRep = _#_
	     {EnterValRep State Stamp ValRep}
	     {SetValRep Info ValRep}
	  end}
	 ValRep = {AnnotateExp Exp State}
	 case Ids of [id(Info Stamp _)] then
	    {SetValRep Info ValRep}
	    {EnterRegStamp State {ValRepToReg ValRep} Stamp}
	 else V in
	    V = {ValRepToValue ValRep}
	    if {Label V} == record andthen {IsTuple V}
	       andthen {Width V} == {Length Ids}
	    then
	       {List.forAllInd Ids
		proc {$ I id(Info Stamp _)}
		   case V.I of ValRep=Reg#_ then
		      {SetValRep Info ValRep}
		      {EnterRegStamp State Reg Stamp}
		   end
		end}
	    else
	       {ForAll Ids
		proc {$ id(Info Stamp _)} Reg in
		   Reg = {NewReg State}
		   {SetValRep Info Reg#top}
		   {EnterRegStamp State Reg Stamp}
		end}
	    end
	 end
      end
   end

   proc {AnnotateExp Exp State ?ValRep}
      case Exp of litExp(_ Lit) then
	 ValRep = {NewReg State}#{LitToValue Lit}
      [] varExp(_ LongId) then
	 ValRep = {LongIdValRep LongId State}
      [] conExp(_ LongId OptExp) then
	 case OptExp of none then
	    ValRep = {LongIdValRep LongId State}
	 [] some(Exp) then ExpValRep V in
	    ExpValRep = {AnnotateExp Exp State}
	    V = case {ValRepToValue {LongIdValRep LongId State}} of con(N) then
		   conval(N ExpValRep)
		else top
		end
	    ValRep = {NewReg State}#V
	 end
      [] tupExp(_ Exps) then VRs in
	 VRs = {Map Exps fun {$ Exp} {AnnotateExp Exp State} end}
	 ValRep = {NewReg State}#{List.toTuple record VRs}
      [] recExp(_ FieldExps) then VRPairs in
	 VRPairs = {Map FieldExps
		    fun {$ field(_ lab(_ S) Exp)}
		       {Intermediate.labToFeature S}#{AnnotateExp Exp State}
		    end}
	 ValRep = {NewReg State}#{List.toRecord record VRPairs}
      [] selExp(_ lab(_ S)) then
	 ValRep = {NewReg State}#selector({Intermediate.labToFeature S})
      [] funExp(_ id(Info Stamp _) Exp) then OldIsToplevel Reg ValRep1 in
	 %--** generate `fast' function
	 {Save State}
	 OldIsToplevel = {IsToplevel State}
	 {SetToplevel State false}
	 Reg = {NewReg State}
	 ValRep1 = Reg#top
	 {EnterValRep State Stamp ValRep1}
	 {EnterRegStamp State Reg Stamp}
	 {SetValRep Info ValRep1}
	 _ = {AnnotateExp Exp State}
	 {SetToplevel State OldIsToplevel}
	 {Restore State}
	 ValRep = {NewReg State}#fn({CompilerSupport.newPredicateRef} unit)
      [] appExp(_ Exp1 Exp2) then ValRep1 ValRep2 in
	 ValRep1 = {AnnotateExp Exp1 State}
	 ValRep2 = {AnnotateExp Exp2 State}
	 ValRep = case {ValRepToValue ValRep1} of con(N) then
		     {NewReg State}#conval(N ValRep2)
		  [] selector(Feature) then V2 in
		     V2 = {ValRepToValue ValRep2}
		     if {Label V2} == record andthen {HasFeature V2 Feature}
		     then V2.Feature
		     else {NewReg State}#top
		     end
		  [] builtin(_) then
		     {NewReg State}#top   %--** partially evaluate application
		  else {NewReg State}#top
		  end
      [] adjExp(_ Exp1 Exp2) then ValRep1 ValRep2 V1 V2 V in
	 ValRep1 = {AnnotateExp Exp1 State}
	 ValRep2 = {AnnotateExp Exp2 State}
	 V1 = {ValRepToValue ValRep1}
	 V2 = {ValRepToValue ValRep2}
	 V = if {Label V1} == record andthen {Label V2} == record then
		{Adjoin V1 V2}
	     else top
	     end
	 ValRep = {NewReg State}#V
      [] andExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = {NewReg State}#top
      [] orExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = {NewReg State}#top
      [] ifExp(_ Exp1 Exp2 Exp3) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 {Save State}
	 _ = {AnnotateExp Exp3 State}
	 {Restore State}
	 ValRep = {NewReg State}#top
      [] whileExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = {NewReg State}#record()
      [] seqExp(_ Exps) then
	 ValRep = {FoldL Exps fun {$ _ Exp} {AnnotateExp Exp State} end unit}
      [] caseExp(_ Exp Matches LongId) then ValRep1 in
	 ValRep1 = {AnnotateExp Exp State}
	 case Matches of [match(_ Pat Exp)] then ValRep2 in
	    ValRep2 = {AnnotatePat Pat State ValRep1}
	    {Bind ValRep1 ValRep2 State}
	    ValRep = {AnnotateExp Exp State}
	 else
	    {ForAll Matches
	     proc {$ match(_ Pat Exp)} ValRep2 in
		{Save State}
		ValRep2 = {AnnotatePat Pat State ValRep1}
		{Bind ValRep1 ValRep2 State}
		_ = {AnnotateExp Exp State}
		{Restore State}
	     end}
	    ValRep = {NewReg State}#top
	 end
	 _ = {LongIdValRep LongId State}
      [] raiseExp(_ Exp) then
	 _ = {AnnotateExp Exp State}
	 ValRep = {NewReg State}#top
      [] handleExp(_ Exp1 id(_ Stamp _) Exp2) then Reg in
	 {Save State}
	 _ = {AnnotateExp Exp1 State}
	 {Restore State}
	 {Save State}
	 Reg = {NewReg State}
	 {EnterValRep State Stamp Reg#top}
	 {EnterRegStamp State Reg Stamp}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = {NewReg State}#top
      [] letExp(_ Decs Exp) then
	 {ForAll Decs proc {$ Dec} {AnnotateDec Dec State} end}
	 ValRep = {AnnotateExp Exp State}
      end
      {SetValRep {Intermediate.infoOf Exp} ValRep}
   end

   fun {AnnotatePat Pat State ValRep}
      {SetValRep {Intermediate.infoOf Pat} ValRep}
      {ValRepToReg ValRep}#
      case Pat of litPat(_ Lit) then
	 {LitToValue Lit}
      [] varPat(_ id(_ Stamp _)) then
	 {EnterValRep State Stamp ValRep}
	 {EnterRegStamp State {ValRepToReg ValRep} Stamp}
	 top
      [] conPat(_ LongId OptPat) then ValRep1 in
	 ValRep1 = {LongIdValRep LongId State}
	 case OptPat of some(Pat) then V in
	    V = case {ValRepToValue ValRep1} of con(N) then
		   case {ValRepToValue ValRep} of conval(!N SubValRep) then
		      {AnnotatePat Pat State SubValRep}
		   else
		      {AnnotatePat Pat State {NewReg State}#top}
		   end
		else
		   {AnnotatePat Pat State {NewReg State}#top}
		end
	    case {ValRepToValue ValRep1} of con(N) then conval(N V)
	    else top
	    end
	 [] none then
	    case {ValRepToValue ValRep1} of V=name(_) then V
	    else top
	    end
	 end
      [] tupPat(_ Pats) then V in
	 V = {ValRepToValue ValRep}
	 if {Label V} == record andthen {IsTuple V}
	    andthen {Width V} == {Length Pats}
	 then
	    {List.toTuple record
	     {List.mapInd Pats
	      fun {$ I Pat} {AnnotatePat Pat State V.I} end}}
	 else
	    {List.toTuple record
	     {Map Pats
	      fun {$ Pat} {AnnotatePat Pat State {NewReg State}#top} end}}
	 end
      [] recPat(_ FieldPats IsOpen) then PatArity V in
	 if IsOpen then
	    raise notImplemented(annotatePat Pat) end   %--**
	 end
	 PatArity = {Arity {List.toRecord x
			    {Map FieldPats
			     fun {$ field(_ lab(_ S) _)}
				{Intermediate.labToFeature S}#unit
			     end}}}
	 V = {ValRepToValue ValRep}
	 if {Label V} == record andthen {Arity V} == PatArity then
	    {List.toRecord record
	     {Map FieldPats
	      fun {$ field(_ lab(_ S) Pat)}
		 {Intermediate.labToFeature S}#
		 {AnnotatePat Pat State V.{Intermediate.labToFeature S}}
	      end}}
	 else
	    {List.toRecord record
	     {Map FieldPats
	      fun {$ field(_ lab(_ S) Pat)}
		 {Intermediate.labToFeature S}#
		 {AnnotatePat Pat State {NewReg State}#top}
	      end}}
	 end
      [] asPat(_ id(_ Stamp _) Pat) then
	 %--** annotate the value constructed by Pat at Id
	 {EnterValRep State Stamp ValRep}
	 {AnnotatePat Pat State ValRep}
      [] altPat(_ _) then
	 raise notImplemented(annotatePat Pat) end   %--**
      [] negPat(_ _) then
	 raise notImplemented(annotatePat Pat) end   %--**
      [] guardPat(_ Pat Exp) then PatValRep in
	 PatValRep = {AnnotatePat Pat State ValRep}
	 _ = {AnnotateExp Exp State}
	 PatValRep
      [] withPat(_ _ _) then
	 raise notImplemented(annotatePat Pat) end   %--**
      end
   end
end
