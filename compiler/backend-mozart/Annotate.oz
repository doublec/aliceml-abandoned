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

   fun {AddReg ValRep=Reg#V State}
      case Reg of unit then {NewReg State}#V
      else ValRep
      end
   end

   fun {ValRepToReg Reg#_}
      Reg
   end

   fun {ValRepToValue ValRep}
      case ValRep of _#V andthen {IsDet V} then V
      [] _#_ then top
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
      [] longId(Info LongId id(_ _ exId(PrintName))) then V in
	 V = {ValRepToValue {LongIdValRep LongId State}}
	 ValRep = if {Label V} == record andthen {HasFeature V PrintName} then
		     V.PrintName
		  else unit#top
		  end
	 {SetValRep Info ValRep}
      [] longId(_ _ id(_ _ inId)) then
	 raise notImplemented(longIdValRep LongId) end   %--**
      end
   end

   fun {CombineValReps Reg1#V1 _#V2}
      Reg1#case V1#V2 of top#_ then V2
	   [] conval(N ValRep1)#conval(N ValRep2) then
	      conval(N {CombineValReps ValRep1 ValRep2})
	   [] record(...)#record(...) andthen {Arity V1} == {Arity V2} then
	      {Record.zip V1 V2 CombineValReps}
	   else V1
	   end
   end

   proc {Bind Reg1#V1 ValRep2=_#V2 State}
      %% arguments: ValRep of arbiter, ValRep of pattern
      {ForAll {GetRegStamps State Reg1}
       proc {$ Stamp}
	  {EnterValRep State Stamp
	   {CombineValReps {LookupValRep State Stamp} ValRep2}}
       end}
      case V1#V2 of conval(N ValRep1)#conval(N ValRep2) then
	 {Bind ValRep1 ValRep2 State}
      [] record(...)#record(...) andthen {Arity V1} == {Arity V2} then
	 {Record.forAllInd V1
	  proc {$ Feature ValRep1}
	     {Bind ValRep1 V2.Feature State}
	  end}
      else skip
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
	    {SetValRep Info {AddReg ValRep State}}
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
      {SetValRep {Intermediate.infoOf Exp} ValRep}
      case Exp of litExp(_ Lit) then
	 ValRep = unit#{LitToValue Lit}
      [] varExp(_ LongId) then
	 ValRep = {LongIdValRep LongId State}
      [] conExp(_ LongId OptExp) then
	 case OptExp of none then
	    ValRep = {LongIdValRep LongId State}
	 [] some(Exp) then ExpValRep V in
	    ExpValRep = {AddReg {AnnotateExp Exp State} State}
	    V = case {ValRepToValue {LongIdValRep LongId State}} of con(N) then
		   conval(N ExpValRep)
		else top
		end
	    ValRep = unit#V
	 end
      [] tupExp(_ Exps) then VRs in
	 VRs = {Map Exps
		fun {$ Exp} {AddReg {AnnotateExp Exp State} State} end}
	 ValRep = unit#{List.toTuple record VRs}
      [] recExp(_ FieldExps) then VRPairs in
	 VRPairs = {Map FieldExps
		    fun {$ field(_ lab(_ S) Exp)}
		       {Intermediate.labToFeature S}#
		       {AddReg {AnnotateExp Exp State} State}
		    end}
	 ValRep = unit#{List.toRecord record VRPairs}
      [] selExp(_ lab(_ S)) then
	 ValRep = unit#selector({Intermediate.labToFeature S})
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
	 ValRep = unit#fn({CompilerSupport.newPredicateRef} unit)
      [] appExp(_ Exp1 Exp2) then ValRep1 ValRep2 in
	 ValRep1 = {AnnotateExp Exp1 State}
	 ValRep2 = {AnnotateExp Exp2 State}
	 ValRep = case {ValRepToValue ValRep1} of con(N) then
		     unit#conval(N {AddReg ValRep2 State})
		  [] selector(Feature) then V2 in
		     V2 = {ValRepToValue ValRep2}
		     if {Label V2} == record andthen {HasFeature V2 Feature}
		     then V2.Feature
		     else unit#top
		     end
		  [] builtin(_) then
		     unit#top   %--** partially evaluate application
		  else unit#top
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
	 ValRep = unit#V
      [] andExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = unit#top
      [] orExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = unit#top
      [] ifExp(_ Exp1 Exp2 Exp3) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 {Save State}
	 _ = {AnnotateExp Exp3 State}
	 {Restore State}
	 ValRep = unit#top
      [] whileExp(_ Exp1 Exp2) then
	 _ = {AnnotateExp Exp1 State}
	 {Save State}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = unit#record()
      [] seqExp(_ Exps) then
	 ValRep = {FoldL Exps
		   fun {$ _ Exp} {AnnotateExp Exp State} end unit#top}
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
	    ValRep = unit#top
	 end
	 _ = {LongIdValRep LongId State}
      [] raiseExp(_ Exp) then
	 _ = {AnnotateExp Exp State}
	 ValRep = unit#top
      [] handleExp(_ Exp1 id(Info Stamp _) Exp2) then Reg ValRep1 in
	 {Save State}
	 _ = {AnnotateExp Exp1 State}
	 {Restore State}
	 {Save State}
	 Reg = {NewReg State}
	 ValRep1 = Reg#top
	 {EnterValRep State Stamp ValRep1}
	 {EnterRegStamp State Reg Stamp}
	 {SetValRep Info ValRep1}
	 _ = {AnnotateExp Exp2 State}
	 {Restore State}
	 ValRep = unit#top
      [] letExp(_ Decs Exp) then
	 {ForAll Decs proc {$ Dec} {AnnotateDec Dec State} end}
	 ValRep = {AnnotateExp Exp State}
      end
   end

   fun {AnnotatePat Pat State ValRep}
      {SetValRep {Intermediate.infoOf Pat} ValRep}
      {ValRepToReg ValRep}#
      case Pat of wildPat(_) then
	 top
      [] litPat(_ Lit) then
	 {LitToValue Lit}
      [] varPat(_ id(_ Stamp _)) then
	 {EnterValRep State Stamp {AddReg ValRep State}}
	 {EnterRegStamp State {ValRepToReg ValRep} Stamp}
	 top
      [] conPat(_ LongId OptPat) then ValRep1 in
	 ValRep1 = {LongIdValRep LongId State}
	 case OptPat of some(Pat) then V in
	    V = case {ValRepToValue ValRep1}#{ValRepToValue ValRep}
		of con(N)#conval(!N SubValRep) then
		   {AnnotatePat Pat State SubValRep}
		else
		   {AnnotatePat Pat State unit#top}
		end
	    case {ValRepToValue ValRep1} of con(N) then
	       conval(N {AddReg V State})
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
	      fun {$ I Pat} {AddReg {AnnotatePat Pat State V.I} State} end}}
	 else
	    {List.toTuple record
	     {Map Pats
	      fun {$ Pat} {AddReg {AnnotatePat Pat State unit#top} State} end}}
	 end
      [] recPat(_ FieldPats HasDots) then PatArity V in
	 PatArity = {Arity {List.toRecord x
			    {Map FieldPats
			     fun {$ field(_ lab(_ S) _)}
				{Intermediate.labToFeature S}#unit
			     end}}}
	 V = {ValRepToValue ValRep}
	 if HasDots then
	    %--** value propagation can be improved if we handle
	    %--** partially known record values
	    if {Label V} == record andthen {List.sub PatArity {Arity V}} then
	       {ForAll FieldPats
		proc {$ field(_ lab(_ S) Pat)}
		   _ = {AnnotatePat Pat State V.{Intermediate.labToFeature S}}
		end}
	    else
	       {ForAll FieldPats
		proc {$ field(_ _ Pat)}
		   _ = {AnnotatePat Pat State unit#top}
		end}
	    end
	    top
	 elseif {Label V} == record andthen {Arity V} == PatArity then
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
		 {Intermediate.labToFeature S}#{AnnotatePat Pat State unit#top}
	      end}}
	 end
      [] asPat(_ id(_ Stamp _) Pat) then ValRep1 ValRep2 in
	 ValRep1 = _#_
	 {EnterValRep State Stamp ValRep1}
	 ValRep2 = {AnnotatePat Pat State ValRep}
	 ValRep1 = {AddReg {CombineValReps ValRep ValRep2} State}
	 {ValRepToValue ValRep2}
      [] altPat(_ Pats) then
	 case Pats of [Pat] then {AnnotatePat Pat State ValRep}
	 else
	    {ForAll Pats
	     proc {$ Pat}
		{Save State}
		_ = {AnnotatePat Pat State ValRep}
		{Restore State}
	     end}
	    top
	 end
      [] negPat(_ Pat) then
	 {Save State}
	 _ = {AnnotatePat Pat State ValRep}
	 {Restore State}
	 top
      [] guardPat(_ Pat Exp) then PatValRep in
	 PatValRep = {AnnotatePat Pat State ValRep}
	 _ = {AnnotateExp Exp State}
	 {ValRepToValue PatValRep}
      [] withPat(_ Pat Decs) then PatValRep in
	 PatValRep = {AnnotatePat Pat State ValRep}
	 {ForAll Decs proc {$ Dec} {AnnotateDec Dec State} end}
	 {ValRepToValue PatValRep}
      end
   end
end
