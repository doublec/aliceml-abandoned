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
%% <test seq> ::= [<pattern test>]
%% <pattern test> ::= <pos>#<test>
%%                 |  neg(<test list>)
%%                 |  alt([<test list>])
%%
%% <pos> ::= [<feature>]   % special name reserved for constructor selection
%%
%% <test> ::= litTest(<desc>)
%%         |  basicNameTest(<name>)
%%         |  nonbasicNameTest(<long id>)
%%         |  conAccess
%%         |  basicConTest(<name>)
%%         |  nonbasicConTest(<long id>)
%%         |  recAccess(<arity> <has dots>)
%%         |  recTest(<arity>)
%%         |  labelTest(<feature>)
%%         |  guardTest(<exp>)
%%         |  decs([<dec>])
%%
%% <arity> ::= <int>
%%          |  [<feature>]   % must be sorted
%%
%% <tree> ::= node(<pos> <test> <tree> <tree> <count> <shared>)
%%         |  leaf(<exp> <count> <shared>)
%%         |  default
%% <count> ::= <cell>(<int>)
%% <shared> ::= ...
%%

functor
import
   Annotate(valRepToValue: ValRepToValue longIdValRep: LongIdValRep)
   Intermediate(litToValue)
export
   BuildTree
define
   ConSel = {NewName}

   proc {MakeTestSeq Pat Pos ValRep State Hd Tl}
      case Pat of wildPat(_) then
	 Hd = Tl
      [] litPat(_ Lit) then
	 Hd = Pos#litTest({Intermediate.litToValue Lit})|Tl
      [] varPat(_ _) then
	 Hd = Tl
      [] conPat(_ LongId OptPat) then ValRep1 in
	 ValRep1 = {LongIdValRep LongId State}
	 case OptPat of some(Pat) then Inter SubValRep in
	    case {ValRepToValue ValRep1} of con(N) then
	       case {ValRepToValue ValRep} of conval(!N ValRep) then
		  Hd = Pos#conAccess|Inter
		  SubValRep = ValRep
	       else
		  Hd = Pos#basicConTest(N)|Inter
		  SubValRep = unit#top
	       end
	    else
	       Hd = Pos#nonbasicConTest(LongId)|Inter
	       SubValRep = unit#top
	    end
	    {MakeTestSeq Pat ConSel|Pos SubValRep State Inter Tl}
	 [] none then
	    case {ValRepToValue ValRep1} of name(N) then
	       Hd = Pos#basicNameTest(N)|Tl
	    else
	       Hd = Pos#nonbasicNameTest(LongId)|Tl
	    end
	 end
      [] tupPat(_ Pats) then V in
	 V = {ValRepToValue ValRep}
	 if {Label V} == record andthen {IsTuple V}
	    andthen {Width V} == {Length Pats}
	 then Inter in
	    Hd = Pos#recAccess({Width V} false)|Inter
	    {List.foldLInd Pats
	     proc {$ I Hd Pat Tl}
		{MakeTestSeq Pat I|Pos V.I State Hd Tl}
	     end Inter Tl}
	 else Inter Unknown in
	    Hd = Pos#recTest({Length Pats})|Inter
	    Unknown = unit#top
	    {List.foldLInd Pats
	     proc {$ I Hd Pat Tl}
		{MakeTestSeq Pat I|Pos Unknown State Hd Tl}
	     end Inter Tl}
	 end
      [] recPat(_ FieldPats HasDots) then Rec PatArity V Inter WithVal in
	 Rec = {List.toRecord x
		{Map FieldPats
		 fun {$ field(_ lab(_ Feature) _)}
		    Feature#unit
		 end}}
	 PatArity = {Arity Rec}
	 V = {ValRepToValue ValRep}
	 if HasDots then
	    case V of record(...) andthen {List.sub PatArity {Arity V}} then
	       Hd = Pos#recAccess({Width V} true)|Inter
	       WithVal = true
	    else
	       {FoldL PatArity
		proc {$ Hd Feature Tl}
		   Hd = Pos#labelTest(Feature)|Tl
		end Hd Inter}
	       WithVal = false
	    end
	 elsecase V of record(...) andthen {Arity V} == PatArity then
	    Hd = Pos#recAccess({Width V} false)|Inter
	    WithVal = true
	 else
	    Hd = Pos#recTest(if {IsTuple Rec} then {Width Rec}
			     else PatArity
			     end)|Inter
	    WithVal = false
	 end
	 if WithVal then
	    {FoldL FieldPats
	     proc {$ Hd field(_ lab(_ Feature) Pat) Tl}
		{MakeTestSeq Pat Feature|Pos V.Feature State Hd Tl}
	     end Inter Tl}
	 else Unknown in
	    Unknown = unit#top
	    {FoldL FieldPats
	     proc {$ Hd field(_ lab(_ Feature) Pat) Tl}
		{MakeTestSeq Pat Feature|Pos Unknown State Hd Tl}
	     end Inter Tl}
	 end
      [] asPat(_ Pat1 Pat2) then Inter in
	 {MakeTestSeq Pat1 Pos ValRep State Hd Inter}
	 {MakeTestSeq Pat2 Pos ValRep State Inter Tl}
      [] altPat(_ Pats) then
	 Hd = alt({Map Pats
		   fun {$ Pat}
		      {MakeTestSeq Pat Pos ValRep State $ nil}
		   end})|Tl
      [] negPat(_ Pat) then
	 Hd = neg({MakeTestSeq Pat Pos ValRep State $ nil})|Tl
      [] guardPat(_ Pat Exp) then Inter in
	 {MakeTestSeq Pat Pos ValRep State Hd Inter}
	 Inter = Pos#guardTest(Exp)|Tl
      [] withPat(_ Pat Decs) then Inter in
	 {MakeTestSeq Pat Pos ValRep State Hd Inter}
	 Inter = Pos#decs(Decs)|Tl
      end
   end

   fun {TestEq Test1 Test2}
      case Test1 of guardTest(_) then false
      [] decs(_) then false
      else Test1 == Test2
      end
   end

   fun {AreParallelTests Test1 Test2}
      case Test1#Test2 of litTest(L1)#litTest(L2) then L1 \= L2
      [] litTest(_)#recTest(_) then true
      [] recTest(_)#litTest(_) then true
      [] recTest(A1)#recTest(A2) then A1 \= A2
      else false
      end
   end

   local
      fun {FindTest Tree Pos0 Test0 ?NewTree ?Hole ?RestTree}
	 case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	    if Pos0 \= Pos then false
	    elseif {TestEq Test Test0} then
	       NewTree = node(Pos Test Hole ElseTree Count Shared)
	       RestTree = ThenTree
	       true
	    elseif {AreParallelTests Test0 Test} then NewElseTree in
	       if {FindTest ElseTree Pos0 Test0 ?NewElseTree ?Hole ?RestTree}
	       then
		  NewTree = node(Pos Test ThenTree NewElseTree Count Shared)
		  true
	       else false
	       end
	    else false
	    end
	 else false
	 end
      end
   in
      proc {MergeIntoTree TestSeq ThenTree ElseTree ?NewTree}
	 case TestSeq of nil then
	    %% ElseTree is unreachable
	    NewTree = ThenTree
	 [] neg(TestSeq)|TestSeqRest then NewThenTree in
	    {MergeIntoTree TestSeqRest ThenTree ElseTree ?NewThenTree}
	    {MergeIntoTree TestSeq ElseTree NewThenTree ?NewTree}
	 [] alt(TestSeqs)|TestSeqRest then NewThenTree in
	    {MergeIntoTree TestSeqRest ThenTree ElseTree ?NewThenTree}
	    NewTree = {FoldR TestSeqs
		       fun {$ TestSeq ElseTree}
			  {MergeIntoTree TestSeq NewThenTree ElseTree}
		       end ElseTree}
	 [] Pos#Test|TestSeqRest then Hole RestTree in
	    if {FindTest ElseTree Pos Test ?NewTree ?Hole ?RestTree} then
	       {MergeIntoTree TestSeqRest ThenTree RestTree ?Hole}
	    else NewThenTree in
	       NewThenTree = {MergeIntoTree TestSeqRest ThenTree default}
	       NewTree = node(Pos Test NewThenTree ElseTree {NewCell 0} _)
	    end
	 end
      end
   end

   fun {PropagateElses Tree DefaultTree}
      case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	 NewElseTree = {PropagateElses ElseTree DefaultTree}
	 NewThenTree = {PropagateElses ThenTree NewElseTree}
      in
	 node(Pos Test NewThenTree NewElseTree Count Shared)
      [] leaf(_ _ _) then
	 Tree
      [] default then
	 DefaultTree
      end
   end

   local
      fun {TestMember Pos0 Test0 TestList}
	 case TestList of Pos#Test|Rest then
	    Pos0 == Pos andthen {TestEq Test0 Test}
	    orelse {TestMember Pos0 Test0 Rest}
	 [] nil then false
	 end
      end

      fun {Disentailed Pos0 Test0 TestList}
	 case TestList of Pos#Test|Rest then
	    Pos0 == Pos andthen {AreParallelTests Test0 Test}
	    orelse {Disentailed Pos0 Test0 Rest}
	 [] nil then false
	 end
      end
   in
      fun {OptimizeTree Tree TrueTests FalseTests}
	 case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	    if {TestMember Pos Test TrueTests} then
	       {OptimizeTree ThenTree TrueTests FalseTests}
	    elseif {TestMember Pos Test FalseTests}
	       orelse {Disentailed Pos Test TrueTests}
	    then
	       {OptimizeTree ElseTree TrueTests FalseTests}
	    else N in
	       N = {Access Count}
	       {Assign Count N + 1}
	       if N == 0 then
		  node(Pos Test
		       {OptimizeTree ThenTree Pos#Test|TrueTests FalseTests}
		       {OptimizeTree ElseTree TrueTests Pos#Test|FalseTests}
		       Count Shared)
	       else
		  Tree
	       end
	    end
	 [] leaf(_ Count _) then
	    {Assign Count {Access Count} + 1}
	    Tree
	 end
      end
   end

   fun {MakeLeaf Exp}
      leaf(Exp {NewCell 0} _)
   end

   fun {BuildTree Matches ValRep ElseExp State}
      {OptimizeTree
       {PropagateElses
	{FoldR Matches
	 fun {$ match(_ Pat ThenExp) ElseTree} TestSeq in
	    {MakeTestSeq Pat nil ValRep State ?TestSeq nil}
	    {MergeIntoTree TestSeq {MakeLeaf ThenExp} ElseTree}
	 end default}
	{MakeLeaf ElseExp}} nil nil}
   end
end
