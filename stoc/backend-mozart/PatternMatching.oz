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
%% <test list> ::= [<pattern test>]
%% <pattern test> ::= <pos>#<test>
%%                 |  neg(<test list>)
%%                 |  alt([<test list>])
%% <pos> ::= [<sel>]
%% <sel> ::= conSel
%%        |  recSel(<feature>)
%% <test> ::= litTest(<desc>)
%%         |  conAccess
%%         |  basicConTest(<name>)
%%         |  nonbasicConTest(<long id>)
%%         |  basicNameTest(<name>)
%%         |  nonbasicNameTest(<long id>)
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
   proc {MakeTestList Pat Pos ValRep State Hd Tl}
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
	    {MakeTestList Pat {Append Pos [conSel]} SubValRep State Inter Tl}
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
		{MakeTestList Pat {Append Pos [recSel(I)]} V.I State Hd Tl}
	     end Inter Tl}
	 else Inter Unk in
	    Hd = Pos#recTest({Width V})|Inter
	    Unk = unit#top
	    {List.foldLInd Pats
	     proc {$ I Hd Pat Tl}
		{MakeTestList Pat {Append Pos [recSel(I)]} Unk State Hd Tl}
	     end Inter Tl}
	 end
      [] recPat(_ FieldPats HasDots) then PatArity V Inter WithVal in
	 PatArity = {Arity {List.toRecord x
			    {Map FieldPats
			     fun {$ field(_ lab(_ Feature) _)}
				Feature#unit
			     end}}}
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
	    Hd = Pos#recTest(PatArity)|Inter
	    WithVal = false
	 end
	 if WithVal then
	    {FoldL FieldPats
	     proc {$ Hd field(_ lab(_ Feature) Pat) Tl} SubPos in
		SubPos = {Append Pos [recSel(Feature)]}
		{MakeTestList Pat SubPos V.Feature State Hd Tl}
	     end Inter Tl}
	 else Unknown in
	    Unknown = unit#top
	    {FoldL FieldPats
	     proc {$ Hd field(_ lab(_ Feature) Pat) Tl} SubPos in
		SubPos = {Append Pos [recSel(Feature)]}
		{MakeTestList Pat SubPos Unknown State Hd Tl}
	     end Inter Tl}
	 end
      [] asPat(_ _ Pat) then
	 {MakeTestList Pat Pos ValRep State Hd Tl}
      [] altPat(_ Pats) then
	 Hd = alt({Map Pats
		   fun {$ Pat}
		      {MakeTestList Pat Pos ValRep State $ nil}
		   end})|Tl
      [] negPat(_ Pat) then
	 Hd = neg({MakeTestList Pat Pos ValRep State $ nil})|Tl
      [] guardPat(_ Pat Exp) then Inter in
	 {MakeTestList Pat Pos ValRep State Hd Inter}
	 Inter = Pos#guardTest(Exp)|Tl
      [] withPat(_ Pat Decs) then Inter in
	 {MakeTestList Pat Pos ValRep State Hd Inter}
	 Inter = Pos#decs(Decs)|Tl
      end
   end

   fun {MayMoveOver Test1 Test2}
      case Test1#Test2 of guardTest(_)#_ then false
      [] decs(_)#_ then false
      [] _#guardTest(_) then false
      [] _#decs(_) then false
      [] X#X then false
      [] basicConTest(_)#nonbasicConTest(_) then false
      [] nonbasicConTest(_)#basicConTest(_) then false
      [] basicNameTest(_)#nonbasicNameTest(_) then false
      [] nonbasicNameTest(_)#basicNameTest(_) then false
      [] recTest(Arity)#labelTest(F) then {Not {Member F Arity}}
      [] labelTest(F)#recTest(Arity) then {Not {Member F Arity}}
      else true
      end
   end

   fun {MakeLeaf Exp}
      leaf(Exp {NewCell 0} _)
   end

   local
      fun {FindTest Tree Pos0 Test0 ?NewTree ?Hole ?RestTree}
	 case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	    if Pos \= Pos0 then false
	    elseif Test == Test0 then
	       NewTree = node(Pos Test Hole ElseTree Count Shared)
	       RestTree = ThenTree
	       true
	    elseif {MayMoveOver Test0 Test} then NewElseTree in
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

      fun {PatternToTree Pattern ThenTree ElseTree}
	 case Pattern of nil then
	    ThenTree
	 [] neg(TestList) then
	    {PatternToTree TestList ElseTree ThenTree}
	 [] alt(TestLists) then
	    {FoldR TestLists
	     fun {$ TestList Tree}
		{MergeSub TestList ThenTree ElseTree Tree}
	     end ElseTree}
	 [] Pos#Test|Rest then NewThenTree in
	    NewThenTree = {PatternToTree Rest ThenTree ElseTree}
	    node(Pos Test NewThenTree ElseTree {NewCell 0} _)
	 end
      end
   in
      proc {MergeSub Pattern ThenTree ElseTree Tree ?NewTree}
	 case Pattern of nil then
	    %% Tree is unreachable
	    NewTree = ThenTree
	 [] neg(TestList) then
	    {MergeSub TestList ElseTree ThenTree Tree ?NewTree}
	 [] alt(TestLists) then
	    NewTree = {FoldR TestLists
		       fun {$ TestList Tree}
			  {MergeSub TestList ThenTree ElseTree Tree}
		       end ElseTree}
	 [] Pos#Test|Rest then
	    case Tree of node(!Pos _ _ _ _ _) andthen Hole RestTree in
	       {FindTest Tree Pos Test ?NewTree ?Hole ?RestTree}
	    then
	       Hole = {MergeSub Rest ThenTree ElseTree RestTree}
	    else NewThenTree in
	       NewThenTree = {PatternToTree Rest ThenTree ElseTree}
	       NewTree = node(Pos Test NewThenTree Tree {NewCell 0} _)
	    end
	 end
      end
   end

   local
      fun {ClipTree Pos0 Test0 Tree}
	 case Tree of node(Pos Test _ ElseTree _ _) then
	    if Pos == Pos0 andthen {MayMoveOver Test0 Test} then
	       {ClipTree Pos0 Test0 ElseTree}
	    else
	       Tree
	    end
	 [] leaf(_ _ _) then
	    Tree
	 end
      end
   in
      fun {PropagateElses Tree DefaultTree}
	 case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	    NewElseTree NewDefaultTree NewThenTree
	 in
	    NewElseTree = {PropagateElses ElseTree DefaultTree}
	    NewDefaultTree = {ClipTree Pos Test NewElseTree}
	    NewThenTree = {PropagateElses ThenTree NewDefaultTree}
	    node(Pos Test NewThenTree NewElseTree Count Shared)
	 [] leaf(_ _ _) then
	    Tree
	 [] default then
	    DefaultTree
	 end
      end
   end

   fun {BuildTree Matches ValRep ElseExp State} ElseTree in
      ElseTree = {MakeLeaf Exp}
      {PropagateElses
       {FoldR Matches
	fun {$ match(_ Pat ThenExp) Tree} TestList in
	   {MakeTestList Pat nil ValRep State ?TestList nil}
	   {MergeSub TestList {MakeLeaf ThenExp} ElseTree Tree}
	end default}
       ElseTree}
      %--** update Counts
   end
end
