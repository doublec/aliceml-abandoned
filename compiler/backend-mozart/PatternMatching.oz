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
%%
%% <pos> ::= <pattern number>|[<sel>#<subtree number>]
%% <pattern number> ::= <int>
%% <sel> ::= <feature>   % a special name is reserved for constructor selection
%% <subtree number> ::= <int>
%%
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
   CompilerSupport(featureLess)
   Annotate(valRepToValue: ValRepToValue longIdValRep: LongIdValRep)
   Intermediate(litToValue)
export
   BuildTree
define
   ConSel = {NewName}

   proc {Incf C ?N}
      N = {Access C} + 1
      {Assign C N}
   end

   local
      fun {FeatureListEq FIs1 FIs2}
	 case FIs1#FIs2 of (F#_|FIr1)#(F#_|FIr2) then
	    {FeatureListEq FIr1 FIr2}
	 [] nil#nil then true
	 else false
	 end
      end
   in
      fun {PosEq _|FIs1 _|FIs2}
	 {FeatureListEq FIs1 FIs2}
      end
   end

   fun {TestEq Test1 Test2}
      case Test1#Test2 of guardTest(_)#_ then false
      [] _#guardTest(_) then false
      [] decs(_)#_ then false
      [] _#decs(_) then false
      else Test1 == Test2
      end
   end

   fun {TestImplies Pos1#Test1 Pos2#Test2}
      {PosEq Pos1 Pos2} andthen {TestEq Test1 Test2}
   end

   proc {MergeTestLists TestList1 TestList2 Hd Tl}
      %--** if value propagation is good enough, this reduces
      %--** to a simple append
      Hd = {Append TestList1
	    {FoldL TestList2
	     proc {$ Hd X Tl}
		if {Some TestList1 fun {$ Y} {TestImplies X Y} end} then
		   Hd = Tl
		else
		   Hd = X|Tl
		end
	     end $ Tl}}
   end

   proc {MakeTestList Pat Pos ValRep State C Hd Tl}
      case Pat of wildPat(_) then
	 Hd = Tl
      [] litPat(_ Lit) then
	 Hd = Pos#litTest({Intermediate.litToValue Lit})|Tl
      [] varPat(_ _) then
	 Hd = Tl
      [] conPat(_ LongId OptPat) then ValRep1 in
	 ValRep1 = {LongIdValRep LongId State}
	 case OptPat of some(Pat) then Inter SubValRep SubPos in
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
	    SubPos = {Append Pos [ConSel#0]}
	    {MakeTestList Pat SubPos SubValRep State C Inter Tl}
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
		{MakeTestList Pat {Append Pos [I#I]} V.I State C Hd Tl}
	     end Inter Tl}
	 else Inter Unknown in
	    Hd = Pos#recTest({Width V})|Inter
	    Unknown = unit#top
	    {List.foldLInd Pats
	     proc {$ I Hd Pat Tl}
		{MakeTestList Pat {Append Pos [I#I]} Unknown State C Hd Tl}
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
	    {List.foldLInd FieldPats
	     proc {$ I Hd field(_ lab(_ Feature) Pat) Tl} SubPos in
		SubPos = {Append Pos [Feature#I]}
		{MakeTestList Pat SubPos V.Feature State C Hd Tl}
	     end Inter Tl}
	 else Unknown in
	    Unknown = unit#top
	    {List.foldLInd FieldPats
	     proc {$ I Hd field(_ lab(_ Feature) Pat) Tl} SubPos in
		SubPos = {Append Pos [Feature#I]}
		{MakeTestList Pat SubPos Unknown State C Hd Tl}
	     end Inter Tl}
	 end
      [] asPat(_ Pat1 Pat2) then TestList1 TestList2 in
	 {MakeTestList Pat1 Pos ValRep State C ?TestList1 nil}
	 {MakeTestList Pat2 Pos ValRep State C ?TestList2 nil}
	 {MergeTestLists TestList1 TestList2 Hd Tl}
      [] altPat(_ Pats) then
	 case Pos of _|FIs then
	    Hd = alt({Map Pats
		      fun {$ Pat} NewPos in
			 NewPos = {Incf C}|FIs
			 {MakeTestList Pat NewPos ValRep State C $ nil}
		      end})|Tl
	 end
      [] negPat(_ Pat) then
	 Hd = neg({MakeTestList Pat Pos ValRep State C $ nil})|Tl
      [] guardPat(_ Pat Exp) then Inter in
	 {MakeTestList Pat Pos ValRep State C Hd Inter}
	 Inter = Pos#guardTest(Exp)|Tl
      [] withPat(_ Pat Decs) then Inter in
	 {MakeTestList Pat Pos ValRep State C Hd Inter}
	 Inter = Pos#decs(Decs)|Tl
      end
   end

   local
      local
	 fun {FeatureListGreater FIs1 FIs2}
	    case FIs1#FIs2 of (_#I|FIr1)#(_#I|FIr2) then
	       {FeatureListGreater FIr1 FIr2}
	    [] (_#I1|_)#(_#I2|_) then I1 < I2
	    [] nil#_ then false
	    [] _#nil then true
	    end
	 end
      in
	 fun {PosGreater Pos1 Pos2}
	    case Pos1 of (N|Fs1)#(N|Fs2) then {FeatureListGreater Fs1 Fs2}
	    else false
	    end
	 end
      end
   in
      fun {MayMoveOver Pos1 Test1 Pos2 Test2}
	 {PosGreater Pos1 Pos2} orelse {PosEq Pos1 Pos2} andthen
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
   end

   fun {MakeLeaf Exp}
      leaf(Exp {NewCell 0} _)
   end

   local
      fun {FindTest Tree Pos0 Test0 ?NewTree ?Hole ?RestTree}
	 case Tree of node(Pos Test ThenTree ElseTree Count Shared) then
	    case Test of guardTest(_) then false
	    [] decs(_) then false
	    elseif {PosEq Pos0 Pos} andthen {TestEq Test Test0} then
	       NewTree = node(Pos Test Hole ElseTree Count Shared)
	       RestTree = ThenTree
	       true
	    elseif {MayMoveOver Pos0 Test0 Pos Test} then NewElseTree in
	       if {FindTest ElseTree Pos0 Test0
		   ?NewElseTree ?Hole ?RestTree}
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
      proc {MergeSub TestList ThenTree ElseTree Tree ?NewTree}
	 case TestList of nil then
	    %% Tree is unreachable
	    NewTree = ThenTree
	 [] neg(TestList)|_ then   %--** what about Rest?
	    {MergeSub TestList ElseTree ThenTree Tree ?NewTree}
	 [] alt(TestLists)|_ then   %--** what about Rest?
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
	       NewThenTree = {MergeSub Rest ThenTree ElseTree default}
	       NewTree = node(Pos Test NewThenTree Tree {NewCell 0} _)
	    end
	 end
      end
   end

   local
      fun {ClipTree Pos0 Test0 Tree}
	 case Tree of node(Pos Test _ ElseTree _ _) then
	    if {MayMoveOver Pos0 Test0 Pos Test} then
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

   fun {BuildTree Matches ValRep ElseExp State} ElseTree C in
      ElseTree = {MakeLeaf Exp}
      C = {NewCell 0}
      {PropagateElses
       {FoldR Matches
	fun {$ match(_ Pat ThenExp) Tree} TestList in
	   {MakeTestList Pat [{Incf C}] ValRep State C ?TestList nil}
	   {MergeSub TestList {MakeLeaf ThenExp} ElseTree Tree}
	end default}
       ElseTree}
      %--** update Counts
   end
end
