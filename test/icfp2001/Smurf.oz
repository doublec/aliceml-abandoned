\define DEBUG

functor
import
   FD
   FS
   Search
\ifdef DEBUG
   System
   Inspector
   Explorer
\endif
   Select at 'x-ozlib://duchier/cp/Select.ozf'
   TagComponent('Tag$': Tag) at 'Tag.ozf'
export
   'Smurf$': SmurfModule
define
   fun {SeqUnion Ss S}
      {Select.seqUnion
       {List.mapInd Ss
	fun {$ I Si}
	   {Select.fs [FS.value.empty Si] {FD.plus {FS.reified.isIn I S} 1}}
	end}}
   end

   AttributeNames = [b ems i tt u size color]

   RootAttributes = attributes(b: 1 ems: 1 i: 1 tt: 1 u: 1
			       size: 11 color: 9)

   fun {MkDataItemAttributes Property IsSpace}
      attributes(b: if IsSpace then {FD.int 1#2}
		    elseif Property.b then 2 else 1
		    end
		 ems: if IsSpace then {FD.int 1#3}
		      elseif Property.s then 3
		      elseif Property.em then 2
		      else 1
		      end
		 i: if IsSpace then {FD.int 1#2}
		    elseif Property.i then 2
		    else 1
		    end
		 tt: if Property.tt then 2
		     else 1
		     end
		 u: Property.u + 1
		 size: case Property.size of ~1 then 11 elseof S then S + 1 end
		 color:
		    if IsSpace andthen Property.u == 0 then {FD.int 1#9}
		    elsecase Property.color of 'R' then 1
		    [] 'G' then 2
		    [] 'B\'' then 3
		    [] 'C' then 4
		    [] 'M' then 5
		    [] 'Y' then 6
		    [] 'K' then 7
		    [] 'W' then 8
		    [] 'UNKNOWN' then 9
		    end)
   end

   fun {MkElementAttributes}
      attributes(b: {FD.int 1#2}
		 ems: {FD.int 1#3}
		 i: {FD.int 1#2}
		 tt: {FD.int 1#2}
		 u: {FD.int 1#4}
		 size: {FD.int 1#11}
		 color: {FD.int 1#9})
   end

   fun {MkSizeTag I}
      tag(name: 'SIZE'(I - 1)
	  p: proc {$ A In Out}
		case A of size then Out = I
		else Out = In
		end
	     end)
   end

   fun {MkColorTag Color I}
      tag(name: 'COLOR'(Color)
	  p: proc {$ A In Out}
		case A of color then Out = I
		else Out = In
		end
	     end)
   end

   Tags = tags(tag(p: proc {$ _ In Out} Out = In end)   % epsilon
	       tag(name: 'PL'
		   p: proc {$ A In Out}
			 case A of b then Out = 1
			 [] ems then Out = 1
			 [] i then Out = 1
			 [] tt then Out = 1
			 [] u then Out = 1
			 else Out = In
			 end
		      end)
	       tag(name: 'B'
		   p: proc {$ A In Out}
			 case A of b then Out = 2
			 else Out = In
			 end
		      end)
	       tag(name: 'EM'
		   p: proc {$ A In Out}
			 case A of ems then Out = {Select.fd [2 1 3] In}
			 else Out = In
			 end
		      end)
	       tag(name: 'I'
		   p: proc {$ A In Out}
			 case A of i then Out = 2
			 else Out = In
			 end
		      end)
	       tag(name: 'S'
		   p: proc {$ A In Out}
			 case A of ems then Out = 3
			 else Out = In
			 end
		      end)
	       tag(name: 'TT'
		   p: proc {$ A In Out}
			 case A of tt then Out = 2
			 else Out = In
			 end
		      end)
	       tag(name: 'U'
		   p: proc {$ A In Out}
			 case A of u then Out = {FD.min {FD.plus In 1} 4}
			 else Out = In
			 end
		      end)
	       {MkSizeTag 1}
	       {MkSizeTag 2}
	       {MkSizeTag 3}
	       {MkSizeTag 4}
	       {MkSizeTag 5}
	       {MkSizeTag 6}
	       {MkSizeTag 7}
	       {MkSizeTag 8}
	       {MkSizeTag 9}
	       {MkSizeTag 10}
	       {MkColorTag 'R' 1}
	       {MkColorTag 'G' 2}
	       {MkColorTag 'B\'' 3}
	       {MkColorTag 'C' 4}
	       {MkColorTag 'M' 5}
	       {MkColorTag 'Y' 6}
	       {MkColorTag 'K' 7}
	       {MkColorTag 'W' 8})

   Epsilon = 1
   MaxTag = {Width Tags}

   RootI = 1

   fun {Constrain Meaning SourceCost}
      NumberOfElements = SourceCost div Tag.minCost
      NumberOfDataItems = {Length Meaning}
\ifdef DEBUG
{System.show numberOf(elements: NumberOfElements dataItems: NumberOfDataItems)}
\endif

      FirstElementI = RootI + 1
      LastElementI = FirstElementI + NumberOfElements - 1
      FirstDataItemI = LastElementI + 1
      LastDataItemI = FirstDataItemI + NumberOfDataItems - 1

      FirstNonRootI = FirstElementI
      LastNonRootI = LastDataItemI
      FirstNonDataItemI = RootI
      LastNonDataItemI = LastElementI
      FirstVertexI = RootI
      LastVertexI = LastNonRootI

      V = {Tuple.make vertices NumberOfDataItems + NumberOfElements + 1}

      %% Initialize root vertex
      V.RootI = root(daughters: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		     daughtersE: {FS.var.upperBound FirstElementI#LastElementI}
		     down: {FS.value.make FirstNonRootI#LastNonRootI}
		     downE: {FS.value.make FirstElementI#LastElementI}
		     eqdown: {FS.value.make FirstVertexI#LastVertexI}
		     eqdownE:
			{FS.value.make FirstNonDataItemI#LastNonDataItemI}
		     scope: {FS.value.make FirstDataItemI#LastDataItemI}
		     attributes: RootAttributes)

      %% Initialize element vertices
      for I in FirstElementI..LastElementI do
	 V.I = element(mother: {FD.int [RootI FirstElementI#LastElementI]}
		       daughters:
			  {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       daughtersE:
			  {FS.var.upperBound FirstElementI#LastElementI}
		       down: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       downE: {FS.var.upperBound FirstElementI#LastElementI}
		       eqdown: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       eqdownE: {FS.var.upperBound FirstElementI#LastElementI}
		       isDownEmpty: {FD.int 0#1}
		       scope: {FS.var.upperBound FirstDataItemI#LastDataItemI}
		       attributes: {MkElementAttributes}
		       tag: {FD.int 1#MaxTag})
      end

      %% Initialize data item vertices
      {List.forAllInd Meaning
       proc {$ J Text#IsSpace#Property} I in
	  I = FirstDataItemI + J - 1
	  V.I = dataItem(mother: {FD.int [RootI FirstElementI#LastElementI]}
			 daughters: FS.value.empty
			 down: FS.value.empty
			 eqdown: {FS.value.singl I}
			 scope: {FS.value.singl I}
			 attributes: {MkDataItemAttributes Property IsSpace}
			 text: Text)
       end}

      %% Treeness Constraints
      for I in FirstElementI..LastElementI do
	 V.I.mother \=: I
      end

      Eqdowns = for I in FirstVertexI..LastVertexI collect: Collect do
		   {Collect V.I.eqdown}
		end

      for I in FirstNonDataItemI..LastNonDataItemI do W in
	 W = V.I
%	 W.down = {Select.union Eqdowns W.daughters}
	 W.down = {FS.union W.downE W.scope}
	 W.eqdown = {FS.partition [{FS.value.singl I} W.down]}
      end

      for I1 in FirstNonDataItemI..LastNonDataItemI do W1 in
	 W1 = V.I1
	 for I2 in FirstNonRootI..LastNonRootI do
	    if I2 \= I1 then W2 in
	       W2 = V.I2
	       (W2.mother =: I1) =: {FS.reified.isIn I2 W1.daughters}
	    end
	 end
      end

      {FS.value.make FirstNonRootI#LastNonRootI} =
      {FS.partition for I in FirstVertexI..LastVertexI collect: Collect do
		       {Collect V.I.daughters}
		    end}

      %% Attribute constraints
      Attributes = {List.toRecord attributes
		    for A in AttributeNames collect: Collect do
		       {Collect
			A#for I in FirstVertexI..LastVertexI collect: Collect
			  do {Collect V.I.attributes.A}
			  end}
		    end}
      Ps = for Tag in 1..MaxTag collect: Collect do {Collect Tags.Tag.p} end

      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 for A in AttributeNames do Inherited in
	     Inherited = {Select.fd Attributes.A W.mother}
	     W.attributes.A = {Select.fd
			       {Map Ps fun {$ P} {P A Inherited} end} W.tag}
	 end
      end

      for I in FirstDataItemI..LastDataItemI do W in
	 W = V.I
	 for A in AttributeNames do
	    W.attributes.A = {Select.fd Attributes.A W.mother}
	 end
      end

      %% Scope constraints
      Scopes = for I in FirstVertexI..LastVertexI collect: Collect do
		  {Collect V.I.scope}
	       end

      V.RootI.scope = {Select.union Scopes V.RootI.daughters}
      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 {FS.int.convex W.scope}
%--**	 W.scope = {Select.union Scopes W.daughters}
	 W.scope = {SeqUnion Scopes W.daughters}
      end

      %% Unused elements are immediate daughters of the root
      for I in FirstElementI..LastElementI do W IsEpsilon in
	 W = V.I
	 IsEpsilon = (W.tag =: Epsilon)
	 IsEpsilon =: {FS.reified.equal W.scope FS.value.empty}
	 IsEpsilon =<: (W.mother =: RootI)
      end

      %% Break symmetries: Sequenced Union Method
      for I in FirstNonRootI..LastNonRootI do
	 V.I.mother <: I
      end

      E = {FS.value.make [RootI FirstElementI#LastElementI]}
      EqdownEs = for I in FirstVertexI..LastVertexI collect: Collect do
		    {Collect case {CondSelect V.I eqdownE unit} of unit then
				FS.value.empty
			     elseof S then S
			     end}
		 end

      for I in FirstNonDataItemI..LastNonDataItemI do W in
	 W = V.I
	 W.daughtersE = {FS.intersect E W.daughters}
	 W.downE = {FS.intersect E W.down}
	 W.eqdownE = {FS.intersect E W.eqdown}
	 W.downE = {SeqUnion EqdownEs W.daughtersE}
	 {FS.int.convex W.eqdownE}
	 if I \= RootI then
	    W.isDownEmpty = {FS.reified.equal W.down FS.value.empty}
	 end
      end

%--**
      for I in FirstElementI..LastElementI - 1 do
	 V.I.isDownEmpty =<: V.(I + 1).isDownEmpty
      end

      %% Cost function
      TagCosts = for I in 1..MaxTag collect: Collect do
		    if I \= Epsilon then
		       {Collect {Tag.cost Tags.I.name}}
		    else
		       {Collect 0}
		    end
		 end

      Cost = {FD.int 0#SourceCost}
      Cost = {FD.sum for I in FirstElementI..LastElementI collect: Collect do
			{Collect {Select.fd TagCosts V.I.tag}}
		     end '=:'}
   in
      V#Cost
   end

   fun {Script Meaning SourceCost}
      proc {$ Res} V
	 NumberOfElements = SourceCost div Tag.minCost
	 FirstElementI = RootI + 1
	 NumberOfActiveElements = {FD.int 0#NumberOfElements}
      in
	 Res = {Constrain {Reverse Meaning} SourceCost}
	 V = Res.1

	 {FD.distribute naive [NumberOfActiveElements]}

	 if NumberOfActiveElements > 0 then
	    {FS.cardRange 1 FS.sup
	     V.(FirstElementI+NumberOfActiveElements-1).down}
	 end
	 if NumberOfActiveElements < NumberOfElements then
	    V.(FirstElementI+NumberOfActiveElements).down = FS.value.empty
	 end

	 {FD.distribute ff
	  {Append
	   for I in 1..{Width V} collect: Collect do
	      case V.I of dataItem(...) then
		 {Collect V.I.mother}
	      else skip
	      end
	   end
	   for I in 1..{Width V} collect: Collect do
	      case V.I of element(...) then
		 {Collect V.I.tag}
		 {Collect V.I.mother}
	      else skip
	      end
	   end}}
      end
   end

   proc {Order _#OldCost _#NewCost}
      OldCost >: NewCost
   end

   local
      fun {GetLowestElement Scope}
	 case {FS.reflect.lowerBound Scope} of nil then 0
	 [] [I#_] then I
	 [] [I] then I
	 end
      end

      fun {ToDocSub Daughters V}
	 {List.foldR {Sort {FS.reflect.lowerBoundList Daughters}
		      fun {$ I1 I2}
			 {GetLowestElement V.I1.scope} <
			 {GetLowestElement V.I2.scope}
		      end}
	  fun {$ I In}
	     case V.I of dataItem(text: Text ...) then
		'TEXT'(Text)|In
	     [] element(tag: !Epsilon ...) then In
	     [] element(tag: Tag daughters: Daughters ...) then
		'TAGGED'(Tags.Tag.name {ToDocSub Daughters V})|In
	     end
	  end nil}
      end
   in
      fun {ToDoc V#_}
	 {ToDocSub V.RootI.daughters V}
      end
   end

\ifndef DEBUG
   local
      proc {SmurfSub O Docs}
	 case {O next($)} of [Res] then DocsRest in
	    Docs = {ToDoc Res}|(!!DocsRest)
	    {SmurfSub O DocsRest}
	 [] nil then
	    Docs = nil
	 end
      end
   in
      fun {Smurf Meaning SourceCost} O Docs in
	 O = {New Search.object
	      script({Script Meaning SourceCost} Order)}
	 thread {SmurfSub O Docs} end
	 !!Docs
      end
   end
\else
   fun {Smurf Meaning SourceCost}
%      {Inspector.inspect {Reverse Meaning}}
%      {Explorer.one {Script Meaning SourceCost}}
      {Explorer.best {Script Meaning SourceCost} Order}
%      {Inspector.inspect {ToDoc {Search.base.best {Script Meaning SourceCost} Order}.1}}
      _
   end
\endif

   SmurfModule = 'Smurf'(smurf: Smurf)
end
