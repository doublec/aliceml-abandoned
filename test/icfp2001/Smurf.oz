functor
import
   FD
   FS
   Search
   Space
   Select at 'x-ozlib://duchier/cp/Select.ozf'
   TagComponent('Tag$': Tag) at 'Tag.ozf'
export
   'Smurf$': SmurfModule
define
   AttributeNames = [b ems i tt u size color]

   RootAttributes = attributes(b:     1
			       ems:   1
			       i:     1
			       tt:    1
			       u:     1
			       size:  11
			       color: 9)

   fun {MkElementAttributes}
      attributes(b:     {FD.int 1#2}
		 ems:   {FD.int 1#3}
		 i:     {FD.int 1#2}
		 tt:    {FD.int 1#2}
		 u:     {FD.int 1#4}
		 size:  {FD.int 1#11}
		 color: {FD.int 1#9})
   end

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

   fun {ComputeMinDepth Attributes}
      {FD.sum
       [Attributes.b     \=: RootAttributes.b
	Attributes.ems   \=: RootAttributes.ems
	Attributes.i     \=: RootAttributes.i
	Attributes.tt    \=: RootAttributes.tt
	{FD.minus Attributes.u RootAttributes.u}
	Attributes.size  \=: RootAttributes.size
	Attributes.color \=: RootAttributes.color
	1] '=:'}
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
	       tag(name: 'B'
		   p: proc {$ A In Out}
			 case A of b then Out = 2
			 else Out = In
			 end
		      end)
	       tag(name: 'I'
		   p: proc {$ A In Out}
			 case A of i then Out = 2
			 else Out = In
			 end
		      end)
	       tag(name: 'TT'
		   p: proc {$ A In Out}
			 case A of tt then Out = 2
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
	       {MkColorTag 'W' 8}
	       tag(name: 'U'
		   p: proc {$ A In Out}
			 case A of u then Out = {FD.min {FD.plus In 1} 4}
			 else Out = In
			 end
		      end)
	       tag(name: 'EM'
		   p: proc {$ A In Out}
			 case A of ems then Out = {Select.fd [2 1 3] In}
			 else Out = In
			 end
		      end)
	       tag(name: 'S'
		   p: proc {$ A In Out}
			 case A of ems then Out = 3
			 else Out = In
			 end
		      end)
	       tag(name: 'PL'
		   p: proc {$ A In Out}
			 case A of b then Out = 1
			 [] ems then Out = 1
			 [] i then Out = 1
			 [] tt then Out = 1
			 [] u then Out = 1
			 else Out = In
			 end
		      end))

   Epsilon = 1
   MaxTag = {Width Tags}

   RootI = 1

   proc {Constrain Meaning SourceCost ?Res}
      NumberOfElements = SourceCost div Tag.minCost
      NumberOfDataItems = {Length Meaning}

      FirstElementI     = RootI + 1
      LastElementI      = FirstElementI + NumberOfElements - 1
      FirstDataItemI    = LastElementI + 1
      LastDataItemI     = FirstDataItemI + NumberOfDataItems - 1

      FirstVertexI      = RootI
      LastVertexI       = LastNonRootI
      FirstNonDataItemI = RootI
      LastNonDataItemI  = LastElementI
      FirstNonRootI     = FirstElementI
      LastNonRootI      = LastDataItemI

      V = {Tuple.make vertices NumberOfDataItems + NumberOfElements + 1}

      %% Initialize root vertex
      V.RootI = root(daughters:  {FS.var.upperBound FirstNonRootI#LastNonRootI}
		     down:       {FS.value.make FirstNonRootI#LastNonRootI}
		     eqdown:     {FS.value.make FirstVertexI#LastVertexI}
		     scope:      {FS.value.make FirstDataItemI#LastDataItemI}
		     attributes: RootAttributes
		     depth:      0)

      %% Initialize element vertices
      for I in FirstElementI..LastElementI do
	 Mother     = {FD.int [RootI FirstElementI#LastElementI]}
	 Daughters  = {FS.var.upperBound FirstNonRootI#LastNonRootI}
	 Down       = {FS.var.upperBound FirstNonRootI#LastNonRootI}
	 Eqdown     = {FS.var.upperBound FirstNonRootI#LastNonRootI}
	 Scope      = {FS.var.upperBound FirstDataItemI#LastDataItemI}
	 Attributes = {MkElementAttributes}
	 Depth      = {FD.int 1#NumberOfElements}
	 MinDepth   = 1
	 Tag        = {FD.int 1#MaxTag}
      in
	 Mother \=: I
	 V.I = element(mother:     Mother
		       daughters:  Daughters
		       down:       Down
		       eqdown:     Eqdown
		       scope:      Scope
		       attributes: Attributes
		       depth:      Depth
		       mindepth:   MinDepth
		       tag:        Tag)
      end

      %% Initialize data item vertices
      {List.forAllInd Meaning
       proc {$ J Text#IsSpace#Property}
	  I = FirstDataItemI + J - 1
	  Mother     = {FD.int [RootI FirstElementI#LastElementI]}
	  Daughters  = FS.value.empty
	  Down       = FS.value.empty
	  Eqdown     = {FS.value.singl I}
	  Scope      = {FS.value.singl I}
	  Attributes = {MkDataItemAttributes Property IsSpace}
	  Depth      = {FD.int 1#(NumberOfElements + 1)}
	  MinDepth   = {FD.int 1#(NumberOfElements + 1)}
       in
	  {ComputeMinDepth Attributes MinDepth}
	  V.I = dataItem(mother:     Mother
			 daughters:  Daughters
			 down:       Down
			 eqdown:     Eqdown
			 scope:      Scope
			 attributes: Attributes
			 depth:      Depth
			 mindepth:   MinDepth
			 text:       Text)
       end}

      %% Treeness Constraints
      Eqdowns = for I in FirstVertexI..LastVertexI collect: Collect do
		   {Collect V.I.eqdown}
		end

      for I in FirstNonRootI..LastNonRootI do W in
	 W = V.I
	 W.down   = {Select.union Eqdowns W.daughters}
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
      for A in AttributeNames do Attributes in
	 Attributes = for I in FirstVertexI..LastVertexI collect: Collect do
			 {Collect V.I.attributes.A}
		      end
	 for I in FirstNonRootI..LastNonRootI do W Inherited in
	    W = V.I
	    Inherited = {Select.fd Attributes W.mother}
	    W.attributes.A = case {CondSelect W tag unit} of unit
			     then Inherited
			     elseof Tag then
				{Select.fd
				 for Tag in 1..MaxTag collect: Collect do
				    {Collect {Tags.Tag.p A Inherited}}
				 end
				 Tag}
			     end
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
	 W.scope = {Select.union Scopes W.daughters}
      end

      %% Unused elements are immediate daughters of the root
      for I in FirstElementI..LastElementI do W IsEpsilon in
	 W = V.I
	 IsEpsilon = (W.tag =: Epsilon)
	 IsEpsilon =: {FS.reified.equal W.scope FS.value.empty}
	 IsEpsilon =<: (W.mother =: RootI)
      end

      %% Break symmetries #1: Depth Method
      Depths = for I in FirstVertexI..LastVertexI collect: Collect do
		  {Collect V.I.depth}
	       end

      for I in FirstElementI..LastElementI do W MotherDepth in
	 W = V.I
	 MotherDepth = {Select.fd Depths W.mother}
	 W.depth = {FD.plus MotherDepth 1}
      end

      for I in FirstElementI..LastElementI - 1 do
	 V.I.depth =<: V.(I + 1).depth
      end

      %% Minimal depth
      for I1 in FirstNonDataItemI..LastNonDataItemI do W1 in
	 W1 = V.I1
	 for I2 in FirstNonRootI..LastNonRootI do
	    if I2 \= I1 then W2 in
	       W2 = V.I2
	       {FS.reified.isIn I2 W1.daughters} =<:
	       ({FD.plus W1.depth 1} >=: W2.mindepth)
	    end
	 end
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

      proc {Distribute} Mothers in
	 %% Step 1: Find nodes that are complete downwards.
	 %%         Pick one, determine its mother.
	 {Space.waitStable}
	 Mothers = for I in FirstElementI..LastElementI collect: Collect do
		      if {IsDet V.I.down} andthen {Not {IsDet V.I.mother}} then
			 {Collect V.I.mother}
		      end
		   end
	 case Mothers of _|_ then AMother in
	    AMother = {FD.choose ff Mothers $ _}
	    {FD.distribute ff [AMother]}
	    {Distribute}
	 [] nil then
	    {FD.distribute ff
	     {Append
	      for I in 1..{Width V} collect: Collect do
		 case {CondSelect V.I mother unit} of unit then skip
		 elseof Mother then {Collect Mother}
		 end
	      end
	      for I in 1..{Width V} collect: Collect do
		 case {CondSelect V.I tag unit} of unit then skip
		 elseof Tag then {Collect Tag}
		 end
	      end}}
	 end
      end
   in
      Res = V#Cost
      {Distribute}
   end

   fun {Script Meaning SourceCost}
      fun {$}
	 {Constrain {Reverse Meaning} SourceCost}
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

   SmurfModule = 'Smurf'(smurf: Smurf)
end
