functor
import
   FD
   FS
   Search
   Select at 'x-ozlib://duchier/cp/Select.ozf'
export
   'Smurf$': SmurfModule
define
   MinCostPerElement = 7   %--** import from Tag

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
		 tt: if IsSpace then {FD.int 1#2}
		     elseif Property.tt then 2
		     else 1
		     end
		 u: Property.u + 1
		 size: case Property.size of ~1 then 11 elseof S then S + 1 end
		 color:
		    if IsSpace then {FD.int 1#11}
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

   fun {MkSizeProc I}
      proc {$ A In Out}
	 case A of size then Out = I
	 else Out = In
	 end
      end
   end

   fun {MkColorProc I}
      proc {$ A In Out}
	 case A of color then Out = I
	 else Out = In
	 end
      end
   end

   Tags = tags(tag(p: proc {$ _ In Out} Out = In end   % epsilon
		   cost: 0)
	       tag(name: 'B'
		   p: proc {$ A In Out}
			 case A of b then Out = 2
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'EM'
		   p: proc {$ A In Out}
			 case A of ems then Out = {Select.fd [2 1 3] In}
			 else Out = In
			 end
		      end
		   cost: 9)
	       tag(name: 'I'
		   p: proc {$ A In Out}
			 case A of i then Out = 2
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'PL'
		   p: proc {$ A In Out}
			 case A of b then Out = 1
			 [] em then Out = 1
			 [] i then Out = 1
			 [] tt then Out = 1
			 [] u then Out = 1
			 else Out = In
			 end
		      end
		   cost: 9)
	       tag(name: 'S'
		   p: proc {$ A In Out}
			 case A of em then Out = 3
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'TT'
		   p: proc {$ A In Out}
			 case A of tt then Out = 2
			 else Out = In
			 end
		      end
		   cost: 9)
	       tag(name: 'U'
		   p: proc {$ A In Out}
			 case A of u then Out = {FD.max {FD.plus In 1} 4}
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'SIZE'(0)
		   p: {MkSizeProc 1}
		   cost: 7)
	       tag(name: 'SIZE'(1)
		   p: {MkSizeProc 2}
		   cost: 7)
	       tag(name: 'SIZE'(2)
		   p: {MkSizeProc 3}
		   cost: 7)
	       tag(name: 'SIZE'(3)
		   p: {MkSizeProc 4}
		   cost: 7)
	       tag(name: 'SIZE'(4)
		   p: {MkSizeProc 5}
		   cost: 7)
	       tag(name: 'SIZE'(5)
		   p: {MkSizeProc 6}
		   cost: 7)
	       tag(name: 'SIZE'(6)
		   p: {MkSizeProc 7}
		   cost: 7)
	       tag(name: 'SIZE'(7)
		   p: {MkSizeProc 8}
		   cost: 7)
	       tag(name: 'SIZE'(8)
		   p: {MkSizeProc 9}
		   cost: 7)
	       tag(name: 'SIZE'(9)
		   p: {MkSizeProc 10}
		   cost: 7)
	       tag(name: 'COLOR'('R')
		   p: {MkColorProc 1}
		   cost: 7)
	       tag(name: 'COLOR'('G')
		   p: {MkColorProc 2}
		   cost: 7)
	       tag(name: 'COLOR'('B\'')
		   p: {MkColorProc 3}
		   cost: 7)
	       tag(name: 'COLOR'('C')
		   p: {MkColorProc 4}
		   cost: 7)
	       tag(name: 'COLOR'('M')
		   p: {MkColorProc 5}
		   cost: 7)
	       tag(name: 'COLOR'('Y')
		   p: {MkColorProc 6}
		   cost: 7)
	       tag(name: 'COLOR'('K')
		   p: {MkColorProc 7}
		   cost: 7)
	       tag(name: 'COLOR'('W')
		   p: {MkColorProc 8}
		   cost: 7))

   RootI = 1
   Epsilon = 1
   MaxTag = {Width Tags}

   fun {Constrain Meaning NumberOfElements}
      NumberOfDataItems = {Length Meaning}
      NumberOfVertices = NumberOfDataItems + NumberOfElements + 1

      FirstDataItemI = RootI + 1
      LastDataItemI = FirstDataItemI + NumberOfDataItems - 1
      FirstElementI = LastDataItemI + 1
      LastElementI = FirstElementI + NumberOfElements - 1

      FirstNonRootI = FirstDataItemI
      LastNonRootI = LastElementI
      FirstVertexI = RootI
      LastVertexI = LastElementI

      V = {Tuple.make vertices NumberOfVertices}

      %% Initialize root vertex
      V.RootI = root(daughters: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		     scope: {FS.var.upperBound FirstDataItemI#LastDataItemI}
		     attributes: RootAttributes)

      %% Initialize data item vertices
      {List.forAllInd Meaning
       proc {$ J Text#IsSpace#Property} I in
	  I = FirstDataItemI + J - 1
	  V.I = dataItem(mother: {FD.int FirstVertexI#LastVertexI}
			 daughters: FS.value.empty
			 down: FS.value.empty
			 eqdown: {FS.value.singl I}
			 scope: {FS.value.singl I}
			 attributes: {MkDataItemAttributes Property IsSpace}
			 text: Text)
       end}

      %% Initialize element vertices
      for J in 1..NumberOfElements do I in
	 I = FirstElementI + J - 1
	 V.I = element(mother: {FD.int FirstVertexI#LastVertexI}
		       daughters:
			  {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       down: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       eqdown: {FS.var.upperBound FirstNonRootI#LastNonRootI}
		       scope: {FS.var.upperBound FirstDataItemI#LastDataItemI}
		       tag: {FD.int 1#MaxTag}
		       attributes: {MkElementAttributes})
      end

      %% Treeness Constraints
      for I in FirstNonRootI..LastNonRootI do
	 V.I.mother \=: I
      end

      Eqdowns = for I in FirstNonRootI..LastNonRootI collect: Collect do
		   {Collect V.I.eqdown}
		end

      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 {Select.union Eqdowns W.daughters W.down}
	 {FS.exclude I W.down}
	 W.eqdown = {FS.partition [{FS.value.singl I} W.down]}
      end

      for I1 in FirstVertexI..LastVertexI do W1 in
	 W1 = V.I1
	 for I2 in FirstNonRootI..LastNonRootI do
	    if I2 \= I1 then W2 in
	       W2 = V.I2
	       (W2.mother =: I1) =: {FS.reified.isIn I2 W1.daughters}
	    end
	 end
      end

      {FS.disjointN for I in FirstVertexI..LastVertexI collect: Collect do
		       {Collect V.I.daughters}
		    end}

      %% Attribute constraints
      Attributes = {List.toRecord attributes
		    {Map [b ems i tt u size color]
		     fun {$ A}
			A#for I in FirstVertexI..LastVertexI collect: Collect
			  do {Collect V.I.attributes.A}
			  end
		     end}}
      Ps = for Tag in 1..MaxTag collect: Collect do {Collect Tags.Tag.p} end

      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 {ForAll AttributeNames
	  proc {$ A} Inherited in
	     Inherited = {Select.fd Attributes.A W.mother}
	     W.attributes.A = {Select.fd
			       {Map Ps fun {$ P} {P A Inherited} end} W.tag}
	  end}
      end

      for I in FirstDataItemI..LastDataItemI do W in
	 W = V.I
	 {ForAll AttributeNames
	  proc {$ A}
	     W.attributes.A = {Select.fd Attributes.A W.mother}
	  end}
      end

      %% Scope constraints
      Scopes = for I in FirstVertexI..LastVertexI collect: Collect do
		  {Collect V.I.scope}
	       end

      {FS.int.convex V.RootI.scope}
      {Select.union Scopes V.RootI.daughters V.RootI.scope}
      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 {FS.int.convex W.scope}
	 {Select.union Scopes W.daughters W.scope}
      end

      %% Unused elements are immediate daughters of the root
      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 (W.tag =: Epsilon) =: {FS.reified.equal W.scope FS.value.empty}
	 (W.tag =: Epsilon) =<: (W.mother =: RootI)
      end

      %% Cost function
      TagCosts = for I in 1..MaxTag collect: Collect do
		    {Collect Tags.I.cost}
		 end

/*--**
      TagCosts = for I in 1..MaxTag collect: Collect do
		    {TagModule.cost Tags.Tag.name}
		 end
*/

      Cost = {FD.int 0#(NumberOfElements * MinCostPerElement)}
      Cost = {FD.sum for I in FirstElementI..LastElementI collect: Collect do
			{Collect {Select.fd TagCosts V.I.tag}}
		     end '=:'}
   in
      V#Cost
   end

   local
      fun {ToDocSub Daughters V}
	 {List.foldR {FS.reflect.lowerBoundList Daughters}
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

   fun {Script Meaning NumberOfElements}
      proc {$ Res} V in
	 Res = {Constrain {Reverse Meaning} NumberOfElements}
	 V = Res.1
	 {FS.distribute naive
	  for I in 1..{Width V} collect: Collect do
	     {Collect V.I.daughters}
	  end}
      end
   end

   proc {Order _#OldCost _#NewCost}
      OldCost >: NewCost
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
      fun {Smurf Meaning NumberOfElements} O Docs in
	 O = {New Search.object
	      script({Script Meaning NumberOfElements} Order)}
	 thread {SmurfSub O Docs} end
	 !!Docs
      end
   end

   SmurfModule = 'Smurf'(smurf: Smurf)
end
