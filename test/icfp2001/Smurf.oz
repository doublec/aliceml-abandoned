functor
import
   FD
   FS
   Search
   Inspector
   Select at 'x-ozlib://duchier/cp/Select.ozf'
export
   'Smurf$': SmurfModule
define
   Tags = tags(0: unit   % epsilon
	       1: 'B'
	       2: 'EM'
	       3: 'I'
	       4: 'PL'
	       5: 'S'
	       6: 'TT'
	       7: 'U'
	       8: 'SIZE'(0)
	       9: 'SIZE'(1)
	       10: 'SIZE'(2)
	       11: 'SIZE'(3)
	       12: 'SIZE'(4)
	       13: 'SIZE'(5)
	       14: 'SIZE'(6)
	       15: 'SIZE'(7)
	       16: 'SIZE'(8)
	       17: 'SIZE'(9)
	       18: 'COLOR'('R')
	       19: 'COLOR'('G')
	       20: 'COLOR'('B\'')
	       21: 'COLOR'('C')
	       22: 'COLOR'('M')
	       23: 'COLOR'('Y')
	       24: 'COLOR'('K')
	       25: 'COLOR'('W'))

   RootI = 0
   Epsilon = 0

   fun {Constrain Meaning NumberOfElements}
      NumberOfDataItems = {Length Meaning}
      NumberOfVertices = NumberOfDataItems + NumberOfElements

      %% Root is vertex with number 0
      Root = root(daughters: {FS.var.upperBound 1#NumberOfVertices}
		  scope: {FS.var.upperBound 1#NumberOfDataItems})
      DataItems = {List.mapInd Meaning
		   fun {$ I Text#_#_}
		      dataItem(mother: {FD.int 0#NumberOfVertices}
			       daughters: FS.value.empty
			       down: FS.value.empty
			       eqdown: {FS.value.singl I}
			       scope: {FS.value.singl I}
			       text: Text)
		   end}
      Elements = for I in 1..NumberOfElements collect: Collect do
		    {Collect
		     element(mother: {FD.int 0#NumberOfVertices}
			     daughters: {FS.var.upperBound 1#NumberOfVertices}
			     down: {FS.var.upperBound 1#NumberOfVertices}
			     eqdown: {FS.var.upperBound 0#NumberOfVertices}
			     scope: {FS.var.upperBound 1#NumberOfDataItems}
			     tag: {FD.int 0#25})}
		 end

      V = {AdjoinAt
	   {List.toTuple vertices {Append DataItems Elements}}
	   RootI Root}

      %% Treeness Constraints
      for I in 1..NumberOfVertices do
	 V.I.mother \=: I
      end

      Eqdowns = for I in 1..NumberOfVertices collect: Collect do
		   {Collect V.I.eqdown}
		end

      for I in 1..NumberOfElements do W in
	 W = V.(NumberOfDataItems + I)
	 {Select.union Eqdowns W.daughters W.down}
	 {FS.exclude I W.down}
	 W.eqdown = {FS.partition [{FS.value.singl NumberOfDataItems + I}
				   W.down]}
      end

      for I in 0..NumberOfVertices do W in
	 W = V.I
	 for I2 in 1..NumberOfVertices do
	    if I2 \= I then W2 in
	       W2 = V.I2
	       (W2.mother =: I) =: {FS.reified.isIn I2 W.daughters}
	    end
	 end
      end

      {FS.disjointN for I in 0..NumberOfVertices collect: Collect do
		       {Collect V.I.daughters}
		    end}

      %% Attribute constraints
      %%--** missing

      %% Scope constraints
      Scopes = for I in 0..NumberOfVertices collect: Collect do
		  {Collect V.I.scope}
	       end

      {FS.int.convex Root.scope}
      {Select.union Scopes Root.daughters Root.scope}
      for I in 1..NumberOfElements do W in
	 W = V.(NumberOfDataItems + I)
	 {FS.int.convex W.scope}
	 {Select.union Scopes W.daughters W.scope}
      end

      %% Unused elements are immediate daughters of the root
      for I in 1..NumberOfElements do W in
	 W = V.(NumberOfDataItems + I)
	 (W.tag =: Epsilon) =: {FS.reified.equal W.scope FS.value.empty}
	 (W.tag =: Epsilon) =<: (W.mother =: RootI)
      end
   in
      V
   end

   local
      fun {ToDocSub Daughters V}
	 {List.foldR {FS.reflect.lowerBoundList Daughters}
	  fun {$ I In}
	     case V.I of dataItem(text: Text ...) then
		'TEXT'(Text)|In
	     [] element(tag: !Epsilon ...) then In
	     [] element(tag: Tag daughters: Daughters ...) then
		'TAGGED'(Tags.Tag {ToDocSub Daughters V})|In
	     end
	  end nil}
      end
   in
      fun {ToDoc V}
	 {ToDocSub V.RootI.daughters V}
      end
   end

   fun {Smurf Meaning NumberOfElements}
      {ToDoc {Search.base.one
	      proc {$ V}
		 V = {Constrain {Reverse Meaning} NumberOfElements}
		 {FS.distribute naive
		  for I in 0..{Width V} - 1 collect: Collect do
		     {Collect V.I.daughters}
		  end}
	      end}.1}
   end

   SmurfModule = 'Smurf'(smurf: Smurf)

   SampleProperty = '#'(b: false em: false i: false s: false tt: false
			u: 0 size: ~1 color: 'UNKNOWN')
   SampleMeaning = [[{ByteString.make 'c'}]#false#SampleProperty
		    [{ByteString.make 'b'}]#false#SampleProperty
		    [{ByteString.make 'a'}]#false#SampleProperty]

   {Inspector.inspect {Smurf SampleMeaning 5}}
end
