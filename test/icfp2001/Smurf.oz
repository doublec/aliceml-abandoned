functor
import
   FD
   FS
   Explorer
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
      V = {List.toRecord '#'
	   0#Root|
	   {ForThread NumberOfDataItems 1 ~1
	    fun {$ In I}
	       I#dataItem(mother: {FD.int 0#NumberOfVertices}
			  daughters: FS.value.empty
			  down: FS.value.empty
			  eqdown: {FS.value.singl I}
			  scope: {FS.value.singl I}
			 )|In
	    end
	    {ForThread NumberOfElements 1 ~1
	     fun {$ In I}
		(NumberOfDataItems + I)#
		element(mother: {FD.int 0#NumberOfVertices}
			daughters: {FS.var.upperBound 1#NumberOfVertices}
			down: {FS.var.upperBound 1#NumberOfVertices}
			eqdown: {FS.var.upperBound 0#NumberOfVertices}
			scope: {FS.var.upperBound 1#NumberOfDataItems}
			tag: {FD.int 0#25}
		       )|In
	     end nil}}}

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

   fun {Smurf Meaning NumberOfElements}
      nil   %--** implement
   end

   SmurfModule = 'Smurf'(smurf: Smurf)

   {Explorer.one proc {$ V}
		    V = {Constrain [1 2 3] 5}
		    {FS.distribute naive
		     for I in 0..{Width V} - 1 collect: Collect do
			{Collect V.I.daughters}
		     end}
		 end}
end
