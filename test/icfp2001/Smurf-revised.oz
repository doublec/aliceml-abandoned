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


   OFF = 1
   ON  = 2
   SW_DOM = [OFF ON]
   fun {NewSWVar} {FD.int SW_DOM} end

   EM_OFF = 1
   EM_ON  = 2
   EM_S   = 3
   EM_DOM = [EM_OFF EM_ON EM_S]
   fun {NewEMVar} {FD.int EM_DOM} end

   U_OFF = 1
   U_MAX = 4
   U_DOM = 1#4
   fun {NewUVar} {FD.int U_DOM} end

   SZ_UNDEF = 11
   SZ_DOM = 1#11
   fun {NewSZVar} {FD.int SZ_DOM} end

   CL_TABLE =
   unit(
      'R'       : 1
      'G'       : 2
      'B\''     : 3
      'C'       : 4
      'M'       : 5
      'Y'       : 6
      'K'       : 7
      'W'       : 8
      'UNKNOWN' : 9)
   CL_UNDEF = 9
   CL_DOM = 1#9
   fun {NewCLVar} {FD.int CL_DOM} end

   RootAttributes = attributes(b:OFF ems:EM_OFF i:OFF tt:OFF
			       u:U_OFF size:SZ_UNDEF color:CL_UNDEF)
   
   fun {MkDataItemAttributes Property IsSpace}
      attributes(b    : if IsSpace then {NewSWVar}
			elseif Property.b then ON else OFF
			end
		 ems  : if IsSpace then {NewEMVar}
			elseif Property.s then EM_S
			elseif Property.em then EM_ON
			else EM_OFF
			end
		 i    : if IsSpace then {NewSWVar}
			elseif Property.i then ON
			else OFF
			end
		 tt   : if IsSpace then {NewSWVar}
			elseif Property.tt then ON
			else OFF
			end
		 u    : Property.u + 1
		 size : case Property.size of ~1 then SZ_UNDEF elseof S then S + 1 end
		 color: if IsSpace then {NewCLVar}
			else CL_TABLE.(Property.color) end)
   end

   fun {MkElementAttributes}
      attributes(b    : {NewSWVar}
		 ems  : {NewEMVar}
		 i    : {NewSWVar}
		 tt   : {NewSWVar}
		 u    : {NewUVar}
		 size : {NewSZVar}
		 color: {NewCLVar})
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
			 case A of b then Out = ON
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
			 case A of i then Out = ON
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'PL'
		   p: proc {$ A In Out}
			 case A of b then Out = OFF
			 [] em then Out = EM_OFF
			 [] i then Out = OFF
			 [] tt then Out = OFF
			 [] u then Out = U_OFF
			 else Out = In
			 end
		      end
		   cost: 9)
	       tag(name: 'S'
		   p: proc {$ A In Out}
			 case A of em then Out = EM_S
			 else Out = In
			 end
		      end
		   cost: 7)
	       tag(name: 'TT'
		   p: proc {$ A In Out}
			 case A of tt then Out = ON
			 else Out = In
			 end
		      end
		   cost: 9)
	       tag(name: 'U'
		   p: proc {$ A In Out}
			 case A of u then Out = {FD.min {FD.plus In 1} U_MAX}
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
   TAG_MIN = Epsilon
   TAG_MAX = {Width Tags}
   TAG_DOM = TAG_MIN#TAG_MAX
   fun {NewTAGVar} {FD.int TAG_DOM} end
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

      DATA_DOM = FirstDataItemI#LastDataItemI
      NON_ROOT_DOM = FirstNonRootI#LastNonRootI
      NODE_DOM = FirstVertexI#LastVertexI

      fun {NewDataVar} {FD.int DATA_DOM} end
      fun {NewNonRootVar} {FD.int NON_ROOT_DOM} end
      fun {NewNodeVar} {FD.int NODE_DOM} end

      fun {NewDataSetVar} {FS.var.upperBound DATA_DOM} end
      fun {NewNonRootSetVar} {FS.var.upperBound NON_ROOT_DOM} end
      fun {NewNodeSetVar} {FS.var.upperBound NODE_DOM} end

      V = {Tuple.make vertices NumberOfVertices}

      %% Initialize root vertex
      V.RootI = root(daughters : {NewNonRootSetVar}
		     scope     : {FS.value.make DATA_DOM}% constant set
		     attributes: RootAttributes)

      %% Initialize data item vertices
      for
	 Text#IsSpace#Property in Meaning
	 J in 1;J+1
      do I = FirstDataItemI + J - 1 EQ={FS.value.singl I} in
	 V.I = dataItem(mother    : {NewNodeVar}
			daughters : FS.value.empty
			down      : FS.value.empty
			index     : I
			eq        : EQ
			eqdown    : EQ
			scope     : EQ
			attributes: {MkDataItemAttributes Property IsSpace}
			text      : Text)
      end

      %% Initialize element vertices
      for J in 1..NumberOfElements do
	 I = FirstElementI + J - 1
	 EQ={FS.value.singl I}
	 DOWN={NewNonRootSetVar}
	 EQDOWN={FS.partition [EQ DOWN]}
	 TAG={NewTAGVar}
	 UNUSED=(TAG=:Epsilon)
      in
	 V.I = element(mother    : {NewNodeVar}
		       daughters : {NewNonRootSetVar}
		       down      : DOWN
		       index     : I
		       eq        : EQ
		       eqdown    : EQDOWN
		       scope     : {NewDataSetVar}
		       tag       : TAG
		       unused    : UNUSED
		       attributes: {MkElementAttributes})
      end

      %% Treeness Constraints
      for I in FirstNonRootI..LastNonRootI do
	 V.I.mother \=: I
      end

      Eqdowns = for I in FirstNonRootI..LastNonRootI collect:Collect do
		   {Collect V.I.eqdown}
		end

      for I in FirstElementI..LastElementI do W = V.I in
	 {Select.union Eqdowns W.daughters W.down}
	 %% useless since enforced by the partition constraint
	 %% {FS.exclude I W.down}
	 %% this is now down earlier
	 %%W.eqdown = {FS.partition [{FS.value.singl I} W.down]}
      end

      for I1 in FirstVertexI..LastVertexI do W1 = V.I1 in
	 for I2 in FirstNonRootI..LastNonRootI do
	    if I2 \= I1 then W2 = V.I2 in
	       (W2.mother =: I1) =: {FS.reified.isIn I2 W1.daughters}
	    end
	 end
      end

      %% Collect daughters just once
      AllDaughters = {Record.map V fun {$ W} W.daughters end}

      %% wrong! not just a disjointess constraint
      %%{FS.disjointN for I in FirstVertexI..LastVertexI collect: Collect do
	%%	       {Collect V.I.daughters}
		%%    end}
      %% must be a partition constraint (see readme)
      %% I think it must be this one (please check)
      {FS.partition AllDaughters
       {FS.value.make FirstNonRootI#LastNonRootI}}

      %% Attribute constraints
      Attributes = {List.toRecord attributes
		    {Map [b ems i tt u size color]
		     fun {$ A}
			A#for I in FirstVertexI..LastVertexI collect:Collect
			  do {Collect V.I.attributes.A}
			  end
		     end}}
      Ps = for Tag in 1..TAG_MAX collect:Collect do {Collect Tags.Tag.p} end

      for I in FirstElementI..LastElementI do W = V.I in
	 for A in AttributeNames do
	     Inherited = {Select.fd Attributes.A W.mother}
	 in
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

      %% useless, this is a constant set
      %%{FS.int.convex V.RootI.scope}
      {Select.union Scopes V.RootI.daughters V.RootI.scope}
      for I in FirstElementI..LastElementI do W in
	 W = V.I
	 {FS.int.convex W.scope}
	 {Select.union Scopes W.daughters W.scope}
      end

      %% Unused elements are immediate daughters of the root
      for I in FirstElementI..LastElementI do W = V.I in
	 W.unused =: {FS.reified.equal W.scope FS.value.empty}
	 W.unused =<: (W.mother =: RootI)
      end

      %% Cost function
      TagCosts = for I in 1..TAG_MAX collect: Collect do
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
	 {FS.distribute naive AllDaughters}
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
