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

   RootAttrVal = attrVal(b:     1
			 ems:   1
			 i:     1
			 tt:    1
			 u:     1
			 size:  11
			 color: 9)

   RootAttrReq = attrReq(b:     FS.value.empty
			 ems:   FS.value.empty
			 i:     FS.value.empty
			 tt:    FS.value.empty
			 u:     FS.value.empty
			 size:  FS.value.empty
			 color: FS.value.empty)

   fun {MkAttrVal}
      attrVal(b:     {FD.int 1#2}
	      ems:   {FD.int 1#3}
	      i:     {FD.int 1#2}
	      tt:    {FD.int 1#2}
	      u:     {FD.int 1#4}
	      size:  {FD.int 1#11}
	      color: {FD.int 1#9})
   end

   local
      fun {MkAttrReq0 MaxVal} X in
	 X = {FS.var.upperBound 1#MaxVal}
	 {FS.cardRange 0 1 X}
	 X
      end
   in
      fun {MkAttrReq}
	 attrReq(b:     {MkAttrReq0 2}
		 ems:   {MkAttrReq0 3}
		 i:     {MkAttrReq0 2}
		 tt:    {MkAttrReq0 2}
		 u:     {MkAttrReq0 4}
		 size:  {MkAttrReq0 11}
		 color: {MkAttrReq0 9})
      end
   end

   local
      S1 = {FS.value.singl 1}
      S2 = {FS.value.singl 2}
      S3 = {FS.value.singl 3}
   in
      fun {MkDataItemAttrReq Property IsSpace}
	 attrReq(b: if IsSpace then FS.value.empty
		    elseif Property.b then S2 else S1
		    end
		 ems: if IsSpace then FS.value.empty
		      elseif Property.s then S3
		      elseif Property.em then S2
		      else S1
		      end
		 i: if IsSpace then FS.value.empty
		    elseif Property.i then S2
		    else S1
		    end
		 tt: if Property.tt then S2
		     else S1
		     end
		 u: {FS.value.singl Property.u + 1}
		 size:
		    {FS.value.singl case Property.size of ~1 then 11
				    elseof S then S + 1
				    end}
		 color:
		    if IsSpace andthen Property.u == 0 then FS.value.empty
		    else
		       {FS.value.singl case Property.color of 'R' then 1
				       [] 'G' then 2
				       [] 'B\'' then 3
				       [] 'C' then 4
				       [] 'M' then 5
				       [] 'Y' then 6
				       [] 'K' then 7
				       [] 'W' then 8
				       [] 'UNKNOWN' then 9
				       end}
		    end)
      end
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

   Epsilon = 1
   DataItemTag = 2

   Tags = tags(tag(p: proc {$ _ In Out} Out = In end)   % epsilon
	       tag(p: proc {$ _ In Out} Out = In end)   % data item
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

   MaxTag = {Width Tags}

   RootI = 1

   proc {Constrain Meaning SourceCost Res}
      NumberOfInnerNodes = SourceCost div Tag.minCost
      NumberOfDataItems  = {Length Meaning}
      NumberOfExtraNodes = (NumberOfDataItems + 1) div 2
      NumberOfVertices   = 1 + NumberOfInnerNodes + NumberOfExtraNodes

\ifdef DEBUG
      {System.show numberOf(innerNodes: NumberOfInnerNodes
			    dataItems:  NumberOfDataItems)}
\endif

      FirstNonRootI = RootI + 1
      LastNonRootI  = NumberOfVertices
      FirstVertexI  = RootI
      LastVertexI   = LastNonRootI

      V = {Tuple.make vertices LastVertexI}

      %% Initialize root vertex
      V.RootI =
      root(daughters:        {FS.var.upperBound FirstNonRootI#LastNonRootI}
	   down:             {FS.value.make FirstNonRootI#LastNonRootI}
	   eqdown:           {FS.value.make FirstVertexI#LastVertexI}
	   attrReq:          RootAttrReq
	   attrVal:          RootAttrVal
	   sigma:            FS.value.empty
	   familleNombreuse: {FD.int 0#1})

      %% Initialize element vertices
      for I in FirstNonRootI..LastNonRootI do
	 Mother           = {FD.int FirstVertexI#(I-1)}
	 Daughters        = {FS.var.upperBound (I+1)#LastNonRootI}
	 Down             = {FS.var.upperBound (I+1)#LastNonRootI}
	 Eqdown           = {FS.var.upperBound I#LastNonRootI}
	 AttrReq          = {MkAttrReq}
	 AttrVal          = {MkAttrVal}
	 Sigma            = {FS.var.upperBound 1#NumberOfDataItems}
	 FamilleNombreuse = {FD.int 0#1}
	 IsDataItemTag    = {FD.int 0#1}
	 Tag              = {FD.int 1#MaxTag}
      in
	 {FS.cardRange 0 1 Sigma}
	 V.I = nonRoot(mother:           Mother
		       daughters:        Daughters
		       down:             Down
		       eqdown:           Eqdown
		       attrReq:          AttrReq
		       attrVal:          AttrVal
		       sigma:            Sigma
		       familleNombreuse: FamilleNombreuse
		       isDataItemTag:    IsDataItemTag
		       tag:              Tag)
      end

      %% Treeness Constraints
      Eqdowns = for I in FirstVertexI..LastVertexI collect: Collect do
		   {Collect V.I.eqdown}
		end

      for I in FirstVertexI..LastVertexI do W in
	 W = V.I
	 W.down = {SeqUnion Eqdowns W.daughters}
	 W.eqdown = {FS.union {FS.value.singl I} W.down}
	 {FS.int.convex W.eqdown}
      end

      for I in FirstVertexI..LastVertexI do W in
	 W = V.I
	 W.familleNombreuse =: ({FS.card W.daughters} >: 1)
      end

      for I1 in FirstVertexI..LastVertexI do W1 in
	 W1 = V.I1
	 for I2 in FirstNonRootI..LastNonRootI do   %--** I1..?
	    if I2 \= I1 then
	       W2 = V.I2
	       IsMother = (W2.mother =: I1)
	    in
	       IsMother =: {FS.reified.isIn I2 W1.daughters}
	       {FD.conj IsMother W2.isDataItemTag} =<: W1.familleNombreuse
	    end
	 end
      end

      {FS.value.make FirstNonRootI#LastNonRootI} =
      {FS.partition for I in FirstVertexI..LastVertexI collect: Collect do
		       {Collect V.I.daughters}
		    end}

      {FS.value.make 1#NumberOfDataItems} =
      {Select.seqUnion for I in FirstVertexI..LastVertexI collect: Collect do
			  {Collect V.I.sigma}
		       end}

      %% Properties of V+ and V-
      VPlus  = {FS.var.upperBound FirstNonRootI#LastNonRootI}
      VMinus = {FS.var.upperBound FirstNonRootI#LastNonRootI}

      {FS.value.make FirstVertexI#LastVertexI} =
      {Select.seqUnion [{FS.value.singl RootI} VPlus VMinus]}

      for I in FirstNonRootI..LastNonRootI do
	 W = V.I
	 IsInVPlus        = {FS.reified.isIn I VPlus}
	 IsInVMinus       = {FS.reified.isIn I VMinus}
	 IsDownEmpty      = {FS.reified.equal W.down FS.value.empty}
	 IsSigmaEmpty     = {FS.reified.equal W.sigma FS.value.empty}
	 IsNonDataItemTag = (W.tag \=: DataItemTag)
      in
	 W.isDataItemTag \=: IsNonDataItemTag
	 IsInVMinus =<: (W.mother =: RootI)
	 IsInVMinus =<: IsDownEmpty
	 IsInVMinus =<: IsSigmaEmpty
	 IsInVMinus =: (W.tag =: Epsilon)
	 IsInVPlus =: (IsDownEmpty \=: IsSigmaEmpty)
	 IsSigmaEmpty =<: IsNonDataItemTag
      end

      %% Attribute constraints
      DataItemAttrReqs = {List.mapInd Meaning
			  fun {$ I _#IsSpace#Property}
			     {MkDataItemAttrReq Property IsSpace}
			  end}

      for A in AttributeNames do
	 AttrReqSeq = {Map DataItemAttrReqs fun {$ AttrReq} AttrReq.A end}
	 AttrValSeq = for I in FirstVertexI..LastVertexI collect: Collect do
			 {Collect V.I.attrVal.A}
		      end
      in
	 for I in FirstVertexI..LastVertexI do W in
	    W = V.I
	    W.attrReq.A = {Select.union AttrReqSeq W.sigma}
	    {FS.subset W.attrReq.A {Select.the $ W.attrVal.A}}
	    if I \= RootI then Inherited in
	       Inherited = {Select.fd AttrValSeq W.mother}
	       W.attrVal.A = {Select.fd
			      for Tag in 1..MaxTag collect: Collect do
				 {Collect {Tags.Tag.p A Inherited}}
			      end
			      W.tag}
	    end
	 end
      end

      %% Cost function
      TagCosts = for I in 1..MaxTag collect: Collect do
		    {Collect case I of !Epsilon then 0
			     [] !DataItemTag then 0
			     else {Tag.cost Tags.I.name}
			     end}
		 end

      Cost = {FD.int 0#SourceCost}
      Cost = {FD.sum for I in FirstNonRootI..LastNonRootI collect: Collect do
			{Collect {Select.fd TagCosts V.I.tag}}
		     end '=:'}
   in
      Res = V#Cost

      {FS.cardRange NumberOfDataItems NumberOfVertices - 1 VPlus}
      {FD.distribute naive [{FS.card VPlus}]}

      {FD.distribute ff
       {Append
	for I in 1..{Width V} collect: Collect do
	   if I \= RootI then
	      {Collect V.I.mother}
	   end
	end
	for I in 1..{Width V} collect: Collect do
	   if I \= RootI then
	      {Collect V.I.tag}
	   end
	end}}
   end

   fun {Script Meaning0 SourceCost}
      Meaning = {Reverse Meaning0}
   in
      proc {$ Res}
	 {Constrain Meaning SourceCost ?Res}
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
	 %--** do we still need sorting?
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
