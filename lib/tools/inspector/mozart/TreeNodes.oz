%%%
%%% Author:
%%%   Thorsten Brunklaus <bruni@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor $
import
   Inspector('nodes' : TreeNodes)
   CreateObjects
   LayoutObjects
   DrawObjects
export
   Nodes
define
   %% Specify Nodes to be exported
   NodeSpecs = [
		%% Generic Node
		generic # [generic generic base]

		%%
		%% Tree Mode
		%%
		
		%% Atomic Nodes
		int        # [a(int) int base]
		float      # [a(float) float base]
		atom       # [atom a(atom) base]
		name       # [name a(name) base]
		procedure  # [a(procedure) a(procedure) base]
		byteString # [a(byteString) a(byteString) base]
		word       # [a(word) a(word) base]
		cell       # [a(cell) labelTuple labelTuple]
		cellInd    # [a(cell) labelTupleInd labelTupleInd]
		
		%% Container Nodes
		tuple     # [a(tuple) a(tuple) labelTuple]
		vector    # [a(vector) a(tuple) labelTuple]
		vectorInd # [a(vector) a(vectorInd) a(vectorInd)]
		list      # [a(list) a(list) pipeTuple]
		record    # [a(record) a(tuple) labelTuple]
		recordInd # [a(recordInd) a(tuple) labelTuple]

		%% Logic-, Future- and Constraint-Variables
		free     # [free free free]
		future   # [future future future]
		fdInt    # [fdInt fdInt fdInt]
		fsVal    # [fsVal fdInt fdInt]
		fsHelper # [fsHelper fdInt fdInt]
		fsVar    # [fsVar fsVar fdInt]

		%%
		%% Relation Mode
		%%
		
		%% Relation Mode Helper
		variableRef # [variableRef variableRef variableRef]

		%% Atomic Nodes
		nameGr      # [a(nameGr) a(nameGr) a(nameGr)]
		cellGr      # [a(cellGr) labelTupleGr labelTupleGr]
		cellGrInd   # [a(cellGr) labelTupleGrInd labelTupleGrInd]

		%% Container Nodes
		tupleGr      # [a(tupleGr) a(tupleGr) labelTupleGr]
		vectorGr     # [a(vectorGr) a(tupleGr) labelTupleGr]
		vectorGrInd  # [a(vectorGr) a(vectorGrInd) a(vectorGrInd)]
		listGr       # [a(listGr) a(listGr) a(listGrM)]
		%% Required by TreeWidget
		pipetupleGrS # [a(listGr) a(listGr) a(listGrS)]
		recordGr     # [a(recordGr) a(tupleGr)  labelTupleGr]
		recordGrInd  # [a(recordGrInd) a(tupleGr) labelTupleGr]

		%% Logic-, Future- and Constraint-Variants
		freeGr   # [a(freeGr) freeGr freeGr]
		futureGr # [a(futureGr) futureGr futureGr]
		fdIntGr  # [a(fdIntGr) fdIntGr fdIntGr]
		fsValGr  # [a(fsValGr) fdIntGr fdIntGr]
		fsVarGr  # [a(fsVarGr) fsVarGr fdIntGr]
	       ]

   %% Node Builder
   local
      fun {CreateTrans Key}
	 if Key == base then createObject else {VirtualString.toAtom Key#'CreateObject'} end
      end
      fun {GetCreate Key}
	 case Key
	 of a(Key) then CreateObjects.{CreateTrans Key}
	 [] Key    then TreeNodes.'create'.{CreateTrans Key}
	 end
      end
      fun {LayoutTrans Key}
	 if Key == base then layoutObject else {VirtualString.toAtom Key#'LayoutObject'} end
      end
      fun {GetLayout Key}
	 case Key
	 of a(Key) then LayoutObjects.{LayoutTrans Key}
	 [] Key    then TreeNodes.'layout'.{LayoutTrans Key}
	 end
      end
      fun {DrawTrans Key}
	 if Key == base then drawObject else {VirtualString.toAtom Key#'DrawObject'} end
      end
      fun {GetDraw Key}
	 case Key
	 of a(Key) then DrawObjects.{DrawTrans Key}
	 [] Key    then TreeNodes.'draw'.{DrawTrans Key}
	 end
      end
   in
      fun {MakeNode [C L D]}
	 {Class.new [{GetCreate C} {GetLayout L} {GetDraw D}] 'attr' 'feat' [final]}
      end
   end
   
   %% Create the Export Record
   Nodes = {Record.make nodes {Map NodeSpecs fun {$ F#_} F end}}
   %% Assign Classes to Export Record
   case NodeSpecs
   of (Feat#Desc)|NodeSpecR then
      Nodes.Feat = Desc %% Assign all without mapping
      {List.forAll NodeSpecR proc {$ Feat#Desc} Nodes.Feat = {MakeNode Desc} end}
   end
end
