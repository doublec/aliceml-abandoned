functor
import
   FS
   SelectFD(select:SFD)      at 'SelectFD.so{native}'
   SelectFS(select:SFS)      at 'SelectFS.so{native}'
   SelectUnion(union:Union) at 'SelectUnion.so{native}'
   SelectInter(inter:Inter) at 'SelectInter.so{native}'
   SeqUnionNat(sequnion:SeqUnion /*bugreset:BugReset*/) at 'SeqUnion.so{native}'
   SelectThe(the:The) at 'SelectThe.so{native}'
%   SelectSuchThatFDEQ(fd_select_such_that_eq:FDSuchThatEQ) at 'SelectSuchThatFDEQ.so{native}'
%   SelectSuchThatFDELT(fd_select_such_that_elt:FDSuchThatELT) at 'SelectSuchThatFDELT.so{native}'
export
   fd:SFD fs:SFS union:Union inter:Inter
   Fdom Fset SeqUnion %%BugReset
   The IndexedUnion
prepare
   fun {First  X#_} X end
   fun {Second _#Y} Y end
define
   Fdom = fdom(nth:SFD
	       /*suchThat:
		  o(eq:FDSuchThatEQ
		    elt:FDSuchThatELT)*/)

   proc {IndexedUnion R Sel Res}
      KeyVals  = if {IsList R} then R else {Record.toListInd R} end
      Keys     = {Map KeyVals First}
      Vals     = {Map KeyVals Second}
      Selector = {FS.var.upperBound 1#{Length Keys}}
      KeySets  = {Map Keys FS.value.make}
   in
      {Union Vals    Selector Res}
      {Union KeySets Selector Sel}
   end

   Fset = fset(nth:SFS
	       union:Union
	       indexedUnion:IndexedUnion
	       inter:Inter
	       seqUnion:SeqUnion
	       the:The)
end

   