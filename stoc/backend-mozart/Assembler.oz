%%%
%%% Authors:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%   Ralf Scheidhauer <scheidhr@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt and Ralf Scheidhauer, 1997
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%
%%% This file is part of Mozart, an implementation of Oz 3:
%%%    http://www.mozart-oz.org
%%%
%%% See the file "LICENSE" or
%%%    http://www.mozart-oz.org/LICENSE.html
%%% for information on usage and redistribution
%%% of this file, and for a DISCLAIMER OF ALL
%%% WARRANTIES.
%%%

%%
%% This file defines the procedure `Assemble' which takes a list of
%% machine instructions, applies peephole optimizations and returns
%% an AssemblerClass object.  Its methods provide for the output,
%% feeding and loading of assembled machine code.
%%
%% Notes:
%% -- The code may contain no backward-only references to labels that
%%    are not reached during a forward-scan through the code.
%% -- The definition(...) and definitionCopy(...) instructions differ
%%    from the format expected by the assembler proper:  An additional
%%    argument stores the code for the definition's body.  This way,
%%    less garbage is produced during code generation.
%%

functor
import
   System(printName)
   CompilerSupport at 'x-oz://boot/CompilerSupport'
   Builtins(getInfo)
export
   InternalAssemble
   Assemble
define
   local
      GetInstructionSize = CompilerSupport.getInstructionSize
      GetOpcode          = CompilerSupport.getOpcode
      AllocateCodeBlock  = CompilerSupport.allocateCodeBlock
      StoreOpcode        = CompilerSupport.storeOpcode
      StoreNumber        = CompilerSupport.storeNumber
      StoreLiteral       = CompilerSupport.storeLiteral
      StoreFeature       = CompilerSupport.storeFeature
      StoreConstant      = CompilerSupport.storeConstant
      StoreInt           = CompilerSupport.storeInt
      StoreXRegIndex     = CompilerSupport.storeXRegisterIndex
      StoreYRegIndex     = CompilerSupport.storeYRegisterIndex
      StoreGRegIndex     = CompilerSupport.storeGRegisterIndex
      StoreProcedureRef  = CompilerSupport.storeProcedureRef
      StoreRecordArity   = CompilerSupport.storeRecordArity
      StoreGRegRef       = CompilerSupport.storeGRegRef
      StoreLocation      = CompilerSupport.storeLocation
      StoreCache         = CompilerSupport.storeCache
      StoreBuiltinname   = CompilerSupport.storeBuiltinname

      proc {StoreRegister CodeBlock Register}
	 case Register of x(I) then
	    {StoreXRegIndex CodeBlock I}
	 [] y(I) then
	    {StoreYRegIndex CodeBlock I}
	 [] g(I) then
	    {StoreGRegIndex CodeBlock I}
	 end
      end

      proc {StoreXRegisterIndex CodeBlock x(Index)}
	 {StoreXRegIndex CodeBlock Index}
      end

      proc {StoreYRegisterIndex CodeBlock y(Index)}
	 {StoreYRegIndex CodeBlock Index}
      end

      proc {StoreGRegisterIndex CodeBlock g(Index)}
	 {StoreGRegIndex CodeBlock Index}
      end

      local
	 BIStoreLabel = CompilerSupport.storeLabel
      in
	 proc {StoreLabel CodeBlock Lbl LabelDict}
	    {BIStoreLabel CodeBlock {Dictionary.get LabelDict Lbl}}
	 end
      end

      local
	 BIStorePredId = CompilerSupport.storePredId
      in
	 proc {StorePredId CodeBlock pid(Name Arity Pos Flags NLiveRegs)}
	    {BIStorePredId CodeBlock Name Arity Pos Flags NLiveRegs}
	 end
      end

      local
	 BINewHashTable  = CompilerSupport.newHashTable
      in
	 proc {StoreHashTableRef CodeBlock ht(ElseLabel List) LabelDict}
	    {BINewHashTable
	     CodeBlock
	     {Dictionary.get LabelDict ElseLabel}
	     {Length List}
	     {Map List
	      fun {$ Entry}
		 case Entry
		 of onScalar(NumOrLit Lbl) then
		    scalar(NumOrLit {Dictionary.get LabelDict Lbl})
		 [] onRecord(Label RecordArity Lbl) then
		    record(Label RecordArity {Dictionary.get LabelDict Lbl})
		 end
	      end}}
	 end
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %%
      %% InstructionSizes maps instr labels to integers
      %%

      InstructionSizes =
      instructionSizes(
	 'skip': {GetInstructionSize 'skip'}
	 'definition': {GetInstructionSize 'definition'}
	 'definitionCopy': {GetInstructionSize 'definitionCopy'}
	 'endDefinition': {GetInstructionSize 'endDefinition'}
	 'move': {GetInstructionSize 'moveXX'}
	 'moveMove': {GetInstructionSize 'moveMoveXYXY'}
	 'createVariable': {GetInstructionSize 'createVariableX'}
	 'createVariableMove': {GetInstructionSize 'createVariableMoveX'}
	 'unify': {GetInstructionSize 'unifyXX'}
	 'putRecord': {GetInstructionSize 'putRecordX'}
	 'putList': {GetInstructionSize 'putListX'}
	 'putConstant': {GetInstructionSize 'putConstantX'}
	 'setVariable': {GetInstructionSize 'setVariableX'}
	 'setValue': {GetInstructionSize 'setValueX'}
	 'setConstant': {GetInstructionSize 'setConstant'}
	 'setProcedureRef': {GetInstructionSize 'setProcedureRef'}
	 'setVoid': {GetInstructionSize 'setVoid'}
	 'getRecord': {GetInstructionSize 'getRecordX'}
	 'getList': {GetInstructionSize 'getListX'}
	 'getListValVar': {GetInstructionSize 'getListValVarX'}
	 'unifyVariable': {GetInstructionSize 'unifyVariableX'}
	 'unifyValue': {GetInstructionSize 'unifyValueX'}
	 'unifyValVar': {GetInstructionSize 'unifyValVarXX'}
	 'unifyNumber': {GetInstructionSize 'unifyNumber'}
	 'unifyLiteral': {GetInstructionSize 'unifyLiteral'}
	 'unifyVoid': {GetInstructionSize 'unifyVoid'}
	 'getLiteral': {GetInstructionSize 'getLiteralX'}
	 'getNumber': {GetInstructionSize 'getNumberX'}
	 'allocateL': {GetInstructionSize 'allocateL'}
	 'allocateL1': {GetInstructionSize 'allocateL1'}
	 'allocateL2': {GetInstructionSize 'allocateL2'}
	 'allocateL3': {GetInstructionSize 'allocateL3'}
	 'allocateL4': {GetInstructionSize 'allocateL4'}
	 'allocateL5': {GetInstructionSize 'allocateL5'}
	 'allocateL6': {GetInstructionSize 'allocateL6'}
	 'allocateL7': {GetInstructionSize 'allocateL7'}
	 'allocateL8': {GetInstructionSize 'allocateL8'}
	 'allocateL9': {GetInstructionSize 'allocateL9'}
	 'allocateL10': {GetInstructionSize 'allocateL10'}
	 'deAllocateL': {GetInstructionSize 'deAllocateL'}
	 'deAllocateL1': {GetInstructionSize 'deAllocateL1'}
	 'deAllocateL2': {GetInstructionSize 'deAllocateL2'}
	 'deAllocateL3': {GetInstructionSize 'deAllocateL3'}
	 'deAllocateL4': {GetInstructionSize 'deAllocateL4'}
	 'deAllocateL5': {GetInstructionSize 'deAllocateL5'}
	 'deAllocateL6': {GetInstructionSize 'deAllocateL6'}
	 'deAllocateL7': {GetInstructionSize 'deAllocateL7'}
	 'deAllocateL8': {GetInstructionSize 'deAllocateL8'}
	 'deAllocateL9': {GetInstructionSize 'deAllocateL9'}
	 'deAllocateL10': {GetInstructionSize 'deAllocateL10'}
	 'callGlobal': {GetInstructionSize 'callGlobal'}
	 'call': {GetInstructionSize 'callX'}
	 'tailCall': {GetInstructionSize 'tailCallX'}
	 'callConstant': {GetInstructionSize 'callConstant'}
	 'callProcedureRef': {GetInstructionSize 'callProcedureRef'}
	 'branch': {GetInstructionSize 'branch'}
	 'exHandler': {GetInstructionSize 'exHandler'}
	 'popEx': {GetInstructionSize 'popEx'}
	 'return': {GetInstructionSize 'return'}
	 'getReturn': {GetInstructionSize 'getReturnX'}
	 'funReturn': {GetInstructionSize 'funReturnX'}
	 'testLiteral': {GetInstructionSize 'testLiteralX'}
	 'testNumber': {GetInstructionSize 'testNumberX'}
	 'testRecord': {GetInstructionSize 'testRecordX'}
	 'testList': {GetInstructionSize 'testListX'}
	 'testBool': {GetInstructionSize 'testBoolX'}
	 'match': {GetInstructionSize 'matchX'}
	 'getVariable': {GetInstructionSize 'getVariableX'}
	 'getVarVar': {GetInstructionSize 'getVarVarXX'}
	 'getVoid': {GetInstructionSize 'getVoid'}
	 'profileProc': {GetInstructionSize 'profileProc'}
	 'callBI': {GetInstructionSize 'callBI'}
	 'inlinePlus1': {GetInstructionSize 'inlinePlus1'}
	 'inlineMinus1': {GetInstructionSize 'inlineMinus1'}
	 'inlinePlus': {GetInstructionSize 'inlinePlus'}
	 'inlineMinus': {GetInstructionSize 'inlineMinus'}
	 'inlineDot': {GetInstructionSize 'inlineDot'}
	 'testBI': {GetInstructionSize 'testBI'}
	 'testLT': {GetInstructionSize 'testLT'}
	 'testLE': {GetInstructionSize 'testLE'}
	 'deconsCall': {GetInstructionSize 'deconsCallX'}
	 'tailDeconsCall': {GetInstructionSize 'tailDeconsCallX'}
	 'consCall': {GetInstructionSize 'consCallX'}
	 'tailConsCall': {GetInstructionSize 'tailConsCallX'})

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %%
      %% Opcodes maps instructions with implicit addressing modes to integers
      %%

      Opcodes =
      opcodes(
	 'skip': {GetOpcode 'skip'}
	 'definition': {GetOpcode 'definition'}
	 'definitionCopy': {GetOpcode 'definitionCopy'}
	 'endDefinition': {GetOpcode 'endDefinition'}
	 'moveXX': {GetOpcode 'moveXX'}
	 'moveXY': {GetOpcode 'moveXY'}
	 'moveYX': {GetOpcode 'moveYX'}
	 'moveYY': {GetOpcode 'moveYY'}
	 'moveGX': {GetOpcode 'moveGX'}
	 'moveGY': {GetOpcode 'moveGY'}
	 'moveMoveXYXY': {GetOpcode 'moveMoveXYXY'}
	 'moveMoveYXYX': {GetOpcode 'moveMoveYXYX'}
	 'moveMoveXYYX': {GetOpcode 'moveMoveXYYX'}
	 'moveMoveYXXY': {GetOpcode 'moveMoveYXXY'}
	 'createVariableX': {GetOpcode 'createVariableX'}
	 'createVariableY': {GetOpcode 'createVariableY'}
	 'createVariableMoveX': {GetOpcode 'createVariableMoveX'}
	 'createVariableMoveY': {GetOpcode 'createVariableMoveY'}
	 'unifyXX': {GetOpcode 'unifyXX'}
	 'unifyXY': {GetOpcode 'unifyXY'}
	 'unifyXG': {GetOpcode 'unifyXG'}
	 'putRecordX': {GetOpcode 'putRecordX'}
	 'putRecordY': {GetOpcode 'putRecordY'}
	 'putListX': {GetOpcode 'putListX'}
	 'putListY': {GetOpcode 'putListY'}
	 'putConstantX': {GetOpcode 'putConstantX'}
	 'putConstantY': {GetOpcode 'putConstantY'}
	 'setVariableX': {GetOpcode 'setVariableX'}
	 'setVariableY': {GetOpcode 'setVariableY'}
	 'setValue':
	    f(x: {GetOpcode 'setValueX'}
	      y: {GetOpcode 'setValueY'}
	      g: {GetOpcode 'setValueG'})
	 'setConstant': {GetOpcode 'setConstant'}
	 'setProcedureRef': {GetOpcode 'setProcedureRef'}
	 'setVoid': {GetOpcode 'setVoid'}
	 'getRecord':
	    f(x: {GetOpcode 'getRecordX'}
	      y: {GetOpcode 'getRecordY'}
	      g: {GetOpcode 'getRecordG'})
	 'getList':
	    f(x: {GetOpcode 'getListX'}
	      y: {GetOpcode 'getListY'}
	      g: {GetOpcode 'getListG'})
	 'getListValVarX': {GetOpcode 'getListValVarX'}
	 'unifyVariableX': {GetOpcode 'unifyVariableX'}
	 'unifyVariableY': {GetOpcode 'unifyVariableY'}
	 'unifyValue':
	    f(x: {GetOpcode 'unifyValueX'}
	      y: {GetOpcode 'unifyValueY'}
	      g: {GetOpcode 'unifyValueG'})
	 'unifyValVarXX': {GetOpcode 'unifyValVarXX'}
	 'unifyValVarXY': {GetOpcode 'unifyValVarXY'}
	 'unifyValVarYX': {GetOpcode 'unifyValVarYX'}
	 'unifyValVarYY': {GetOpcode 'unifyValVarYY'}
	 'unifyValVarGX': {GetOpcode 'unifyValVarGX'}
	 'unifyValVarGY': {GetOpcode 'unifyValVarGY'}
	 'unifyNumber': {GetOpcode 'unifyNumber'}
	 'unifyLiteral': {GetOpcode 'unifyLiteral'}
	 'unifyVoid': {GetOpcode 'unifyVoid'}
	 'getLiteral':
	    f(x: {GetOpcode 'getLiteralX'}
	      y: {GetOpcode 'getLiteralY'}
	      g: {GetOpcode 'getLiteralG'})
	 'getNumber':
	    f(x: {GetOpcode 'getNumberX'}
	      y: {GetOpcode 'getNumberY'}
	      g: {GetOpcode 'getNumberG'})
	 'allocateL': {GetOpcode 'allocateL'}
	 'allocateL1': {GetOpcode 'allocateL1'}
	 'allocateL2': {GetOpcode 'allocateL2'}
	 'allocateL3': {GetOpcode 'allocateL3'}
	 'allocateL4': {GetOpcode 'allocateL4'}
	 'allocateL5': {GetOpcode 'allocateL5'}
	 'allocateL6': {GetOpcode 'allocateL6'}
	 'allocateL7': {GetOpcode 'allocateL7'}
	 'allocateL8': {GetOpcode 'allocateL8'}
	 'allocateL9': {GetOpcode 'allocateL9'}
	 'allocateL10': {GetOpcode 'allocateL10'}
	 'deAllocateL': {GetOpcode 'deAllocateL'}
	 'deAllocateL1': {GetOpcode 'deAllocateL1'}
	 'deAllocateL2': {GetOpcode 'deAllocateL2'}
	 'deAllocateL3': {GetOpcode 'deAllocateL3'}
	 'deAllocateL4': {GetOpcode 'deAllocateL4'}
	 'deAllocateL5': {GetOpcode 'deAllocateL5'}
	 'deAllocateL6': {GetOpcode 'deAllocateL6'}
	 'deAllocateL7': {GetOpcode 'deAllocateL7'}
	 'deAllocateL8': {GetOpcode 'deAllocateL8'}
	 'deAllocateL9': {GetOpcode 'deAllocateL9'}
	 'deAllocateL10': {GetOpcode 'deAllocateL10'}
	 'callGlobal': {GetOpcode 'callGlobal'}
	 'call':
	    f(x: {GetOpcode 'callX'}
	      y: {GetOpcode 'callY'}
	      g: {GetOpcode 'callG'})
	 'tailCallX': {GetOpcode 'tailCallX'}
	 'tailCallG': {GetOpcode 'tailCallG'}
	 'callConstant': {GetOpcode 'callConstant'}
	 'callProcedureRef': {GetOpcode 'callProcedureRef'}
	 'branch': {GetOpcode 'branch'}
	 'exHandler': {GetOpcode 'exHandler'}
	 'popEx': {GetOpcode 'popEx'}
	 'return': {GetOpcode 'return'}
	 'getReturn':
	    f(x: {GetOpcode 'getReturnX'}
	      y: {GetOpcode 'getReturnY'})
	 'funReturn':
	    f(x: {GetOpcode 'funReturnX'}
	      y: {GetOpcode 'funReturnY'}
	      g: {GetOpcode 'funReturnG'})
	 'testLiteral':
	    f(x: {GetOpcode 'testLiteralX'}
	      y: {GetOpcode 'testLiteralY'}
	      g: {GetOpcode 'testLiteralG'})
	 'testNumber':
	    f(x: {GetOpcode 'testNumberX'}
	      y: {GetOpcode 'testNumberY'}
	      g: {GetOpcode 'testNumberG'})
	 'testRecord':
	    f(x: {GetOpcode 'testRecordX'}
	      y: {GetOpcode 'testRecordY'}
	      g: {GetOpcode 'testRecordG'})
	 'testList':
	    f(x: {GetOpcode 'testListX'}
	      y: {GetOpcode 'testListY'}
	      g: {GetOpcode 'testListG'})
	 'testBool':
	    f(x: {GetOpcode 'testBoolX'}
	      y: {GetOpcode 'testBoolY'}
	      g: {GetOpcode 'testBoolG'})
	 'match':
	    f(x: {GetOpcode 'matchX'}
	      y: {GetOpcode 'matchY'}
	      g: {GetOpcode 'matchG'})
	 'getVariableX': {GetOpcode 'getVariableX'}
	 'getVariableY': {GetOpcode 'getVariableY'}
	 'getVarVarXX': {GetOpcode 'getVarVarXX'}
	 'getVarVarXY': {GetOpcode 'getVarVarXY'}
	 'getVarVarYX': {GetOpcode 'getVarVarYX'}
	 'getVarVarYY': {GetOpcode 'getVarVarYY'}
	 'getVoid': {GetOpcode 'getVoid'}
	 'profileProc': {GetOpcode 'profileProc'}
	 'callBI': {GetOpcode 'callBI'}
	 'inlinePlus1': {GetOpcode 'inlinePlus1'}
	 'inlineMinus1': {GetOpcode 'inlineMinus1'}
	 'inlinePlus': {GetOpcode 'inlinePlus'}
	 'inlineMinus': {GetOpcode 'inlineMinus'}
	 'inlineDot': {GetOpcode 'inlineDot'}
	 'testBI': {GetOpcode 'testBI'}
	 'testLT': {GetOpcode 'testLT'}
	 'testLE': {GetOpcode 'testLE'}
	 'deconsCall':
	    f(x: {GetOpcode 'deconsCallX'}
	      y: {GetOpcode 'deconsCallY'}
	      g: {GetOpcode 'deconsCallG'})
	 'tailDeconsCall':
	    f(x: {GetOpcode 'tailDeconsCallX'}
	      g: {GetOpcode 'tailDeconsCallG'})
	 'consCall':
	    f(x: {GetOpcode 'consCallX'}
	      y: {GetOpcode 'consCallY'}
	      g: {GetOpcode 'consCallG'})
	 'tailConsCall':
	    f(x: {GetOpcode 'tailConsCallX'}
	      g: {GetOpcode 'tailConsCallG'}))

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %%
      %% The StoreInstr Procedure
      %%

      proc {StoreInstr Instr CodeBlock LabelDict}
	 case Instr of 'skip' then
	    {StoreOpcode CodeBlock Opcodes.'skip'}
	 [] 'definition'(X1 X2 X3 X4 X5) then
	    {StoreOpcode CodeBlock Opcodes.'definition'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreLabel CodeBlock X2 LabelDict}
	    {StorePredId CodeBlock X3}
	    {StoreProcedureRef CodeBlock X4}
	    {StoreGRegRef CodeBlock X5}
	 [] 'definitionCopy'(X1 X2 X3 X4 X5) then
	    {StoreOpcode CodeBlock Opcodes.'definitionCopy'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreLabel CodeBlock X2 LabelDict}
	    {StorePredId CodeBlock X3}
	    {StoreProcedureRef CodeBlock X4}
	    {StoreGRegRef CodeBlock X5}
	 [] 'endDefinition'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'endDefinition'}
	    {StoreLabel CodeBlock X1 LabelDict}
	 [] 'move'(X1=x(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveXX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'move'(X1=x(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveXY'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'move'(X1=y(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveYX'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'move'(X1=y(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveYY'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'move'(X1=g(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveGX'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'move'(X1=g(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveGY'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'moveMove'(X1=x(_) X2=y(_) X3=x(_) X4=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveMoveXYXY'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	    {StoreYRegisterIndex CodeBlock X4}
	 [] 'moveMove'(X1=y(_) X2=x(_) X3=y(_) X4=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveMoveYXYX'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreYRegisterIndex CodeBlock X3}
	    {StoreXRegisterIndex CodeBlock X4}
	 [] 'moveMove'(X1=x(_) X2=y(_) X3=y(_) X4=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveMoveXYYX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	    {StoreYRegisterIndex CodeBlock X3}
	    {StoreXRegisterIndex CodeBlock X4}
	 [] 'moveMove'(X1=y(_) X2=x(_) X3=x(_) X4=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'moveMoveYXXY'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	    {StoreYRegisterIndex CodeBlock X4}
	 [] 'createVariable'(X1=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'createVariableX'}
	    {StoreXRegisterIndex CodeBlock X1}
	 [] 'createVariable'(X1=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'createVariableY'}
	    {StoreYRegisterIndex CodeBlock X1}
	 [] 'createVariableMove'(X1=x(_) X2) then
	    {StoreOpcode CodeBlock Opcodes.'createVariableMoveX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'createVariableMove'(X1=y(_) X2) then
	    {StoreOpcode CodeBlock Opcodes.'createVariableMoveY'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'unify'(X1 X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyXX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'unify'(X1 X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyXY'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'unify'(X1 X2=g(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyXG'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreGRegisterIndex CodeBlock X2}
	 [] 'putRecord'(X1 X2 X3=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putRecordX'}
	    {StoreLiteral CodeBlock X1}
	    {StoreRecordArity CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	 [] 'putRecord'(X1 X2 X3=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putRecordY'}
	    {StoreLiteral CodeBlock X1}
	    {StoreRecordArity CodeBlock X2}
	    {StoreYRegisterIndex CodeBlock X3}
	 [] 'putList'(X1=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putListX'}
	    {StoreXRegisterIndex CodeBlock X1}
	 [] 'putList'(X1=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putListY'}
	    {StoreYRegisterIndex CodeBlock X1}
	 [] 'putConstant'(X1 X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putConstantX'}
	    {StoreConstant CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'putConstant'(X1 X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'putConstantY'}
	    {StoreConstant CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'setVariable'(X1=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'setVariableX'}
	    {StoreXRegisterIndex CodeBlock X1}
	 [] 'setVariable'(X1=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'setVariableY'}
	    {StoreYRegisterIndex CodeBlock X1}
	 [] 'setValue'(X1) then
	    Opcode = Opcodes.'setValue'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'setConstant'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'setConstant'}
	    {StoreConstant CodeBlock X1}
	 [] 'setProcedureRef'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'setProcedureRef'}
	    {StoreProcedureRef CodeBlock X1}
	 [] 'setVoid'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'setVoid'}
	    {StoreInt CodeBlock X1}
	 [] 'getRecord'(X1 X2 X3) then
	    Opcode = Opcodes.'getRecord'.{Label X3}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreLiteral CodeBlock X1}
	    {StoreRecordArity CodeBlock X2}
	    {StoreRegister CodeBlock X3}
	 [] 'getList'(X1) then
	    Opcode = Opcodes.'getList'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'getListValVar'(X1 X2 X3) then
	    {StoreOpcode CodeBlock Opcodes.'getListValVarX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	 [] 'unifyVariable'(X1=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyVariableX'}
	    {StoreXRegisterIndex CodeBlock X1}
	 [] 'unifyVariable'(X1=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyVariableY'}
	    {StoreYRegisterIndex CodeBlock X1}
	 [] 'unifyValue'(X1) then
	    Opcode = Opcodes.'unifyValue'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'unifyValVar'(X1=x(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarXX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'unifyValVar'(X1=x(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarXY'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'unifyValVar'(X1=y(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarYX'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'unifyValVar'(X1=y(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarYY'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'unifyValVar'(X1=g(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarGX'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'unifyValVar'(X1=g(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'unifyValVarGY'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'unifyNumber'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'unifyNumber'}
	    {StoreNumber CodeBlock X1}
	 [] 'unifyLiteral'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'unifyLiteral'}
	    {StoreLiteral CodeBlock X1}
	 [] 'unifyVoid'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'unifyVoid'}
	    {StoreInt CodeBlock X1}
	 [] 'getLiteral'(X1 X2) then
	    Opcode = Opcodes.'getLiteral'.{Label X2}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreLiteral CodeBlock X1}
	    {StoreRegister CodeBlock X2}
	 [] 'getNumber'(X1 X2) then
	    Opcode = Opcodes.'getNumber'.{Label X2}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreNumber CodeBlock X1}
	    {StoreRegister CodeBlock X2}
	 [] 'allocateL'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'allocateL'}
	    {StoreInt CodeBlock X1}
	 [] 'allocateL1' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL1'}
	 [] 'allocateL2' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL2'}
	 [] 'allocateL3' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL3'}
	 [] 'allocateL4' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL4'}
	 [] 'allocateL5' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL5'}
	 [] 'allocateL6' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL6'}
	 [] 'allocateL7' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL7'}
	 [] 'allocateL8' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL8'}
	 [] 'allocateL9' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL9'}
	 [] 'allocateL10' then
	    {StoreOpcode CodeBlock Opcodes.'allocateL10'}
	 [] 'deAllocateL' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL'}
	 [] 'deAllocateL1' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL1'}
	 [] 'deAllocateL2' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL2'}
	 [] 'deAllocateL3' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL3'}
	 [] 'deAllocateL4' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL4'}
	 [] 'deAllocateL5' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL5'}
	 [] 'deAllocateL6' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL6'}
	 [] 'deAllocateL7' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL7'}
	 [] 'deAllocateL8' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL8'}
	 [] 'deAllocateL9' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL9'}
	 [] 'deAllocateL10' then
	    {StoreOpcode CodeBlock Opcodes.'deAllocateL10'}
	 [] 'callGlobal'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'callGlobal'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'call'(X1 X2) then
	    Opcode = Opcodes.'call'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'tailCall'(X1=x(_) X2) then
	    {StoreOpcode CodeBlock Opcodes.'tailCallX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'tailCall'(X1=g(_) X2) then
	    {StoreOpcode CodeBlock Opcodes.'tailCallG'}
	    {StoreGRegisterIndex CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'callConstant'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'callConstant'}
	    {StoreConstant CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'callProcedureRef'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'callProcedureRef'}
	    {StoreProcedureRef CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'branch'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'branch'}
	    {StoreLabel CodeBlock X1 LabelDict}
	 [] 'exHandler'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'exHandler'}
	    {StoreLabel CodeBlock X1 LabelDict}
	 [] 'popEx' then
	    {StoreOpcode CodeBlock Opcodes.'popEx'}
	 [] 'return' then
	    {StoreOpcode CodeBlock Opcodes.'return'}
	 [] 'getReturn'(X1) then
	    Opcode = Opcodes.'getReturn'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'funReturn'(X1) then
	    Opcode = Opcodes.'funReturn'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'testLiteral'(X1 X2 X3) then
	    Opcode = Opcodes.'testLiteral'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreLiteral CodeBlock X2}
	    {StoreLabel CodeBlock X3 LabelDict}
	 [] 'testNumber'(X1 X2 X3) then
	    Opcode = Opcodes.'testNumber'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreNumber CodeBlock X2}
	    {StoreLabel CodeBlock X3 LabelDict}
	 [] 'testRecord'(X1 X2 X3 X4) then
	    Opcode = Opcodes.'testRecord'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreLiteral CodeBlock X2}
	    {StoreRecordArity CodeBlock X3}
	    {StoreLabel CodeBlock X4 LabelDict}
	 [] 'testList'(X1 X2) then
	    Opcode = Opcodes.'testList'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreLabel CodeBlock X2 LabelDict}
	 [] 'testBool'(X1 X2 X3) then
	    Opcode = Opcodes.'testBool'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreLabel CodeBlock X2 LabelDict}
	    {StoreLabel CodeBlock X3 LabelDict}
	 [] 'match'(X1 X2) then
	    Opcode = Opcodes.'match'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreHashTableRef CodeBlock X2 LabelDict}
	 [] 'getVariable'(X1=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVariableX'}
	    {StoreXRegisterIndex CodeBlock X1}
	 [] 'getVariable'(X1=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVariableY'}
	    {StoreYRegisterIndex CodeBlock X1}
	 [] 'getVarVar'(X1=x(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVarVarXX'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'getVarVar'(X1=x(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVarVarXY'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'getVarVar'(X1=y(_) X2=x(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVarVarYX'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'getVarVar'(X1=y(_) X2=y(_)) then
	    {StoreOpcode CodeBlock Opcodes.'getVarVarYY'}
	    {StoreYRegisterIndex CodeBlock X1}
	    {StoreYRegisterIndex CodeBlock X2}
	 [] 'getVoid'(X1) then
	    {StoreOpcode CodeBlock Opcodes.'getVoid'}
	    {StoreInt CodeBlock X1}
	 [] 'profileProc' then
	    {StoreOpcode CodeBlock Opcodes.'profileProc'}
	 [] 'callBI'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'callBI'}
	    {StoreBuiltinname CodeBlock X1}
	    {StoreLocation CodeBlock X2}
	 [] 'inlinePlus1'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'inlinePlus1'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'inlineMinus1'(X1 X2) then
	    {StoreOpcode CodeBlock Opcodes.'inlineMinus1'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	 [] 'inlinePlus'(X1 X2 X3) then
	    {StoreOpcode CodeBlock Opcodes.'inlinePlus'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	 [] 'inlineMinus'(X1 X2 X3) then
	    {StoreOpcode CodeBlock Opcodes.'inlineMinus'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	 [] 'inlineDot'(X1 X2 X3 X4) then
	    {StoreOpcode CodeBlock Opcodes.'inlineDot'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreFeature CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	    {StoreCache CodeBlock X4}
	 [] 'testBI'(X1 X2 X3) then
	    {StoreOpcode CodeBlock Opcodes.'testBI'}
	    {StoreBuiltinname CodeBlock X1}
	    {StoreLocation CodeBlock X2}
	    {StoreLabel CodeBlock X3 LabelDict}
	 [] 'testLT'(X1 X2 X3 X4) then
	    {StoreOpcode CodeBlock Opcodes.'testLT'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	    {StoreLabel CodeBlock X4 LabelDict}
	 [] 'testLE'(X1 X2 X3 X4) then
	    {StoreOpcode CodeBlock Opcodes.'testLE'}
	    {StoreXRegisterIndex CodeBlock X1}
	    {StoreXRegisterIndex CodeBlock X2}
	    {StoreXRegisterIndex CodeBlock X3}
	    {StoreLabel CodeBlock X4 LabelDict}
	 [] 'deconsCall'(X1) then
	    Opcode = Opcodes.'deconsCall'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'tailDeconsCall'(X1) then
	    Opcode = Opcodes.'tailDeconsCall'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	 [] 'consCall'(X1 X2) then
	    Opcode = Opcodes.'consCall'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 [] 'tailConsCall'(X1 X2) then
	    Opcode = Opcodes.'tailConsCall'.{Label X1}
	 in
	    {StoreOpcode CodeBlock Opcode}
	    {StoreRegister CodeBlock X1}
	    {StoreInt CodeBlock X2}
	 end
      end

      local
	 IsUniqueName           = CompilerSupport.isUniqueName
	 IsCopyableName         = CompilerSupport.isCopyableName
	 IsCopyableProcedureRef = CompilerSupport.isCopyableProcedureRef

	 fun {ListToVirtualString Vs In FPToIntMap}
	    case Vs of V|Vr then
	       {ListToVirtualString Vr
		In#' '#{MyValueToVirtualString V FPToIntMap} FPToIntMap}
	    [] nil then In
	    end
	 end

	 fun {TupleSub I N In Value FPToIntMap}
	    if I =< N then
	       {TupleSub I + 1 N
		In#' '#{MyValueToVirtualString Value.I FPToIntMap}
		Value FPToIntMap}
	    else In
	    end
	 end

	 fun {TupleToVirtualString Value FPToIntMap}
	    {TupleSub 2 {Width Value}
	     {Label Value}#'('#{MyValueToVirtualString Value.1 FPToIntMap}
	     Value FPToIntMap}#')'
	 end

	 fun {MyValueToVirtualString Val FPToIntMap}
	    if {IsName Val} then
	       case Val of true then 'true'
	       [] false then 'false'
	       [] unit then 'unit'
	       elseif {IsUniqueName Val} then
		  %--** these only work if the name's print name is friendly
		  %--** and all names' print names are distinct
		  '<U: '#{System.printName Val}#'>'
	       elseif {IsCopyableName Val} then
		  '<M: '#{System.printName Val}#'>'
	       else
		  '<N: '#{System.printName Val}#'>'
	       end
	    elseif {IsAtom Val} then
	       %% the atom must not be mistaken for a token
	       if {HasFeature InstructionSizes Val} then '\''#Val#'\''
	       else
		  case Val of lbl then '\'lbl\''
		  [] pid then '\'pid\''
		  [] ht then '\'ht\''
		  [] onScalar then '\'onScalar\''
		  [] onRecord then '\'onRecord\''
		  [] cmi then '\'cmi\''
		  [] pos then '\'pos\''
		  else
		     {Value.toVirtualString Val 0 0}
		  end
	       end
	    elseif {ForeignPointer.is Val} then I in
	       %% foreign pointers are assigned increasing integers
	       %% in order of appearance so that diffs are sensible
	       I = {ForeignPointer.toInt Val}
	       if {IsCopyableProcedureRef Val} then '<Q: '
	       else '<P: '
	       end#
	       case {Dictionary.condGet FPToIntMap I unit} of unit then N in
		  N = {Dictionary.get FPToIntMap 0} + 1
		  {Dictionary.put FPToIntMap 0 N}
		  {Dictionary.put FPToIntMap I N}
		  N
	       elseof V then
		  V
	       end#'>'
	    elsecase Val of V1|Vr then
	       {ListToVirtualString Vr
		'['#{MyValueToVirtualString V1 FPToIntMap}
		FPToIntMap}#']'
	    [] V1#V2 then
	       {MyValueToVirtualString V1 FPToIntMap}#"#"#
	       {MyValueToVirtualString V2 FPToIntMap}
	    elseif {IsTuple Val} then
	       {TupleToVirtualString Val FPToIntMap}
	    else
	       {Value.toVirtualString Val 1000 1000}
	    end
	 end
      in
	 fun {InstrToVirtualString Instr FPToIntMap}
	    if {IsAtom Instr} then
	       Instr
	    elsecase Instr of putConstant(C R) then
	       'putConstant('#{Value.toVirtualString C 1000 1000}#' '#
	       {MyValueToVirtualString R FPToIntMap}#')'
	    [] setConstant(C R) then
	       'setConstant('#{Value.toVirtualString C 1000 1000}#' '#
	       {MyValueToVirtualString R FPToIntMap}#')'
	    else
	       {TupleToVirtualString Instr FPToIntMap}
	    end
	 end
      end
   in
      class AssemblerClass
	 prop final
	 attr InstrsHd InstrsTl LabelDict Size
	 feat Profile controlFlowInfo
	 meth init(ProfileSwitch ControlFlowInfoSwitch)
	    InstrsHd <- 'skip'|@InstrsTl
	    LabelDict <- {NewDictionary}
	    Size <- InstructionSizes.'skip'
	    %% Code must not start at address 0, since this is interpreted as
	    %% NOCODE by the emulator - thus the dummy instruction 'skip'.
	    self.Profile = ProfileSwitch
	    self.controlFlowInfo = ControlFlowInfoSwitch
	 end
	 meth newLabel(?L)
	    L = {NewName}
	    {Dictionary.put @LabelDict L _}
	 end
	 meth declareLabel(L)
	    if {Dictionary.member @LabelDict L} then skip
	    else {Dictionary.put @LabelDict L _}
	    end
	 end
	 meth isLabelUsed(I $)
	    {Dictionary.member @LabelDict I}
	 end
	 meth setLabel(L)
	    if {Dictionary.member @LabelDict L} then
	       {Dictionary.get @LabelDict L} = @Size
	    else
	       {Dictionary.put @LabelDict L @Size}
	    end
	 end
	 meth checkLabels()
	    {ForAll {Dictionary.entries @LabelDict}
	     proc {$ L#V}
		if {IsFree V} then
		   {Exception.raiseError compiler(assembler undeclaredLabel L)}
		end
	     end}
	 end
	 meth append(Instr) NewTl in
	    case Instr
	    of definition(_ L _ _ _) then AssemblerClass, declareLabel(L)
	    [] definitionCopy(_ L _ _ _) then AssemblerClass, declareLabel(L)
	    [] endDefinition(L) then AssemblerClass, declareLabel(L)
	    [] branch(L) then AssemblerClass, declareLabel(L)
	    [] exHandler(L) then AssemblerClass, declareLabel(L)
	    [] testBI(_ _ L) then AssemblerClass, declareLabel(L)
	    [] testLT(_ _ _ L) then
	       AssemblerClass, declareLabel(L)
	    [] testLE(_ _ _ L) then
	       AssemblerClass, declareLabel(L)
	    [] testLiteral(_ _ L) then
	       AssemblerClass, declareLabel(L)
	    [] testNumber(_ _ L) then
	       AssemblerClass, declareLabel(L)
	    [] testBool(_ L1 L2) then
	       AssemblerClass, declareLabel(L1)
	       AssemblerClass, declareLabel(L2)
	    [] testRecord(_ _ _ L) then
	       AssemblerClass, declareLabel(L)
	    [] testList(_ L) then
	       AssemblerClass, declareLabel(L)
	    [] match(_ HT) then ht(L Cases) = HT in
	       AssemblerClass, declareLabel(L)
	       {ForAll Cases
		proc {$ Case}
		   case Case
		   of onScalar(_ L) then AssemblerClass, declareLabel(L)
		   [] onRecord(_ _ L) then AssemblerClass, declareLabel(L)
		   end
		end}
	    [] lockThread(L _) then AssemblerClass, declareLabel(L)
	    else skip
	    end
	    @InstrsTl = Instr|NewTl
	    InstrsTl <- NewTl
	    Size <- @Size + InstructionSizes.{Label Instr}
	    case Instr of definition(_ _ _ _ _) andthen self.Profile then
	       AssemblerClass, append(profileProc)
	    [] definitionCopy(_ _ _ _ _) andthen self.Profile then
	       AssemblerClass, append(profileProc)
	    else skip
	    end
	 end
	 meth output($) AddrToLabelMap FPToIntMap in
	    AssemblerClass, MarkEnd()
	    AddrToLabelMap = {NewDictionary}
	    FPToIntMap = {NewDictionary}
	    {Dictionary.put FPToIntMap 0 0}
	    {ForAll {Dictionary.entries @LabelDict}
	     proc {$ Label#Addr}
		if {IsDet Addr} then
		   {Dictionary.put AddrToLabelMap Addr Label}
		end
	     end}
	    '%% Code Size:\n'#@Size#' % words\n'#
	    AssemblerClass, OutputSub(@InstrsHd AddrToLabelMap FPToIntMap 0 $)
	 end
	 meth OutputSub(Instrs AddrToLabelMap FPToIntMap Addr ?VS)
	    case Instrs of Instr|Ir then LabelVS NewInstr VSRest NewAddr in
	       LabelVS = if {Dictionary.member AddrToLabelMap Addr} then
			    'lbl('#Addr#')'#
			    if Addr < 100 then '\t\t' else '\t' end
			 else '\t\t'
			 end
	       AssemblerClass, TranslateInstrLabels(Instr ?NewInstr)
	       VS = (LabelVS#{InstrToVirtualString NewInstr FPToIntMap}#'\n'#
		     VSRest)
	       NewAddr = Addr + InstructionSizes.{Label Instr}
	       AssemblerClass, OutputSub(Ir AddrToLabelMap FPToIntMap NewAddr
					 ?VSRest)
	    [] nil then
	       VS = ""
	    end
	 end
	 meth load(Globals ?P) CodeBlock in
	    AssemblerClass, MarkEnd()
	    {AllocateCodeBlock @Size Globals ?CodeBlock ?P}
	    {ForAll @InstrsHd
	     proc {$ Instr} {StoreInstr Instr CodeBlock @LabelDict} end}
	 end
	 meth MarkEnd()
	    @InstrsTl = nil
	 end

	 meth TranslateInstrLabels(Instr $)
	    case Instr of definition(X1 L X2 X3 X4) then A in
	       A = {Dictionary.get @LabelDict L}
	       definition(X1 A X2 X3 X4)
	    [] definitionCopy(X1 L X2 X3 X4) then A in
	       A = {Dictionary.get @LabelDict L}
	       definitionCopy(X1 A X2 X3 X4)
	    [] endDefinition(L) then A in
	       A = {Dictionary.get @LabelDict L}
	       endDefinition(A)
	    [] branch(L) then A in
	       A = {Dictionary.get @LabelDict L}
	       branch(A)
	    [] exHandler(L) then A in
	       A = {Dictionary.get @LabelDict L}
	       exHandler(A)
	    [] testBI(X1 X2 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testBI(X1 X2 A)
	    [] testLT(X1 X2 X3 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testLT(X1 X2 X3 A)
	    [] testLE(X1 X2 X3 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testLE(X1 X2 X3 A)
	    [] testLiteral(X1 X2 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testLiteral(X1 X2 A)
	    [] testNumber(X1 X2 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testNumber(X1 X2 A)
	    [] testRecord(X1 X2 X3 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testRecord(X1 X2 X3 A)
	    [] testList(X1 L) then A in
	       A = {Dictionary.get @LabelDict L}
	       testList(X1 A)
	    [] testBool(X1 L1 L2) then A1 A2 in
	       A1 = {Dictionary.get @LabelDict L1}
	       A2 = {Dictionary.get @LabelDict L2}
	       testBool(X1 A1 A2)
	    [] match(X HT) then ht(L Cases) = HT A NewCases in
	       A = {Dictionary.get @LabelDict L}
	       NewCases = {Map Cases
			   fun {$ Case}
			      case Case of onScalar(X L) then A in
				 A = {Dictionary.get @LabelDict L}
				 onScalar(X A)
			      [] onRecord(X1 X2 L) then A in
				 A = {Dictionary.get @LabelDict L}
				 onRecord(X1 X2 A)
			      end
			   end}
	       match(X ht(A NewCases))
	    [] lockThread(L X) then A in
	       A = {Dictionary.get @LabelDict L}
	       lockThread(A X)
	    else
	       Instr
	    end
	 end
      end
   end

   proc {GetClears Instrs ?Clears ?Rest}
      case Instrs of I1|Ir then
	 case I1 of clear(_) then Cr in
	    Clears = I1|Cr
	    {GetClears Ir ?Cr ?Rest}
	 else
	    Clears = nil
	    Rest = Instrs
	 end
      [] nil then
	 Clears = nil
	 Rest = nil
      end
   end

   proc {SetVoids Instrs InI ?OutI ?Rest}
      case Instrs of I1|Ir then
	 case I1 of setVoid(J) then
	    {SetVoids Ir InI + J ?OutI ?Rest}
	 else
	    OutI = InI
	    Rest = Instrs
	 end
      [] nil then
	 OutI = InI
	 Rest = nil
      end
   end

   proc {UnifyVoids Instrs InI ?OutI ?Rest}
      case Instrs of I1|Ir then
	 case I1 of unifyVoid(J) then
	    {UnifyVoids Ir InI + J ?OutI ?Rest}
	 else
	    OutI = InI
	    Rest = Instrs
	 end
      [] nil then
	 OutI = InI
	 Rest = nil
      end
   end

   proc {GetVoids Instrs InI ?OutI ?Rest}
      case Instrs of I1|Ir then
	 case I1 of getVoid(J) then
	    {GetVoids Ir InI + J ?OutI ?Rest}
	 else
	    OutI = InI
	    Rest = Instrs
	 end
      [] nil then
	 OutI = InI
	 Rest = nil
      end
   end

   proc {MakeDeAllocate I Assembler}
      case I of 0 then skip
      [] 1 then {Assembler append(deAllocateL1)}
      [] 2 then {Assembler append(deAllocateL2)}
      [] 3 then {Assembler append(deAllocateL3)}
      [] 4 then {Assembler append(deAllocateL4)}
      [] 5 then {Assembler append(deAllocateL5)}
      [] 6 then {Assembler append(deAllocateL6)}
      [] 7 then {Assembler append(deAllocateL7)}
      [] 8 then {Assembler append(deAllocateL8)}
      [] 9 then {Assembler append(deAllocateL9)}
      [] 10 then {Assembler append(deAllocateL10)}
      else {Assembler append(deAllocateL)}
      end
   end

   fun {SkipDeadCode Instrs Assembler}
      case Instrs of I1|Rest then
	 case I1 of lbl(I) andthen {Assembler isLabelUsed(I $)} then Instrs
	 [] endDefinition(I) andthen {Assembler isLabelUsed(I $)} then Instrs
	 [] globalVarname(_) then Instrs
	 [] localVarname(_) then Instrs
	 else {SkipDeadCode Rest Assembler}
	 end
      [] nil then nil
      end
   end

   proc {EliminateDeadCode Instrs Assembler}
      {Peephole {SkipDeadCode Instrs Assembler} Assembler}
   end

   fun {HasLabel Instrs L}
      case Instrs of lbl(!L)|_ then true
      [] lbl(_)|Rest then {HasLabel Rest L}
      else false
      end
   end

   proc {Peephole Instrs Assembler}
      case Instrs of lbl(I)|Rest then
	 {Assembler setLabel(I)}
	 {Peephole Rest Assembler}
      [] definition(Register Label PredId ProcedureRef GRegRef Code)|Rest then
	 {Assembler
	  append(definition(Register Label PredId ProcedureRef GRegRef))}
	 {Peephole Code Assembler}
	 {Peephole Rest Assembler}
      [] definitionCopy(Register Label PredId ProcedureRef GRegRef Code)|Rest
      then
	 {Assembler
	  append(definitionCopy(Register Label PredId ProcedureRef GRegRef))}
	 {Peephole Code Assembler}
	 {Peephole Rest Assembler}
      [] clear(_)|_ then Clears Rest in
	 {GetClears Instrs ?Clears ?Rest}
	 case Rest of deAllocateL(_)|_ then skip
	 else
	    {ForAll Clears
	     proc {$ clear(Y)}
		{Assembler append(clear(Y))}
	     end}
	 end
	 {Peephole Rest Assembler}
      [] move(X1=x(_) Y1=y(_))|move(X2=x(_) Y2=y(_))|Rest then
	 {Assembler append(moveMove(X1 Y1 X2 Y2))}
	 {Peephole Rest Assembler}
      [] move(Y1=y(_) X1=x(_))|move(Y2=y(_) X2=x(_))|Rest then
	 {Assembler append(moveMove(Y1 X1 Y2 X2))}
	 {Peephole Rest Assembler}
      [] move(X1=x(_) Y1=y(_))|move(Y2=y(_) X2=x(_))|Rest then
	 {Assembler append(moveMove(X1 Y1 Y2 X2))}
	 {Peephole Rest Assembler}
      [] createVariable(R)|move(R X=x(_))|Rest then
	 {Peephole createVariableMove(R X)|Rest Assembler}
      [] createVariable(X=x(_))|move(X R)|Rest then
	 {Peephole createVariableMove(R X)|Rest Assembler}
      [] putRecord('|' 2 R)|Rest then
	 {Assembler append(putList(R))}
	 {Peephole Rest Assembler}
      [] setVoid(I)|Rest then OutI Rest1 in
	 {SetVoids Rest I ?OutI ?Rest1}
	 {Assembler append(setVoid(OutI))}
	 {Peephole Rest1 Assembler}
      [] getRecord('|' 2 X1=x(_))|
	 unifyValue(X2=x(_))|unifyVariable(X3=x(_))|Rest
      then
	 {Assembler append(getListValVar(X1 X2 X3))}
	 {Peephole Rest Assembler}
      [] getRecord('|' 2 R)|Rest then
	 {Assembler append(getList(R))}
	 {Peephole Rest Assembler}
      [] unifyValue(R1)|unifyVariable(R2)|Rest then
	 {Assembler append(unifyValVar(R1 R2))}
	 {Peephole Rest Assembler}
      [] unifyVoid(I)|Rest then OutI Rest1 in
	 {UnifyVoids Rest I ?OutI ?Rest1}
	 {Assembler append(unifyVoid(OutI))}
	 {Peephole Rest1 Assembler}
      [] (allocateL(I)=I1)|Rest then
	 case I of 0 then skip
	 [] 1 then {Assembler append(allocateL1)}
	 [] 2 then {Assembler append(allocateL2)}
	 [] 3 then {Assembler append(allocateL3)}
	 [] 4 then {Assembler append(allocateL4)}
	 [] 5 then {Assembler append(allocateL5)}
	 [] 6 then {Assembler append(allocateL6)}
	 [] 7 then {Assembler append(allocateL7)}
	 [] 8 then {Assembler append(allocateL8)}
	 [] 9 then {Assembler append(allocateL9)}
	 [] 10 then {Assembler append(allocateL10)}
	 else {Assembler append(I1)}
	 end
	 {Peephole Rest Assembler}
      [] deAllocateL(I)|return|(Rest=lbl(_)|deAllocateL(!I)|return|_) then
	 {Peephole Rest Assembler}
      [] deAllocateL(I)|Rest then
	 {MakeDeAllocate I Assembler}
	 {Peephole Rest Assembler}
      [] 'skip'|Rest then
	 {Peephole Rest Assembler}
      [] branch(L)|Rest then Rest1 in
	 {Assembler declareLabel(L)}
	 Rest1 = {SkipDeadCode Rest Assembler}
	 case Rest1 of lbl(!L)|_ then skip
	 else {Assembler append(branch(L))}
	 end
	 {Peephole Rest1 Assembler}
      [] return|Rest then
	 {Assembler append(return)}
	 {EliminateDeadCode Rest Assembler}
      [] (callBI(Builtinname Args)=I1)|Rest
	 andthen {Not Assembler.controlFlowInfo}
      then BIInfo in
	 BIInfo = {Builtins.getInfo Builtinname}
	 if {CondSelect BIInfo doesNotReturn false} then
	    case Rest of deAllocateL(I)|return|_ then
	       {MakeDeAllocate I Assembler}
	    else skip
	    end
	 end
	 case Builtinname of 'Int.\'+1\'' then [X1]#[X2] = Args in
	    {Assembler append(inlinePlus1(X1 X2))}
	 [] 'Int.\'-1\'' then [X1]#[X2] = Args in
	    {Assembler append(inlineMinus1(X1 X2))}
	 [] 'Number.\'+\'' then [X1 X2]#[X3] = Args in
	    {Assembler append(inlinePlus(X1 X2 X3))}
	 [] 'Number.\'-\'' then [X1 X2]#[X3] = Args in
	    {Assembler append(inlineMinus(X1 X2 X3))}
	 [] 'Value.\'>\'' then [X1 X2]#Out = Args in
	    {Assembler append(callBI('Value.\'<\'' [X2 X1]#Out))}
	 [] 'Value.\'>=\'' then [X1 X2]#Out = Args in
	    {Assembler append(callBI('Value.\'=<\'' [X2 X1]#Out))}
	 else
	    {Assembler append(I1)}
	 end
%--** this does not work with current liveness analysis
%--**	 if {CondSelect BIInfo doesNotReturn false} then
%--**	    {EliminateDeadCode Rest Assembler}
%--**	 else
	    {Peephole Rest Assembler}
%--**	 end
      [] callGlobal(G ArityAndIsTail)|deAllocateL(I)|return|Rest
	 andthen ArityAndIsTail mod 2 == 0
      then
	 {MakeDeAllocate I Assembler}
	 {Assembler append(callGlobal(G ArityAndIsTail + 1))}
	 {EliminateDeadCode Rest Assembler}
      [] call(R Arity)|deAllocateL(I)|return|Rest then NewR in
	 case R of y(_) then
	    {Assembler append(move(R NewR=x(Arity)))}
	 else
	    NewR = R
	 end
	 {MakeDeAllocate I Assembler}
	 {Assembler append(tailCall(NewR Arity))}
	 {EliminateDeadCode Rest Assembler}
      [] callProcedureRef(ProcedureRef ArityAndIsTail)|
	 deAllocateL(I)|return|Rest andthen ArityAndIsTail mod 2 == 0
      then
	 {MakeDeAllocate I Assembler}
	 {Assembler append(callProcedureRef(ProcedureRef ArityAndIsTail + 1))}
	 {EliminateDeadCode Rest Assembler}
      [] callConstant(Abstraction ArityAndIsTail)|
	 deAllocateL(I)|return|Rest
	 andthen {IsDet Abstraction}
	 andthen {IsProcedure Abstraction}
	 andthen ArityAndIsTail mod 2 == 0
      then
	 {MakeDeAllocate I Assembler}
	 {Assembler append(callConstant(Abstraction ArityAndIsTail + 1))}
	 {EliminateDeadCode Rest Assembler}
      [] (testBI(Builtinname Args L1)=I1)|Rest then NewInstrs in
	 case Rest of branch(L2)|NewRest then BIInfo in
	    BIInfo = {Builtins.getInfo Builtinname}
	    case {CondSelect BIInfo negated unit} of unit then skip
	    elseof NegatedBuiltinname then
	       NewInstrs = (testBI(NegatedBuiltinname Args L2)|
			    'skip'|branch(L1)|NewRest)
	    end
	 else skip
	 end
	 if {IsDet NewInstrs} then
	    {Peephole NewInstrs Assembler}
	 else
	    case Builtinname of 'Value.\'<\'' then [X1 X2]#[X3] = Args in
	       {Assembler append(testLT(X1 X2 X3 L1))}
	    [] 'Value.\'=<\'' then [X1 X2]#[X3] = Args in
	       {Assembler append(testLE(X1 X2 X3 L1))}
	    [] 'Value.\'>=\''then [X1 X2]#[X3] = Args in
	       {Assembler append(testLE(X2 X1 X3 L1))}
	    [] 'Value.\'>\'' then [X1 X2]#[X3] = Args in
	       {Assembler append(testLT(X2 X1 X3 L1))}
	    else
	       {Assembler append(I1)}
	    end
	    {Peephole Rest Assembler}
	 end
      [] testRecord(R '|' 2 L)|Rest then
	 {Assembler append(testList(R L))}
	 {Peephole Rest Assembler}
      [] match(R ht(ElseL [onScalar(X L)]))|Rest andthen {HasLabel Rest L} then
	 if {IsNumber X} then
	    {Assembler append(testNumber(R X ElseL))}
	 else
	    {Assembler append(testLiteral(R X ElseL))}
	 end
	 {Peephole Rest Assembler}
      [] match(R ht(ElseL [onRecord(Label RecordArity L)]))|Rest
	 andthen {HasLabel Rest L}
      then
	 case Label#RecordArity of '|'#2 then
	    {Assembler append(testList(R ElseL))}
	 else
	    {Assembler append(testRecord(R Label RecordArity ElseL))}
	 end
	 {Peephole Rest Assembler}
      [] (match(_ _)=I1)|Rest then
	 {Assembler append(I1)}
	 {EliminateDeadCode Rest Assembler}
      [] getVariable(R1)|getVariable(R2)|Rest then
	 {Assembler append(getVarVar(R1 R2))}
	 {Peephole Rest Assembler}
      [] getVoid(I)|Rest then OutI Rest1 in
	 {GetVoids Rest I ?OutI ?Rest1}
	 case Rest1 of getVariable(_)|_ then
	    {Assembler append(getVoid(OutI))}
	 else skip
	 end
	 {Peephole Rest1 Assembler}
      [] deconsCall(R)|deAllocateL(I)|return|Rest then NewR in
	 case R of y(_) then
	    {Assembler append(move(R NewR=x(2)))}
	 else
	    NewR = R
	 end
	 {MakeDeAllocate I Assembler}
	 {Assembler append(tailDeconsCall(NewR))}
	 {EliminateDeadCode Rest Assembler}
      [] consCall(R Arity)|deAllocateL(I)|return|Rest then NewR in
	 case R of y(_) then
	    {Assembler append(move(R NewR=x(Arity)))}
	 else
	    NewR = R
	 end
	 {MakeDeAllocate I Assembler}
	 {Assembler append(tailConsCall(NewR Arity))}
	 {EliminateDeadCode Rest Assembler}
      [] I1|Rest then
	 {Assembler append(I1)}
	 {Peephole Rest Assembler}
      [] nil then skip
      end
   end

   proc {InternalAssemble Code Switches ?Assembler}
      ProfileSwitch = {CondSelect Switches profile false}
      ControlFlowInfoSwitch = {CondSelect Switches controlflowinfo false}
      Verify = {CondSelect Switches verify true}
      DoPeephole = {CondSelect Switches peephole true}
   in
      Assembler = {New AssemblerClass
		   init(ProfileSwitch ControlFlowInfoSwitch)}
      if DoPeephole then
	 {Peephole Code Assembler}
      else
	 {ForAll Code
	  proc {$ Instr}
	     case Instr of lbl(I) then
		{Assembler setLabel(I)}
	     else
		{Assembler append(Instr)}
	     end
	  end}
      end
      if Verify then
	 {Assembler checkLabels()}
      end
   end

   proc {Assemble Code Globals Switches ?P ?VS}
      Assembler = {InternalAssemble Code Switches}
   in
      {Assembler load(Globals ?P)}
      VS = {ByNeed fun {$} {Assembler output($)} end}
   end
end
