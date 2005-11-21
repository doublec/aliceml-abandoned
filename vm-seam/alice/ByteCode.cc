//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/ByteCode.hh"
#endif

#include <cstdio>
#include "alice/ByteCode.hh"
using namespace ByteCodeInstr;

#ifdef THREADED
void **ByteCode::instrTable = NULL;
word ByteCode::threadedTable = INVALID_POINTER;
#endif // THREADED

// this is a naive disassemble
// are more sofisticated one would expand immediate-env addresses
ProgramCounter ByteCode::DisassembleOne(std::FILE *f, ProgramCounter PC, 
					Chunk *code, Tuple *imEnv) {
  ReadBuffer *codeBuffer = ReadBuffer::New(code);

  fprintf(f,"PC %d: ",PC);
  u_int instr;
  GET_INSTR(codeBuffer,PC,instr);

#ifdef THREADED			
  instr = ThreadedToNumber((void*)instr);
#endif

  switch(instr) {
  case await:
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"await R%d\n",reg);
    }
    return PC;
  case iadd:
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"iadd R%d, R%d, R%d\n",r0,r1,r2);
    }
    return PC;
  case isub:
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"isub R%d, R%d, R%d\n",r0,r1,r2);
    }
    return PC;
  case idec:
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"idec R%d, R%d\n",r0,r1);
    }
    return PC;
  case iinc:
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"iinc R%d, R%d\n",r0,r1);
    }
    return PC;
  case set_global: // reg, index
    {
      GET_1R1I(codeBuffer,PC,reg,index);
      fprintf(f,"set_global R%d, %d\n",reg,index);
    }
    return PC;
  case seam_set_nargs: // nargs
    {
      GET_1I(codeBuffer,PC,nargs);
      fprintf(f,"seam_set_nargs %d\n",nargs);
    }
    return PC;
  case seam_set_sreg: // reg, index
    {
      GET_1R1I(codeBuffer,PC,reg,index);
      fprintf(f,"seam_set_sreg R%d, %d\n",reg,index);      
    }
    return PC;
  case spec_closure: // r0,r1,i0,i1
    {
      GET_2R2I(codeBuffer,PC,r0,r1,i0,i1);
      fprintf(f,"spec_closure R%d, R%d, %d, %d\n",r0,r1,i0,i1);
    }
    return PC;
  case ccc1: 
    {
      fprintf(f,"ccc1\n");
    }
    return PC;
  case cccn: // nArgs
    {
      GET_1I(codeBuffer,PC,nArgs);
      fprintf(f,"cccn %d\n",nArgs);
    }
    return PC;
  case seam_ccc1: 
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"seam_ccc1 R%d\n",reg);
    }
    return PC;
  case seam_cccn: // args
    {
      GET_1I(codeBuffer,PC,argsAddr);
      Vector *args = Vector::FromWordDirect(imEnv->Sel(argsAddr));
      fprintf(f,"seam_cccn [");
      for(u_int i=0; i<args->GetLength(); i++) {
	if(i>0) fprintf(stderr,",");
	TagVal *argOpt = TagVal::FromWord(args->Sub(i));
	if(argOpt != INVALID_POINTER) {
	  fprintf(stderr,"R%d",Store::DirectWordToInt(argOpt->Sel(0)));
	} else
	  fprintf(stderr,"_");
      }
      fprintf(stderr,"]\n");
    }
    return PC;
  case seam_call: // r, n
    {
      GET_1R1I(codeBuffer,PC,r,n);
      fprintf(f,"seam_call R%d, %d\n",r,n);
    }
    return PC;
  case seam_call0: // r
    {
      GET_1R(codeBuffer,PC,r);
      fprintf(f,"seam_call0 R%d\n",r);
    }
    return PC;
  case seam_call1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"seam_call1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case seam_call2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"seam_call2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case seam_call3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"seam_call3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;
  case seam_tailcall: // r
    {
      GET_1R1I(codeBuffer,PC,r,n);
      fprintf(f,"seam_tailcall R%d, %d\n",r,n);
    }
    return PC;
  case seam_tailcall0: // r
    {
      GET_1R(codeBuffer,PC,r);
      fprintf(f,"seam_tailcall0 R%d\n",r);
    }
    return PC;
  case seam_tailcall1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"seam_tailcall1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case seam_tailcall2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"seam_tailcall2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case seam_tailcall3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"seam_tailcall3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;
  case self_call: // n
    {
      GET_1I(codeBuffer,PC,n);
      fprintf(f,"self_call %d\n",n);
    }
    return PC;
  case self_call1: // r0
    {
      GET_1R(codeBuffer,PC,r0);
      fprintf(f,"self_call1 R%d\n",r0);
    }
    return PC;
  case self_call2: // r0,r1
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"self_call2 R%d, R%d\n",r0,r1);
    }
    return PC;
  case self_call3: // r0,r1,r2
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"self_call3 R%d, R%d, R%d\n",r0,r1,r2);
    }
    return PC;
  case self_tailcall: // n
    {
      GET_1I(codeBuffer,PC,n);
      fprintf(f,"self_tailcall %d\n",n);
    }
    return PC;
  case self_tailcall1: // r0
    {
      GET_1R(codeBuffer,PC,r0);
      fprintf(f,"self_tailcall1 R%d\n",r0);
    }
    return PC;
  case self_tailcall2: // r0,r1
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"self_tailcall2 R%d, R%d\n",r0,r1);
    }
    return PC;
  case self_tailcall3: // r0,r1,r2
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"self_tailcall3 R%d, R%d, R%d\n",r0,r1,r2);
    }
    return PC;
  case bci_call: // r,n
    {
      GET_1R1I(codeBuffer,PC,r,n);
      fprintf(f,"bci_call R%d, %d\n",r,n);
    }
    return PC;
  case bci_call0: // r
    {
      GET_1R(codeBuffer,PC,r);
      fprintf(f,"bci_call0 R%d\n",r);
    }
    return PC;
  case bci_call1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"bci_call1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case bci_call2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"bci_call2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case bci_call3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"bci_call3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;     
  case bci_call_direct1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"bci_call_direct1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case bci_call_direct2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"bci_call_direct2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case bci_call_direct3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"bci_call_direct3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;
  case bci_tailcall: // r,n
    {
      GET_1R1I(codeBuffer,PC,r,n);
      fprintf(f,"bci_tailcall R%d, %d\n",r,n);
    }
    return PC;
  case bci_tailcall1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"bci_tailcall1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case bci_tailcall2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"bci_tailcall2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case bci_tailcall3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"bci_tailcall3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;     
  case bci_tailcall_direct1: // reg,r0
    {
      GET_2R(codeBuffer,PC,reg,r0);
      fprintf(f,"bci_tailcall_direct1 R%d, R%d\n",reg,r0);
    }
    return PC;
  case bci_tailcall_direct2: // reg,r0,r1
    {
      GET_3R(codeBuffer,PC,reg,r0,r1);
      fprintf(f,"bci_tailcall_direct2 R%d, R%d, R%d\n",reg,r0,r1);
    }
    return PC;
  case bci_tailcall_direct3: // reg,r0,r1,r2
    {
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);
      fprintf(f,"bci_tailcall_direct3 R%d, R%d, R%d, R%d\n",reg,r0,r1,r2);
    }
    return PC;
  case seam_call_prim: // n,prim
    {
      GET_2I(codeBuffer,PC,n,prim);
      fprintf(f,"seam_call_prim %d, %d\n",n,prim);
    }
    return PC;
  case seam_call_prim0: // prim
    {
      GET_1I(codeBuffer,PC,prim);
      fprintf(f,"seam_call_prim0 %d\n",prim);
    }
    return PC;
  case seam_call_prim1: // r0,prim
    {
      GET_1R1I(codeBuffer,PC,r0,prim);
      fprintf(f,"seam_call_prim1 R%d, %d\n",r0,prim);
    }
    return PC;
  case seam_call_prim2: // r0,r1,prim
    {
      GET_2R1I(codeBuffer,PC,r0,r1,prim);
      fprintf(f,"seam_call_prim2 R%d, R%d, %d\n",r0,r1,prim);
    }
    return PC;
  case seam_call_prim3: // r0,r1,r2,prim
    {
      GET_3R1I(codeBuffer,PC,r0,r1,r2,prim);
      fprintf(f,"seam_call_prim3 R%d, R%d, R%d, %d\n",r0,r1,r2,prim);
    }
    return PC;
 case seam_tailcall_prim: // r,n
    {
      GET_2I(codeBuffer,PC,n,prim);
      fprintf(f,"seam_tailcall_prim %d, %d\n",n,prim);
    }
    return PC;
  case seam_tailcall_prim0: // prim
    {
      GET_1I(codeBuffer,PC,prim);
      fprintf(f,"seam_tailcall_prim0 %d\n",prim);
    }
    return PC;
  case seam_tailcall_prim1: // r0,prim
    {
      GET_1R1I(codeBuffer,PC,r0,prim);
      fprintf(f,"seam_tailcall_prim1 R%d, %d\n",r0,prim);
    }
    return PC;
  case seam_tailcall_prim2: // r0,r1,prim
    {
      GET_2R1I(codeBuffer,PC,r0,r1,prim);
      fprintf(f,"seam_tailcall_prim2 R%d, R%d, %d\n",r0,r1,prim);
    }
    return PC; 
  case seam_tailcall_prim3: // r0,r1,r2,prim
    {
      GET_3R1I(codeBuffer,PC,r0,r1,r2,prim);
      fprintf(f,"seam_tailcall_prim3 R%d, R%d, R%d, %d\n",r0,r1,r2,prim);
    }
    return PC;
  case seam_return: // n
    {
      GET_1I(codeBuffer,PC,n);
      fprintf(f,"seam_return %d\n",n);
    }
    return PC;
  case seam_return1:
    {
      GET_1R(codeBuffer,PC,r0);
      fprintf(f,"seam_return1 R%d\n",r0);
    }
    return PC;
  case seam_return2:
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"seam_return2 R%d, R%d\n",r0,r1);
    }
    return PC;
  case seam_return3:
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"seam_return3 R%d, R%d, R%d\n",r0,r1,r2);
    }
    return PC;
  case seam_return4:
    {
      GET_4R(codeBuffer,PC,r0,r1,r2,r3);
      fprintf(f,"seam_return4 R%d, R%d, R%d, R%d\n",r0,r1,r2,r3);
    }
    return PC;
  case seam_load_sreg: // r, index
    {
      GET_1R1I(codeBuffer,PC,r,index);
      fprintf(f,"seam_load_sreg R%d, %d\n",r,index);
    }
    return PC;
  case install_handler:
    {
      GET_1I(codeBuffer,PC,hp);
      fprintf(f,"install_handler %d\n",hp);
    }
    return PC;     
  case remove_handler:
    {
      fprintf(f,"remove_handler\n");
    }
    return PC;     
  case raise_normal:
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"raise_normal R%d\n",reg);
    }
    return PC;   
  case raise_direct:
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"raise_direct R%d\n",reg);
    }
    return PC;
  case load_global: // r,i
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"load_global R%d, %d\n",reg,iaddr);
    }
    return PC;
  case load_immediate: // r, i
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"load_immediate R%d, %d\n",reg,iaddr);
    }
    return PC;
  case load_int: // r, int
    {
      GET_1R1I(codeBuffer,PC,reg,i);
      fprintf(f,"load_int R%d, %d\n",reg,(int)i);
    }
    return PC;
  case load_zero: // r
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"load_zero R%d\n",reg);
    }
    return PC;
  case load_reg: // r0, r1
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"load_reg R%d, R%d\n",r1,r2);
    }
    return PC;
  case swap_regs: // r0, r1
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"swap_regs R%d, R%d\n",r1,r2);
    }
    return PC;
  case load_cell:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"load_cell R%d, R%d\n",r1,r2);
    }  
    return PC;
  case set_cell:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"set_cell R%d, R%d\n",r1,r2);
    }  
    return PC;
  case new_cell:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"new_cell R%d, R%d\n",r1,r2);
    }
    return PC;
  case new_vec:
    {
      GET_1R1I(codeBuffer,PC,r,size);
      fprintf(f,"new_vec R%d, %d\n",r,size);	
    }
    return PC;
  case init_vec:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"init_vec R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case load_vec:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"load_vec R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case init_polyrec:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"init_polyrec R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case init_tup:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"init_tup R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case new_tup:
    {
      GET_1R1I(codeBuffer,PC,r,addr);
      fprintf(f,"new_tup R%d, %d\n",r,addr);	
    }
    return PC;
  case new_pair:
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"new_pair R%d <- (R%d,R%d)\n",r0,r1,r2);	
    }
    return PC;
  case new_triple:
    {
      GET_4R(codeBuffer,PC,r0,r1,r2,r3);
      fprintf(f,"new_triple R%d <- (R%d,R%d,R%d)\n",r0,r1,r2,r3);	
    }
    return PC;
  case new_tagval:
    {  
      GET_1R2I(codeBuffer,PC,r,addr,tag);
      fprintf(f,"new_tagval R%d, %d, %d\n",r,addr,tag);	
    }
    return PC;    
  case new_bigtagval:
    {
      GET_1R2I(codeBuffer,PC,r,addr,tag);
      fprintf(f,"new_bigtagval R%d, %d, %d\n",r,addr,tag);	
    }
    return PC;
  case init_tagval:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"init_tagval R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case init_bigtagval:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(f,"init_bigtagval R%d, R%d, %d\n",r0,r1,index);	
    }
    return PC;
  case new_con:
    {
      GET_1R1I(codeBuffer,PC,r,iaddr);
      fprintf(f,"new_con R%d, %d\n",r,iaddr);
    }
    return PC;
  case prepare_con:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,size);
      fprintf(f,"prepare_con R%d, R%d, %d\n",r0,r1,size);
    }
    return PC;
  case init_con:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"init_con R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;      
  case load_con:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"load_con R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;      
  case load_tagval:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"load_tagval R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;      
  case load_bigtagval:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"load_bigtagval R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;      
  case new_polyrec:
    {
      GET_1R1I(codeBuffer,PC,r,iaddr);
      fprintf(f,"new_polyrec R%d, %d\n",r,iaddr);
    }
    return PC;
  case select_tup:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"select_tup R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;
  case select_tup0:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"select_tup0 R%d, R%d\n",r1,r2);
    }
    return PC;
  case select_tup1:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"select_tup1 R%d, R%d\n",r1,r2);
    }
    return PC;
  case select_tup2:
    {
      GET_2R(codeBuffer,PC,r1,r2);
      fprintf(f,"select_tup2 R%d, R%d\n",r1,r2);
    }
    return PC;
  case get_tup2:
    {
      GET_3R(codeBuffer,PC,r0,r1,r2);
      fprintf(f,"get_tup2 (R%d,R%d) <- R%d\n",r0,r1,r2);
    }
    return PC;
  case get_tup3:
    {
      GET_4R(codeBuffer,PC,r0,r1,r2,r3);
      fprintf(f,"get_tup3 (R%d,R%d,R%d) <- R%d\n",r0,r1,r2,r3);
    }
    return PC;
  case lazyselect_polyrec:
    {
      GET_2R1I(codeBuffer,PC,r1,r2,index);
      fprintf(f,"lazyselect_polyrec R%d, R%d, %d\n",r1,r2,index);
    }
    return PC;     
  case lazyselect_polyrec_n:
    {
      GET_1R2I(codeBuffer,PC,r0,i0,i1);
      fprintf(f,"lazyselect_polyrec_n R%d, %d, %d\n",r0,i0,i1);
    }
    return PC;     
  case jump: // target
    {
      GET_1I(codeBuffer,PC,jumpTarget);
      fprintf(f,"jump %d\n",(int)jumpTarget);
    }
    return PC;
  case check_preempt_jump: // target
    {
      GET_1I(codeBuffer,PC,jumpTarget);
      fprintf(f,"check_preempt_jump %d\n",(int)jumpTarget);
    }
    return PC;
  case itest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"itest R%d, %d\n",reg,iaddr);
    }
    return PC;
  case tagtest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"tagtest R%d, %d\n",reg,iaddr);
    }
    return PC;
  case bigtagtest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"bigtagtest R%d, %d\n",reg,iaddr);
    }
    return PC;  
  case tagtest1:
    {
      GET_1R2I(codeBuffer,PC,reg,tag,target);
      fprintf(f,"tagtest1 R%d, %d, %d\n",reg,tag,target);
    }
    return PC;
  case bigtagtest1:
    {
      GET_1R2I(codeBuffer,PC,reg,tag,target);
      fprintf(f,"bigtagtest1 R%d, %d, %d\n",reg,tag,target);
    }
    return PC;  
  case ctagtest:
    {
      ProgramCounter oldPC;
      GET_1R1I(codeBuffer,PC,reg,size);
      fprintf(f,"ctagtest R%d, %d\n",reg,size);
      for(u_int i=0;i<size;i++) {
	oldPC = PC;
	GET_1I(codeBuffer,PC,target);
	fprintf(f,"PC %d: %d -> %d\n",oldPC,i,target);
      }
    }
    return PC;
  case cbigtagtest:
    {
      ProgramCounter oldPC;
      GET_1R1I(codeBuffer,PC,reg,size);
      fprintf(f,"cbigtagtest R%d, %d\n",reg,size);
      for(u_int i=0;i<size;i++) {
	oldPC = PC;
	GET_1I(codeBuffer,PC,target);
	fprintf(f,"PC %d: %d -> %d\n",oldPC,i,target);
      }
    }
    return PC;  
  case ctagtest_direct:
    {
      ProgramCounter oldPC;
      GET_1R1I(codeBuffer,PC,reg,size);
      fprintf(f,"ctagtest_direct R%d, %d\n",reg,size);
      for(u_int i=0;i<size;i++) {
	oldPC = PC;
	GET_1I(codeBuffer,PC,target);
	fprintf(f,"PC %d: %d -> %d\n",oldPC,i,target);
      }
    }
    return PC;
  case cbigtagtest_direct:
    {
      ProgramCounter oldPC;
      GET_1R1I(codeBuffer,PC,reg,size);
      fprintf(f,"cbigtagtest_direct R%d, %d\n",reg,size);
      for(u_int i=0;i<size;i++) {
	oldPC = PC;
	GET_1I(codeBuffer,PC,target);
	fprintf(f,"PC %d: %d -> %d\n",oldPC,i,target);
      }
    }
    return PC;  
  case vectest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"vectest R%d, %d\n",reg,iaddr);
    }
    return PC;
  case citest:
    {
      GET_1R2I(codeBuffer,PC,r,size,offset);
      fprintf(f,"citest R%d, %d, %d\n",r,size,offset);
      for(u_int i=0;i<size;i++) {
	ProgramCounter oldPC = PC;
	GET_1I(codeBuffer,PC,target);
	fprintf(f,"PC %d: %d -> %d\n",oldPC,i+offset,target);
      }
    }
    return PC;
  case ijump_eq: // r, val, target
    {
      GET_1R2I(codeBuffer,PC,reg,number,jumpTarget);
      fprintf(f,"ijump_eq R%d, %d, %d\n",reg,number,(int)jumpTarget);
    }
    return PC;
  case rtest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"rtest R%d, %d\n",reg,iaddr);
    }
    return PC;
  case rjump_eq:
    {
      GET_1R2I(codeBuffer,PC,reg,number,jumpTarget);
      fprintf(f,"rjump_eq R%d, %d, %d\n",reg,number,(int)jumpTarget);
    }
    return PC;
  case stest:
    {
      GET_1R1I(codeBuffer,PC,reg,iaddr);
      fprintf(f,"stest R%d, %d\n",reg,iaddr);
    }
    return PC;
  case sjump_eq:
    {
      GET_1R2I(codeBuffer,PC,reg,number,jumpTarget);
      fprintf(f,"sjump_eq R%d, %d, %d\n",reg,number,(int)jumpTarget);
    }
    return PC;
  case contest:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,target);
      fprintf(f,"contest R%d, R%d, %d\n",r0,r1,target);
    }
    return PC;
  case mk_closure:
    {
      GET_1R2I(codeBuffer,PC,reg,codeAddr,size);
      fprintf(f,"mk_closure R%d, %d, %d\n",reg,codeAddr,size);
    }
    return PC;
  case init_closure:
    {
      GET_2R1I(codeBuffer,PC,r0,r1,index);
      fprintf(stderr,"init_closure R%d, R%d, %d\n",r0,r1,index);
    }
    return PC;
  case debug_msg:
    {
      GET_1I(codeBuffer,PC,iaddr);
      String *s = String::FromWordDirect(imEnv->Sel(iaddr));
      fprintf(f,"debug_msg %d -> %s\n",iaddr, s->ExportC());
    }
    return PC;
  case inlined_future_byneed:
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"inlined_future_byneed R%d, R%d\n",r0,r1);
    }
    return PC;
  case inlined_hole_hole:
    {
      GET_1R(codeBuffer,PC,reg);
      fprintf(f,"inlined_hole_hole R%d\n",reg);
    }
    return PC;
  case inlined_hole_fill:
    {
      GET_2R(codeBuffer,PC,r0,r1);
      fprintf(f,"inlined_hole_fill R%d, R%d\n",r0,r1);
    }
    return PC;
  default:
    {
      fprintf(f, "instr number %d unkown\n",instr);
      Error("unkown instruction");
    }
    return PC;
    }
}

void ByteCode::Disassemble(std::FILE *f, ProgramCounter pc, 
			   Chunk *code, Tuple *imEnv) {
#ifdef THREADED
  ProgramCounter end = (ProgramCounter) (code->GetBase() + code->GetSize());
  while(pc < end) {
#else
  u_int max = code->GetSize();
  while(4 * pc < max) {
#endif
    pc = ByteCode::DisassembleOne(f,pc,code,imEnv);
  }
}
