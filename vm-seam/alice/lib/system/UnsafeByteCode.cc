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

#include "alice/Authoring.hh"
#include "alice/AbstractCode.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeAlign.hh"
#include "alice/ByteCodeFrame.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/HotSpotConcreteCode.hh"

#include <cstdio>
#define DEBUG_PRINT(S) /*printf S*/

#define DECLARE_MYBLOCKTYPE(t, a, x)		\
  t *a = t::FromWord(x);			
  // check:   if (a == INVALID_POINTER) { REQUEST(x); }

#define DECLARE_VEC(v,x) DECLARE_MYBLOCKTYPE(Vector, v, x)
#define DECLARE_TUP(t,x) DECLARE_MYBLOCKTYPE(Tuple, t, x)
#define DECLARE_STR(s,x) DECLARE_MYBLOCKTYPE(String, s, x)


namespace {

  // jumps are all relative to the beginning of the code (i.e. absolute)
  // change this by uncommenting "-oldPC"

  // classes to facilitate the computation of forward jumps
  class Label {
  protected:
    u_int oldPC;
    u_int patchPos;
    Label *next;
  public:
    Label(u_int pc, u_int pos = 0) : oldPC(pc), patchPos(pos) {}
    void SetNext(Label *n) { next = n; }
    Label* GetNext() { return next; }
    virtual void patch(u_int PC, Tuple *imEnv) = 0;
  };
  class JumpLabel : public Label {
  public:
    JumpLabel(u_int pc, u_int pos) : Label(pc,pos) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch JumpLabel at %d to %d\n",oldPC,PC-oldPC));
      SET_INSTR_1I(patchPos,ByteCodeInstr::jump,PC-oldPC);
    }
  };
  class ConTestLabel : public Label {
  private:
    u_int reg0, reg1;
  public:
    ConTestLabel(u_int pc, u_int pos, u_int r0, u_int r1) 
      : Label(pc,pos), reg0(r0), reg1(r1) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch ConTestLabel at %d to %d\n",oldPC,PC-oldPC));
      SET_INSTR_2R1I(patchPos,ByteCodeInstr::contest,reg0,reg1,PC-oldPC);
    }
  };
  // this is an absolute jump
  class HandlerLabel : public Label {
  private:
    u_int tupAddr;
  public:
    HandlerLabel(u_int pc, u_int iaddr) : Label(pc), tupAddr(iaddr) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch HandlerLabel at %d to %d\n",oldPC,PC));
      Tuple *tup = Tuple::FromWordDirect(imEnv->Sel(tupAddr));
      tup->Init(2,Store::IntToWord(PC));
    }
  };
  class JumpEqLabel : public Label {
  private:
    u_int instr;
    u_int reg;
    u_int im;
  public:
    JumpEqLabel(u_int pc, u_int pos, u_int ins, u_int r, u_int i)
      : Label(pc,pos), instr(ins), reg(r), im(i) {
    }
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch JumpEqLabel at %d to %d\n",oldPC,PC-oldPC));
      SET_INSTR_1R2I(patchPos,instr,reg,im,PC-oldPC);
    }
  };
  class TableLabel : public Label {
  protected:
    u_int addr;
    word key;
  public:
    TableLabel(u_int pc, u_int a, word k=INVALID_POINTER) 
      : Label(pc), addr(a), key(k) {}
  };
  class ChunkMapLabel : public TableLabel {
  public:
    ChunkMapLabel(u_int pc, u_int a, word k) : TableLabel(pc,a,k) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch ChunkMapLabel %p to %d, which has addr %p,table=%p\n",
		  key, PC-oldPC,Store::IntToWord(PC-oldPC),
		  ChunkMap::FromWord(imEnv->Sel(addr))));
      ChunkMap *map = ChunkMap::FromWord(imEnv->Sel(addr));
      map->Put(key,Store::IntToWord(PC-oldPC));
    }
  };
  class IntMapLabel : public TableLabel {
  public:
    IntMapLabel(u_int pc, u_int a, word k) : TableLabel(pc,a,k) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch IntMapLabel %d=%p to %d, oldPC=%d, table=%p\n",
		  Store::WordToInt(key),key, PC-oldPC,oldPC,
		  IntMap::FromWord(imEnv->Sel(addr))));
      IntMap *map = IntMap::FromWord(imEnv->Sel(addr));
      map->Put(key,Store::IntToWord(PC-oldPC));
    }
  };
  class VectorLabel : public TableLabel {
  protected:
    u_int index;
  public:
    VectorLabel(u_int pc, u_int a, u_int i) : TableLabel(pc,a), index(i) {}
    void patch(u_int PC, Tuple *imEnv) {
      DEBUG_PRINT(("patch VectorLabel %d to %d\n",index,PC-oldPC));
      Vector *vec = Vector::FromWord(imEnv->Sel(addr));
      vec->Init(index,Store::IntToWord(PC-oldPC));
    }
  };
  class JumpTableLabel : public Label {
  public:
    JumpTableLabel(u_int pc, u_int pos) : Label(pc,pos) {}
    void patch(u_int PC, Tuple *imEnv) {
      SET_1I(patchPos, PC - oldPC);
    }
  };

  class PatchTable {
  private:  
    u_int size;
    Label **labels;
    u_int *addrs;
  public:
    PatchTable(u_int s) : size(s) {
      labels = new Label*[size];
      for(u_int i=0; i<size; i++) {
	labels[i] = NULL;
      }
      addrs = new u_int[size];
    }
    ~PatchTable() {
      for(u_int i=0; i<size; i++) {
	Label *label = labels[i];
	while(label != NULL) {
	  Label *tmp = label->GetNext();
	  delete label;
	  label = tmp;
	}
      }
      delete labels;
      delete addrs;
    }
    // handling of forward jumps
    void patchForwardJumps(u_int IC, u_int PC, Tuple *imEnv) {
      Label *label = labels[IC], *tmp;
      while(label != NULL) { 
	label->patch(PC,imEnv);
	label = label->GetNext();
      }
    }
    void AddLabel(u_int pos, Label *label) {
      label->SetNext(labels[pos]);
      labels[pos] = label;
    }
    // handling of backward jumps 
    void setConcreteAddr(u_int IC, u_int PC) {
      addrs[IC] = PC;
    }
    u_int computeBackwardJump(u_int absJumpAddr, u_int PC) {
      DEBUG_PRINT(("jump back from %d to %d\n",PC,addrs[absJumpAddr]));
      return addrs[absJumpAddr] - PC; 
    }
  };


  // assembler

  word assemble(Vector *code, Vector *imVec, word nbLocals) {
    DEBUG_PRINT(("assemble given code ...\n"));
    Tuple *imEnv = Tuple::New(imVec->GetLength());
    Vector *args = Vector::New(0);
    u_int len = code->GetLength();
    u_int PC = 0;
    PatchTable patchTable(len);

    WriteBuffer::Init();

    for(u_int IC = 0; IC < len; IC++) {
      DEBUG_PRINT(("IC %"U_INTF". ", IC));
      // label handling
      patchTable.setConcreteAddr(IC,PC);
      patchTable.patchForwardJumps(IC,PC,imEnv);

      DECLARE_VEC(insVec, code->Sub(IC));
      u_int instr = Store::DirectWordToInt(insVec->Sub(0)); 
      DEBUG_PRINT(("assemble instr no %d\n",instr));
      switch(instr) {
      case ByteCodeInstr::citest:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int tableAddr = Store::DirectWordToInt(insVec->Sub(2));
	  u_int offset = Store::DirectWordToInt(insVec->Sub(3));

	  DECLARE_VEC(tests, imVec->Sub(tableAddr));
	  u_int nTests = tests->GetLength();
	  SET_INSTR_1R2I(PC,instr,reg,nTests,offset);
	  u_int jumpTablePC = PC;

	  for(u_int i=0; i<nTests; i++) {
	    u_int jumpTarget = Store::WordToInt(tests->Sub(i));
	    u_int oldJumpTablePC = jumpTablePC;
	    SET_1I(jumpTablePC,0);	  
	    if(jumpTarget < IC) {  // backward jump
	      s_int offset = patchTable.computeBackwardJump(jumpTarget,PC);
	      SET_1I(oldJumpTablePC,offset);
	    }
	    else
	      patchTable.AddLabel(jumpTarget,
				  new JumpTableLabel(PC,oldJumpTablePC));
	  }
	}
	break;
      case ByteCodeInstr::rtest:
      case ByteCodeInstr::stest:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int testAddr = Store::DirectWordToInt(insVec->Sub(2));

	  DECLARE_VEC(tests, imVec->Sub(testAddr));	
	  u_int nTests  = tests->GetLength();
	  ChunkMap *map = ChunkMap::New(nTests * 2);
	  imEnv->Init(testAddr, map->ToWord());
	  SET_INSTR_1R1I(PC,instr,reg,testAddr);

	  for (u_int i = nTests; i--;) {
	    DECLARE_TUP(pair, tests->Sub(i));
	    word key = pair->Sel(0);
	    u_int jumpTarget = Store::WordToInt(pair->Sel(1));
	    if(jumpTarget < IC) { // backward jump
	      s_int offset = patchTable.computeBackwardJump(jumpTarget,PC);
	      map->Put(key,Store::IntToWord(offset)); 
	    }
	    else {
	      DEBUG_PRINT(("set patch label for key %p in table %p\n",key,map));
	      patchTable.AddLabel(jumpTarget,
				  new ChunkMapLabel(PC,testAddr,key));
	    }	  
	  }
	}   
	break;
      case ByteCodeInstr::contest:
	{
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(2));
	  s_int jumpTarget = Store::DirectWordToInt(insVec->Sub(3));
	  s_int newTarget=0;
	  u_int oldPC = PC;
	  SET_INSTR_2R1I(PC,instr,r0,r1,newTarget);

	  if(jumpTarget < IC) { // backward jump
	    newTarget = patchTable.computeBackwardJump(jumpTarget,PC);
	    SET_INSTR_2R1I(oldPC,instr,r0,r1,newTarget);
	  }
	  else 
	    patchTable.AddLabel(jumpTarget,new ConTestLabel(PC,oldPC,r0,r1));	
	}   
	break;
      case ByteCodeInstr::vectest:
      case ByteCodeInstr::bigtagtest:      
      case ByteCodeInstr::tagtest:  
      case ByteCodeInstr::itest:  
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int testAddr = Store::DirectWordToInt(insVec->Sub(2));

	  DECLARE_VEC(tests, imVec->Sub(testAddr));
	  u_int nTests = tests->GetLength();
	  IntMap *map = IntMap::New(2 * nTests);
	  imEnv->Init(testAddr, map->ToWord());
	  SET_INSTR_1R1I(PC,instr,reg,testAddr);

	  for (u_int i = nTests; i--;) {
	    DECLARE_TUP(pair, tests->Sub(i));
	    word key = pair->Sel(0);
	    s_int jumpTarget = Store::WordToInt(pair->Sel(1));
	    if(jumpTarget < IC) { // backward jump
	      s_int offset = patchTable.computeBackwardJump(jumpTarget,PC);
	      map->Put(key,Store::IntToWord(offset)); 		     
	    }
	    else {
	      DEBUG_PRINT(("set patch label for key %p in table %p\n",key,map));

	      patchTable.AddLabel(jumpTarget,
				  new IntMapLabel(PC,testAddr,key));
	    }	  
	  }
	}
	break;
      case ByteCodeInstr::ctagtest:
      case ByteCodeInstr::cbigtagtest:
      case ByteCodeInstr::ctagtest_direct:
      case ByteCodeInstr::cbigtagtest_direct:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int tableAddr = Store::DirectWordToInt(insVec->Sub(2));

	  DECLARE_VEC(tests, imVec->Sub(tableAddr));
	  u_int nTests = tests->GetLength();
	  imEnv->Init(tableAddr,Store::IntToWord(0)); // dummy
	  SET_INSTR_1R1I(PC,instr,reg,nTests);
	  u_int jumpTablePC = PC;

	  for(u_int i=0; i<nTests; i++) {
	    s_int jumpTarget = Store::WordToInt(tests->Sub(i));
	    u_int oldJumpTablePC = jumpTablePC;
	    SET_1I(jumpTablePC,0);	  
	    if(jumpTarget < IC) {  // backward jump
	      s_int offset = patchTable.computeBackwardJump(jumpTarget,PC);
	      SET_1I(oldJumpTablePC,offset);
	    }
	    else
	      patchTable.AddLabel(jumpTarget,
				  new JumpTableLabel(PC,oldJumpTablePC));
	  }
	}
	break;
      case ByteCodeInstr::new_con:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(2));

	  imEnv->Init(iaddr, imVec->Sub(iaddr));
	  SET_INSTR_1R1I(PC,instr,reg,iaddr);
	}
	break;
      case ByteCodeInstr::seam_prim_call1:
      case ByteCodeInstr::seam_prim_tailcall1:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  u_int reg = Store::DirectWordToInt(insVec->Sub(2));

	  imEnv->Init(iaddr, Store::IntToWord(0)); // dummy
	  Closure *closure = Closure::FromWordDirect(imVec->Sub(iaddr));
	  ConcreteCode *concreteCode = 
	    ConcreteCode::FromWord(closure->GetConcreteCode());		
	  Interpreter *interpreter = concreteCode->GetInterpreter();

	  SET_INSTR_1R1I(PC,instr,reg, reinterpret_cast<u_int>(interpreter));
	}
	break;


      case ByteCodeInstr::new_polyrec:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(2));
	  Vector *vec = Vector::FromWordDirect(imVec->Sub(iaddr));
	  u_int length = vec->GetLength();
	  Vector *uVec = Vector::New(length);

	  for(u_int i=length; i--;) {
	    UniqueString *ustring = 
	      UniqueString::New(String::FromWordDirect(vec->Sub(i)));
	    uVec->Init(i,ustring->ToWord());
	  }

	  imEnv->Init(iaddr, uVec->ToWord());
	  SET_INSTR_1R1I(PC,instr,reg,iaddr);
	}
	break;
      case ByteCodeInstr::debug_msg:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  imEnv->Init(iaddr, imVec->Sub(iaddr));
	  SET_INSTR_1I(PC,instr,iaddr);
	}
	break;
      case ByteCodeInstr::seam_prim_call:
      case ByteCodeInstr::seam_prim_tailcall:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  u_int nArgs = Store::DirectWordToInt(insVec->Sub(2));

	  imEnv->Init(iaddr, Store::IntToWord(0)); // dummy
	  Closure *closure = Closure::FromWordDirect(imVec->Sub(iaddr));
	  ConcreteCode *concreteCode = 
	    ConcreteCode::FromWord(closure->GetConcreteCode());		
	  Interpreter *interpreter = concreteCode->GetInterpreter();

	  SET_INSTR_2I(PC,instr,iaddr, reinterpret_cast<u_int>(interpreter));
	}
	break;
      case ByteCodeInstr::install_handler:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  Tuple *tup = Tuple::FromWordDirect(imVec->Sub(iaddr));
	  u_int jumpTarget = Store::DirectWordToInt(tup->Sel(2));

	  if(jumpTarget < IC) { // backward jump
	    u_int offset;
	    offset = patchTable.computeBackwardJump(jumpTarget, PC);
	    tup->Init(2,Store::IntToWord(offset));
	  }
	  else 
	    patchTable.AddLabel(jumpTarget, new HandlerLabel(PC,iaddr)); 	

	  imEnv->Init(iaddr, tup->ToWord());
	  SET_INSTR_1I(PC,instr,iaddr);	
	}
	break;
      case ByteCodeInstr::check_preempt_jump:
      case ByteCodeInstr::jump:
	{
	  s_int jumpTarget = Store::DirectWordToInt(insVec->Sub(1));
	  u_int oldPC = PC;
	  SET_INSTR_1I(PC,instr,0);

	  if(jumpTarget < IC) { // backward jump
	    jumpTarget = patchTable.computeBackwardJump(jumpTarget, PC);
	    SET_INSTR_1I(oldPC,instr,jumpTarget);
	  }
	  else
	    patchTable.AddLabel(jumpTarget, new JumpLabel(PC,oldPC)); 	
	}
	break;
      case ByteCodeInstr::seam_cccn:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  imEnv->Init(iaddr, imVec->Sub(iaddr));
	  Vector *regs = Vector::FromWordDirect(imVec->Sub(iaddr));
	  u_int nargs = regs->GetLength();
	  args = Vector::New(nargs);
	  for(u_int i=nargs; i--; ) {
	    TagVal *idDef = TagVal::New(Types::SOME,1);
	    idDef->Init(0,regs->Sub(i));
	    args->Init(i,idDef->ToWord());
	  }
	  SET_INSTR_1R(PC,instr,iaddr);
	}
	break;
      case ByteCodeInstr::cccn:
	{
	  u_int nargs = Store::DirectWordToInt(insVec->Sub(1));
	  SET_INSTR_1I(PC,instr,nargs);
	  args = Vector::New(nargs);
	  for(u_int i=nargs; i--; ) {
	    TagVal *idDef = TagVal::New(Types::SOME,1);
	    idDef->Init(0,Store::IntToWord(i));
	    args->Init(i,idDef->ToWord());
	  }
	}
	break;
      case ByteCodeInstr::self_call:
      case ByteCodeInstr::seam_return:
	{
	  u_int nargs = Store::DirectWordToInt(insVec->Sub(1));
	  SET_INSTR_1I(PC,instr,nargs);
	}
	break;
      case ByteCodeInstr::ccc1:
	{
	  SET_INSTR(PC,instr);
	  args = Vector::New(1);
	  TagVal *idDef = TagVal::New(Types::SOME,1);
	  idDef->Init(0,Store::IntToWord(0));
	  args->Init(0,idDef->ToWord());
	}
	break;
      case ByteCodeInstr::self_call0:
      case ByteCodeInstr::remove_handler:
	{
	  SET_INSTR(PC,instr);
	}
	break;     

      case ByteCodeInstr::set_global:
      case ByteCodeInstr::new_vec:
      case ByteCodeInstr::new_tup:
      case ByteCodeInstr::load_immediate:
      case ByteCodeInstr::load_global:
      case ByteCodeInstr::load_int:
      case ByteCodeInstr::seam_call:
      case ByteCodeInstr::seam_tailcall:
	{
	  u_int reg = Store::WordToInt(insVec->Sub(1));
	  u_int im = Store::WordToInt(insVec->Sub(2));
	  
	  SET_INSTR_1R1I(PC,instr,reg,im);
	}
	break;
      case ByteCodeInstr::seam_ccc1:
	{
	  u_int reg = Store::WordToInt(insVec->Sub(1));	
	  SET_INSTR_1R(PC,instr,reg);
	  args = Vector::New(1);
	  TagVal *idDef = TagVal::New(Types::SOME,1);
	  idDef->Init(0,Store::IntToWord(reg));
	  args->Init(0,idDef->ToWord());
	}
	break;
      case ByteCodeInstr::await:
      case ByteCodeInstr::seam_return1:
      case ByteCodeInstr::raise_normal:
      case ByteCodeInstr::raise_direct: 
      case ByteCodeInstr::load_zero:
      case ByteCodeInstr::self_call1:
      case ByteCodeInstr::seam_call0:
      case ByteCodeInstr::seam_tailcall0:
	{
	  u_int reg = Store::WordToInt(insVec->Sub(1));
	  SET_INSTR_1R(PC,instr,reg);
	}
	break;
      case ByteCodeInstr::rjump_eq:
      case ByteCodeInstr::sjump_eq:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int iaddr = Store::WordToInt(insVec->Sub(2));
	  s_int jumpTarget = Store::WordToInt(insVec->Sub(3));
	  imEnv->Init(iaddr,imVec->Sub(iaddr));
	  u_int oldPC = PC;
	  SET_INSTR_1R2I(PC,instr,0,0,0);

	  if(jumpTarget < IC) { // backward jump
	    jumpTarget = patchTable.computeBackwardJump(jumpTarget, PC);
	    SET_INSTR_1R2I(oldPC,instr,reg,iaddr,jumpTarget);
	  }
	  else 
	    patchTable.AddLabel(jumpTarget,
				new JumpEqLabel(PC,oldPC,instr,reg,iaddr));
	}
	break;
      case ByteCodeInstr::ijump_eq:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int x = Store::WordToInt(insVec->Sub(2));
	  s_int jumpTarget = Store::WordToInt(insVec->Sub(3));
	  u_int oldPC = PC;
	  SET_INSTR_1R2I(PC,instr,0,0,0);

	  if(jumpTarget < IC) { // backward jump
	    jumpTarget = patchTable.computeBackwardJump(jumpTarget, PC);
	    SET_INSTR_1R2I(oldPC,instr,reg,x,jumpTarget);
	  }
	  else 
	    patchTable.AddLabel(jumpTarget,
				new JumpEqLabel(PC,oldPC,instr,reg,x));

	}
	break;
      case ByteCodeInstr::mk_closure:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(2));
	  u_int intIm = Store::DirectWordToInt(insVec->Sub(3));
	  
	  imEnv->Init(iaddr, imVec->Sub(iaddr));
	  SET_INSTR_1R2I(PC,instr,reg,iaddr,intIm);
	}
	break;
      case ByteCodeInstr::lazyselect_polyrec:
	{
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(3));
	  UniqueString *ustring = 
	    UniqueString::New(String::FromWordDirect(imVec->Sub(iaddr)));

	  imEnv->Init(iaddr, ustring->ToWord());
	  SET_INSTR_2R1I(PC,instr,r1,r2,iaddr);
	}
	break;
      case ByteCodeInstr::lazyselect_polyrec_n:
	{
	  u_int reg = Store::DirectWordToInt(insVec->Sub(1));
	  u_int iaddr1 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int iaddr2 = Store::DirectWordToInt(insVec->Sub(3));
	  Vector *regs = Vector::FromWordDirect(imVec->Sub(iaddr1));
	  Vector *labels = Vector::FromWordDirect(imVec->Sub(iaddr2));
	  u_int nLabels = labels->GetLength();
	  Vector *ulabels = Vector::New(nLabels);
	  for(u_int i=0; i<nLabels; i++) {
	    UniqueString *ustring = 
	      UniqueString::New(String::FromWordDirect(labels->Sub(i)));
	    ulabels->Init(i,ustring->ToWord());
	  }
	  imEnv->Init(iaddr1, regs->ToWord());
	  imEnv->Init(iaddr2, ulabels->ToWord());
	  SET_INSTR_1R2I(PC,instr,reg,iaddr1,iaddr2);
	}
	break;
      case ByteCodeInstr::new_cell:
      case ByteCodeInstr::load_cell:
      case ByteCodeInstr::set_cell:  
      case ByteCodeInstr::load_reg:
      case ByteCodeInstr::swap_regs:
      case ByteCodeInstr::select_tup0:
      case ByteCodeInstr::select_tup1:
      case ByteCodeInstr::select_tup2:
      case ByteCodeInstr::self_call2:
      case ByteCodeInstr::seam_call1:
      case ByteCodeInstr::seam_tailcall1:
      case ByteCodeInstr::seam_return2:
      case ByteCodeInstr::iinc:
      case ByteCodeInstr::idec:
	{
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(2));

	  SET_INSTR_2R(PC,instr,r1,r2);
	}
	break;
      case ByteCodeInstr::load_con:
      case ByteCodeInstr::init_con:
      case ByteCodeInstr::load_vec:
      case ByteCodeInstr::init_vec:
      case ByteCodeInstr::init_tup:
      case ByteCodeInstr::init_closure:
      case ByteCodeInstr::init_polyrec:
      case ByteCodeInstr::select_tup:
      case ByteCodeInstr::prepare_con:
	{
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int index = Store::DirectWordToInt(insVec->Sub(3));

	  SET_INSTR_2R1I(PC,instr,r1,r2,index);
	}
	break;
      case ByteCodeInstr::new_tagval:
      case ByteCodeInstr::new_bigtagval:
	{
	  u_int r = Store::DirectWordToInt(insVec->Sub(1));
	  u_int size = Store::DirectWordToInt(insVec->Sub(2));
	  u_int tag = Store::DirectWordToInt(insVec->Sub(3));

	  SET_INSTR_1R2I(PC,instr,r,size,tag);
	}
	break;
      case ByteCodeInstr::load_tagval:
      case ByteCodeInstr::init_tagval:
	{
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int index = Store::DirectWordToInt(insVec->Sub(3)) 
	    + TagVal::GetOffset();

	  SET_INSTR_2R1I(PC,instr,r0,r1,index);
	}
	break;
      case ByteCodeInstr::load_bigtagval:
      case ByteCodeInstr::init_bigtagval:
	{
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int index = Store::DirectWordToInt(insVec->Sub(3)) 
	    + BigTagVal::GetOffset();

	  SET_INSTR_2R1I(PC,instr,r0,r1,index);
	}
	break;
      case ByteCodeInstr::get_tup2:
      case ByteCodeInstr::new_pair:
      case ByteCodeInstr::seam_call2:
      case ByteCodeInstr::self_call3:
      case ByteCodeInstr::seam_tailcall2:
      case ByteCodeInstr::seam_return3:
      case ByteCodeInstr::iadd:
      case ByteCodeInstr::isub:
	{
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(3));

	  SET_INSTR_3R(PC,instr,r0,r1,r2);
	}
	break;
      case ByteCodeInstr::get_tup3:
      case ByteCodeInstr::new_triple:
      case ByteCodeInstr::seam_call3:
      case ByteCodeInstr::seam_tailcall3:
	{
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(3));
	  u_int r3 = Store::DirectWordToInt(insVec->Sub(4));

	  SET_INSTR_4R(PC,instr,r0,r1,r2,r3);
	}
	break;
      case ByteCodeInstr::seam_prim_call0:
      case ByteCodeInstr::seam_prim_tailcall0:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));

	  imEnv->Init(iaddr, Store::IntToWord(0)); // dummy
	  Closure *closure = Closure::FromWordDirect(imVec->Sub(iaddr));
	  ConcreteCode *concreteCode = 
	    ConcreteCode::FromWord(closure->GetConcreteCode());		
	  Interpreter *interpreter = concreteCode->GetInterpreter();

	  SET_INSTR_1I(PC,instr, reinterpret_cast<u_int>(interpreter));
	}
	break;
      case ByteCodeInstr::seam_prim_call2:
      case ByteCodeInstr::seam_prim_tailcall2:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(3));

	  imEnv->Init(iaddr, Store::IntToWord(0)); // dummy
	  Closure *closure = Closure::FromWordDirect(imVec->Sub(iaddr));
	  ConcreteCode *concreteCode = 
	    ConcreteCode::FromWord(closure->GetConcreteCode());		
	  Interpreter *interpreter = concreteCode->GetInterpreter();

	  SET_INSTR_2R1I(PC,instr,r0,r1, reinterpret_cast<u_int>(interpreter));
	}
	break;
      case ByteCodeInstr::seam_prim_call3:
      case ByteCodeInstr::seam_prim_tailcall3:
	{
	  u_int iaddr = Store::DirectWordToInt(insVec->Sub(1));
	  u_int r0 = Store::DirectWordToInt(insVec->Sub(2));
	  u_int r1 = Store::DirectWordToInt(insVec->Sub(3));
	  u_int r2 = Store::DirectWordToInt(insVec->Sub(4));

	  imEnv->Init(iaddr, Store::IntToWord(0)); // dummy
	  Closure *closure = Closure::FromWordDirect(imVec->Sub(iaddr));
	  ConcreteCode *concreteCode = 
	    ConcreteCode::FromWord(closure->GetConcreteCode());		
	  Interpreter *interpreter = concreteCode->GetInterpreter();

	  SET_INSTR_3R1I(PC,instr,r0,r1,r2, reinterpret_cast<u_int>(interpreter));
	}
	break;
      default:
	fprintf(stderr,"UnsafeByteCode::assemble: unkown instruction tag %"U_INTF"\n",
		instr);
      }
    }

    Chunk *codeChunk = WriteBuffer::FlushCode();

    // debug output
    /*  
    fprintf(stderr,"assembled print code:\n");
    ReadBuffer *buffer = ReadBuffer::New(codeChunk);
    u_int size = codeChunk->GetSize() / 4;
    for (u_int i=0; i<size; i++)
      fprintf(stderr,"%d\n",buffer->GetSlotInt(i));
    */

    // create a dummy abstract code value
    // you probably have to add several other information if some
    // function tries to access an undefined field

    Vector *liveness = Vector::New(0);
    TagVal *abstractCode =
      TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
    Tuple *coord = Tuple::New(3);
    coord->Init(0,String::New("\"manually assembled code\"")->ToWord());
    coord->Init(1,Store::IntToWord(0));
    coord->Init(2,Store::IntToWord(0));
    abstractCode->Init(0,coord->ToWord());
    abstractCode->Init(3,args->ToWord());
    abstractCode->Init(4,Store::IntToWord(0)); // outArityOpt == NONE
    abstractCode->Init(6,liveness->ToWord());

    // fake inline info, prevent assembled function from beeing inlined
    InlineInfo *inlineInfo = 
      InlineInfo::New(Map::New(0),liveness,Array::New(0),
		      Store::DirectWordToInt(nbLocals),
		      100000);

    ByteConcreteCode *bcc = 
      ByteConcreteCode::NewInternal(abstractCode,
				    codeChunk,
				    imEnv->ToWord(),
				    nbLocals,
				    inlineInfo->ToWord());
    return bcc->ToWord();
  }

}


// primitives

DEFINE4(UnsafeByteCode_assemble) {
  DECLARE_VECTOR(codeVec, x0);
  DECLARE_VECTOR(imVec, x1);
  DECLARE_VECTOR(globals, x2);
  word nbLocals = x3;

  DEBUG_PRINT(("number of locals = %d\n",Store::DirectWordToInt(nbLocals)));
  word bcc = assemble(codeVec,imVec,nbLocals);
  u_int nGlobals = globals->GetLength();
  DEBUG_PRINT(("build closure ...\n"));
  Closure *closure = Closure::New(bcc,nGlobals+1);
  closure->Init(0,closure->ToWord());
  for(u_int i=1; i<=nGlobals; i++) 
    closure->Init(i,globals->Sub(i-1));
  DEBUG_PRINT(("return closure\n"));
  RETURN(closure->ToWord());
} END

DEFINE3(UnsafeByteCode_preAssemble) {
  DECLARE_VECTOR(codeVec, x0);
  DECLARE_VECTOR(imVec, x1);
  word nbLocals = x2;
  word bcc = assemble(codeVec,imVec,nbLocals);
  RETURN(bcc);
} END

DEFINE1(UnsafeByteCode_compile) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER)
    REQUEST(cc);

  TagVal *abstractCode;

  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    abstractCode = acc->GetAbstractCode();
  } 
#if HAVE_LIGHTNING
  else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    abstractCode = reinterpret_cast<TagVal*>(ncc->GetAbstractRepresentation());
  }
#endif
  else if (b->GetInterpreter() == ByteCodeInterpreter::self) {
    fprintf(stderr,"byte concrete code found, nothing to be done\n");
    RETURN_UNIT;
  }
  else {
    fprintf(stderr,"unkown interpreter, do nothing\n");
    RETURN_UNIT;
  }

  word w = HotSpotConcreteCode::New(abstractCode);
  HotSpotConcreteCode *hsc = HotSpotConcreteCode::FromWordDirect(w);
  ByteCodeJitter jitter;
  fprintf(stderr,"start byte code jitter\n");
  jitter.Compile(hsc);
  closure->SetConcreteCode(hsc->ToWord());
  fprintf(stderr,"changed concrete code to byte code\n");

  RETURN_UNIT;
} END

DEFINE1(UnsafeByteCode_lazyCompile) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER)
    REQUEST(cc);

  TagVal *abstractCode;

  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    abstractCode = acc->GetAbstractCode();
  } 
#if HAVE_LIGHTNING
  else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    abstractCode = reinterpret_cast<TagVal*>(ncc->GetAbstractRepresentation());
  }
#endif
  else if (b->GetInterpreter() == ByteCodeInterpreter::self) {
    fprintf(stderr,"byte concrete code found, nothing to be done\n");
    RETURN_UNIT;
  }
  else {
    fprintf(stderr,"unkown interpreter, do nothing\n");
    RETURN_UNIT;
  }

  fprintf(stderr,"lazy byte concrete code created\n");
  closure->SetConcreteCode(ByteConcreteCode::New(abstractCode));

  RETURN_UNIT;
} END


AliceDll word UnsafeByteCode() {
  Record *record = Record::New(4);
  INIT_STRUCTURE(record, "UnsafeByteCode", "assemble", 
		 UnsafeByteCode_assemble, 4); 
  INIT_STRUCTURE(record, "UnsafeByteCode", "preAssemble", 
		 UnsafeByteCode_preAssemble, 3); 
  INIT_STRUCTURE(record, "UnsafeByteCode", "compile", 
		 UnsafeByteCode_compile, 1); 
  INIT_STRUCTURE(record, "UnsafeByteCode", "lazyCompile", 
		 UnsafeByteCode_lazyCompile, 1); 
  RETURN_STRUCTURE("UnsafeByteCode$", record);
}

