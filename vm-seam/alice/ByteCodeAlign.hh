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

/*
 * This file includes marcos for instruction layout intended for 32 bit 
 * architectures. 
 * - Abbreviations used in macro definition:
 *   R = register(s)
 *   I = immediate(s)
 * - "index" globally indicates the current slot of the code buffer 
 *    and is manipulated accordingly to maintain this invariant. 
 *    So, if you use PC as index, the PC is autamatically incremented.
 * 
 */

#ifndef __ALICE_BYTE_CODE_ALIGN32_HH__
#define __ALICE_BYTE_CODE_ALIGN32_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeAlign.hh"
#endif

#include "alice/ByteCodeBuffer.hh"

class ByteCode; // forward declaration

/*
 * We use a very simple layout both for switch and threaded interpreter
 *
 * ----------------------------- 
 * |       instruction         |
 * -----------------------------
 * |       register_0          |
 * -----------------------------
 * |       register_1          |
 * -----------------------------
 * |         ...               |
 * -----------------------------
 * |       immediate_0         |
 * -----------------------------
 * |          ...              |
 *
 * So registers and immediates are encoded in the same way
 */                                             

// This compiler switch changes the code layout insofar that it packs
// two registers into one code slot.        
//#define SMALL_REGS
#undef SMALL_REGS

// set immediates
#define ENCODE_1I(index,i)			\
  WriteBuffer::SetSlot(index++,i)

#define ENCODE_2I(index,i1,i2)			\
  ENCODE_1I(index,i1);				\
  ENCODE_1I(index,i2)					
						
#define ENCODE_3I(index,i1,i2,i3)		\
  ENCODE_2I(index,i1,i2);			\
  ENCODE_1I(index,i3)					
			
// get immediates			
#ifdef THREADED
#define DECODE_1I(buffer,index,i) 		\
    i = *index++
#else
#define DECODE_1I(buffer,index,i)		\
  i = buffer->GetSlotInt(index++)			   
#endif
		
#define DECODE_2I(buffer,index,i1,i2)		\
  DECODE_1I(buffer,index,i1);			\
  DECODE_1I(buffer,index,i2)			
						
#define DECODE_3I(buffer,index,i1,i2,i3)	\
  DECODE_2I(buffer,index,i1,i2);		\
  DECODE_1I(buffer,index,i3)


// set registers
#define ENCODE_1R(index,r) {			\
  u_int slot = r & 0xFFFF;			\
  ENCODE_1I(index,slot);			\
}

#ifdef SMALL_REGS
#define ENCODE_2R(index,r1,r2) {		\
  u_int slot = (r1 << 16) | (r2 & 0xFFFF);	\
  ENCODE_1I(index,slot);			\
}
#else
#define ENCODE_2R(index,r1,r2) {		\
  ENCODE_1I(index,r1); 				\
  ENCODE_1I(index,r2);				\
}
#endif

#define ENCODE_3R(index,r1,r2,r3)		\
  ENCODE_2R(index,r1,r2);			\
  ENCODE_1R(index,r3)

#define ENCODE_4R(index,r1,r2,r3,r4)		\
  ENCODE_2R(index,r1,r2);			\
  ENCODE_2R(index,r3,r4)

// get registers
#define DECODE_1R(buffer,index,r) 		\
  DECODE_1I(buffer,index,r)

#ifdef SMALL_REGS
#define DECODE_2R(buffer,index,r1,r2) {		\
  u_int slot;					\
  DECODE_1I(buffer,index,slot);			\
  r1 = slot >> 16;				\
  r2 = slot & 0xFFFF;				\
}
#else
#define DECODE_2R(buffer,index,r1,r2) {		\
  DECODE_1I(buffer,index,r1); 			\
  DECODE_1I(buffer,index,r2);			\
}
#endif

#define DECODE_3R(buffer,index,r1,r2,r3)	\
  DECODE_2R(buffer,index,r1,r2);		\
  DECODE_1R(buffer,index,r3);

#define DECODE_4R(buffer,index,r1,r2,r3,r4)	\
  DECODE_2R(buffer,index,r1,r2);		\
  DECODE_2R(buffer,index,r3,r4);

#ifdef THREADED
// ensure that the lookup table is initialized
#define ENCODE_INSTR(index,instr) \
  ENCODE_1I(index, reinterpret_cast<u_int>(ByteCode::LookupInstr(instr)))
#else
#define ENCODE_INSTR(index,instr)	\
  ENCODE_1I(index,instr)
#endif // THREADED

#define DECODE_INSTR(buffer,index,instr)	\
  DECODE_1I(buffer,index,instr)

#define SKIP_INSTR(PC) PC++

// set instructions + arguments
#define SET_1I(index,i) ENCODE_1I(index,i)
#define SET_1R(index,r) SET_1I(index,r)  

#define SET_INSTR(index,instr) ENCODE_INSTR(index,instr)	
							
#define SET_INSTR_1I(index,instr,i) {		\
  SET_INSTR(index,instr);			\
  ENCODE_1I(index,i); }

#define SET_INSTR_2I(index,instr,i1,i2) {	\
  SET_INSTR(index,instr);			\
  ENCODE_2I(index,i1,i2); }
							
#define SET_INSTR_1R(index,instr,r) {		\
  SET_INSTR(index,instr);			\
  ENCODE_1R(index,r);				\
}

#define SET_INSTR_2R(index,instr,r1,r2) {	\
  SET_INSTR(index,instr);			\
  ENCODE_2R(index,r1,r2);			\
}

#define SET_INSTR_3R(index,instr,r1,r2,r3) {	\
  SET_INSTR(index,instr);			\
  ENCODE_3R(index,r1,r2,r3);			\
}

#define SET_INSTR_4R(index,instr,r1,r2,r3,r4) {	\
  SET_INSTR(index,instr);			\
  ENCODE_4R(index,r1,r2,r3,r4);			\
}

#define SET_INSTR_1R1I(index,instr,r,i) {		\
  SET_INSTR_1R(index,instr,r);			        \
  ENCODE_1I(index,i); }
							
#define SET_INSTR_1R2I(index,instr,r,i1,i2) {		\
  SET_INSTR_1R(index,instr,r);			        \
  ENCODE_2I(index,i1,i2); }
							
#define SET_INSTR_1R3I(index,instr,r,i1,i2,i3) {	\
  SET_INSTR_1R(index,instr,r);			        \
  ENCODE_3I(index,i1,i2,i3); }
							
#define SET_INSTR_2R1I(index,instr,r1,r2,i) {		\
  SET_INSTR_2R(index,instr,r1,r2);			\
  ENCODE_1I(index,i); }

#define SET_INSTR_2R2I(index,instr,r1,r2,i1,i2) {	\
  SET_INSTR_2R(index,instr,r1,r2);			\
  ENCODE_2I(index,i1,i2); }

#define SET_INSTR_3R1I(index,instr,r1,r2,r3,i) {	\
  SET_INSTR_3R(index,instr,r1,r2,r3);			\
  ENCODE_1I(index,i); }
									
// get instruction + arguments

#define GET_INSTR(buffer,index,instr)		\
  DECODE_INSTR(buffer,index,instr)      
							
#define GET_1I(buffer,index,i)			\
  u_int i;					\
  DECODE_1I(buffer,index,i)

#define GET_2I(buffer,index,i1,i2)		\
  u_int i1, i2;					\
  DECODE_2I(buffer,index,i1,i2)   

#define GET_3I(buffer,index,i1,i2,i3)		\
  u_int i1, i2, i3;				\
  DECODE_3I(buffer,index,i1,i2, i3)   

#define GET_1R(buffer,index,r)			\
  u_int r;					\
  DECODE_1R(buffer,index,r)

#define GET_2R(buffer,index,r1,r2)		\
  u_int r1, r2;					\
  DECODE_2R(buffer,index,r1,r2)

#define GET_3R(buffer,index,r1,r2,r3)		\
  u_int r1, r2, r3;				\
  DECODE_3R(buffer,index,r1,r2,r3)

#define GET_4R(buffer,index,r1,r2,r3,r4)	\
  u_int r1, r2, r3, r4;				\
  DECODE_4R(buffer,index,r1,r2,r3,r4)

#define GET_1R1I(buffer,index,r,i)		\
  GET_1R(buffer,index,r);			\
  GET_1I(buffer,index,i)				
							
#define GET_1R2I(buffer,index,r,i1,i2)		\
  GET_1R(buffer,index,r);			\
  GET_2I(buffer,index,i1,i2)				
							
#define GET_1R3I(buffer,index,r,i1,i2,i3)	\
  GET_1R(buffer,index,r);			\
  GET_3I(buffer,index,i1,i2,i3)					
							
#define GET_2R1I(buffer,index,r1,r2,i)		\
  GET_2R(buffer,index,r1,r2);			\
  GET_1I(buffer,index,i)				

#define GET_2R2I(buffer,index,r1,r2,i1,i2)	\
  GET_2R(buffer,index,r1,r2);			\
  GET_2I(buffer,index,i1,i2)				
	
#define GET_3R1I(buffer,index,r1,r2,r3,i)	\
  GET_3R(buffer,index,r1,r2,r3);		\
  GET_1I(buffer,index,i)				

// rewrite instruction

#ifdef THREADED
#define REWRITE_INSTR(buffer,index,instr) {				\
    *index = reinterpret_cast<u_int>(ByteCode::LookupInstr(instr));			\
  }
#else
#define REWRITE_INSTR(buffer,index,instr) {	\
    buffer->RewriteSlot(index,instr);		\
  }
#endif // THREADED

 
#endif // __ALICE_BYTE_CODE_ALIGN32_HH__
