/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer inline functions (PowerPC)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/



#ifndef __lightning_funcs_h
#define __lightning_funcs_h

#if !defined(__GNUC__) && !defined(__GNUG__)
#error Go get GNU C, I do not know how to flush the cache
#error with this compiler.
#else
/* Why doesn't this compile?!?
 * static void
 * jit_flush_code(start, end)
 *      void 	*start;
 *      void	*end;
 */

static void
jit_flush_code(void* start, void* end)
{
#ifndef LIGHTNING_CROSS
  static int cache_line_size, cache_line_shift;
  register char *ddest, *idest;
  register int n, dcache_lines, icache_lines;

  if (cache_line_size == 0) {
    char buffer[8192];
    int i, probe;

    /* Find out the size of a cache line by zeroing one */
    memset(buffer, 0xFF, 8192);
    __asm__ __volatile__ ("dcbz 0,%0"::"r"(buffer + 4096));

    /* Probe for the beginning of the cache line. */
    for(i = 0, probe = 4096; probe; probe >>= 1)
      if (buffer[i | probe] != 0x00)
        i |= probe;

    /* i is now just before the start of the cache line */
    i++;
    for(cache_line_shift = 0, cache_line_size = 1; 
	 i + cache_line_size < 8192; cache_line_shift++, cache_line_size <<= 1)
      if (buffer[i + cache_line_size] != 0x00)
        break;
  }

  n = ((char *) end) - ((char *)start);
  dcache_lines = icache_lines = (n + cache_line_size - 1) >> cache_line_shift;

  /* Force data cache write-backs */
  for (ddest = start; dcache_lines--; ddest += cache_line_size) {
    __asm__ __volatile__ ("dcbst 0,%0"::"r"(ddest));
  }
  __asm__ __volatile__ ("sync"::);

  /* Now invalidate the instruction cache */
  for (idest = start; icache_lines--; idest += cache_line_size) {
    __asm__ __volatile__ ("icbi 0,%0; isync"::"r"(idest));
  }
  __asm__ __volatile__ ("isync"::);
#endif
}
#endif

#define _jit (*jit)

/* Emit a trampoline for a function.
 * Upon entrance to the trampoline:
 *   - R0      = return address for the function
 *   - LR      = address where the real code for the function lies
 *   - R3-R8   = parameters
 * After jumping to the address pointed to by R10:
 *   - LR      = address where the epilog lies (the function must return there)
 *   - R25-R20 = parameters (order is reversed, 1st argument is R25)
 */
static jit_insn *
_jit_trampoline(jit, n)
     register jit_state *jit;
     register int	n;
{
  static jit_insn	trampolines[200];
  static jit_insn	*p_trampolines[6], *free = trampolines;
  register int		i, ofs, frame_size;

  if (!p_trampolines[n]) {
    _jit.pc = p_trampolines[n] = free;

    frame_size = 8 + (6 + n) * 4;	/* r26..r31 + args		   */
    STWUrm(1, -frame_size, 1);		/* stwu  r1, -x(r1)		   */

    for (ofs = 8, i = 26 - n; i <= 31; ofs += 4, i++) {
      STWrm(i, ofs, 1);			/* stw   rI, ofs(r1)		   */
    }
    STWrm(0, ofs+4, 1);			/* stw   r0, x(r1)		   */
    for (i = 0; i < n; i++) {
      MRrr(25-i, 3+i);			/* save parameters in r25..r20	   */
    }
    BLRL();				/* blrl				   */
    LWZrm(0, ofs+4, 1);			/* lwz   r0, x(r1)  (ret.addr.)    */
    MTLRr(0);				/* mtspr LR, r0			   */

    for (ofs = 8, i = 26 - n; i <= 31; ofs += 4, i++) {
      LWZrm(i, ofs, 1);			/* lwz   rI, ofs(r1)		   */
    }
    ADDIrri(1, 1, frame_size);		/* addi  r1, r1, x		   */
    BLR();				/* blr				   */

    jit_flush_code(p_trampolines[n], _jit.pc);
    free = _jit.pc;
  }

  return p_trampolines[n];
}

static void
_jit_prolog(jit, n)
     register jit_state *jit;
     register int	n;
{
  register jit_insn	*save_pc, *trampo;

  save_pc = _jit.pc;
  trampo = _jit_trampoline(jit, n);
  _jit.pc = save_pc;

  _jitl.nextarg_get = 25;
  MFLRr(0);
  MOVEIri(10, trampo);
  MTLRr(10);
  BLRL();				/* blrl				  */
  MFLRr(31);				/* mflr  r31			  */
}

#undef _jit

#endif /* __lightning_funcs_h */
