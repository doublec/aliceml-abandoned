/******************************** -*- C -*- ****************************
 *
 *	Floating-point miscellanea using GNU lightning
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2002 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "lightning.h"

static jit_insn codeBuffer[300];
static struct jit_fp buffer[300];
static double a;

void
int_test(what, code)
     char     *what;
     jit_code code;
{
  a = -2.6; printf("%s\t\t%d ", what, code.iptr());
  a = -2.4; printf("%d ", code.iptr());
  a = 0.0; printf("%d ", code.iptr());
  a = 2.4; printf("%d ", code.iptr());
  a = 2.6; printf("%d\n", code.iptr());
}

int
main()
{
  jit_code code;
  code.ptr = (char *) codeBuffer;

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_cmp(JIT_R1, JIT_R0,
    jitfp_ldi_d(&a)
  );
  jit_subr_i(JIT_RET, JIT_R0, JIT_R1);	/* [greater] - [less] = -1/0/1 */
  jit_ret();

  jit_flush_code(codeBuffer, jit_get_ip().ptr);
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  int_test("compare", code);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_trunc(JIT_RET,
    jitfp_ldi_d(&a)
  );
  jit_ret();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  int_test("trunc", code);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_ceil(JIT_RET,
    jitfp_ldi_d(&a)
  );
  jit_ret();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  int_test("ceil", code);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_floor(JIT_RET,
    jitfp_ldi_d(&a)
  );
  jit_ret();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  int_test("floor", code);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_round(JIT_RET,
    jitfp_ldi_d(&a)
  );
  jit_ret();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  int_test("round", code);
#endif

#ifdef JIT_TRANSCENDENTAL
  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_sti_d(&a,
    jitfp_log(
      jitfp_exp(jitfp_imm(1.0))
    )
  );
  jit_ret();
  code.vptr();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  printf("log e = \t%f\n", a);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_sti_d(&a,
    jitfp_atn(
      jitfp_imm(1.732050807657)
    )
  );
  jit_ret();
  code.vptr();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  printf("pi =         \t%f\n", a*3);
#endif

  jit_set_ip(codeBuffer);
  jit_leaf(0);
  jitfp_begin(buffer);
  jitfp_sti_d(&a,
    jitfp_tan(
      jitfp_ldi_d(&a)
    )
  );
  jit_ret();
  code.vptr();
#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, codeBuffer, jit_get_ip().ptr);
#endif
#ifndef LIGHTNING_CROSS
  printf("tan^2 pi/3 = \t%f\n", a*a);
#endif

#endif /* JIT_TRANSCEDENTAL */

  return (0);
}
