/******************************** -*- C -*- ****************************
 *
 *	Sample RPN calculator using GNU lightning
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000 Free Software Foundation, Inc.
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

static jit_insn codeBuffer[1024];

typedef int (*pifi)(int);	/* Pointer to Int Function of Int */


pifi compile_rpn(char *expr)
{
  pifi fn;
  int ofs;
  fn = (pifi) (jit_get_ip().iptr);
  jit_leaf(1);
  ofs = jit_arg_i();
  jit_getarg_i(JIT_R0, ofs);

  while (*expr) {
    char buf[32];
    int n;
    if (sscanf(expr, "%[0-9]%n", buf, &n)) {
      expr += n - 1;
      jit_pushr_i(JIT_R0);
      jit_movi_i(JIT_R0, atoi(buf));
    } else if (*expr == '+') {
      jit_popr_i(JIT_R1);
      jit_addr_i(JIT_R0, JIT_R1, JIT_R0);
    }
    else if (*expr == '-') {
      jit_popr_i(JIT_R1);
      jit_subr_i(JIT_R0, JIT_R1, JIT_R0);
    }
    else if (*expr == '*') {
      jit_popr_i(JIT_R1);
      jit_mulr_i(JIT_R0, JIT_R1, JIT_R0);
    }
    else if (*expr == '/') {
      jit_popr_i(JIT_R1);
      jit_divr_i(JIT_R0, JIT_R1, JIT_R0);
    }
    else {
      fprintf(stderr, "cannot compile: %s\n", expr);
      abort();
    }
  ++expr;
  }
  jit_movr_i(JIT_RET, JIT_R0);
  jit_ret();

  jit_flush_code((char *)fn, jit_get_ip().ptr);

#ifdef LIGHTNING_DISASSEMBLE
  disassemble(stderr, (char *)fn, jit_get_ip().ptr);
#endif
  return fn;
}


int main()
{
  pifi c2f, f2c;
  int i;

  jit_set_ip(codeBuffer);
  c2f = compile_rpn("9*5/32+");
  f2c = compile_rpn("32-5*9/");

#ifndef LIGHTNING_CROSS  
  printf("\nC:"); for (i = 0; i <= 100; i += 10) printf("%3d ", i);
  printf("\nF:"); for (i = 0; i <= 100; i += 10) printf("%3d ", c2f(i));
  printf("\n");
  printf("\nF:"); for (i = 32; i <= 212; i += 10) printf("%3d ", i);
  printf("\nC:"); for (i = 32; i <= 212; i += 10) printf("%3d ", f2c(i));
  printf("\n");
#endif
  return 0;
}
