/******************************** -*- C -*- ****************************
 *
 *	Floating-point function invocation using GNU lightning
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

static jit_insn codeBuffer[300];
static struct jit_fp buffer[300];


typedef int (*intFunc)(int,int);
typedef double (*dblFunc)(double,double);
typedef float (*floatFunc)(float,float);


dblFunc makeDblFunc()
     /* Generate a function that computes and returns the sum of 
        its two double arguments (return an int)
        i.e., double foo(double x,double y) { return x + y;}
     */
{
   dblFunc retVal;
   int dbl1,dbl2;
   jit_set_ip(codeBuffer);
   retVal = (dblFunc)jit_get_ip().iptr; 
   jit_prolog(2);
   jitfp_begin(buffer);   
   dbl1 = jit_arg_d();
   dbl2 = jit_arg_d();
   
   
   jitfp_retval(jitfp_add(jitfp_getarg_d(dbl1),
                          jitfp_getarg_d(dbl2)));
   
   jit_ret();
   jit_flush_code((char*)retVal,jit_get_ip().ptr);  
   
#ifdef LIGHTNING_DISASSEMBLE
   disassemble(stderr, retVal, jit_get_ip().ptr);
#endif

   return retVal;
}


floatFunc makeFloatFunc()
     /* Generate a function that computes and returns the sum of 
        its two double arguments (return an int)
        i.e., double foo(double x,double y) { return x + y;}
     */
{
   floatFunc retVal;
   int dbl1,dbl2;
   //jit_set_ip(codeBuffer);
   retVal = (floatFunc)jit_get_ip().iptr; 
   jit_prolog(2);
   jitfp_begin(buffer);   
   dbl1 = jit_arg_f();
   dbl2 = jit_arg_f();
   
   
   jitfp_retval(jitfp_add(jitfp_getarg_f(dbl1),
                          jitfp_getarg_f(dbl2)));
   
   jit_ret();
   jit_flush_code((char*)retVal,jit_get_ip().ptr);  
   
#ifdef LIGHTNING_DISASSEMBLE
   disassemble(stderr, retVal, jit_get_ip().ptr);
#endif

   return retVal;
}

dblFunc makeCallFunc(dblFunc theFunc) 
{
   dblFunc retVal;
   int dbl1,dbl2;
   //jit_set_ip(codeBuffer);
   retVal = (dblFunc)jit_get_ip().iptr; 
   jit_prolog(2);
   jitfp_begin(buffer);   
   dbl1 = jit_arg_d();
   dbl2 = jit_arg_d();

   jitfp_prepare(0,0,2);
   jitfp_pusharg_d(jitfp_mul(jitfp_getarg_d(dbl1),
                             jitfp_getarg_d(dbl2)));
   jitfp_pusharg_d(jitfp_getarg_d(dbl1));
   jit_finish((void*)theFunc);
   jit_ret();
   jit_flush_code((char*)retVal,jit_get_ip().ptr);  
   
#ifdef LIGHTNING_DISASSEMBLE
   disassemble(stderr, retVal, jit_get_ip().ptr);
#endif

   return retVal;
}

floatFunc makeCallFloatFunc(floatFunc theFunc) 
{
   floatFunc retVal;
   int dbl1,dbl2;
   //jit_set_ip(codeBuffer);
   retVal = (floatFunc)jit_get_ip().iptr; 
   jit_prolog(2);
   jitfp_begin(buffer);   
   dbl1 = jit_arg_f();
   dbl2 = jit_arg_f();

   jitfp_prepare(0,2,0);
   jitfp_pusharg_f(jitfp_mul(jitfp_getarg_f(dbl1),
                             jitfp_getarg_f(dbl2)));
   jitfp_pusharg_f(jitfp_getarg_f(dbl1));
   jit_finish((void*)theFunc);
   jit_ret();
   jit_flush_code((char*)retVal,jit_get_ip().ptr);  
   
#ifdef LIGHTNING_DISASSEMBLE
   disassemble(stderr, retVal, jit_get_ip().ptr);
#endif

   return retVal;
}


int main(int argc,char* argv[])
{
   dblFunc myFunc2 = makeDblFunc();
   floatFunc myFunc3 = makeFloatFunc();
   dblFunc callIt1  = makeCallFunc(myFunc2);
   floatFunc callIt2  = makeCallFloatFunc(myFunc3);

#ifndef LIGHTNING_CROSS
   double y = callIt1(10.5,15.3);
   float a = 1.5;
   float b = 10.5;
   float z = callIt2(a,b);
   printf("result is %f\t %f\n",y,z);
#endif

   return 0;
}
