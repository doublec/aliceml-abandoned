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

// instruction definition

// be careful that they are lexicographically ordered; otherwise the ByteCode
// Assembler won't work

// the second argument to INSTR describes the instruction arguments

INSTR(await,			ARGS_1R)
INSTR(bci_call,			ARGS_ALICEFUN_DYNR)
INSTR(bci_call0,		ARGS_ALICEFUN)
INSTR(bci_call1,		ARGS_ALICEFUN_1R)
INSTR(bci_call2,		ARGS_ALICEFUN_2R)
INSTR(bci_call3,		ARGS_ALICEFUN_3R)
INSTR(bci_tailcall,		ARGS_ALICEFUN_DYNR)
INSTR(bci_tailcall0,		ARGS_ALICEFUN)
INSTR(bci_tailcall1,		ARGS_ALICEFUN_1R)
INSTR(bci_tailcall2,		ARGS_ALICEFUN_2R)
INSTR(bci_tailcall3,		ARGS_ALICEFUN_3R)
INSTR(bigtagtest,		ARGS_1R_1I)
INSTR(bigtagtest1,		ARGS_1R_1I_JUMP)
INSTR(cbigtagtest,		ARGS_1R_DYNJUMP)
INSTR(cbigtagtest_direct,	ARGS_1R_DYNJUMP)
INSTR(ccc0,			ARGS_NONE)
INSTR(ccc1,			ARGS_NONE)
INSTR(cccn,			ARGS_1I)
INSTR(check_preempt_jump,	ARGS_JUMP)
INSTR(citest,			ARGS_1R_1I_DYNJUMP)
INSTR(contest,			ARGS_2R_JUMP)
INSTR(ctagtest,			ARGS_1R_DYNJUMP)
INSTR(ctagtest_direct,		ARGS_1R_DYNJUMP)
INSTR(debug_msg,		ARGS_1I)
INSTR(dummy_raiseoverflow,	ARGS_NONE)
INSTR(get_arg0,			ARGS_1R)
INSTR(get_arg0_direct,		ARGS_NONE)
INSTR(get_args,			ARGS_1I)
INSTR(get_args_direct,		ARGS_1I)
INSTR(get_tup2,			ARGS_3R)
INSTR(get_tup3,			ARGS_4R)
INSTR(iadd,			ARGS_3R)
INSTR(idec,			ARGS_2R)
INSTR(iequal,			ARGS_2R_1I)
INSTR(igreater,			ARGS_3R)
INSTR(igreater_eq,		ARGS_3R)
INSTR(iinc,			ARGS_2R)
INSTR(ijump_eq,			ARGS_1R_1I_JUMP)
INSTR(iless,			ARGS_3R)
INSTR(iless_eq,			ARGS_3R)
INSTR(immediate_call,		ARGS_1I_DYNR)
INSTR(immediate_call0,		ARGS_1I)
INSTR(immediate_call1,		ARGS_1R_1I)
INSTR(immediate_call2,		ARGS_2R_1I)
INSTR(immediate_call3,		ARGS_3R_1I)
INSTR(immediate_tailcall,	ARGS_1I_DYNR)
INSTR(immediate_tailcall0,	ARGS_1I)
INSTR(immediate_tailcall1,	ARGS_1R_1I)
INSTR(immediate_tailcall2,	ARGS_2R_1I)
INSTR(immediate_tailcall3,	ARGS_3R_1I)
INSTR(init_bigtagval,		ARGS_2R_1I)
INSTR(init_closure,		ARGS_2R_1I)
INSTR(init_con,			ARGS_2R_1I)
INSTR(init_polyrec,		ARGS_2R_1I)
INSTR(init_tagval,		ARGS_2R_1I)
INSTR(init_tup,			ARGS_2R_1I)
INSTR(init_vec,			ARGS_2R_1I)
INSTR(inlined_array_length,	ARGS_2R)
INSTR(inlined_array_sub,	ARGS_3R)
INSTR(inlined_array_usub,	ARGS_3R)
INSTR(inlined_equal,		ARGS_3R)
INSTR(inlined_equal_identity,	ARGS_3R)
INSTR(inlined_future_byneed,	ARGS_2R)
INSTR(inlined_hole_fill,	ARGS_2R)
INSTR(inlined_hole_hole,	ARGS_1R)
INSTR(inlined_vector_length,	ARGS_2R)
INSTR(inlined_vector_sub,	ARGS_3R)
INSTR(inlined_vector_usub,	ARGS_3R)
INSTR(install_handler,		ARGS_1I)
INSTR(isub,			ARGS_3R)
INSTR(itest,			ARGS_1R_1I)
INSTR(jump,			ARGS_JUMP)
INSTR(lazyselect_polyrec,	ARGS_2R_1I)
INSTR(lazyselect_polyrec_n,	ARGS_1R_2I)
INSTR(load_bigtagval,		ARGS_2R_1I)
INSTR(load_bigtagval1,		ARGS_2R)
INSTR(load_bigtagval2,		ARGS_3R)
INSTR(load_bigtagval3,		ARGS_4R)
INSTR(load_cell,		ARGS_2R)
INSTR(load_con,			ARGS_2R_1I)
INSTR(load_global,		ARGS_1R_1I)
INSTR(load_immediate,		ARGS_1R_1I)
INSTR(load_int,			ARGS_1R_1I)
INSTR(load_reg,			ARGS_2R)
INSTR(load_tagval,		ARGS_2R_1I)
INSTR(load_tagval1,		ARGS_2R)
INSTR(load_tagval2,		ARGS_3R)
INSTR(load_tagval3,		ARGS_4R)
INSTR(load_vec,			ARGS_2R_1I)
INSTR(load_zero,		ARGS_1R)
INSTR(mk_closure,		ARGS_1R_2I)
INSTR(mk_closure_init1,		ARGS_1R_1I_STATR1)
INSTR(mk_closure_init2,		ARGS_1R_1I_STATR2)
INSTR(mk_closure_init3,		ARGS_1R_1I_STATR3)
INSTR(mk_closure_init4,		ARGS_1R_1I_STATR4)
INSTR(new_bigtagval,		ARGS_1R_2I)
INSTR(new_bigtagval_init,	ARGS_1R_1I_DYNR)
INSTR(new_bigtagval_init1,	ARGS_1R_1I_STATR1)
INSTR(new_bigtagval_init2,	ARGS_1R_1I_STATR2)
INSTR(new_bigtagval_init3,	ARGS_1R_1I_STATR3)
INSTR(new_bigtagval_init4,	ARGS_1R_1I_STATR4)
INSTR(new_cell,			ARGS_2R)
INSTR(new_con,			ARGS_1R_1I)
INSTR(new_pair,			ARGS_3R)
INSTR(new_polyrec,		ARGS_1R_1I)
INSTR(new_tagval,		ARGS_1R_2I)
INSTR(new_tagval_init,		ARGS_1R_1I_DYNR)
INSTR(new_tagval_init1,		ARGS_1R_1I_STATR1)
INSTR(new_tagval_init2,		ARGS_1R_1I_STATR2)
INSTR(new_tagval_init3,		ARGS_1R_1I_STATR3)
INSTR(new_tagval_init4,		ARGS_1R_1I_STATR4)
INSTR(new_triple,		ARGS_4R)
INSTR(new_tup,			ARGS_1R_1I)
INSTR(new_vec,			ARGS_1R_1I)
INSTR(prepare_con,		ARGS_2R_1I)
INSTR(raise,			ARGS_1R)
INSTR(reraise,			ARGS_1R)
INSTR(remove_handler,		ARGS_NONE)
INSTR(rewrite_call,		ARGS_ALICEFUN_DYNR)
INSTR(rewrite_call0,		ARGS_ALICEFUN)
INSTR(rewrite_call1,		ARGS_ALICEFUN_1R)
INSTR(rewrite_call2,		ARGS_ALICEFUN_2R)
INSTR(rewrite_call3,		ARGS_ALICEFUN_3R)
INSTR(rewrite_tailcall,		ARGS_ALICEFUN_DYNR)
INSTR(rewrite_tailcall0,	ARGS_ALICEFUN)
INSTR(rewrite_tailcall1,	ARGS_ALICEFUN_1R)
INSTR(rewrite_tailcall2,	ARGS_ALICEFUN_2R)
INSTR(rewrite_tailcall3,	ARGS_ALICEFUN_3R)
INSTR(rjump_eq,			ARGS_1R_1I_JUMP)
INSTR(rtest,			ARGS_1R_1I)
INSTR(seam_call,		ARGS_1R_DYNR)
INSTR(seam_call0,		ARGS_1R)
INSTR(seam_call1,		ARGS_2R)
INSTR(seam_call2,		ARGS_3R)
INSTR(seam_call3,		ARGS_4R)
INSTR(seam_ccc1,		ARGS_1R)
INSTR(seam_cccn,		ARGS_1I)
INSTR(seam_prim_call,		ARGS_PRIMFUN_DYNR)
INSTR(seam_prim_call0,		ARGS_PRIMFUN)
INSTR(seam_prim_call1,		ARGS_PRIMFUN_1R)
INSTR(seam_prim_call2,		ARGS_PRIMFUN_2R)
INSTR(seam_prim_call3,		ARGS_PRIMFUN_3R)
INSTR(seam_prim_tailcall,	ARGS_PRIMFUN_DYNR)
INSTR(seam_prim_tailcall0,	ARGS_PRIMFUN)
INSTR(seam_prim_tailcall1,	ARGS_PRIMFUN_1R)
INSTR(seam_prim_tailcall2,	ARGS_PRIMFUN_2R)
INSTR(seam_prim_tailcall3,	ARGS_PRIMFUN_3R)
INSTR(seam_return,		ARGS_DYNR)
INSTR(seam_return0,		ARGS_NONE)
INSTR(seam_return1,		ARGS_1R)
INSTR(seam_return2,		ARGS_2R)
INSTR(seam_return3,		ARGS_3R)
INSTR(seam_return_int,		ARGS_1I)
INSTR(seam_return_zero,		ARGS_NONE)
INSTR(seam_tailcall,		ARGS_1R_DYNR)
INSTR(seam_tailcall0,		ARGS_1R)
INSTR(seam_tailcall1,		ARGS_2R)
INSTR(seam_tailcall2,		ARGS_3R)
INSTR(seam_tailcall3,		ARGS_4R)
INSTR(select_tup,		ARGS_2R_1I)
INSTR(select_tup0,		ARGS_2R)
INSTR(select_tup1,		ARGS_2R)
INSTR(select_tup2,		ARGS_2R)
INSTR(self_call,		ARGS_DYNR)
INSTR(self_call0,		ARGS_NONE)
INSTR(self_call1,		ARGS_1R)
INSTR(self_call2,		ARGS_2R)
INSTR(self_call3,		ARGS_3R)
INSTR(set_cell,			ARGS_2R)
INSTR(set_global,		ARGS_1R_1I)
INSTR(sjump_eq,			ARGS_1R_1I_JUMP)
INSTR(spec_closure,		ARGS_2R_1I)
INSTR(stest,			ARGS_1R_1I)
INSTR(swap_regs,		ARGS_2R)
INSTR(tagtest,			ARGS_1R_1I)
INSTR(tagtest1,			ARGS_1R_1I_JUMP)
INSTR(vectest,			ARGS_1R_1I)
