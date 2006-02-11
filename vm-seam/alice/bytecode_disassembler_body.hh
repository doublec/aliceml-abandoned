//
// this output is automatically generated from ByteCodeInstrs.hh
//

case await:
    {
       fprintf(f,"await");
       fprintf(stderr,"\n");
    }
    return PC;

case bci_call:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"bci_call I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case bci_call0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"bci_call0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_call1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"bci_call1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_call2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"bci_call2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_call3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"bci_call3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_tailcall:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"bci_tailcall I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case bci_tailcall0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"bci_tailcall0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_tailcall1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"bci_tailcall1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_tailcall2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"bci_tailcall2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bci_tailcall3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"bci_tailcall3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bigtagtest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"bigtagtest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case bigtagtest1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"bigtagtest1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case cbigtagtest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"cbigtagtest R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1I(codeBuffer,PC,im);
           fprintf(stderr,"I%d ",im);
       }
       if(1>0) {
         GET_1I(codeBuffer,PC,im);
         fprintf(stderr,"I%d",im);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case cbigtagtest_direct:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"cbigtagtest_direct R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1I(codeBuffer,PC,im);
           fprintf(stderr,"I%d ",im);
       }
       if(1>0) {
         GET_1I(codeBuffer,PC,im);
         fprintf(stderr,"I%d",im);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case ccc1:
    {
       fprintf(f,"ccc1");
       fprintf(stderr,"\n");
    }
    return PC;

case cccn:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"cccn I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case check_preempt_jump:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"check_preempt_jump I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case citest:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"citest R%d I%d I%d",r0,i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1I(codeBuffer,PC,im);
           fprintf(stderr,"I%d ",im);
       }
       if(2>0) {
         GET_1I(codeBuffer,PC,im);
         fprintf(stderr,"I%d",im);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case contest:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"contest R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case ctagtest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"ctagtest R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1I(codeBuffer,PC,im);
           fprintf(stderr,"I%d ",im);
       }
       if(1>0) {
         GET_1I(codeBuffer,PC,im);
         fprintf(stderr,"I%d",im);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case ctagtest_direct:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"ctagtest_direct R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1I(codeBuffer,PC,im);
           fprintf(stderr,"I%d ",im);
       }
       if(1>0) {
         GET_1I(codeBuffer,PC,im);
         fprintf(stderr,"I%d",im);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case debug_msg:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"debug_msg I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case get_arg0:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"get_arg0 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case get_arg0_direct:
    {
       fprintf(f,"get_arg0_direct");
       fprintf(stderr,"\n");
    }
    return PC;

case get_args:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"get_args I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case get_args_direct:
    {
       fprintf(f,"get_args_direct");
       fprintf(stderr,"\n");
    }
    return PC;

case get_tup2:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"get_tup2 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case get_tup3:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"get_tup3 R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case iadd:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"iadd R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case idec:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"idec R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case iinc:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"iinc R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case ijump_eq:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"ijump_eq R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_call:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"immediate_call I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_call0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"immediate_call0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_call1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"immediate_call1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_call2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"immediate_call2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_call3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"immediate_call3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_tailcall:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"immediate_tailcall I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_tailcall0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"immediate_tailcall0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_tailcall1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"immediate_tailcall1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_tailcall2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"immediate_tailcall2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case immediate_tailcall3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"immediate_tailcall3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_bigtagval:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_bigtagval R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_closure:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_closure R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_con:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_con R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_polyrec:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_polyrec R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_tagval:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_tagval R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_tup:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_tup R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case init_vec:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"init_vec R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case inlined_future_byneed:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"inlined_future_byneed R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case inlined_hole_fill:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"inlined_hole_fill R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case inlined_hole_hole:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"inlined_hole_hole R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case install_handler:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"install_handler I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case isub:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"isub R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case itest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"itest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case jump:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"jump I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case lazyselect_polyrec:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"lazyselect_polyrec R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case lazyselect_polyrec_n:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"lazyselect_polyrec_n R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case load_bigtagval:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"load_bigtagval R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_bigtagval1:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"load_bigtagval1 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case load_bigtagval2:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"load_bigtagval2 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case load_bigtagval3:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"load_bigtagval3 R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case load_cell:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"load_cell R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case load_con:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"load_con R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_global:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"load_global R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_immediate:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"load_immediate R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_int:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"load_int R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_reg:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"load_reg R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case load_tagval:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"load_tagval R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_tagval1:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"load_tagval1 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case load_tagval2:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"load_tagval2 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case load_tagval3:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"load_tagval3 R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case load_vec:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"load_vec R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case load_zero:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"load_zero R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case mk_closure:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"mk_closure R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"new_bigtagval R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval_init:
    {
       fprintf(f,"new_bigtagval_init");
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval_init1:
    {
       GET_3I(codeBuffer,PC,i0,i1,i2);
       fprintf(f,"new_bigtagval_init1 I%d I%d I%d",i0,i1,i2);
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval_init2:
    {
       GET_3I(codeBuffer,PC,i0,i1,i2);
       fprintf(f,"new_bigtagval_init2 I%d I%d I%d",i0,i1,i2);
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval_init3:
    {
       GET_3I(codeBuffer,PC,i0,i1,i2);
       fprintf(f,"new_bigtagval_init3 I%d I%d I%d",i0,i1,i2);
       fprintf(stderr,"\n");
    }
    return PC;

case new_bigtagval_init4:
    {
       GET_3I(codeBuffer,PC,i0,i1,i2);
       fprintf(f,"new_bigtagval_init4 I%d I%d I%d",i0,i1,i2);
       fprintf(stderr,"\n");
    }
    return PC;

case new_cell:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"new_cell R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_con:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"new_con R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case new_pair:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"new_pair R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case new_polyrec:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"new_polyrec R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval:
    {
       fprintf(f,"new_tagval");
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval_init:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"new_tagval_init I%d I%d",i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval_init1:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"new_tagval_init1 I%d I%d",i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval_init2:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"new_tagval_init2 I%d I%d",i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval_init3:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"new_tagval_init3 I%d I%d",i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tagval_init4:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"new_tagval_init4 I%d I%d",i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case new_triple:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"new_triple R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case new_tup:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"new_tup R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case new_vec:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"new_vec R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case prepare_con:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"prepare_con R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case raise_direct:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"raise_direct R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case raise_normal:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"raise_normal R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case remove_handler:
    {
       fprintf(f,"remove_handler");
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_call:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"rewrite_call I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_call0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"rewrite_call0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_call1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"rewrite_call1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_call2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"rewrite_call2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_call3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"rewrite_call3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_tailcall:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"rewrite_tailcall I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_tailcall0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"rewrite_tailcall0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_tailcall1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"rewrite_tailcall1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_tailcall2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"rewrite_tailcall2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rewrite_tailcall3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"rewrite_tailcall3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case rjump_eq:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"rjump_eq R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case rtest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"rtest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_call:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"seam_call R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(1>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_call0:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"seam_call0 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_call1:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"seam_call1 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_call2:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"seam_call2 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_call3:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"seam_call3 R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_ccc1:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"seam_ccc1 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_cccn:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"seam_cccn I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_call:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"seam_prim_call I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_call0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"seam_prim_call0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_call1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"seam_prim_call1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_call2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"seam_prim_call2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_call3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"seam_prim_call3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_tailcall:
    {
       GET_2I(codeBuffer,PC,i0,i1);
       fprintf(f,"seam_prim_tailcall I%d I%d",i0,i1);
       fprintf(stderr," [");
       for(s_int i=0; i<i1-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(2>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_tailcall0:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"seam_prim_tailcall0 I%d",i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_tailcall1:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"seam_prim_tailcall1 R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_tailcall2:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"seam_prim_tailcall2 R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_prim_tailcall3:
    {
       GET_3R1I(codeBuffer,PC,r0,r1,r2,i0);
       fprintf(f,"seam_prim_tailcall3 R%d R%d R%d I%d",r0,r1,r2,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_return:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"seam_return I%d",i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(1>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_return0:
    {
       fprintf(f,"seam_return0");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_return1:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"seam_return1 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_return2:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"seam_return2 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_return3:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"seam_return3 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_tailcall:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"seam_tailcall R%d I%d",r0,i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(1>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case seam_tailcall0:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"seam_tailcall0 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_tailcall1:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"seam_tailcall1 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_tailcall2:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"seam_tailcall2 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case seam_tailcall3:
    {
       GET_4R(codeBuffer,PC,r0,r1,r2,r3);
       fprintf(f,"seam_tailcall3 R%d R%d R%d R%d",r0,r1,r2,r3);
       fprintf(stderr,"\n");
    }
    return PC;

case select_tup:
    {
       GET_2R1I(codeBuffer,PC,r0,r1,i0);
       fprintf(f,"select_tup R%d R%d I%d",r0,r1,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case select_tup0:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"select_tup0 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case select_tup1:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"select_tup1 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case select_tup2:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"select_tup2 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case self_call:
    {
       GET_1I(codeBuffer,PC,i0);
       fprintf(f,"self_call I%d",i0);
       fprintf(stderr," [");
       for(s_int i=0; i<i0-1; i++) {
           GET_1R(codeBuffer,PC,reg);
           fprintf(stderr,"R%d ",reg);
       }
       if(1>0) {
         GET_1R(codeBuffer,PC,reg);
         fprintf(stderr,"R%d",reg);
       }
       fprintf(stderr,"]");
       fprintf(stderr,"\n");
    }
    return PC;

case self_call0:
    {
       fprintf(f,"self_call0");
       fprintf(stderr,"\n");
    }
    return PC;

case self_call1:
    {
       GET_1R(codeBuffer,PC,r0);
       fprintf(f,"self_call1 R%d",r0);
       fprintf(stderr,"\n");
    }
    return PC;

case self_call2:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"self_call2 R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case self_call3:
    {
       GET_3R(codeBuffer,PC,r0,r1,r2);
       fprintf(f,"self_call3 R%d R%d R%d",r0,r1,r2);
       fprintf(stderr,"\n");
    }
    return PC;

case set_cell:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"set_cell R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case set_global:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"set_global R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case sjump_eq:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"sjump_eq R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case spec_closure:
    {
       GET_2R2I(codeBuffer,PC,r0,r1,i0,i1);
       fprintf(f,"spec_closure R%d R%d I%d I%d",r0,r1,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case stest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"stest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case swap_regs:
    {
       GET_2R(codeBuffer,PC,r0,r1);
       fprintf(f,"swap_regs R%d R%d",r0,r1);
       fprintf(stderr,"\n");
    }
    return PC;

case tagtest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"tagtest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;

case tagtest1:
    {
       GET_1R2I(codeBuffer,PC,r0,i0,i1);
       fprintf(f,"tagtest1 R%d I%d I%d",r0,i0,i1);
       fprintf(stderr,"\n");
    }
    return PC;

case vectest:
    {
       GET_1R1I(codeBuffer,PC,r0,i0);
       fprintf(f,"vectest R%d I%d",r0,i0);
       fprintf(stderr,"\n");
    }
    return PC;
