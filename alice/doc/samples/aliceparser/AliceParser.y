(* Author:									*)
(*   Benedikt Grundmann <bgrund@ps.uni-sb.de>					*)
(*										*)
(* Copyright:									*)
(*   Benedikt Grundmann, 2005   						*)
(*   Converted from ml-yacc grammar by Andreas Rossberg                         *)
(*										*)
(* Last change:									*)
(*   $Date$ by $Author$				*)
(*   $Revision$							*)

structure AliceParser =
struct

type opt_int = int option

type pos = int * int 

exception Error of pos * pos


fun parseError (leftPos, rightPos) =
    raise Error (leftPos, rightPos)


token 
	(* Reserved words for the core language *)
	  ABSTYPE | AND | ANDALSO | AS | CASE | DO | DATATYPE | ELSE
	| END | EXCEPTION | FN | FUN | HANDLE | IF | IN | INFIX
	| INFIXR | LET | LOCAL | NONFIX | OF | OP | OPEN | ORELSE
	| RAISE | REC | THEN | TYPE | VAL | WITH | WITHTYPE | WHILE
	| LPAR | RPAR | LBRACK | RBRACK | LBRACE | RBRACE | COMMA | COLON
	| SEMICOLON | DOTS | UNDERBAR | BAR | EQUALS | DARROW | ARROW | HASH

	(* Alice core extensions *)
	| DOT | HASHBRACK
	| FINALLY
	| ASSERT of opt_int 
        | FILE | LINE
	| EXTTYPE | CONSTRUCTOR
	| NON | WITHFUN | WITHVAL
	| LAZY | SPAWN

	(* Additional reserved words for the modules language *)
	| EQTYPE | FUNCTOR | INCLUDE | SHARING | SIG
	| SIGNATURE | STRUCT | STRUCTURE | WHERE | COLONGREATER

	(* Alice module extensions *)
	| ANY | FCT | PACK | UNPACK

	(* Alice component extensions *)
	| IMPORT | FROM

	(* Secret internal keywords *)
	| PRIMITIVE | OVERLOAD | EQEQTYPE | REFTYPE | PERVASIVE

	(* Special constants *)
	| ZERO | DIGIT of int | NUMERIC of LargeInt.int
	| INT of LargeInt.int | WORD of LargeWord.word
	| REAL of LargeReal.real
	| STRING of WideString.string | CHAR of WideChar.char

	(* Identifiers *)
	| ALPHA of string | SYMBOL of string | STAR
	| TYVAR of string | ETYVAR of string

assocl  SHARING
assocl  SEMICOLON
assocr VAL FUN TYPE EQTYPE DATATYPE ABSTYPE EXTTYPE CONSTRUCTOR EXCEPTION
       STRUCTURE FUNCTOR SIGNATURE
       IMPORT
       PRIMITIVE OVERLOAD EQEQTYPE REFTYPE
assocr LOCAL OPEN INFIX INFIXR NONFIX INCLUDE
assocr AND
assocl  DARROW		(* L/R is arbitrary *)
assocl  BAR		(* L/R is arbitrary *)
assocl  OF		(* L/R is arbitrary *)
assocl  DO		(* L/R is arbitrary *)
assocl  ELSE		(* L/R is arbitrary *)
assocl  LAZY		(* L/R is arbitrary *)
assocl  SPAWN		(* L/R is arbitrary *)
assocl  RAISE		(* L/R is arbitrary *)
assocl  IN		(* L/R is arbitrary *)
assocl  HANDLE FINALLY
assocr ORELSE
assocr ANDALSO
assocl  WITHVAL
assocl  WITHFUN
assocr AS
assocr NON		(* L/R is arbitrary *)
assocl  COLON COLONGREATER
assocr ARROW
assocr WHERE


(* %start component *)

  (* Constants *)

  rule scon = 
	  ZERO		=> ( () ) 
	| DIGIT		=> ( () )
	| NUMERIC	=> ( () )
	| INT		=> ( () )
	| WORD		=> ( () )
	| STRING	=> ( () )
	| CHAR		=> ( () )
	| REAL		=> ( () )

  rule d =
	  ZERO		=> ( () )
	| DIGIT		=> ( () )


  (* Identifiers and labels *)

  rule lab =
	  ALPHA		=> ( () )
	| SYMBOL	=> ( () )
	| STAR		=> ( () )
	| DIGIT		=> ( () )
	| NUMERIC	=> ( () )

  rule vid =
	  vid'		=> ( () )
	| EQUALS	=> ( () )

    rule vid' =
	  ALPHA		=> ( () )
	| SYMBOL	=> ( () )
	| STAR		=> ( () )

  rule tycon =
	  ALPHA		=> ( () )
	| SYMBOL	=> ( () )

  rule tyvar =
        TYVAR		=> ( () )

  rule strid =
          ALPHA		=> ( () )
	| PERVASIVE	=> ( () )

  rule sigid =
          ALPHA		=> ( () )

  rule longvid =
	  longvid'      => ( () )
	| EQUALS        => ( () )
     
     rule longvid' =
	  vid'                  => ( () )
	| longstrid, DOT, vid	=> ( () )

  rule longtycon =
	  tycon			=> ( () )
	| longstrid, DOT, tycon	=> ( () )

  rule longstrid =
	  strid			=> ( () )
	| longstrid, DOT, strid	=> ( () ) 

  rule longsigid =
	  sigid			=> ( () )
	| longstrid, DOT, sigid	=> ( () )

  rule OP_opt =
	  OP			=> ( () )
	| skip                  => ( () )

  rule LAZY_SPAWN_opt =
	  LAZY			=> ( () )
	| SPAWN			=> ( () )
	| skip  		=> ( () )


  (* rule Core = Expressions *)

  rule atexp =
	  scon		                        => ( () )
	| FILE		                        => ( () )
	| LINE                                  => ( () )
	| longvid                               => ( () )
	| OP, longvid                           => ( () )
	| LBRACE, exprow_opt, RBRACE            => ( () )
	| LBRACE, atexp, WHERE, exprow, RBRACE  => ( () )
	| HASH, lab	                        => ( () )
	| LPAR, RPAR	                        => ( () )
	| LPAR, exp_COMMA_list2, RPAR           => ( () )
	| LBRACK, exp_COMMA_list0, RBRACK       => ( () )
	| HASHBRACK, exp_COMMA_list0, RBRACK    => ( () )
	| LPAR, exp_SEMICOLON_list2, RPAR       => ( () )
	| LET, dec, IN, exp_SEMICOLON_list1, END => ( () )
	| LPAR, exp, RPAR	                => ( () )


     rule exp_COMMA_list0 =
	  exp_COMMA_list1		=> ( () )
	| skip                          => ( () )

     rule exp_COMMA_list1 =
	  exp, COMMA, exp_COMMA_list1	=> ( () )
	| exp				=> ( () )

     rule exp_COMMA_list2 =
	  exp, COMMA, exp_COMMA_list1	=> ( () )

     rule exp_SEMICOLON_list1 =
	  exp, SEMICOLON, exp_SEMICOLON_list1	=> ( () )
	| exp					=> ( () )

     rule exp_SEMICOLON_list2 =
	  exp, SEMICOLON, exp_SEMICOLON_list1	=> ( () )


  rule exprow =
	  lab, EQUALS, exp, COMMA_exprow_opt    => ( () )
	| vid', COLON_ty_opt, COMMA_exprow_opt  => ( () )

     rule COMMA_exprow_opt =
	  COMMA, exprow		                => ( () )
	| skip		                        => ( () )

     rule exprow_opt =
	  exprow		                => ( () )
	| skip		                        => ( () )


  rule appexp =
	  atexp			                => ( () )
	| appexp, atexp		                => ( () )

  rule infexp =
	  appexp		                => ( () )

  rule exp =
	  infexp		        => ( () )
	| exp, COLON, ty	        => ( () )
	| e1 as exp, ANDALSO, e2 as exp	=> ( () )
	| e1 as exp, ORELSE, e2 as exp	=> ( () )
	| e1 as exp, FINALLY, e2 as exp	=> ( () )
	| exp, HANDLE, match	=> ( () )
	| RAISE, exp		=> ( () )
	| LAZY, exp		=> ( () )
	| SPAWN, exp		=> ( () )
	| IF, e1 as exp, THEN, e2 as exp, ELSE, e3 as exp
				=> ( () )
	| WHILE, e1 as exp, DO, e2 as exp	
                                => ( () )
	| CASE, exp, OF, match	=> ( () )
	| FN, match		=> ( () )
	| REC, pat, DARROW, exp	=> ( () )
	| PACK, atstrexp, COLON, atsigexp
				=> ( () )
	| PACK, atstrexp, COLONGREATER, atsigexp => ( () )
	| ASSERT, e1 as exp, DO, e2 as exp	
                                => ( () )
	| ASSERT, e1 as exp, OF, pat, DO, e2 as exp
				=> ( () )
	| ASSERT, exp		=> ( () )
	| ASSERT, exp, OF, pat	=> ( () )
	| ASSERT, exp, RAISE, pat  => ( () )

    (* Core: Matches *)

  rule match =
	  mrule, BAR_match_opt	=> ( () )

     rule BAR_match_opt =
	  BAR, match		=> ( () )
	| skip prec DARROW	=> ( () )

  rule mrule =
	  pat,DARROW,exp	=> ( () )



    (* Core: Declarations *)

  rule dec =
	  dec1		=> ( () )
	| skip	        => ( () )

     rule dec' =
	  dec1'		=> ( () )
	| dec', dec' prec SEMICOLON
			=> ( () )

     rule dec1 =
	  dec1'		=> ( () )
	| SEMICOLON	=> ( () )
	| dec1, dec1 prec SEMICOLON
			=> ( () )

     rule dec1' =
	  dec1''	                => ( () )
	| LOCAL, d1 as dec, IN, d2 as dec, END      
                                        => ( () )


     rule dec1'' =
	  VAL, valbind	                => ( () )
	| VAL, tyvarseq1, valbind       => ( () )
	| FUN, fvalbind	                => ( () )
	| FUN, tyvarseq1, fvalbind      => ( () )
	| TYPE, typbind	                => ( () )
	| EQTYPE, typbind               => ( () )
	| EQEQTYPE, typbind             => ( () )
	| DATATYPE, datbind0, WITHTYPE_typbind_opt      => ( () )
	| DATATYPE, datbind1, WITHTYPE_typbind_opt      => ( () )
	| DATATYPE, tycon, EQUALS, DATATYPE, longtycon   => ( () )
	| ABSTYPE, datbind, WITHTYPE_typbind_opt, WITH, dec, END        => ( () )
	| EXTTYPE, extbind              => ( () )
	| CONSTRUCTOR, econbind         => ( () )
	| EXCEPTION, exbind             => ( () )
	| STRUCTURE, strbind            => ( () )
	| SIGNATURE, sigbind            => ( () )
	| FUNCTOR, funbind              => ( () )
	| OPEN, longstrid_list1         => ( () )
	| INFIX, d_opt, vid_list1       => ( () )
	| INFIXR, d_opt, vid_list1      => ( () )
	| NONFIX, vid_list1             => ( () )
	| OVERLOAD, longtyconseq, AS, tyvar, OP_opt, vid, COLON, ty, EQUALS, longvidseq
			=> ( () )
	| OVERLOAD, tyvar, OP_opt, vid, COLON, ty, EQUALS, longvid      => ( () )
	| PRIMITIVE, VAL, OP_opt, vid, COLON, ty, EQUALS, STRING         => ( () )
	| PRIMITIVE, FUN, OP_opt, vid, COLON, ty, EQUALS, STRING         => ( () )
	| PRIMITIVE, TYPE, tyvarseq, tycon, EQUALS, STRING              => ( () )
	| PRIMITIVE, EQTYPE, tyvarseq, tycon, EQUALS, STRING            => ( () )
	| PRIMITIVE, EQEQTYPE, tyvarseq, tycon, EQUALS, STRING          => ( () )
	| PRIMITIVE, EXTTYPE, tyvarseq, tycon, EQUALS, STRING           => ( () )
	| PRIMITIVE, REFTYPE, tyvar, tycon, EQUALS, OP_opt, vid, OF, tyvar => ( () )
	| PRIMITIVE, CONSTRUCTOR, OP_opt, vid, OF_ty_opt, COLON, tyvarseq, longtycon,
	  EQUALS, STRING => ( () )
	| PRIMITIVE, EXCEPTION, OP_opt, vid, OF_ty_opt, EQUALS, STRING  => ( () )
	| PRIMITIVE, STRUCTURE, strid, COLON, sigexp, EQUALS, STRING    => ( () )
	| PRIMITIVE, FUNCTOR, strid, atstrpat_list0, COLON, sigexp, EQUALS, STRING => ( () )
	| PRIMITIVE, SIGNATURE, sigid, atstrpat_list0, EQUALS, STRING   => ( () )

     rule WITHTYPE_typbind_opt =
	  WITHTYPE, typbind	=> ( () )
	| skip		        => ( () )

     rule vid_list1 =
	  vid, vid_list1	=> ( () )
	| vid			=> ( () )

     rule longstrid_list1 =
	  longstrid, longstrid_list1	=> ( () )
	| longstrid			=> ( () )

     rule d_opt =
	  d			=> ( () )
	| skip	        	=> ( () )



    (* Core: Bindings *)

  rule valbind =
	  pat, EQUALS, exp, AND_valbind_opt     => ( () )
	| REC, valbind                          => ( () )

      rule AND_valbind_opt =
	  AND, valbind	                        => ( () )
	| skip	                                => ( () )


  rule fvalbind =
	  fmatch, AND_fvalbind_opt              => ( () )
	| LAZY, fmatch, AND_fvalbind_opt        => ( () )
	| SPAWN, fmatch, AND_fvalbind_opt       => ( () )
     
     rule AND_fvalbind_opt =
	  AND, fvalbind	                        => ( () )
	| skip	                                => ( () )

  rule fmatch =
	  fmrule, BAR_fmatch_opt                => ( () )

     rule BAR_fmatch_opt =
	  BAR, fmatch	                        => ( () )
	| skip	                                => ( () )

  rule fmrule =
	  fpat, EQUALS, exp                     => ( () )

  rule fpat =
	  pat		                        => ( () )

  rule typbind =
	  tyvarseq, tycon, AND_typbind_opt              => ( () )
	| tyvarseq, tycon, EQUALS, ty, AND_typbind_opt  => ( () )

     rule AND_typbind_opt =
	  AND, typbind	                                => ( () )
	| skip	                                        => ( () )


  rule datbind =
	  tyvarseq, tycon, EQUALS, conbind, AND_datbind_opt     => ( () )

     rule datbind0 =
	  tycon, EQUALS, conbind, AND_datbind_opt               => ( () )

     rule datbind1 =
	  tyvarseq1, tycon, EQUALS, conbind, AND_datbind_opt    => ( () )
          
     rule AND_datbind_opt =
	  AND, datbind	                                        => ( () )
	| skip	                                                => ( () )


  rule conbind =
	  OP_opt, vid, OF_ty_opt, BAR_conbind_opt               => ( () )

     rule BAR_conbind_opt =
	  BAR, conbind	                                        => ( () )
	| skip	                                                => ( () )

     rule OF_ty_opt =
	  OF, ty		                                => ( () )
	| skip	                                                => ( () )


  rule extbind =
	  tyvarseq, tycon, AND_extbind_opt                      => ( () )

     rule AND_extbind_opt =
	  AND, extbind	                                        => ( () )
	| skip	                                                => ( () )


  rule econbind =
	  OP_opt, vid, OF_ty_opt, COLON, tyvarseq, longtycon, AND_econbind_opt
			                                        => ( () )
	| o1 as OP_opt, vid, EQUALS, o2 as OP_opt, longvid, AND_econbind_opt
			                                        => ( () )

     rule AND_econbind_opt =
	  AND, econbind	                                        => ( () )
	| skip	                                                => ( () )


  rule exbind =
	  OP_opt, vid, OF_ty_opt, AND_exbind_opt                => ( () )
	| o1 as OP_opt, vid, EQUALS, o2 as OP_opt, longvid, AND_exbind_opt  
                                                                => ( () )

     rule AND_exbind_opt =
	  AND, exbind	                                        => ( () )
	| skip	                                                => ( () )



    (* Core: Patterns *)

  rule atpat =
	  UNDERBAR	                                        => ( () )
	| longvid'                                              => ( () )
	| OP, longvid	                                        => ( () )
	| scon		                                        => ( () )
	| LBRACE, patrow_opt, RBRACE                            => ( () )
	| LPAR, RPAR	                                        => ( () )
	| LPAR, pat_COMMA_list2, RPAR                           => ( () )
	| LBRACK, pat_COMMA_list0, RBRACK                       => ( () )
	| HASHBRACK, pat_COMMA_list0, RBRACK                    => ( () )
	| LPAR, pat_BAR_list2, RPAR                             => ( () )
	| LPAR, pat, RPAR                                       => ( () )


     rule pat_COMMA_list0 =
	  pat_COMMA_list1		                        => ( () )
	| skip			                                => ( () )

     rule pat_COMMA_list1 =
	  pat, COMMA, pat_COMMA_list1	                        => ( () )
	| pat				                        => ( () )

     rule pat_COMMA_list2 =
	  pat, COMMA, pat_COMMA_list1	                        => ( () )

     rule pat_BAR_list2 =
	  p1 as pat, BAR, p2 as pat			        => ( () )
	| pat, BAR, pat_BAR_list2		                => ( () )


  rule patrow =
	  DOTS		                                        => ( () )
	| lab, EQUALS, pat, COMMA_patrow_opt                    => ( () )
	| vid', COLON_ty_opt, AS_pat_opt, COMMA_patrow_opt      => ( () )

     rule COMMA_patrow_opt =
	  COMMA, patrow	                                        => ( () )
	| skip	                                                => ( () )

     rule COLON_ty_opt =
	  COLON, ty	                                        => ( () )
	| skip	                                                => ( () )

     rule AS_pat_opt =
	  AS, pat	                                        => ( () )
	| skip	                                                => ( () )

     rule patrow_opt =
	  patrow	                                        => ( () )
	| skip	                                                => ( () )



  rule infpat =
	  atpat		                                        => ( () )
	| infpat, atpat	                                        => ( () )

  rule pat =
	  infpat	                                        => ( () )
	| pat, COLON, ty	                                => ( () )
	| NON, pat	                                        => ( () )
	| p1 as pat, AS, p2 as pat	                        => ( () )
	| pat, WHERE, atexp                                     => ( () )
	| pat, WITHVAL, valbind, END                            => ( () )
	| pat, WITHFUN, fvalbind, END                           => ( () )
	| pat, WITHVAL, valbind, WHERE, atexp                   => ( () )
	| pat, WITHFUN, fvalbind, WHERE, atexp                  => ( () )


    (* Core: Types *)

  rule ty =
	  tupty		                                        => ( () )
	| tupty, ARROW, ty                                      => ( () )

     rule tupty =
	  ty_STAR_list	                                        => ( () )

     rule ty_STAR_list =
	  consty, STAR, ty_STAR_list	                        => ( () )
	| consty	 		                        => ( () )

     rule consty =
	  atty			                                => ( () )
	| tyseq, longtycon	                                => ( () )

     rule atty =
	  UNDERBAR	                                        => ( () )
	| tyvar		                                        => ( () )
	| LBRACE, tyrow_opt, RBRACE                             => ( () ) 
	| LPAR, ty, RPAR	                                => ( () )

  rule tyrow =
	  lab, COLON, ty, COMMA_tyrow_opt                       => ( () )

     rule COMMA_tyrow_opt =
	  COMMA, tyrow	                                        => ( () )
	| skip	                                                => ( () )

     rule tyrow_opt =
	  tyrow		                                        => ( () )
	| skip	                                                => ( () )



  (* Core: Sequences *)

  rule tyseq =
	  consty			                        => ( () )
	| skip			                                => ( () )
	| LPAR, ty_COMMA_list2, RPAR	                        => ( () )

     rule ty_COMMA_list2 =
	  ty, COMMA, ty_COMMA_list2	                        => ( () )
	| t1 as ty, COMMA, t2 as ty                             => ( () )


  rule tyvarseq =
	  tyvarseq1			                        => ( () )
	| skip			                                => ( () )
     
     rule tyvarseq1 =
	  tyvar				                        => ( () )
	| LPAR, tyvar_COMMA_list1, RPAR	                        => ( () )

     rule tyvar_COMMA_list1 =
	  tyvar, COMMA, tyvar_COMMA_list1	                => ( () )
	| tyvar				                        => ( () )


  rule longtyconseq =
	  longtyconseq1			                        => ( () )
	| skip			                                => ( () )
        
     rule longtyconseq1 =
	  longtycon			                        => ( () )
	| LPAR, longtycon_COMMA_list1, RPAR                     => ( () )

     rule longtycon_COMMA_list1 =
	  longtycon, COMMA, longtycon_COMMA_list1               => ( () )
	| longtycon			                        => ( () )


  rule longvidseq =
	  longvidseq1			                        => ( () )
	| skip			                                => ( () )
     
     rule longvidseq1 =
	  longvid			                        => ( () )
	| LPAR, longvid_COMMA_list1, RPAR	                => ( () )

     rule longvid_COMMA_list1 =
	  longvid, COMMA, longvid_COMMA_list1                   => ( () )
	| longvid			                        => ( () )


    (* Modules: Structures *)

  rule atstrexp =
	  STRUCT, dec, END                                      => ( () )
	| longstrid	                                        => ( () )
	| LPAR, strexp, RPAR                                    => ( () )
	| LPAR, dec, RPAR                                       => ( () )
	| LET, dec, IN, strexp, END                             => ( () )

  rule appstrexp =
	  atstrexp	                                        => ( () )
	| appstrexp, atstrexp                                   => ( () )

  rule strexp =
	  appstrexp	                                        => ( () )
	| strexp, COLON, sigexp                                 => ( () )
	| strexp, COLONGREATER, sigexp                          => ( () )
	| FCT, strpat, DARROW, strexp                           => ( () )
	| UNPACK, infexp, COLON, sigexp                         => ( () )
	| LAZY, strexp	                                        => ( () )
	| SPAWN, strexp	                                        => ( () )

  rule atstrpat =
	  LPAR, strid, COLON, sigexp, RPAR                      => ( () )
	| LPAR, UNDERBAR, COLON, sigexp, RPAR                   => ( () )
	| LPAR, spec, RPAR                                      => ( () )

  rule strpat =
	  atstrpat	                                        => ( () )
	| strid, COLON, sigexp                                  => ( () )
	| UNDERBAR, COLON, sigexp                               => ( () )

  rule strpat' =
	  atstrpat	                                        => ( () )
	| strid, COLON, atsigexp                                => ( () )
	| UNDERBAR, COLON, atsigexp                             => ( () )

  rule strbind =
	  strid, COLON_sigexp_opt, EQUALS, strexp__AND_strbind_opt => ( () )
	| strid, COLONGREATER, sigexp, EQUALS, strexp__AND_strbind_opt => ( () )
	| UNDERBAR, COLON_sigexp_opt, EQUALS, strexp__AND_strbind_opt => ( () )

     rule AND_strbind_opt =
	  AND, strbind	                                        => ( () )
	| skip	                                                => ( () )

     rule strexp__AND_strbind_opt =
	  appstrexp, AND_strbind_opt                            => ( () )
	| strexp, COLON, sigexp__AND_strbind_opt                => ( () )
	| strexp, COLONGREATER, sigexp__AND_strbind_opt         => ( () )
	| FCT, strpat, DARROW, strexp__AND_strbind_opt          => ( () )
	| UNPACK, infexp, COLON, sigexp__AND_strbind_opt        => ( () )
	| LAZY, strexp__AND_strbind_opt                         => ( () )
	| SPAWN, strexp__AND_strbind_opt                        => ( () )

     rule sigexp__AND_strbind_opt =
	  sigexp', AND_strbind_opt                              => ( () )
	| FCT, strpat', ARROW, sigexp__AND_strbind_opt          => ( () )
	| atsigexp, ARROW, sigexp__AND_strbind_opt              => ( () )
	| sigexp, WHERE, rea__AND_strbind_opt                   => ( () )

     rule rea__AND_strbind_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_strbind_opt
			    => ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_strbind_opt
			    => ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_strbind_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_strbind_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_strbind_opt
			=> ( () )
	| STRUCTURE, longstrid, COLON_sigexp_opt, EQUALS, longstrid,
						AND_rea_opt__AND_strbind_opt
			=> ( () )
	| FUNCTOR, longstrid, COLON_sigexp_opt, EQUALS, longstrid,
						AND_rea_opt__AND_strbind_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_strbind_opt
			=> ( () )

     rule AND_rea_opt__AND_strbind_opt =
	  AND_strbind_opt	                                => ( () )
	| AND, rea__AND_strbind_opt                             => ( () )

     rule sigexp__AND_rea_opt__AND_strbind_opt =
	  appsigexp, AND_rea_opt__AND_strbind_opt               => ( () )

     rule COLON_sigexp_opt =
	  COLON, sigexp	                                        => ( () )
	| skip	                                                => ( () )



    (* Modules: Signatures *)

  rule atsigexp =
	  ANY		                                        => ( () )
	| SIG, spec, END	                                => ( () )
	| longsigid	                                        => ( () )
	| LET, dec, IN, sigexp, END                             => ( () )
	| LPAR, sigexp, RPAR                                    => ( () )
	| LPAR, spec, RPAR                                      => ( () )

  rule appsigexp =
	  atsigexp	                                        => ( () )
	| appsigexp, atstrexp                                   => ( () )

  rule sigexp =
	  sigexp'	                                        => ( () )
	| FCT, strpat', ARROW, sigexp                           => ( () )
	| atsigexp, ARROW, sigexp                               => ( () )
	| sigexp, WHERE, rea                                    => ( () )

     rule sigexp' =
	  appsigexp	                                        => ( () )
	| sigexp, WHERE, longstrid, EQUALS, longstrid           => ( () )

  rule sigbind =
	  sigid, atstrpat_list0, EQUALS, sigexp__AND_sigbind_opt        => ( () )

     rule atstrpat_list0 =
	  atstrpat_list1                                        => ( () )
	| skip	                                                => ( () )

     rule AND_sigbind_opt =
	  AND, sigbind	                                        => ( () )
	| skip	                                                => ( () )

     rule sigexp__AND_sigbind_opt =
	  sigexp', AND_sigbind_opt                              => ( () )
	| FCT, strpat', ARROW, sigexp__AND_sigbind_opt          => ( () )
	| atsigexp, ARROW, sigexp__AND_sigbind_opt              => ( () )
	| sigexp, WHERE, rea__AND_sigbind_opt                   => ( () )

     rule rea__AND_sigbind_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_sigbind_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_sigbind_opt
			=> ( () )

     rule AND_rea_opt__AND_sigbind_opt =
	  AND_sigbind_opt	                        => ( () )
	| AND, rea__AND_sigbind_opt                     => ( () )

     rule sigexp__AND_rea_opt__AND_sigbind_opt =
	  appsigexp, AND_rea_opt__AND_sigbind_opt       => ( () )

  rule rea =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt => ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt => ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid, 
                    AND_rea_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid, 
                    AND_rea_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS, sigexp__AND_rea_opt
			=> ( () )

     rule AND_rea_opt =
	  AND, rea	                        => ( () )
	| skip	                                => ( () )

     rule sigexp__AND_rea_opt =
	  appsigexp, AND_rea_opt                => ( () )

  (* Modules: Functors *)

  rule funbind =
	  LAZY_SPAWN_opt, strid, atstrpat_list1, COLON_sigexp_opt,
		EQUALS, strexp__AND_funbind_opt
			=> ( () )
	| LAZY_SPAWN_opt, strid, atstrpat_list1, COLONGREATER, sigexp,
		EQUALS, strexp__AND_funbind_opt
			=> ( () )

     rule atstrpat_list1 =
	  atstrpat		                => ( () )
	| atstrpat, atstrpat_list1              => ( () )

     rule AND_funbind_opt =
	  AND, funbind		                => ( () )
	| skip		                        => ( () )

     rule strexp__AND_funbind_opt =
	  appstrexp, AND_funbind_opt                    => ( () )
	| strexp, COLON, sigexp__AND_funbind_opt        => ( () )
	| strexp, COLONGREATER, sigexp__AND_funbind_opt => ( () )
	| FCT, strpat, DARROW, strexp__AND_funbind_opt  => ( () )
	| UNPACK, infexp, COLON, sigexp__AND_funbind_opt => ( () )
	| LAZY, strexp__AND_funbind_opt                 => ( () )
	| SPAWN, strexp__AND_funbind_opt                => ( () )

     rule sigexp__AND_funbind_opt =
	  sigexp', AND_funbind_opt                      => ( () )
	| FCT, strpat', ARROW, sigexp__AND_funbind_opt  => ( () )
	| atsigexp, ARROW, sigexp__AND_funbind_opt      => ( () )
	| sigexp, WHERE, rea__AND_funbind_opt           => ( () )

     rule rea__AND_funbind_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                AND_rea_opt__AND_funbind_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_funbind_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_funbind_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_funbind_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_funbind_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_funbind_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_funbind_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_funbind_opt
			=> ( () )

     rule AND_rea_opt__AND_funbind_opt =
	  AND_funbind_opt                               => ( () )
	| AND, rea__AND_funbind_opt                     => ( () )

     rule sigexp__AND_rea_opt__AND_funbind_opt =
	  appsigexp, AND_rea_opt__AND_funbind_opt
	  		=> ( () )

  (* Modules: Specifications *)

  rule spec =
	  spec1		                => ( () )
	| skip	                        => ( () )

     rule spec1 =
	  spec1'	                                => ( () )
	| SEMICOLON	                                => ( () )
	| spec1, spec1' prec SEMICOLON                  => ( () )
	| spec1, SEMICOLON prec SEMICOLON               => ( () )
	| SHARING, TYPE, longtycon_EQUALS_list2         => ( () )
	| spec1, SHARING, TYPE, longtycon_EQUALS_list2  => ( () )
	| SHARING, SIGNATURE, longsigid_EQUALS_list2    => ( () )
	| spec1, SHARING, SIGNATURE, longsigid_EQUALS_list2 => ( () )
	| SHARING, longstrid_EQUALS_list2               => ( () )
	| spec1, SHARING, longstrid_EQUALS_list2        => ( () )

     rule spec1' =
	  VAL, valdesc	=> ( () )
	| FUN, valdesc	=> ( () )
	| TYPE, typdesc	=> ( () )
	| EQTYPE, typdesc
	  		=> ( () )
	| EQEQTYPE, typdesc
	  		=> ( () )
	| DATATYPE, datdesc0, WITHTYPE_typdesc_opt
			=> ( () )
	| DATATYPE, datdesc1, WITHTYPE_typdesc_opt
			=> ( () )
	| DATATYPE, tycon, EQUALS, DATATYPE, longtycon
			=> ( () )
	| EXTTYPE, extdesc
			=> ( () )
	| CONSTRUCTOR, econdesc
			=> ( () )
	| EXCEPTION, exdesc
			=> ( () )
	| STRUCTURE, strdesc
			=> ( () )
	| SIGNATURE, sigdesc
			=> ( () )
	| FUNCTOR, fundesc
			=> ( () )
	| INCLUDE, sigexp
			=> ( () )
	| INFIX, d_opt, vid_list1
			=> ( () )
	| INFIXR, d_opt, vid_list1
			=> ( () )
	| NONFIX, vid_list1
			=> ( () )

     rule WITHTYPE_typdesc_opt =
	  WITHTYPE, typdesc	        => ( () )
	| skip		                => ( () )

     rule longtycon_EQUALS_list1 =
	  longtycon, EQUALS, longtycon_EQUALS_list1
				=> ( () )
	| longtycon		=> ( () )

     rule longtycon_EQUALS_list2 =
	  longtycon, EQUALS, longtycon_EQUALS_list1
				=> ( () )
     rule longsigid_EQUALS_list1 =
	  longsigid, EQUALS, longsigid_EQUALS_list1
				=> ( () )
	| longsigid		=> ( () )

     rule longsigid_EQUALS_list2 =
	  longsigid, EQUALS, longsigid_EQUALS_list1
				=> ( () )
     rule longstrid_EQUALS_list1 =
	  longstrid, EQUALS, longstrid_EQUALS_list1
				=> ( () )
	| longstrid		=> ( () )

     rule longstrid_EQUALS_list2 =
	  longstrid, EQUALS, longstrid_EQUALS_list1
				=> ( () )


  (* Modules: Descriptions *)

  rule valdesc =
	  OP_opt, vid, COLON, ty, AND_valdesc_opt
	  		=> ( () )
	| o1 as OP_opt, vid, EQUALS, o2 as OP_opt, longvid, AND_valdesc_opt
	  		=> ( () )
     rule AND_valdesc_opt =
	  AND, valdesc	        => ( () )
	| skip	                => ( () )

  rule typdesc =
	  tyvarseq, tycon, AND_typdesc_opt
			=> ( () )
	| tyvarseq, tycon, EQUALS, ty, AND_typdesc_opt
			=> ( () )
     
     rule AND_typdesc_opt =
	  AND, typdesc	                        => ( () )
	| skip	                                => ( () )

  rule datdesc =
	  tyvarseq, tycon, EQUALS, condesc, AND_datdesc_opt
			=> ( () )
     rule datdesc0 =
	  tycon, EQUALS, condesc, AND_datdesc_opt
	  		=> ( () )
     rule datdesc1 =
	  tyvarseq1, tycon, EQUALS, condesc, AND_datdesc_opt
	  		=> ( () )
     rule AND_datdesc_opt =
	  AND, datdesc	                => ( () )
	| skip	                        => ( () )

  rule condesc =
	  OP_opt, vid, OF_ty_opt, BAR_condesc_opt
			=> ( () )
     rule BAR_condesc_opt =
	  BAR, condesc	        => ( () )
	| skip	                => ( () )

  rule extdesc =
	  tyvarseq, tycon, AND_extdesc_opt
			        => ( () )
     rule AND_extdesc_opt =
	  AND, extdesc	        => ( () )
	| skip	                => ( () )

  rule econdesc =
	  OP_opt, vid, OF_ty_opt, COLON, tyvarseq, longtycon, AND_econdesc_opt
		        	=> ( () )
	| o1 as OP_opt, vid, EQUALS, o2 as OP_opt, longvid, AND_econdesc_opt
			        => ( () )
     rule AND_econdesc_opt =
	  AND, econdesc	        => ( () )
	| skip	                => ( () )


  rule exdesc =
	  OP_opt, vid, OF_ty_opt, AND_exdesc_opt
	  		        => ( () )
	| o1 as OP_opt, vid, EQUALS, o2 as OP_opt, longvid, AND_exdesc_opt
			        => ( () )
     rule AND_exdesc_opt =
	  AND, exdesc	        => ( () )
	| skip	                => ( () )

  rule strdesc =
	  strid, COLON, sigexp__AND_strdesc_opt
			=> ( () )
	| strid, COLON_sigexp_opt, EQUALS, longstrid, AND_strdesc_opt
	  		=> ( () )
     rule AND_strdesc_opt =
	  AND, strdesc	=> ( () )
	| skip	        => ( () )

     rule sigexp__AND_strdesc_opt =
	  sigexp', AND_strdesc_opt
			=> ( () )
	| FCT, strpat', ARROW, sigexp__AND_strdesc_opt
			=> ( () )
	| atsigexp, ARROW, sigexp__AND_strdesc_opt
			=> ( () )
	| sigexp, WHERE, rea__AND_strdesc_opt
			=> ( () )

     rule rea__AND_strdesc_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_strdesc_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_strdesc_opt
			=> ( () )

     rule AND_rea_opt__AND_strdesc_opt =
	  AND_strdesc_opt	=> ( () )
	| AND, rea__AND_strdesc_opt
	  			=> ( () )

     rule sigexp__AND_rea_opt__AND_strdesc_opt =
	  appsigexp, AND_rea_opt__AND_strdesc_opt
	  		=> ( () )

  rule sigdesc =
	  sigid, atstrpat_list0, AND_sigdesc_opt
			=> ( () )
	| sigid, atstrpat_list0, EQUALS, sigexp__AND_sigdesc_opt
			=> ( () )

     rule AND_sigdesc_opt =
	  AND, sigdesc	        => ( () )
	| skip	                => ( () )

     rule sigexp__AND_sigdesc_opt =
	  sigexp', AND_sigdesc_opt
	  		        => ( () )
	| FCT, strpat', ARROW, sigexp__AND_sigdesc_opt
			        => ( () )
	| atsigexp, ARROW, sigexp__AND_sigdesc_opt
			        => ( () )
	| sigexp, WHERE, rea__AND_sigdesc_opt
		        	=> ( () )

     rule rea__AND_sigdesc_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                        AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                        AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_sigdesc_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_sigdesc_opt
			=> ( () )

     rule AND_rea_opt__AND_sigdesc_opt =
	  AND_sigdesc_opt	=> ( () )
	| AND, rea__AND_sigdesc_opt
	  			=> ( () )

     rule sigexp__AND_rea_opt__AND_sigdesc_opt =
	  appsigexp, AND_rea_opt__AND_sigdesc_opt
	  		=> ( () )

  rule fundesc =
	  strid, atstrpat_list0, COLON, sigexp__AND_fundesc_opt
			=> ( () )

     rule AND_fundesc_opt =
	  AND, fundesc	=> ( () )
	| skip	        => ( () )

     rule sigexp__AND_fundesc_opt =
	  sigexp', AND_fundesc_opt
			=> ( () )
	| FCT, strpat', ARROW, sigexp__AND_fundesc_opt
			=> ( () )
	| atsigexp, ARROW, sigexp__AND_fundesc_opt
			=> ( () )
	| sigexp, WHERE, rea__AND_fundesc_opt
			=> ( () )

     rule rea__AND_fundesc_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_fundesc_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_fundesc_opt
			=> ( () )

     rule AND_rea_opt__AND_fundesc_opt =
	  AND_fundesc_opt
			=> ( () )
	| AND, rea__AND_fundesc_opt
	  		=> ( () )

     rule sigexp__AND_rea_opt__AND_fundesc_opt =
	  appsigexp, AND_rea_opt__AND_fundesc_opt
	  		=> ( () )

    (* Components: Imports *)

  rule imp =
	  imp1		=> ( () )
	| skip	        => ( () )

     rule imp1 =
	  imp1'		=> ( () )
	| imp1, imp1' prec SEMICOLON
			=> ( () )
	| SEMICOLON	=> ( () )

     rule imp1' =
	  VAL, valitem	=> ( () )
	| FUN, valitem	=> ( () )
	| TYPE, typitem	=> ( () )
	| EQTYPE, typitem
			=> ( () )
	| EQEQTYPE, typitem
			=> ( () )
	| DATATYPE, datitem
			=> ( () )
	| EXTTYPE, extitem
			=> ( () )
	| CONSTRUCTOR, econitem
			=> ( () )
	| EXCEPTION, exitem
			=> ( () )
	| STRUCTURE, stritem
			=> ( () )
	| SIGNATURE, sigitem
			=> ( () )
	| FUNCTOR, funitem
			=> ( () )
	| INFIX, d_opt, vid_list1
			=> ( () )
	| INFIXR, d_opt, vid_list1
			=> ( () )
	| NONFIX, vid_list1
			=> ( () )


    (* Components: Items *)

  rule valitem =
	  OP_opt, vid, AND_valitem_opt
	  		=> ( () )
	| OP_opt, vid, COLON, ty, AND_valitem_opt
	  		=> ( () )
     
     rule AND_valitem_opt =
	  AND, valitem	=> ( () )
	| skip	        => ( () )

  rule typitem =
	  tycon, AND_typitem_opt
			=> ( () )
	| tyvarseq1, tycon, AND_typitem_opt
			=> ( () )

     rule AND_typitem_opt =
	  AND, typitem	        => ( () )
	| skip	                => ( () )

  rule datitem =
	  tycon, AND_datitem_opt
			=> ( () )
	| tycon, EQUALS, conitem, AND_datitem_opt
			=> ( () )
	| tyvarseq1, tycon, EQUALS, conitem, AND_datitem_opt
			=> ( () )
     rule AND_datitem_opt =
	  AND, datitem	        => ( () )
	| skip	                => ( () )

  rule conitem =
	  OP_opt, vid, OF_ty_opt, BAR_conitem_opt
			=> ( () )
     rule BAR_conitem_opt =
	  BAR, conitem	        => ( () )
	| skip	                => ( () )

  rule extitem =
	  tycon, AND_extitem_opt
			        => ( () )
	| tyvarseq1, tycon, AND_extitem_opt
			        => ( () )
     rule AND_extitem_opt =
	  AND, extitem	        => ( () )
	| skip	                => ( () )

  rule econitem =
	  OP_opt, vid, AND_econitem_opt
		    	        => ( () )
	| OP_opt, vid, OF_ty_opt, COLON, tyvarseq, longtycon, AND_econitem_opt
			        => ( () )
     rule AND_econitem_opt =
	  AND, econitem	        => ( () )
	| skip	                => ( () )


  rule exitem =
	  OP_opt, vid, AND_exitem_opt
	  		=> ( () )
	| OP_opt, vid, OF, ty, AND_exitem_opt
	  		=> ( () )
     rule AND_exitem_opt =
	  AND, exitem	=> ( () )
	| skip	=> ( () )

  rule stritem =
	  strid, AND_stritem_opt
			=> ( () )
	| strid, COLON, sigexp__AND_stritem_opt
			=> ( () )
     rule AND_stritem_opt =
	  AND, stritem	=> ( () )
	| skip	        => ( () )

     rule sigexp__AND_stritem_opt =
	  sigexp', AND_stritem_opt
			=> ( () )
	| FCT, strpat', ARROW, sigexp__AND_stritem_opt
			=> ( () )
	| atsigexp, ARROW, sigexp__AND_stritem_opt
			=> ( () )
	| sigexp, WHERE, rea__AND_stritem_opt
			=> ( () )

     rule rea__AND_stritem_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                        AND_rea_opt__AND_stritem_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                        AND_rea_opt__AND_stritem_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_stritem_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_stritem_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_stritem_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_stritem_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_stritem_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_stritem_opt
			=> ( () )

     rule AND_rea_opt__AND_stritem_opt =
	  AND_stritem_opt	=> ( () )
	| AND, rea__AND_stritem_opt
	  			=> ( () )

     rule sigexp__AND_rea_opt__AND_stritem_opt =
	  appsigexp, AND_rea_opt__AND_stritem_opt
	  		=> ( () )

  rule sigitem =
	  sigid, AND_sigitem_opt
			=> ( () )
	| sigid, atstrpat_list1, AND_sigitem_opt
			=> ( () )
     rule AND_sigitem_opt =
	  AND, sigitem	=> ( () )
	| skip	        => ( () )

  rule funitem =
	  strid, AND_funitem_opt
			=> ( () )
	| strid, atstrpat_list0, COLON, sigexp__AND_funitem_opt
			=> ( () )
     rule AND_funitem_opt =
	  AND, funitem	=> ( () )
	| skip	        => ( () )

     rule sigexp__AND_funitem_opt =
	  sigexp', AND_funitem_opt
			=> ( () )
	| FCT, strpat', ARROW, sigexp__AND_funitem_opt
			=> ( () )
	| atsigexp, ARROW, sigexp__AND_funitem_opt
			=> ( () )
	| sigexp, WHERE, rea__AND_funitem_opt
			=> ( () )

     rule rea__AND_funitem_opt =
	  VAL, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_funitem_opt
			=> ( () )
	| FUN, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid, 
                    AND_rea_opt__AND_funitem_opt
			=> ( () )
	| CONSTRUCTOR, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_funitem_opt
			=> ( () )
	| EXCEPTION, o1 as OP_opt, v1 as longvid, EQUALS, o2 as OP_opt, v2 as longvid,
						AND_rea_opt__AND_funitem_opt
			=> ( () )
	| TYPE, tyvarseq, longtycon, EQUALS, ty, AND_rea_opt__AND_funitem_opt
			=> ( () )
	| STRUCTURE, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_funitem_opt
			=> ( () )
	| FUNCTOR, str1 as longstrid, COLON_sigexp_opt, EQUALS, str2 as longstrid,
						AND_rea_opt__AND_funitem_opt
			=> ( () )
	| SIGNATURE, longsigid, atstrpat_list0, EQUALS,
					sigexp__AND_rea_opt__AND_funitem_opt
			=> ( () )

     rule AND_rea_opt__AND_funitem_opt =
	  AND_funitem_opt
			=> ( () )
	| AND, rea__AND_funitem_opt
	  		=> ( () )

     rule sigexp__AND_rea_opt__AND_funitem_opt =
	  appsigexp, AND_rea_opt__AND_funitem_opt
	  		=> ( () )

    (* Components: Announcements *)

  rule ann0 =
	  ann1			=> ( () )
	| skip		        => ( () )

     rule ann1 =
	  IMPORT, imp, FROM, STRING
	  			=> ( () )
	| IMPORT, STRING        => ( () )
	| IMPORT, PRIMITIVE, imp, FROM, STRING
	  			=> ( () )
	| IMPORT, PRIMITIVE, STRING
				=> ( () )
	| a1 as ann1, a2 as ann1 prec SEMICOLON
				=> ( () )
	| SEMICOLON		=> ( () )


    (* Components: Programs *)

  rule program =
	  dec' 		=> ( () )
	| exp  		=> ( () )
	| dec', SEMICOLON, program_opt'
	  		=> ( () )
	| exp, SEMICOLON, program_opt'
	  		=> ( () )

     rule program_opt =
	  program	=> ( () )
	| skip		=> ( () )

     rule program_opt' =
	  program_opt		        => ( () )
	| SEMICOLON, program_opt'       => ( () )



  (* Components: Compilation units *)

  rule component =
	  ann0, program_opt	=> ( () )


  (* Entry point *)
  parser parseComponent = component

end
