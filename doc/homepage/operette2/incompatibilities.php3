<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE>Stockhausen Operette 1 - SML Incompatibilities</TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>

  <H1>
  stock<BR>
  hausen.<BR>
  <BR>
  -<BR>
  incompati<BR>
  bilities<BR>
  with SML<BR>
  -
  </H1>

  <?php
    include ("menu.php3")
  ?>

  <H2>overview ___________________________</H2>
  <BR><BR>

  <P>
    Most Stockhausen extensions to SML'97 are conservative. There are some
    incompatibilies to SML'97 however. Most of them are quite pathological,
    caught on compile time, and can easily be fixed in an SML compatible way.
  </P>

  <H2><A name=keywords>reserved words _____________________</A></H2>
  <BR><BR>

  <P>
    The following are reserved words in Stockhausen and may not be used as
    identifiers:
  </P>

  <PRE>
	any     constructor   fct     from       import     non
	pack    unpack        when    withfun    withval
  </PRE>


  <H2><A name=openinfix>open and infix _____________________</A></H2>
  <BR><BR>

  <P>
    Open pulls in infix status. Opening a structure that
  </P>

  <OL>
    <LI> contains infix declarations, and </LI>
    <LI> has not been constrained by a signature </LI>
  </OL>

  <P>
    will change the infix environment, while in SML it would not.
  </P>

  <P>
    Example:
  </P>

  <PRE>
	structure S =
	  struct
	    infix ++
	    fun l1++l2 = l1 @ l2
	  end

	open S

	val l = ++([1],[2])		(* error: misplaced infix *)
  </PRE>

  <P>
    Fixes:
  </P>

  <UL>
    <LI>
      Use appropriate signature constraints (always a good idea anyway):

      <PRE>
	signature SIG =
	  sig
	    val ++ : 'a list * 'a list -> 'a list
	  end

	structure S: SIG =
	  struct
	    infix ++
	    fun l1++l2 = l1 @ l2
	  end

	open S

	val l = ++([1],[2])
      </PRE>
    </LI>

    <LI>
      Or insert <TT>op</TT> on every use of such identifiers:

      <PRE>
	val l = op++([1],[2])
      </PRE>
    </LI>
  </UL>



  <H2>val rec ____________________________</H2>
  <BR><BR>

  <P>
    Recursive value bindings do not remove constructor status on the
    identifiers bound. You cannot bind functions to an identifier that was a
    constructor previously.
  </P>

  <P>
    Example:
  </P>

  <PRE>
	val rec NONE = fn x => x
	fun NONE x = x
  </PRE>

  <P>
    This is legal in SML'97 (though it was not in SML'90 and SML/NJ for example
    does not allow it either). In Stockhausen it produces a type clash
    (due to the extended <TT>val</TT> <TT>rec</TT>).
  </P>

  <P>
    Fix:
  </P>

  <UL>
    <LI> Rename your function, this is perverse anyway. </LI>
  </UL>



  <H2>constructor arity __________________</H2>
  <BR><BR>

  <P>
    For interoperability reasons, Stockhausen currently has the concept of
    syntactic arity for constructors. For example, in
  </P>

  <PRE>
	type     t = int * real
	datatype t = A of int * real | B of {a:bool, b:string} | C of t
  </PRE>

  <P>
    constructor <TT>A</TT> and <TT>B</TT> both have arity 2, while
    <TT>C</TT> has arity 1. Usually,
    syntactic arity can be ignored. However, signature matching is restricted
    to disallow changing the syntactic arity of constructors.
  </P>

  <P>
    Example:
  </P>

  <UL>
    <LI>
      <P>
        The following program will not elaborate in the current version of
        Stockhausen:
      </P>

      <PRE>
	signature S1 =
	  sig
	    datatype t = C of int * real
	  end

	structure M1 :> S1 =	(* error: mismatch *)
	  sig
	    type     u = int * real
	    datatype t = C of u
	  end
      </PRE>

      <P>
        Neither will:
      </P>

      <PRE>
	type u = int * real

	signature S2 =
	  sig
	    datatype t = C of u
	  end

	structure M2 :> S2 =	(* error: mismatch *)
	  sig
	    datatype t = C of int * real
	  end
      </PRE>
    </LI>
  </UL>

  <P>
    Fix:
  </P>

  <UL>
    <LI>
      <P>
        In most cases, modules can be rewritten by either expanding type
        synonyms or by introducing auxiliary type synonyms:
      </P>

      <PRE>
	structure M1 :> S1 =
	  sig
	    type     u = int * real
	    datatype t = C of int * real
	  end

	structure M2 :> S2 =
	  sig
	    type     u = int * real
	    datatype t = C of u
	  end
      </PRE>

      <P>
        Transformations can get tedious in the case of higher-order functors,
        however.
      </P>
    </LI>
  </UL>

  <P>
    Note that the syntactic arity is calculated after elimination of derived
    forms. Therefore <TT>C</TT> has arity 3 in the following example:
  </P>

  <PRE>
	datatype t = C of u
	withtype u = int * int * string
  </PRE>



  <H2><A name=funid>namespaces _________________________</A></H2>
  <BR><BR>

  <P>
    Stockhausen provides higher-order functors. To integrate them smoothly into
    the language it was necessary to give up the separation between namespaces
    for structures and those for functors - both are modules.
  </P>

  <P>
    This may break programs that declare structures and functors with identical
    names.
  </P>

  <P>
    Example:
  </P>

  <UL>
    <LI>
      <P>
        The following program will not compile:
      </P>

      <PRE>
	functor Table() = struct (* ... *) end

	structure Table  = Table()
	structure Table' = Table()	(* error: Table is not a functor *)
      </PRE>
    </LI>
  </UL>

  <P>
    Fix:
  </P>

  <UL>
    <LI>
      <P>
	Rename your functors or structures appropriately. It is a good idea to
	stick to naming conventions that denote functors with names like
	<TT>MakeTable</TT>.
      </P>
    </LI>
  </UL>




  <H2>include ____________________________</H2>
  <BR><BR>

  <P>
    Because of syntactic ambiguity with uses of
    <A href="modules.php3#paramsig">parameterized signatures</A>,
    Stockhausen does not support the multiple include derived form.
  </P>

  <P>
    Example:
  </P>

  <PRE>
	signature S =
	  sig
	    include A B		(* error: A is not parameterized *)
	  end
  </PRE>

  <P>
    Fix:
  </P>

  <UL>
    <LI>
      <P>
        Split it into several include specifications:
      </P>

      <PRE>
	signature S =
	  sig
	    include A
	    include B
	  end
      </PRE>
    </LI>
  </UL>



  <H2>sharing ____________________________</H2>
  <BR><BR>

  <P>
    In Stockhausen, datatypes are not generative, but are just structural types
    similar to records. This has an impact on the use of sharing constraints,
    which require flexible (ie. generative) type constructors.
  </P>

  <P>
    Stockhausen relaxes the rules for sharing constraints and allows sharing of
    type constructors as long as one of the following conditions holds:
  </P>

  <OL>
    <LI> both are identical type synonyms, or </LI>
    <LI> the type constructor introduced later (lexically) is abstract. </LI>
  </OL>

  <P>
    This makes sharing between datatypes possible in most cases. There are
    exceptions, however.
  </P>

  <P>
    Example:
  </P>

  <UL>
    <LI>
      <P>
        Exceptions are signatures like:
      <P>

      <PRE>
	signature A =
	  sig
	    type t
	  end

	signature B =
	  sig
	    datatype t = C
	  end

	signature S =
	  sig
	    structure A : A
	    structure B : B
	    sharing type A.t = B.t	(* error: types incompatible *)
	  end
      </PRE>

      <P>
        Signature <TT>S</TT> will not elaborate because type
	<TT>B.t</TT> is specified after <TT>A.t</TT> and is neither
	abstract nor identical to <TT>A.t</TT> (which is abstract).
      </P>
    </LI>
  </UL>

  <P>
    Fixes:
  </P>

  <UL>
    <LI>
      <P>
        Signature <TT>S</TT> can be made valid by reordering structure
	specifications:
      </P>

      <PRE>
	signature S =
	  sig
	    structure B : B
	    structure A : A
	    sharing type A.t = B.t
	  end
      </PRE>

      <P>
        Suitable reordering is not always possible, however.
      </P>
    </LI>

    <LI>
      <P>
        In general, we consider sharing constraints an obsolete feature of SML.
        Use <TT>where</TT> constraints instead (Stockhausen as well as SML/NJ
	support <TT>where</TT> constraints for whole structures).
      </P>
    </LI>
  </UL>


  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       <A href="/~rossberg/">Andreas Rossberg</A> -
       last modified <?php echo date("Y/m/d") ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
