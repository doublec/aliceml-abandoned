<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2 - Extended Module System",
		"extended <BR> module <BR> system") ?>


<?php section("overview", "overview") ?>

  <P>
    Alice extends the SML module system in various ways, by providing:
  </P>

  <UL>
    <LI> <A href="#local">local modules</A> </LI>
    <LI> <A href="#higher">higher-order functors</A> </LI>
    <LI> <A href="laziness.php3#modules">lazy evaluation</A> </LI>
    <LI> <A href="#top">top signature</A> </LI>
    <LI> <A href="#sigmembers">signature members</A> (*) </LI>
    <LI> <A href="#paramsig">parameterized signatures</A> (*) </LI>
    <LI> manifest structures and values (*) </LI>
    <LI> <A href="#fixity">fixity specifications</A> in signatures </LI>
    <LI> some syntactic sugar (eg. <A href="#wildcards">wildcards</A>)
         and straightening </LI>
  </UL>

  <P>
    Items marked with (*) are not fully implemented in Operette 2.
  </P>

  <P>
    The last item includes allowing <TT>op</TT> in signatures, having
    a <TT>withtype</TT> derived form in signatures (analoguous to
    declarations), and allowing redundant parentheses in structure and
    signature expressions.
  </P>

  <P>
    A <A href="#syntax">syntax summary</A> is given below.
  </P>



<?php section("local", "local modules") ?>

  <P>
    Alice discards SML's separation between core declarations
    (<I>dec</I>), structure declarations (<I>strdec</I>), and toplevel
    declarations (<I>topdec</I>). As a consequence, structures can be
    declared local to an expression (via <TT>let</TT>) and functors as well
    as signatures can be nested into structures. For example:
  </P>

  <PRE>
	fun sortIntList ns =
	    let
	        structure Tree = MkBinTree(type t = int)
	    in
		Tree.toList(foldl Tree.insert Tree.empty ns)
	    end
  </PRE>



<?php section("higher", "higher-order functors") ?>

  <P>
    A direct consequence of allowing functors everywhere is the presence
    of higher-order functors, even if a bit cumbersome:
  </P>

  <PRE>
	functor F(X : S1) =
	struct
	    functor G(Y : S2) = struct (* ... *) end
	end

	structure M = let structure Z = F(X) in G(Y) end
  </PRE>

  <P>
    This is exactly how SML/NJ introduces higher-order functors. Alice
    provides higher-order functors in a more first-class fashion. The above
    example can be written more directly as:
  </P>

  <PRE>
	functor F(X : S1)(Y : S2) = struct (* ... *) end

	structure M = F(X)(Y)
  </PRE>

  <P>
    Actually, Alice even allows omitting the parentheses:
  </P>

  <PRE>
	structure M = F X Y
  </PRE>

  <P>
    Alice has real functor expressions.
    Similar to <TT>fun</TT> declarations, <TT>functor</TT> declarations are
    mere derived forms. The declaration for <TT>F</TT> above is just sugar
    for:
  </P>

  <PRE>
	structure F = fct(X : S1) => fct(Y : S2) => struct (* ... *) end
  </PRE>

  <P>
    The keyword <TT>fct</TT> starts a functor expression very much like
    <TT>fn</TT> begins a function expression in the core language. Functor
    expressions can be arbitrarily mixed with other structure expressions.
    In contrast to SML, there is no distinction between structure and functor
    identifiers
    (see <A href="incompatibilities.php3#funid">incompatibilities</A>).
  </P>

  <P>
    (Note: We decided to keep the keyword <TT>structure</TT> - and the syntactic
    classes <I>strid</I>, <I>strexp</I>, etc. - for compatibility reasons,
    although <TT>module</TT> might now be more appropriate.)
  </P>

  <P>
    The syntax for signatures has been extended to contain functor types.
    For example, functor <TT>F</TT> can be described by the following signature:
  </P>

  <PRE>
	structure F : fct(X : S1) -> fct(Y : S2) -> sig (* ... *) end
  </PRE>

  <P>
    As a comfortable derived form the following SML/NJ compatible syntax
    is provided for functor descriptions in signatures:
  </P>

  <PRE>
	functor F(X : S1)(Y : S2) : sig (* ... *) end
  </PRE>

  <P><A name=top>
    For completeness of the lattice of signature types we also introduced a
    top signature, denoted by the keyword <TT>any</TT>:
  </A></P>

  <PRE>
	functor Id(X : any) = X
  </PRE>
  


<?php section("sigmembers", "signature members") ?>

  <P>
    Like structures and functors, signatures can also be declared anywhere.
    In particular, this allows signatures inside structures, and
    consequently, nested signatures:
  </P>

  <PRE>
	signature S =
	sig
	    signature T = sig (* ... *) end
	end

	structure X :> S =
	struct
	    signature T = sig (* ... *) end
	end
  </PRE>

  <P>
    Such manifest signatures must always be matched exactly (Note: matching
    of manifest signatures is not checked yet).
    However, analoguous to types, we allow for abstract signature members:
  </P>

  <PRE>
	signature S =
	sig
	    signature T
	    structure X : T
	end
  </PRE>

  <DIV class=note>
  <P>
    Note that this feature renders the type system of Alice undecidable
    (the same is true for O'Caml, which has a very similar module language).
    We do not consider this a problem in practice, however, since the simplest
    program to make the type checker loop already is highly artificial:
  </P>

  <PRE>
	signature I =
	sig
	    signature A
	    functor F(X : sig
	                      signature A = A
	                      functor F(X : A) : sig end
	                  end) : sig end
	end

	signature J =
	sig
	    signature A = I
	    functor F(X : I) : sig end
	end

	(* Try to check J &lt;= I *)

	functor Loop(X : J) = X : I
  </PRE>

  <P>
    Currently the compiler has no upper limit on the number of substitutions it
    does during signature matching, so this example will actually make it loop
    until memory is exhausted.
  </P>
  </DIV>



<?php section("paramsig", "parameterized signatures") ?>

  <P>
    Functors often require putting <TT>where</TT> constraints on signatures
    to denote exact return types. This can become quite tedious. Alice
    provides an alternative by generalizing signature identifiers to signature
    constructors, parameterized over structure values:
  </P>

  <PRE>
	signature SET(Elem : sig type t end) =
	sig
	    type elem = Elem.t
	    type t
	    (* ... *)
	end

	functor MakeSet(Elem : sig type t end) :> SET(Elem) =
	struct
	    (* ... *)
	end
  </PRE>

  <P>
    The same derived forms as for functor declarations apply, so <TT>SET</TT>
    and <TT>MakeSet</TT> can be defined as:
  </P>

  <PRE>
	signature SET(type t) =
	sig
	    type elem = t
	    type t
	    (* ... *)
	end

	functor MakeSet(type t) :> SET(type t = t) =
	struct
	    (* ... *)
	end
  </PRE>

  <P class=note>
    Caveat: parameterized signatures are not yet properly treated
    in Operette 2.
  </P>



<?php section("wildcards", "wildcards") ?>

  <P>
    The module system can be abused to type some more delicate
    operations, like pickling:
  </P>

  <PRE>
	signature UNIT = sig end

	functor Pickle(type t  val x : t) : UNIT
  </PRE>

  <P>
    When utilizing modules for this purpose the result of a functor application
    is uninteresting. Similar to core declarations, wildcards are thus provided
    as a derived form:
  </P>

  <PRE>
	structure _ = Pickle(type t = int  val x = 43)
  </PRE>

  <P>
    Similarly, wildcards are allowed for functor parameters:
  </P>

  <PRE>
	functor F(_ : S) = struct (* don't actually need argument *) end
  </PRE>

  <P>
    This is particularly useful in signatures:
  </P>

  <PRE>
	signature FF = fct(_ : A) -> B
  </PRE>



<?php section("fixity", "fixity") ?>

  <P>
    Signatures can contain fixity specifications:
  </P>

  <PRE>
	signature S =
	    sig
	        type t
	        infix 2 ++
		val x :    t
	        val op++ : t * t -> t
	    end
  </PRE>

  <P>
    To match a signature with infix specifications, a structure must provide
    the same infix status directives. The infix environment is part of
    a structures principal signature.
  </P>

  <P>
    Opening a structure with unconstrained infix members pulls in the according
    infix status in the local environment:
  </P>

  <PRE>
	structure M :> S = struct (* ... *) end

	open M
	val z = x ++ x
  </PRE>

  <P>
    Note that this feature produces a syntactic
    <A href="incompatibilities.php3#openinfix">incompatibility</A> with SML
    showing up in some rare cases.
  </P>



<?php section("syntax", "syntax") ?>

  <P>
    The syntax for modules very much resembles the syntax of core
    language expressions:
  </P>

  <H3>Structures</H3>

  <TABLE>
    <TR>
      <TD> <I>atstrexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>struct</TT> <I>dec</I> <TT>end</TT> </TD>
      <TD> structure </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>longstrid</I> </TD>
      <TD> structure identifier </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>let</TT> <I>dec</I> <TT>in</TT> <I>strexp</I> <TT>end</TT> </TD>
      <TD> local declarations </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>(</TT> <I>strexp</I> <TT>)</TT> </TD>
      <TD> parentheses </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>appstrexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>atstrexp</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>appstrexp</I> <I>atstrexp</I> </TD>
      <TD> functor application </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>strexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>appstrexp</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>strexp</I> <TT>:</TT> <I>sigexp</I> </TD>
      <TD> transparent constraint </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>strexp</I> <TT>:></TT> <I>sigexp</I> </TD>
      <TD> opaque constraint </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>fct</TT> <I>strpat</I> <TT>=></TT> <I>strexp</I> </TD>
      <TD> functor </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>strpat</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>(</TT> <I>strid</I> <TT>:</TT> <I>sigexp</I> <TT>)</TT> </TD>
      <TD> </TD>
    </TR>
  </TABLE>


  <H3>Signatures</H3>

  <TABLE>
    <TR>
      <TD> <I>atsigexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>any</TT> </TD>
      <TD> top </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>sig</TT> <I>spec</I> <TT>end</TT> </TD>
      <TD> ground signature </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>longsigid</I> </TD>
      <TD> signature identifier </TD>
    </TR>
<!--
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>let</TT> <I>dec</I> <TT>in</TT> <I>sigexp</I> <TT>end</TT> </TD>
      <TD> local declarations </TD>
    </TR>
-->
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>(</TT> <I>sigexp</I> <TT>)</TT> </TD>
      <TD> parentheses </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>appsigexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>atsigexp</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>appsigexp</I> <I>atstrexp</I> </TD>
      <TD> signature application </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>sigexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>appsigexp</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>fct</TT> <I>strpat</I> <TT>-></TT> <I>sigexp</I> </TD>
      <TD> functor </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>sigexp</I> <TT>where</TT> <I>rea</I> </TD>
      <TD> specialization </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>rea</I> </TD>
      <TD align="center">::=</TD>
<!--
      <TD> <TT>val</TT> <I>longvid</I> <TT>=</TT> <I>longvid</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>constructor</TT> <I>longvid</I> <TT>=</TT> <I>longvid</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
-->
      <TD> <TT>type</TT> <I>tyvarseq</I> <I>longtycon</I>
           <TT>=</TT> <I>ty</I> </TD>
      <TD> </TD>
    </TR>
<!--
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>structure</TT> <I>longstrid</I> <TT>=</TT> <I>longstrid</I> </TD>
      <TD> </TD>
    </TR>
-->
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <TT>signature</TT> <I>longsigid</I>
           <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           <TT>=</TT> <I>sigexp</I> </TD>
      <TD> (n>=0) </TD>
    </TR>
    <TR></TR>
    <TR valign=baseline>
      <TD> <I>sigbind</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>sigid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           <TT>=</TT> <I>sigexp</I>
           &lt;<TT>and</TT> <I>sigbind</I>&gt; </TD>
      <TD> (n>=0) </TD>
    </TR>
    <TR></TR>
    <TR valign=baseline>
      <TD> <I>sigdesc</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>sigid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           &lt;<TT>=</TT> <I>sigexp</I>&gt;
           &lt;<TT>and</TT> <I>sigdesc</I>&gt; </TD>
      <TD> (n>=0) </TD>
    </TR>
  </TABLE>


  <H3>Derived Forms</H3>

  <TABLE>
    <TR>
      <TD> <I>atstrexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>(</TT> <I>dec</I> <TT>)</TT> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD> <I>strpat</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>(</TT> <I>spec</I> <TT>)</TT> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>(</TT> <TT>_</TT> <TT>:</TT> <I>sigexp</I> <TT>)</TT> </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>dec</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>functor</TT> <I>fstrbind</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD> <I>strbind</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>_</TT> &lt;<TT>:</TT> <I>sigexp</I>&gt; <TT>=</TT> <I>strexp</I>
           &lt;<TT>and</TT> <I>strbind</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR valign=baseline>
      <TD> <I>fstrbind</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>strid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           <TT>=</TT> <I>strexp</I>
           &lt;<TT>and</TT> <I>fstrbind</I>&gt; </TD>
      <TD> (n>=1) </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>atsigexp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>(</TT> <I>spec</I> <TT>)</TT> </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>spec</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>functor</TT> <I>fstrdesc</I> </TD>
      <TD> </TD>
    </TR>
    <TR valign=baseline>
      <TD> <I>fstrdesc</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>strid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           <TT>:</TT> <I>sigexp</I>
           &lt;<TT>and</TT> <I>fstrdesc</I>&gt; </TD>
      <TD> (n>=1) </TD>
    </TR>
  </TABLE>


<?php footing() ?>
