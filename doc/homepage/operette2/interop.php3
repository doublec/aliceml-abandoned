<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1 - Interoperability with Mozart",
	      "interoper<BR>ability") ?>

<?php section("overview", "overview") ?>
  <P>Stockhausen Operette 1 is based on <A href="http://www.mozart-oz.org/"
    >Mozart</A> and supports interoperability between Oz and Alice.</P>
  <P>The Stockhausen compiler translates components into pickled Oz functors.
    It is possible to mix Oz and Alice code on a per-component basis.
    This document explains how to:</P>
  <UL>
    <LI><A href="#ozfromalice">import Oz functors into Alice components</A>
    <LI><A href="#alicefromoz">import Alice functors into Oz components</A>
  </UL>

<?php section("datarepresentation", "data representation") ?>

  <H3>Basic Types</H3>
  <P>Values of basic types are mapped to corresponding Oz basic types.
    To support <TT>word</TT> operations, Mozart has been extended by a
    corresponding basic type.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>Oz Representation
      <TR><TD>int<TD>Infinite-precision integer
      <TR><TD>char<TD>Infinite-precision integer
      <TR><TD>word<TD>Provided by boot module <TT>x-oz://boot/Word</TT>
      <TR><TD>real<TD>Float
      <TR><TD>string<TD>Byte string
    </TABLE>
  </CENTER>

  <H3>Tuples and Records</H3>
  <P>Alice records are translated to Oz records by mapping all labels
    consisting only of digits to the corresponding integer feature, and
    all labels containing a letter to the corresponding atom feature.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>Oz Representation
      <TR>
	<TD>Unit value <TT>()</TT> resp. <TT>{}</TT>
	<TD>Literal <TT>unit</TT>
      <TR><TD>Tuple<TD>Tuple with label <TT>'#'</TT>
      <TR><TD>Record<TD>Record with label <TT>'#'</TT>
    </TABLE>
  </CENTER>

  <H3>Procedures</H3>
  <P>Conceptually, all Alice functions are unary.  The typical idiom is
    to pass multiple arguments as a tuple of arguments.  This case is
    treated specially by the Alice compiler.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Procedure<TH>Oz Representation
      <TR>
	<TD>Function in which every pattern is either an explicit
	  <I>n</I>-tuple or a wildcard
	<TD><I>n</I>+1-ary procedure, having <I>n</I> input arguments
	  and one output argument
      <TR><TD>Any other function<TD>Binary procedure
    </TABLE>
  </CENTER>

  <H3>Constructors</H3>
  <P>With a few exceptions simplifying interoperability, constructors of
    closed datatypes are mapped to atoms and constructors of open datatypes
    are mapped to names.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>Oz Representation
      <TR>
	<TD><TT>true</TT>, <TT>false</TT>
	<TD>Literals <TT>true</TT>, <TT>false</TT>
      <TR>
	<TD><TT>::</TT>, <TT>nil</TT>
	<TD><TT>'|'</TT>, <TT>nil</TT>
      <TR>
	<TD>Other constructors of closed datatypes
	<TD>Atoms with corresponding print name
      <TR><TD>Constructors of open datatypes<TD>Names
    </TABLE>
  </CENTER>

  <H3>Constructed Values</H3>
  <P>Conceptually, all Alice constructors are unary.  For interoperability,
    constructors syntactically declared taking a record as argument are
    treated as <A href="incompatibilities.php3#conarity"><I>n</I>-ary
    constructors</A>.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Constructed Value<TH>Oz Representation
      <TR>
	<TD>Constructed value of an <I>n</I>-ary constructor, <I>n</I> &gt; 0
	<TD>Record with the literal corresponding to the constructor as label
	  and the argument record's labels as features
      <TR><TD><TT>ref </TT><I>x</I><TD><TT>{NewCell </TT><I>x</I><TT>}</TT>
    </TABLE>
  </CENTER>
  <P>For example, assuming a declaration</P>
  <PRE>        datatype t1 = C1 of int * int</PRE>
  <P>the Alice expression <TT>C1 (1, 2)</TT> evaluates to the Oz value
    <TT>'C1'(1 2)</TT> due to the constructor <TT>C1</TT> being binary.
    However, assuming the declaration</P>
  <PRE>        datatype 'a t2 = C2 of 'a</PRE>
  <P>the Alice expression <TT>C2 (1, 2)</TT> evaluates to the Oz value
    <TT>'C2'(1#2)</TT> because the constructor <TT>C2</TT> is unary.</P>

  <H3>Futures</H3>
  <P>Alice futures map directly to Oz transients.  Promises are transparent
    on the Oz side.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Future<TH>Oz Representation
      <TR><TD>Future<TD>Future
      <TR><TD>By-need future<TD>By-need future
      <TR><TD>Promise<TD>Logic variable
    </TABLE>
  </CENTER>

  <H3>Other Library Types</H3>
  <P>Some abstract library types are implemented natively:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Library Type<TH>Oz Representation
      <TR><TD><TT>array</TT><TD>Array
      <TR><TD><TT>vector</TT><TD>Tuple with label <TT>'#'</TT>
      <TR><TD><TT>Thread.thread</TT><TD>Thread
    </TABLE>
  </CENTER>

  <H3>Modules</H3>
  <P>Alice modules are translated to Oz data structures such:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Module<TH>Oz Representation
      <TR><TD>Structure<TD>Record
      <TR><TD>Functor<TD>Binary procedure
    </TABLE>
  </CENTER>
  <P>Structure members are represented under record features.
    The feature names are computed as shown in the following table:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Identifier Class<TH>Oz Feature Representation
      <TR>
	<TD>Value or constructor,<BR>e.g., <TT>val x</TT>,
	  <TT>constructor C</TT>
	<TD>Identifier name as atom,<BR>e.g., <TT>'x'</TT>, <TT>'C'</TT>
      <TR>
	<TD>Structure or functor,<BR>e.g., <TT>structure S</TT>
	<TD>Dollar-prefixed identifier as atom,<BR>e.g., <TT>'$S'</TT>
    </TABLE>
  </CENTER>
  <P>Signatures and types are not yet represented at run time.  Note that
    signature coercion is not performed operationally, that is, all structure
    members are visible on the Oz side.</P>

  <H3>Components</H3>
  <P>Alice components are translated into Oz functors:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Component<TH>Oz Representation
      <TR><TD>Compiled component<TD>Pickled functor
    </TABLE>
  </CENTER>
  <P>The export of the generated functor is the record corresponding
    to the component considered as a single structure.</P>

<?php section("ozfromalice", "oz from alice") ?>

<?php section("alicefromoz", "alice from oz") ?>

<?php footing() ?>
