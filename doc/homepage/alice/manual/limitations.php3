<?php include("macros.php3"); ?>
<?php heading("Limitations", "implementation\nlimitations") ?>


<?php section("overview", "overview") ?>

<P>This page provides an overview of the limitations of the current version of
the Alice system.</P>


<?php section("language", "language") ?>

<UL>
  <LI> <P>No overloading</P>
       <P>Top-level arithmetic operators are only defined for type
       <TT>int</TT>.</P>
  </LI>

  <LI> <P>Equality types are ignored</P>
       <P>The equality attribute of types is ignored. The <TT>=</TT> operator
       may be applied to values of any type. For functions and similar values
       it uses physical equality.</P>
  </LI>

  <LI> <P>Signature inclusion not supported</P>
       <P>Signatures containt <TT>include</TT> specifications cannot be
       handled.</P>
  </LI>

  <LI> <P>No polymorphic sealing</P>
       <P>Dynamic <A href="packages.php3#dynamic">sealing</A> is not performed
       in cases where the signature is not statically known.</P>
  </LI>

</UL>



<?php section("library", "library") ?>

<UL>
  <LI> <P>Missing structures</P>
       <P>Some of the mandatory structures of the <A
	href="http://SML.sourceforge.net/Basis/">Standard ML Basis Library</A>
	are missing. These are:</P>
	<UL>
	  <LI> <A href="http://SML.sourceforge.net/Basis/date"><TT>Date</TT></A></LI>
	  <LI> <A href="http://SML.sourceforge.net/Basis/timer"><TT>Timer</TT></A></LI>
	  <LI> <A href="http://SML.sourceforge.net/Basis/os-path"><TT>OS.Path</TT></A></LI>
	</UL>
  </LI>

  <LI> <P>Incomplete structures</P>
       <P>Some of the implemented structures and functors from the <A
	href="http://SML.sourceforge.net/Basis/">Standard ML Basis Library</A>
	lack parts of their functionality. These includes:</P>
	<UL>
	  <LI> <A href="library/ieee-real.php3"><TT>IEEEReal</TT></A></LI>
	  <LI> <A href="library/real.php3"><TT>Real</TT></A> and <A href="library/real"><TT>LargeReal</TT></A></LI>
	  <LI> <A href="library/substring.php3"><TT>Substring</TT></A></LI>
	  <LI> <A href="library/time.php3"><TT>Time</TT></A></LI>
	  <LI> <A href="library/os-filesys.php3"><TT>OS.FileSys</TT></A></LI>
	  <LI> <A href="library/os-io.php3"><TT>OS.IO</TT></A></LI>
	  <LI> <A href="library/unix.php3"><TT>Unix</TT></A></LI>
	  <LI> <A href="library/stream-io.php3"><TT>StreamIO</TT></A></LI>
	</UL>
  </LI>

</UL>


<?php footing() ?>
