<?php include("macros.php3"); ?>
<?php heading("Limitations", "implementation\nlimitations") ?>


<?php section("overview", "overview") ?>

<P>This page provides an overview of the limitations of the current version of
the Alice system.</P>


<?php section("language", "language") ?>

<DL>
  <DT>No overloading</DT>
  <DD><P>Top-level arithmetic operators are only defined for
	type<TT>int</TT>.</P>
  </DD>

  <DT>Equality types are ignored</DT>
  <DD><P>The equality attribute of types is ignored. The <TT>=</TT> operator
      may be applied to values of any type. For functions and similar values
      it uses physical equality.</P>
  </DD>

  <DT>Signature inclusion not supported</DT>
  <DD><P>Signatures containt <TT>include</TT> specifications cannot be
      handled.</P>
  </DD>

  <DT>No polymorphic sealing</DT>
  <DD><P><A href="packages.php3#dynamic">Dynamic sealing</A> is not performed
       in cases where the signature is not statically known.</P>
  </DD>
</DL>



<?php section("library", "library") ?>

<DL>
  <DT>Missing structures</DD>
  <DD><P>Some of the mandatory structures of the <A
	href="http://SML.sourceforge.net/Basis/">Standard ML Basis Library</A>
	are missing. These are:</P>
	<UL>
	  <LI> <A href="http://SML.sourceforge.net/Basis/date.html"><TT>Date</TT></A></LI>
	  <LI> <A href="http://SML.sourceforge.net/Basis/timer.html"><TT>Timer</TT></A></LI>
	  <LI> <A href="http://SML.sourceforge.net/Basis/os-path.html"><TT>OS.Path</TT></A></LI>
	</UL>
	<P></P>
  </DD>

  <DT>Incomplete structures</DT>
  <DD><P>Some of the implemented structures and functors from the <A
	href="http://SML.sourceforge.net/Basis/">Standard ML Basis Library</A>
	lack parts of their functionality. These includes:</P>
	<UL>
	  <LI> <A href="library/ieee-real.php3"><TT>IEEEReal</TT></A></LI>
	  <LI> <A href="library/real.php3"><TT>Real</TT></A> and <A href="library/real.php3"><TT>LargeReal</TT></A></LI>
	  <LI> <A href="library/substring.php3"><TT>Substring</TT></A></LI>
	  <LI> <A href="library/time.php3"><TT>Time</TT></A></LI>
	  <LI> <A href="library/os-file-sys.php3"><TT>OS.FileSys</TT></A></LI>
	  <LI> <A href="library/os-io.php3"><TT>OS.IO</TT></A></LI>
	  <LI> <A href="library/unix.php3"><TT>Unix</TT></A></LI>
	  <LI> <A href="library/stream-io.php3"><TT>StreamIO</TT></A></LI>
	</UL>
	<P></P>
  </DD>
</DL>


<?php footing() ?>
