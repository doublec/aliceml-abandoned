<?php include("macros.php3"); ?>
<?php heading("The Alice library", "the\nalice\nlibrary") ?>


<?php section("structure", "structure") ?>

<P>Alice is equipped with an extended version of the <A
href="http://SML.sourceforge.net/Basis/">Standard ML Basis Library</A>. 
Most additional modules deal with Alice ML extensions over Standard ML.</P>

<P>The library is split into <A
href="../dynamics.php3#components">components</A>. Usually, one component
exports a single module (structure or functor) or signature. The components are
named after the module or signature they export. Signature component names have
a <TT>-sig</TT> suffix appended to avoid problems with case insensitive
operating systems.</P>

<P>Library components are imported through URIs using the <TT>x-alice:</TT>
scheme. The library is organized hierarchically as follows:</P>

<UL>
  <LI><A href="#fundamental"><TT>"x-alice:/lib/fundamental/"</TT></A>
	- fundamental types and data structures </LI>
  <LI><A href="#system"><TT>"x-alice:/lib/system/"</TT></A>
	- I/O and other system related functionality </LI>
  <LI><A href="#distribution"><TT>"x-alice:/lib/distribution/"</TT></A>
	- distributed programming </LI>
  <LI><A href="#constraints"><TT>"x-alice:/lib/constraints/"</TT></A>
	- constraint programming </LI>
  <LI><A href="#tools"><TT>"x-alice:/lib/tools/"</TT></A>
	- interactive tools </LI>
  <LI><A href="#gtk"><TT>"x-alice:/lib/gtk/"</TT></A>
	- Gtk+ GUI toolkit </LI>
</UL>

<P>The Standard ML <A href="#toplevel.php3">top-level environment</A> is
available as usual.</P>


<?php section("import", "import") ?>

<P>Components from the fundamental library, and those components of the system
library that are part of the Standard ML Basis Library, are imported implicitly.
Other components can be accessed through appropriate explicit import
announcements. For example, a program using the <A
href="url.php3"><TT>Url</TT></A> component has to be prefixed by the
following announcement:</P>

<PRE class=code>
import structure Url from "x-alice:/lib/system/Url"</PRE>

<P>The reference pages for the individual library items contain the synopsis of
the appropriate import announcement for each item. Note that it is not
necessary to import a signature in order to use a structure implementing that
signature, although the library pages usually contain the synopsis for both.
Signatures only need to be imported if used explicitly.</P>


<?php subsection("import-all", "Interactive import") ?>

<P>The <A href="usage.php#interactive">interactive toplevel</A> system imports
the <EM>complete</EM> library, except <TT>/lib/gtk/</TT>, implicitly. Loading of
components is performed lazily, though.</P>

<P>To access non-library components (e.g. compiled programs) in the toplevel,
import announcements must still be used. For convenience, it is possible to
abbreviate import announcements as follows interactively:</P>

<PRE class=code>
import "build/HelloWorld"</PRE>

<P>Such an announcement will import everything exported by the corresponding
component.</P>


<!--
<?php section("compatibility", "compatibility") ?>

  <P>
    <EM>Note:</EM> Most other SML implementations currently still implement
    a <A href="http://www.dina.kvl.dk/~sestoft/sml/">previous version of the
    Standard Basis library</A>. Some incompatible changes in the recent
    version may thus cause some SML code ported from other systems to be
    rejected by Alice. Watch out particularly for uses of array and vector
    modules.
  </P>
-->

<?php section("toplevel", "top-level") ?>

<P>The <A href="toplevel.php3">top-level environment</A> contains all
types, exceptions and values that are available unqualified.</P>


<?php section("fundamental", "fundamental library") ?>

<?php subsection("fundamental-sigs", "Signatures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="alt.php3">ALT</A></TD> </TR>
    <TR> <TD><A href="array.php3">ARRAY</A></TD> </TR>
    <TR> <TD><A href="array-slice.php3">ARRAY_SLICE</A></TD> </TR>
    <TR> <TD><A href="bool.php3">BOOL</A></TD> </TR>
    <TR> <TD><A href="byte.php3">BYTE</A></TD> </TR>
    <TR> <TD><A href="char.php3">CHAR</A></TD> </TR>
    <TR> <TD><A href="future.php3">FUTURE</A></TD> </TR>
    <TR> <TD><A href="general.php3">GENERAL</A></TD> </TR>
    <TR> <TD><A href="hole.php3">HOLE</A></TD> </TR>
    <TR> <TD><A href="ieee-real.php3">IEEE_REAL</A></TD> </TR>
    <TR> <TD><A href="integer.php3">INTEGER</A></TD> </TR>
    <TR> <TD><A href="list.php3">LIST</A></TD> </TR>
    <TR> <TD><A href="list-pair.php3">LIST_PAIR</A></TD> </TR>
    <TR> <TD><A href="lock.php3">LOCK</A></TD> </TR>
    <TR> <TD><A href="math.php3">MATH</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">MONO_ARRAY</A></TD> </TR>
    <TR> <TD><A href="mono-array-slice.php3">MONO_ARRAY_SLICE</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">MONO_VECTOR</A></TD> </TR>
    <TR> <TD><A href="mono-vector-slice.php3">MONO_VECTOR_SLICE</A></TD> </TR>
    <TR> <TD><A href="option.php3">OPTION</A></TD> </TR>
    <TR> <TD><A href="package.php3">PACKAGE</A></TD> </TR>
    <TR> <TD><A href="pair.php3">PAIR</A></TD> </TR>
    <TR> <TD><A href="promise.php3">PROMISE</A></TD> </TR>
    <TR> <TD><A href="real.php3">REAL</A></TD> </TR>
    <TR> <TD><A href="ref.php3">REF</A></TD> </TR>
    <TR> <TD><A href="string.php3">STRING</A></TD> </TR>
    <TR> <TD><A href="substring.php3">SUBSTRING</A></TD> </TR>
    <TR> <TD><A href="text.php3">TEXT</A></TD> </TR>
    <TR> <TD><A href="thread.php3">THREAD</A></TD> </TR>
    <TR> <TD><A href="time.php3">TIME</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">UNIQUE_STRING</A></TD> </TR>
    <TR> <TD><A href="vector.php3">VECTOR</A></TD> </TR>
    <TR> <TD><A href="vector-pair.php3">VECTOR_PAIR</A></TD> </TR>
    <TR> <TD><A href="vector-slice.php3">VECTOR_SLICE</A></TD> </TR>
    <TR> <TD><A href="word.php3">WORD</A></TD> </TR>
  </TABLE>

<?php subsection("fundamental-structs", "Structures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="alt.php3">Alt</A></TD> </TR>
    <TR> <TD><A href="array.php3">Array</A></TD> </TR>
    <TR> <TD><A href="array-slice.php3">ArraySlice</A></TD> </TR>
    <TR> <TD><A href="bool.php3">Bool</A></TD> </TR>
    <TR> <TD><A href="byte.php3">Byte</A></TD> </TR>
    <TR> <TD><A href="char.php3">Char</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">CharArray</A></TD> </TR>
    <TR> <TD><A href="mono-array-slice.php3">CharArraySlice</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">CharVector</A></TD> </TR>
    <TR> <TD><A href="mono-vector-slice.php3">CharVectorSlice</A></TD> </TR>
    <TR> <TD><A href="future.php3">Future</A></TD> </TR>
    <TR> <TD><A href="general.php3">General</A></TD> </TR>
    <TR> <TD><A href="hole.php3">Hole</A></TD> </TR>
    <TR> <TD><A href="ieee-real.php3">IEEEReal</A></TD> </TR>
    <TR> <TD><A href="integer.php3">Int</A></TD> </TR>
    <TR> <TD><A href="integer.php3">LargeInt</A></TD> </TR>
    <TR> <TD><A href="real.php3">LargeReal</A></TD> </TR>
    <TR> <TD><A href="word.php3">LargeWord</A></TD> </TR>
    <TR> <TD><A href="list.php3">List</A></TD> </TR>
    <TR> <TD><A href="list-pair.php3">ListPair</A></TD> </TR>
    <TR> <TD><A href="lock.php3">Lock</A></TD> </TR>
    <TR> <TD><A href="math.php3">Math</A></TD> </TR>
    <TR> <TD><A href="option.php3">Option</A></TD> </TR>
    <TR> <TD><A href="package.php3">Package</A></TD> </TR>
    <TR> <TD><A href="pair.php3">Pair</A></TD> </TR>
    <TR> <TD><A href="integer.php3">Position</A></TD> </TR>
    <TR> <TD><A href="promise.php3">Promise</A></TD> </TR>
    <TR> <TD><A href="real.php3">Real</A></TD> </TR>
    <TR> <TD><A href="ref.php3">Ref</A></TD> </TR>
    <TR> <TD><A href="string.php3">String</A></TD> </TR>
    <TR> <TD><A href="substring.php3">Substring</A></TD> </TR>
    <TR> <TD><A href="text.php3">Text</A></TD> </TR>
    <TR> <TD><A href="thread.php3">Thread</A></TD> </TR>
    <TR> <TD><A href="time.php3">Time</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">UniqueString</A></TD> </TR>
    <TR> <TD><A href="vector.php3">Vector</A></TD> </TR>
    <TR> <TD><A href="vector-pair.php3">VectorPair</A></TD> </TR>
    <TR> <TD><A href="vector-slice.php3">VectorSlice</A></TD> </TR>
    <TR> <TD><A href="char.php3">WideChar</A></TD> </TR>
    <TR> <TD><A href="string.php3">WideString</A></TD> </TR>
    <TR> <TD><A href="substring.php3">WideSubstring</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">WideUniqueString</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word8</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word31</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">Word8Array</A></TD> </TR>
    <TR> <TD><A href="mono-array-slice.php3">Word8ArraySlice</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">Word8Vector</A></TD> </TR>
    <TR> <TD><A href="mono-vector-slice.php3">Word8VectorSlice</A></TD> </TR>
  </TABLE>

<?php subsection("fundamental-funs", "Functors") ?>

  <TABLE class=tt>
    <TR> <TD><A href="byneed.php3">ByNeed</A></TD> </TR>
  </TABLE>


<?php section("system", "system library") ?>

<?php subsection("system-sigs", "Signatures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="bin-io.php3">BIN_IO</A></TD> </TR>
    <TR> <TD><A href="command-line.php3">COMMAND_LINE</A></TD> </TR>
    <TR> <TD><A href="component.php3">COMPONENT</A></TD> </TR>
    <TR> <TD><A href="component-manager.php3">COMPONENT_MANAGER</A></TD> </TR>
    <TR> <TD><A href="config.php3">CONFIG</A></TD> </TR>
    <TR> <TD><A href="http.php3">HTTP</A></TD> </TR>
    <TR> <TD><A href="http-client.php3">HTTP_CLIENT</A></TD> </TR>
    <TR> <TD><A href="http-server.php3">HTTP_SERVER</A></TD> </TR>
    <TR> <TD><A href="imperative-io.php3">IMPERATIVE_IO</A></TD> </TR>
    <TR> <TD><A href="io.php3">IO</A></TD> </TR>
    <TR> <TD><A href="os.php3">OS</A></TD> </TR>
    <TR> <TD><A href="os-file-sys.php3">OS_FILE_SYS</A></TD> </TR>
    <TR> <TD><A href="os-io.php3">OS_IO</A></TD> </TR>
    <TR> <TD><A href="os-process.php3">OS_PROCESS</A></TD> </TR>
    <TR> <TD><A href="pickle.php3">PICKLE</A></TD> </TR>
    <TR> <TD><A href="prim-io.php3">PRIM_IO</A></TD> </TR>
    <TR> <TD><A href="resolver.php3">RESOLVER</A></TD> </TR>
    <TR> <TD><A href="resolver-handler.php3">RESOLVER_HANDLER</A></TD> </TR>
    <TR> <TD><A href="socket.php3">SOCKET</A></TD> </TR>
    <TR> <TD><A href="stream-io.php3">STREAM_IO</A></TD> </TR>
    <TR> <TD><A href="text-io.php3">TEXT_IO</A></TD> </TR>
    <TR> <TD><A href="text-stream-io.php3">TEXT_STREAM_IO</A></TD> </TR>
    <TR> <TD><A href="unix.php3">UNIX</A></TD> </TR>
    <TR> <TD><A href="url.php3">URL</A></TD> </TR>
  </TABLE>

<?php subsection("system-structs", "Structures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="bin-io.php3">BinIO</A></TD> </TR>
    <TR> <TD><A href="prim-io.php3">BinPrimIO</A></TD> </TR>
    <TR> <TD><A href="command-line.php3">CommandLine</A></TD> </TR>
    <TR> <TD><A href="component.php3">Component</A></TD> </TR>
    <TR> <TD><A href="component-manager.php3">ComponentManager</A></TD> </TR>
    <TR> <TD><A href="config.php3">Config</A></TD> </TR>
    <TR> <TD><A href="http.php3">Http</A></TD> </TR>
    <TR> <TD><A href="http-client.php3">HttpClient</A></TD> </TR>
    <TR> <TD><A href="http-server.php3">HttpServer</A></TD> </TR>
    <TR> <TD><A href="io.php3">IO</A></TD> </TR>
    <TR> <TD><A href="os.php3">OS</A></TD> </TR>
    <TR> <TD><A href="os-file-sys.php3">OS.FileSys</A></TD> </TR>
    <TR> <TD><A href="os-io.php3">OS.IO</A></TD> </TR>
    <TR> <TD><A href="os-process.php3">OS.Process</A></TD> </TR>
    <TR> <TD><A href="pickle.php3">Pickle</A></TD> </TR>
    <TR> <TD><A href="resolver.php3">Resolver</A></TD> </TR>
    <TR> <TD><A href="socket.php3">Socket</A></TD> </TR>
    <TR> <TD><A href="text-io.php3">TextIO</A></TD> </TR>
    <TR> <TD><A href="prim-io.php3">TextPrimIO</A></TD> </TR>
    <TR> <TD><A href="text-io.php3">TextIO.StreamIO</A></TD> </TR>
    <TR> <TD><A href="unix.php3">Unix</A></TD> </TR>
    <TR> <TD><A href="url.php3">Url</A></TD> </TR>
  </TABLE>

<?php subsection("system-funs", "Functors") ?>

  <TABLE class=tt>
    <TR> <TD><A href="imperative-io.php3">ImperativeIO</A></TD> </TR>
    <TR> <TD><A href="prim-io.php3">PrimIO</A></TD> </TR>
    <TR> <TD><A href="stream-io.php3">StreamIO</A></TD> </TR>
  </TABLE>



<?php section("distribution", "distribution library") ?>

<?php subsection("distribution-sigs", "Signatures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="remote.php3">REMOTE</A></TD> </TR>
  </TABLE>

<?php subsection("distribution-structs", "Structures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="remote.php3">Remote</A></TD> </TR>
  </TABLE>


<?php section("constraints", "constraints library") ?>

<?php subsection("constraints-sigs", "Signatures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="fd.php3">FD</A></TD> </TR>
    <TR> <TD><A href="fs.php3">FS</A></TD> </TR>
    <TR> <TD><A href="linear.php3">LINEAR</A></TD> </TR>
    <TR> <TD><A href="search.php3">SEARCH</A></TD> </TR>
    <TR> <TD><A href="space.php3">SPACE</A></TD> </TR>
  </TABLE>

<?php subsection("constraints-structs", "Structures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="fd.php3">FD</A></TD> </TR>
    <TR> <TD><A href="fs.php3">FS</A></TD> </TR>
    <TR> <TD><A href="linear.php3">Linear</A></TD> </TR>
    <TR> <TD><A href="search.php3">Search</A></TD> </TR>
    <TR> <TD><A href="space.php3">Space</A></TD> </TR>
  </TABLE>


<?php section("tools", "tools library") ?>

<?php subsection("tools-sigs", "Signatures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="explorer.php3">EXPLORER</A></TD> </TR>
    <TR> <TD><A href="inspector.php3">INSPECTOR</A></TD> </TR>
  </TABLE>

<?php subsection("tools-structs", "Structures") ?>

  <TABLE class=tt>
    <TR> <TD><A href="explorer.php3">Explorer</A></TD> </TR>
    <TR> <TD><A href="inspector.php3">Inspector</A></TD> </TR>
  </TABLE>


<?php footing() ?>
