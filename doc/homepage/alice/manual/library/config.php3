<?php include("macros.php3"); ?>
<?php heading("The Config structure",
	      "The <TT>Config</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature CONFIG
    structure Config : CONFIG
  </PRE>

  <P>
    The <TT>Config</TT> structure serves to provide system information
    that is either platform- or installation-dependent.
  </P>

<?php section("import", "import") ?>

  <PRE>
    import signature CONFIG from "x-alice:/lib/system/CONFIG-sig"
    import structure Config from "x-alice:/lib/system/Config"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature CONFIG =
    sig
	datatype <A href="#platform-type">platform</A> = WIN32 | UNIX

	val <A href="#platform">platform</A> : platform
	val <A href="#vm">vm</A> : string

	val <A href="#homeDir">homeDir</A> : string option

	val <A href="#pathEscape">pathEscape</A> : char option
	val <A href="#pathSeparator">pathSeparator</A> : char
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>datatype <A name="platform-type">platform</A> = WIN32 | UNIX</TT>
    </DT>
    <DD>
      <P>The type of supported platforms.</P>
    </DD>

    <DT>
      <TT><A name="platform">platform</A></TT>
    </DT>
    <DD>
      <P>The platform the current process executes on.</P>
    </DD>

    <DT>
      <TT><A name="vm">vm</A></TT>
    </DT>
    <DD>
      <P>A string identification of the virtual machine that the current
	process is running.</P>
    </DD>

    <DT>
      <TT><A name="homeDir">homeDir</A></TT>
    </DT>
    <DD>
      <P>The installation directory of the Alice system, if known.
	Initialized from the <TT>STOCKHOME</TT> environment variable.</P>
    </DD>

    <DT>
      <TT><A name="pathEscape">pathEscape</A></TT>
    </DT>
    <DD>
      <P>The character, if any, the current platform uses to escape
	characters in path names.  Typically <TT>NONE</TT> under Windows,
	<TT>#"\\"</TT> under Unix.</P>
    </DD>

    <DT>
      <TT><A name="pathSeparator">pathSeparator</A></TT>
    </DT>
    <DD>
      <P>The character used on the current platform to separate path names
	in path list specifications, such as the <TT>PATH</TT> environment
	variable used under Windows and Unix.  Typically <TT>#";"</TT> under
	Windows, <TT>#":"</TT> under Unix.</P>
    </DD>
  </DL>

<?php footing() ?>
