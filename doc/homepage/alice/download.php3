<?php include("macros.php3"); ?>

<?php heading("Alice Download", "download") ?>

  <BR><BR><BR><BR>

<?php section("current", "release") ?>

  <P>
    The public release of the Alice system is scheduled for second half of 2002
    and will be announced in appropriate newsgroups. 
  </P>


<?php section("preview", "preview") ?>

  <P>
    <B>For the brave</B>, a snapshot of the current development version is
    available as a <I>technology preview</I>:
  </P>

  <UL>
    <LI>Linux (tar files of the installation build on Red Hat 7.2):

    <UL>
      <LI><A href="download/mozart-devel.tgz"><TT>mozart-devel.tgz</TT></A></LI>
      <LI><A href="download/stockhausen-devel.tgz"><TT>stockhausen-devel.tgz</TT></A></LI>
    </UL>

    <P>
      To install, untar both files somewhere and set the following environment
      variables:
    </P>

    <PRE>
        OZHOME    = /somewhere/mozart-devel/
        STOCKHOME = /somewhere/stockhausen-devel/
	PATH      = /somewhere/stockhausen-devel/bin:$PATH
    </PRE>

    <LI>Windows (installer packages):

    <UL>
      <LI><A href="download/mozart-1.3.0-20020424.exe"><TT>mozart-1.3.0-20020424.exe</TT></A></LI>
      <LI><A href="download/stockhausen-0.3.0-20020424.exe"><TT>stockhausen-0.3.0-20020424.exe</TT></A></LI>
    </UL>

    <P>
      To install, execute both programs and follow the instructions.
      If you have trouble installing Mozart, please read the
      <A href="http://www.mozart-oz.org/download/ftp.cgi?action=windows">
      installation notes for Windows</A>.
    </P>
  </UL>

  <DIV class=note>
  <P>
    Note: The current version of Alice is based on <A
    href="http://www.mozart-oz.org/">Mozart</A>. For that reason you have to
    download and install the Mozart packages provided above in order to
    run Alice.
  </P>
  </DIV>

  <P>
    <B>Disclaimer:</B> This is a prototype version. It may be <I>buggy</I>,
    <I>incomplete</I>, <I>slow</I> and <I>rough</I> to use. We do not promise
    anything!
    Still, we highly welcome any feedback. Please send comments or questions to
    <A href="mailto:stockhausen@.ps.uni-sb.de">stockhausen@ps.uni-sb.de</A>.
    If you discover bugs, we appreciate reports to the
    <A href="bugzilla/">bug tracking system</A>.
  </P>

  <P>
    The Alice manual is available <A href="manual/">online</A>, along with
    some small <A href="manual/examples.php3">examples</A>.
    Users of Emacs might want to use the following extended SML mode:
  </P>

  <UL>
    <LI><A href="download/sml-mode.tgz"><TT>sml-mode.tgz</TT></A></LI>
  </UL>


<?php section("local", "local installation") ?>

  <P>
    A development version of Alice is installed in the
    <A href="/">Programming System Lab</A> and can be readily used on the
    local systems.
  </P>


<?php footing() ?>
