<?php include("macros.php3"); ?>

<?php heading("Constraints", "constraints") ?>



<?php section("overview", "overview") ?>

  <P>
    Alice provides much of the constraint functionality available in
    <A href="http::/www.mozart-oz.org">Mozart</A>, but in a typeful way.
  <P>
  <P>In particular, three components are fairly complete:</P>

  <UL>
    <LI>
      <TT><A name="fd">structure</A>
      <A href="http://www.mozart-oz.org/documentation/system/node14.html#chapter.fd"">FD</A>
      <BR>from "x-alice:/lib/constraints/FD"</TT>
    </LI>
    <LI>
      <TT><A name="fs">structure</A>
      <A href="http://www.mozart-oz.org/documentation/system/node32.html#chapter.fs"">FS</A>
      <BR>from "x-alice:/lib/constaints/FS"</TT>
    </LI>
    <LI>
      <TT><A name="search">structure</A>
      <A href="http://www.mozart-oz.org/documentation/system/node44.html#chapter.space">Space</A>
      <BR>from "x-alice:/lib/constaints/Space"</TT>
    </LI>
  </UL>

  <P>
    Search currently supports only basic search mechanisms, such as single, best and
    all solutions search.
  <P>

  <UL>
    <LI>
      <TT><A name="search">structure</A> Search
      <BR>from "x-alice:/lib/constraints/Search"</TT>
    </LI>
  </UL>

  <PRE>
         structure Search :
         sig
             type 'a pruner = 'a * 'a -> unit

             val searchOne : (unit -> 'a) -> 'a option
             val searchAll : (unit -> 'a) -> 'a list
             val searchBest : (unit -> 'a) * 'a pruner -> 'a option
         end
  </PRE>

  <P>
    Search can also be performed interactively using the
    <A href="explorer.php3">Alice Explorer</A>.
  </P>

  <P>
    To learn more about constraint programming, we refer to the
    <A href="http://www.mozart-oz.org/documentation/fdt/index.html">Mozart tutorial</A>.
  </P>

<?php footing() ?>
