<?php include("macros.php3"); ?>

<?php heading("Constraints", "constraints") ?>



<?php section("overview", "overview") ?>

  <P>
    Alice provides much of the constraint functionality available in
    <A href="http::/www.mozart-oz.org">Mozart</A>, but in a typeful way.
    Constraints can either be programmed directly using the core functional API
    or by just describing them using a convenient operator-based approach.
    The latter allows for easy constraint construction and
    leaves the decision of choosing the best constraint to the system.
  </P>
  <P>
    For example, see the the classic <TT>SEND + MORE = MONEY</TT> problem below.
    The problem is given as a script (nullary function)
    which then can be passed to different search engines.
  </P>

  <PRE>
   fun money () =
       let
	   val V as #[S, E, N, D, M, O, R, Y] = vec(8, [0`#9])
       in
	   distinct V;
	   post (S `<> `0);
	   post (M `<> `0);
	   post (             `1000`*S `+ `100`*E `+ `10`*N `+ D `+
	                      `1000`*M `+ `100`*O `+ `10`*R `+ E `=
                 `10000`*M `+ `1000`*O `+ `100`*N `+ `10`*E `+ Y );
	   distribute(FD.FIRSTFAIL, V);
	   {S, E, N, D, M, O, R, Y}
       end

   (* Interactive visual search *)
   Explorer.exploreOne money;

   (* Show all possible solutions *)
   inspect (Search.searchAll money);
  </PRE>

  <P>
    To learn more about constraint programming in general, we refer to the
    <A href="http://www.mozart-oz.org/documentation/fdt/index.html">Mozart tutorial</A>.
  </P>

<?php section("api", "api") ?>
  
  <P>The constraint programming api is provided by the following components:
  </P>

  <UL>
    <LI>
      <TT><A name="fd">structure</A>
      <A href="http://www.ps.uni-sb.de/alice/manual/library/fd.php3">FD</A>
      <BR>from "x-alice:/lib/constraints/FD"</TT>
    </LI>
    <LI>
      <TT><A name="linear">structure</A>
      <A href="http://www.ps.uni-sb.de/alice/manual/library/linear.php3">Linear</A>
      <BR>from "x-alice:/lib/constaints/Linear"</TT>
    </LI>
    <LI>
      <TT><A name="fs">structure</A>
      <A href="http://www.ps.uni-sb.de/alice/manual/library/fs.php3">FS</A>
      <BR>from "x-alice:/lib/constaints/FS"</TT>
    </LI>
    <LI>
      <TT><A name="space">structure</A>
      <A href="http://www.ps.uni-sb.de/alice/manual/library/space.php3">Space</A>
      <BR>from "x-alice:/lib/constaints/Space"</TT>
    </LI>
    <LI>
      <TT><A name="search">structure</A>
      <A href="http://www.ps.uni-sb.de/alice/manual/library/search.php3">Search</A>
      <BR>from "x-alice:/lib/constaints/Search"</TT>
    </LI>
  </UL>

<?php footing() ?>
