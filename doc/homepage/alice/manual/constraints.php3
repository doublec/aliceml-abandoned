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
   fun Money () =
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
   Explorer.exploreOne Money;

   (* Show all possible solutions *)
   inspect (Search.searchAll Money);
  </PRE>

<?php section("posting constraints", "posting constraints") ?>
  <P>
    Easy constraint creation is provided by the component shown below.
  </P>
  <UL>
    <LI>
      <TT><A name="post">structure</A> Constr
      <BR>from "x-alice:/lib/constraints/Constr"</TT>
    </LI>
  </UL>

  <PRE>
         structure Constr :
	 sig
	     infix  7  `*
	     infix  6  `+ `-
             infix  5  `#
	     infix  4  `= `<> `> `>= `< `<= `<->

	     datatype domain_element =
		 `` of int
	       | `# of int * int

	     type domain = domain_element list

	     datatype term =
		 fd of FD.fd
	       | `  of int
	       | `+ of term * term
	       | `- of term * term
	       | `* of term * term

	     datatype rel =
		 `<   of term * term
	       | `<=  of term * term
	       | `=   of term * term
	       | `<>  of term * term
	       | `>=  of term * term
	       | `>   of term * term
	       | `<-> of rel * term

	     val var : domain option -> term
	     val bin : unit -> term
	     val vec : int * domain -> term vector

	     val distribute : FD.dist_mode * term vector -> unit
	     val distinct : term vector -> unit
	     val post : rel -> unit
	 end
  </PRE>
  <P>
    To start posting constraints, it is necessary to open the structure <TT>Constr</TT>.
  </P>

<?php section("lowlevel api", "lowlevel api") ?>
  
  <P>The lowlevel constraint programming api is provided by the
     following three components:
  </P>

  <UL>
    <LI>
      <TT><A name="fd">structure</A>
      <A href="http://www.mozart-oz.org/documentation/system/node14.html#chapter.fd">FD</A>
      <BR>from "x-alice:/lib/constraints/FD"</TT>
    </LI>
    <LI>
      <TT><A name="fs">structure</A>
      <A href="http://www.mozart-oz.org/documentation/system/node32.html#chapter.fs">FS</A>
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
  </P>

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

