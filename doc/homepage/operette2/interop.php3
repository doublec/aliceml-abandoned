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

<?php section("ozfromalice", "oz from alice") ?>

<?php section("alicefromoz", "alice from oz") ?>

<?php footing() ?>
