<?php
  include("/services/ps/httpd/html/pslab.php");

  function alice_project_header($title) {
    global $baseurl;
    $baseurl = "";
    pslab_bibheader($title);
    $baseurl = "/alice";
?>
    <IMG align=right src="alice.gif">

    <P class=margin>
      <?php pslab_uni(); ?>
      <?php pslab_menu(); ?>
      <BR><BR>
      <A href="http://www.coli.uni-sb.de/sfb378/"
	><IMG src="/images/sfb378.gif" border=0 vspace=8></A>
    </P>
<?php
  }

  $suburls =
    array(array("text" => "Home",
		"url"  => "index.php3"),
	  array("text" => "People",
		"url"  => "people.php3"),
	  array("text" => "Papers",
		"url"  => "papers.php3"),
	  array("text" => "",
		"url"  => ""),
	  array("text" => "Manual",
		"url"  => "manual/"),
	  array("text" => "Download",
		"url"  => "download.php3"),
	  array("text" => "Bugs",
		"url"  => "bugzilla/"));
?>
