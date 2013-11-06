$MAX_SPLIT_DEPTH = 5;
$DESTDIR = "html";
$LOCAL_ICONS = 1;
$INFO = "";
$WHITE_BACKGROUND = 1;
$NO_SIMPLE_MATH = 1;
$HTML_VERSION = "4.0,table,math";
$STYLESHEET = "style.css";

sub top_navigation_panel {
    '<DIV class=margin><H1>alice<BR>constraint<BR>tutorial.<BR><BR><BR></H1>' .
    '<P class="menu"><A href="../index.html">Contents</A><BR>'.
    '<A href="../tour.html">Tour</A><BR>'.
    '<A href="../language.html">Language</A><BR>'.
    '<A href="../library/index.html">Library</A><BR>'.
    '<A href="index.html">CP Tutorial</A><BR>'.
    '<A href="../tools.html">Tools</A><BR>'.
    '<A href="../sitemap.html">Index</A><BR>'.
    '</P><A href="http://www.ps.uni-sb.de/alice/">'.
    '<IMG src="logo-small.gif" border=0 alt="Alice Project"></A></DIV>' .
    &navigation_panel
}

1;
