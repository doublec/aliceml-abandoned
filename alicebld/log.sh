log_start() {
    title="Alice build performed `date` on `uname -n`"
    cat 1>&4 <<EOF
<HTML><HEAD>
<TITLE>$title</TITLE>
</HEAD><BODY>
<H1>$title</H1>
EOF
}

log_start_section() {
    cat 1>&4 <<EOF
<H2>$*</H2>
<PRE>
EOF
}

log_end_section() {
    cat 1>&4 <<EOF
</PRE>
EOF
}

log_end() {
    cat 1>&4 <<EOF
</BODY>
EOF
}
