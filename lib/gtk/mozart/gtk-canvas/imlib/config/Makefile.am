EXTRA_DIST = imrc.in im_palette.pal im_palette-small.pal im_palette-tiny.pal
sysconf_DATA = imrc im_palette.pal im_palette-small.pal im_palette-tiny.pal

imrc: imrc.in
	sed -e 's,\@sysconfdir\@,$(sysconfdir),g' \
		< $(srcdir)/imrc.in > imrc.tmp \
	&& mv imrc.tmp imrc

