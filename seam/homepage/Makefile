PREFIX=/services/ps/httpd/html/seam

HOME_FILES = index.html \
             contact.html \
             download.html \
             papers.html \
             project.html \
             people.html

MANUAL_FILES = index.html \
               store.html \
               generic.html \
               ffi.html \
               alice.html

INSTALLDIRS= $(PREFIX) $(PREFIX)/manual

INSTALLMAINTARGETS = $(HOME_FILES)
INSTALLMANUALTARGETS = $(MANUAL_FILES:%.html=manual/%.html)

install:
	mkdir -p -m 775 $(INSTALLDIRS)
	install -m 555 $(INSTALLMAINTARGETS) $(PREFIX)
	install -m 555 $(INSTALLMANUALTARGETS) $(PREFIX)/manual
