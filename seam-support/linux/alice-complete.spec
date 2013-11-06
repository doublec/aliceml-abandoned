Name: alice-complete
Version: 1.4
Release: 1
License: X11
Summary: The Alice Programming System
Group: Development/Languages
Source0: seam-%{version}.tar.gz
Source1: alice-%{version}.tar.gz
Source2: alice-gtk-%{version}.tar.gz
Source3: alice-gecode-%{version}.tar.gz
Source4: alice-sqlite-%{version}.tar.gz
Source5: alice-xml-%{version}.tar.gz
Source6: alice-regex-%{version}.tar.gz
Source7: alice-runtime-%{version}.tar.gz
Vendor: The Alice Project
URL: http://www.ps.uni-sb.de/alice/
Packager: Guido Tack <tack@ps.uni-sb.de>

BuildArchitectures: i386
BuildRoot: %{_tmppath}/%{name}-%{version}

%description
Alice is a functional programming language based on Standard ML, extended
with support for concurrent, distributed, and constraint programming.

This package provides the Alice Programming System, an implementation of
Alice. It includes an interpreter-like
interactive toplevel, a batch compiler, a static linker, the Inspector (a
tool for dynamically inspecting Alice data structures), the Explorer (a
tool for interactively investigating search problems), a binding for the
Gtk+2 GUI library, and a comprehensive library.

More information on Alice and the Alice Programming System is available at
the Alice web site at http://www.ps.uni-sb.de/alice/.

%prep
rm -rf %{buildroot}
%setup -q -D -a 0 -c
%setup -q -D -a 1 -c
%setup -q -D -a 2 -c
%setup -q -D -a 3 -c
%setup -q -D -a 4 -c
%setup -q -D -a 5 -c
%setup -q -D -a 6 -c
%setup -q -D -a 7 -c

%build
cd seam-%{version}
%configure
cd ../alice-%{version}
%configure
cd ../alice-runtime-%{version}
%configure

%install
cd seam-%{version}
%makeinstall
cd ../alice-%{version}
%makeinstall
cd ../alice-gtk-%{version}
make compiledll MUST_GENERATE=no
make installdll MUST_GENERATE=no INSTALLDIR=%{buildroot}/usr/share/alice/lib/gtk
cd ../alice-gecode-%{version}
make compiledll
make installdll INSTALLDIR=%{buildroot}/usr/share/alice/lib/gecode
cd ../alice-sqlite-%{version}
make compiledll
make installdll INSTALLDIR=%{buildroot}/usr/share/alice/lib/sqlite
cd ../alice-xml-%{version}
make compiledll
make installdll INSTALLDIR=%{buildroot}/usr/share/alice/lib/xml
cd ../alice-regex-%{version}
make compiledll
make installdll INSTALLDIR=%{buildroot}/usr/share/alice/lib/regex
cd ../alice-runtime-%{version}
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%doc alice-%{version}/AUTHORS alice-%{version}/ChangeLog
%doc alice-%{version}/COPYING alice-%{version}/NEWS alice-%{version}/README
%docdir %{_datadir}/%{name}/doc
%{_bindir}/*
%{_includedir}/*
%{_libdir}/*
%{_datadir}/alice
%{_datadir}/aclocal/seam.m4
%{_datadir}/applications/*
%{_datadir}/pixmaps/*
%{_datadir}/man/man1/*

%changelog
* Mon Dec 20 2004 Guido Tack <tack@ps.uni-sb.de>
- Initial release with SEAM

* Wed Apr 14 2004 Guido Tack <tack@ps.uni-sb.de>
- new upstream version

* Tue Oct 15 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- initial RPM release

* Thu Feb 13 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- new upstream version
