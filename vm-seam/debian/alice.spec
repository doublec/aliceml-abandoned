Name: alice
Version: 1.2
Release: 1
Copyright: X11
Summary: The Alice Programming System
Group: Development/Languages
Source0: alice-%{version}.tar.gz
Vendor: The Alice Project
URL: http://www.ps.uni-sb.de/alice/
Packager: Guido Tack <tack@ps.uni-sb.de>
Requires: /bin/bash
Requires: /bin/sh
Requires: libc.so.6
Requires: libc.so.6(GLIBC_2.0)
Requires: libc.so.6(GLIBC_2.1)
Requires: libc.so.6(GLIBC_2.1.3)
Requires: libc.so.6(GLIBC_2.2)
Requires: libc.so.6(GLIBC_2.3)
Requires: libdl.so.2
Requires: libdl.so.2(GLIBC_2.0)
Requires: libdl.so.2(GLIBC_2.1)
Requires: libgcc_s.so.1
Requires: libgcc_s.so.1(GCC_3.0)
Requires: libgmp.so.3
Requires: libm.so.6
Requires: libm.so.6(GLIBC_2.0)
Requires: libstdc++.so.5
Requires: libstdc++.so.5(GLIBCPP_3.2)
Requires: libz.so.1

BuildArchitectures: i386
BuildRoot: %{_tmppath}/%{name}-%{version}
AutoReqProv: no

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

%build
cd alice-%{version}
%configure

%install
cd alice-%{version}
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

%changelog
* Mon Dec 20 2004 Guido Tack <tack@ps.uni-sb.de>
- Initial release with SEAM

* Wed Apr 14 2004 Guido Tack <tack@ps.uni-sb.de>
- new upstream version

* Tue Oct 15 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- initial RPM release

* Thu Feb 13 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- new upstream version
