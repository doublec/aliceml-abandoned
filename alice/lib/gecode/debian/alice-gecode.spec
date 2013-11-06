Name: alice-gecode
Version: 1.3
Release: 1
License: X11
Summary: The Alice Programming System, Gecode Binding
Group: Development/Languages
Source0: alice-gecode-%{version}.tar.gz
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

%build
cd alice-gecode-%{version}
make compiledll

%install
cd alice-gecode-%{version}
make installdll INSTALLDIR=%{buildroot}/usr/share/alice/lib/gecode

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%{_datadir}/alice/lib/gecode/*

%changelog
* Mon Dec 20 2004 Guido Tack <tack@ps.uni-sb.de>
- Initial release with SEAM

* Wed Apr 14 2004 Guido Tack <tack@ps.uni-sb.de>
- new upstream version

* Tue Oct 15 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- initial RPM release

* Thu Feb 13 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- new upstream version
