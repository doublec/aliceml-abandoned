Name: seam
Version: 1.3
Release: 1
License: X11
Summary: The Simple Extensible Abstract Machine
Group: Development/Languages
Source: seam-%{version}.tar.gz
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
cd seam-%{version}
%configure

%install
cd seam-%{version}
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%docdir %{_datadir}/%{name}/doc
%{_bindir}/*
%{_includedir}/*
%{_libdir}/*
%{_datadir}/aclocal/seam.m4

%changelog
* Mon Dec 20 2004 Guido Tack <tack@ps.uni-sb.de>
- Initial release with SEAM
