Name: alice-runtime
Version: 1.0.1
Release: 1
Copyright: X11
Summary: The Alice Programming System, Runtime Components
Group: Development/Languages
Source: %{name}-%{version}.tar.gz
Vendor: The Alice Project
URL: http://www.ps.uni-sb.de/alice/
Packager: Guido Tack <tack@ps.uni-sb.de>
Requires: alice >= 1.0.1, alice-gtk >= 1.0.1, alice-gecode >= 1.0.1
BuildArchitectures: noarch
BuildRoot: %{_tmppath}/%{name}-%{version}

%description
Alice is a functional programming language based on Standard ML, extended
with support for concurrent, distributed, and constraint programming.

This package provides the Alice Programming System, an implementation of
Alice based on the Mozart virtual machine.  It includes an interpreter-like
interactive toplevel, a batch compiler, a static linker, the Inspector (a
tool for dynamically inspecting Alice data structures), the Explorer (a
tool for interactively investigating search problems), a binding for the
Gnome toolkit GUI library, and a comprehensive library.

More information on Alice and the Alice Programming System is available at
the Alice web site at http://www.ps.uni-sb.de/alice/.

%prep
rm -rf %{buildroot}
%setup -q

%build
%configure

%install
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%doc AUTHORS ChangeLog COPYING NEWS README
%{_datadir}/alice

%changelog
* Wed Apr 14 2004 Guido Tack <tack@ps.uni-sb.de>
- new upstream version

* Tue Oct 15 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- initial RPM release

* Thu Feb 13 2002 Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
- new upstream version
