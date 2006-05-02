Name: gecode
Version: 1.1.0
Release: 1
License: BSD
Summary: generic constraint development environment
Group: Development/Libraries
Source: gecode-%{version}.tar.gz
Vendor: The Gecode Project
URL: http://www.gecode.org
Packager: Guido Tack <tack@ps.uni-sb.de>

BuildArchitectures: i386
BuildRoot: %{_tmppath}/%{name}-%{version}

%description
Gecode is an open, free, portable, accessible, and efficient environment 
for developing constraint-based systems and applications.

More information on Gecode is available at
the Gecode web site at http://www.gecode.org.

%prep
rm -rf %{buildroot}
%setup -q -D -a 0 -c

%build
cd gecode-%{version}
%configure --prefix=/usr --disable-examples

%install
cd gecode-%{version}
%makeinstall sharedlibdir=%{buildroot}%{_libdir}

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
%{_includedir}/*
%{_libdir}/*

%changelog
* Tue May  2 2006 Christian Mueller <cmueller@ps.uni-sb.de>
- initial Gecode rpm package
