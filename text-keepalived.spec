%global pkg_name text-keepalived

%bcond_without doc
%bcond_without prof

# ghc does not emit debug information
%global debug_package %{nil}

Name:           ghc-%{pkg_name}
Version:        0.0.2
Release:        3.%{?dist}
Summary:        Text.Keepalived: A library for keepalived.conf
Group:          Development/Libraries
License:        BSD
URL:            http://github.com/maoe/haskell-keepalived
#Source0:        http://hackage.haskell.org/packages/archive/%{pkg_name}/%{version}/%{pkg_name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc, ghc-rpm-macros
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif

%description
This package provides the Haskell %{pkg_name} library and a command line tool (kc) for ghc.


%package devel
Summary:        Haskell %{pkg_name} library
Group:          Development/Libraries
Provides:       ghc-%{pkg_name} = %{version}-%{release}
Requires:       ghc = %{ghc_version}
Requires(post): ghc = %{ghc_version}
Requires(preun): ghc = %{ghc_version}

%description devel
This package contains the development files for %{name}
built for ghc-%{ghc_version}.


%if %{with doc}
%package doc
Summary:        Documentation for %{name}
Group:          Development/Libraries
Requires:       ghc-doc = %{ghc_version}
Requires(post): ghc-doc = %{ghc_version}
Requires(postun): ghc-doc = %{ghc_version}

%description doc
This package contains development documentation files for the %{name} library.
%endif


%if %{with prof}
%package prof
Summary:        Profiling libraries for %{name}
Group:          Development/Libraries
Requires:       %{name}-devel = %{version}-%{release}
Requires:       ghc-prof = %{ghc_version}

%description prof
This package contains profiling libraries for %{name}
built for ghc-%{ghc_version}.
%endif


%prep
if [ -d %{pkg_name} ]; then
  cd %{pkg_name}
  git pull
else
  git clone git://github.com/maoe/%{pkg_name}.git
fi

%build
cd %{pkg_name}
%cabal clean
%cabal_configure --ghc %{?with_prof:-p} -O2 -fvia-C -foptc-O2
%cabal build
%if %{with doc}
%cabal haddock
%endif
%ghc_gen_scripts

%install
rm -rf $RPM_BUILD_ROOT
cd %{pkg_name}
%cabal_install
%ghc_install_scripts
%ghc_gen_filelists %{name}


%clean
rm -rf $RPM_BUILD_ROOT


%post devel
%ghc_register_pkg


%if %{with doc}
%post doc
%ghc_reindex_haddock
%endif


%preun devel
if [ "$1" -eq 0 ] ; then
  %ghc_unregister_pkg
fi


%if %{with doc}
%postun doc
if [ "$1" -eq 0 ] ; then
  %ghc_reindex_haddock
fi
%endif

%files devel -f %{pkg_name}/%{name}-devel.files
%defattr(-,root,root,-)
%{_docdir}/%{name}-%{version}

%if %{with doc}
%files doc -f %{pkg_name}/%{name}-doc.files
%defattr(-,root,root,-)
%endif

%if %{with prof}
%files prof -f %{pkg_name}/%{name}-prof.files
%defattr(-,root,root,-)
%endif

%changelog
* Thu Nov 19 2009 Mitsutoshi Aoe <maoe.maoe@gmail.com> - 0.0.2-2
- split prof package
* Thu Nov 19 2009 Mitsutoshi Aoe <maoe.maoe@gmail.com> - 0.0.2-1
- update to 0.0.2
