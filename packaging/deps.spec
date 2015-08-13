Name:           @@NAME@@-deps
Version:        @@VERSION@@
Release:        1%{?dist}
Summary:        Summary
License:        None

Group:          Development/Libraries
Source0:        @@NAME@@-deps.tar.gz
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

%description
Build dependencies for @@NAME@@

%prep
%setup -q -n @@NAME@@-deps

%build

%install

mkdir -p $RPM_BUILD_ROOT/usr/share
cd ..
cp -al @@NAME@@-deps $RPM_BUILD_ROOT/usr/share/@@NAME@@-deps

%files
/usr/share/@@NAME@@-deps

%changelog
