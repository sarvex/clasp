Name:    clasp
Version: 1.0.0
Release: 1%{?dist}
Summary: Common Lisp implementation that brings Common Lisp and C++ Together

License: GPLv3+
URL:     https://github.com/clasp-developers/clasp
Source0: https://github.com/clasp-developers/clasp/archive/refs/heads/yitzchak_rpm.tar.gz

BuildRequires: boost-devel clang elfutils-libelf-devel fmt-devel
BuildRequires: gmp-devel libbsd-devel libffi-devel libunwind-devel llvm-devel
BuildRequires: ncurses-devel ninja-build sbcl zlib-devel


%description
Common Lisp implementation that brings Common Lisp and C++ Together.
Clasp is a new Common Lisp implementation that seamlessly interoperates
with C++ libraries and programs using LLVM for compilation to native
code. This allows Clasp to take advantage of a vast array of preexisting
libraries and programs, such as out of the scientific computing
ecosystem. Embedding them in a Common Lisp environment allows you to
make use of rapid prototyping, incremental development, and other
capabilities that make it a powerful language.

%prep
%setup -n clasp-yitzchak_rpm

%build
./koga --package-path=%{buildroot}/ --skip-sync=ansi-test,mps --bin-path=/usr/bin/ --share-path=/usr/share/clasp/ --lib-path=/usr/lib/clasp/
ninja -C build

%install
ninja -C build install

%files
/usr/bin/clasp
/usr/bin/iclasp-boehmprecise
%dir /usr/share/clasp/

%changelog
* Thu June 2 2022 Tarn W. Burton <twburton@gmail.com>
-
