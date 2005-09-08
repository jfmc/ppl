# Autoconf source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PPL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

# Process this file with autoconf to produce a configure script.

# Every other copy of the package version number gets its value from here.
AC_INIT([the Parma Polyhedra Library],[0.8pre5],[ppl-devel@cs.unipr.it],[ppl])

# Minimum autoconf version required.
AC_PREREQ(2.59)

# Make sure the sources are there.
AC_CONFIG_SRCDIR(src/Polyhedron_public.cc)

# Use Automake.
AM_INIT_AUTOMAKE([foreign dist-bzip2 dist-zip 1.7.3])

AC_SUBST(VERSION)

# Version number machinery.
changequote(<<, >>)dnl
if test -n "`expr $VERSION : '\([0-9]*\)\.[0-9]*\.[0-9]*'`"
then
  PPL_VERSION_MAJOR=`expr $VERSION : '\([0-9]*\)\.[0-9]*\.[0-9]*'`
  PPL_VERSION_MINOR=`expr $VERSION : '[0-9]*\.\([0-9]*\)\.[0-9]*'`
  PPL_VERSION_REVISION=`expr $VERSION : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
  PPL_VERSION_BETA=`expr $VERSION : '[0-9]*\.[0-9]*\.[0-9]*pre\([0-9]*\)'`
else
  PPL_VERSION_MAJOR=`expr $VERSION : '\([0-9]*\)\.[0-9]*'`
  PPL_VERSION_MINOR=`expr $VERSION : '[0-9]*\.\([0-9]*\)'`
  PPL_VERSION_REVISION=0
  PPL_VERSION_BETA=`expr $VERSION : '[0-9]*\.[0-9]*pre\([0-9]*\)'`
fi
if test -z "$PPL_VERSION_BETA"
then
  PPL_VERSION_BETA=0
fi
changequote([, ])dnl
AC_SUBST(PPL_VERSION_MAJOR)
AC_SUBST(PPL_VERSION_MINOR)
AC_SUBST(PPL_VERSION_REVISION)
AC_SUBST(PPL_VERSION_BETA)

AC_DEFINE_UNQUOTED(PPL_CONFIGURE_OPTIONS, "$ac_configure_args", [This contains the options with which `configure' was invoked.])
PPL_CONFIGURE_OPTIONS="\"$ac_configure_args\""
AC_SUBST(PPL_CONFIGURE_OPTIONS)

# Installation directories.
AC_SUBST([docdir], ['${datadir}/doc/ppl'])

# Generate a configuration header file.
AC_CONFIG_HEADER(config.h)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

# Check for host type.
AC_CANONICAL_HOST

AH_TOP([
#ifndef PPL_ppl_config_h
#define PPL_ppl_config_h 1
])

AH_BOTTOM([
/*
  In order for the definition of `int64_t' to be seen by Comeau C/C++,
  we must make sure <stdint.h> is included before <sys/types.hh> is
  (even indirectly) included.
*/
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#endif /* !defined(PPL_ppl_config_h) */
])

# C compiler

AC_ARG_WITH(cc,
[  --with-cc=xxx           use XXX as the C compiler],
CC=$with_cc)

# C++ compiler

AC_ARG_WITH(cxx,
[  --with-cxx=xxx          use XXX as the C++ compiler],
CXX=$with_cxx)

# Compilation flags

COMP_FLAGS=""
OPT_FLAGS=""

enableval=yes
AC_MSG_CHECKING([whether to compile with debug info])
AC_ARG_ENABLE(debugging,
[  --enable-debugging      compile with debugging information])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  OPT_FLAGS="-g"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-debugging, needs yes or no])
  ;;
esac

enableval=no
AC_MSG_CHECKING([whether to compile for profiling])
AC_ARG_ENABLE(profiling,
[  --enable-profiling      compile for profiling])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  OPT_FLAGS="-g"
  COMP_FLAGS="$COMP_FLAGS -pg -DPROFILING=1"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-profiling, needs yes or no])
  ;;
esac

enableval=no
AC_MSG_CHECKING([whether to compile for test coverage])
AC_ARG_ENABLE(coverage,
[  --enable-coverage       compile for test coverage])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  OPT_FLAGS="-g"
  COMP_FLAGS="$COMP_FLAGS -fprofile-arcs -ftest-coverage"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-coverage, needs yes or no])
  ;;
esac

enableval=no
AC_MSG_CHECKING([whether to enable checking of run-time assertions])
AC_ARG_ENABLE(assertions,
[  --enable-assertions     check run-time assertions])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-assertions, needs yes or no])
  ;;
esac
enable_assertions=${enableval}

enableval=no
AC_MSG_CHECKING([whether to enable even more run-time assertions])
AC_ARG_ENABLE(more-assertions,
[  --enable-more-assertions check even more run-time assertions])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-more-assertions, needs yes or no])
  ;;
esac
enable_more_assertions=${enableval}

if test x"$enable_more_assertions" = xyes
then
  enable_assertions=yes
  AC_DEFINE(EXTRA_ROW_DEBUG, 1, [Enable more assertions when defined.])
  debug_flag="-DEXTRA_ROW_DEBUG=1"
fi

if test x"$enable_assertions" = xno
then
  AC_DEFINE(NDEBUG, 1, [Assertions are disabled when this is defined.])
  debug_flag="-DNDEBUG=1"
fi

AM_CONDITIONAL(ASSERTIONS_ENABLED, test x"$enable_assertions" = xyes)


arch=no
enableval=standard
AC_MSG_CHECKING([whether to enable optimizations])
AC_ARG_ENABLE(optimization,
[  --enable-optimization   enable compiler optimizations])
case "${enableval}" in
sspeed)
  AC_MSG_RESULT(sspeed)
  OPT_FLAGS="$OPT_FLAGS -O3 -fomit-frame-pointer"
  arch=yes
  ;;
speed)
  AC_MSG_RESULT(speed)
  OPT_FLAGS="$OPT_FLAGS -O3"
  arch=yes
  ;;
size)
  AC_MSG_RESULT(size)
  OPT_FLAGS="$OPT_FLAGS -Os"
  arch=yes
  ;;
standard | yes)
  AC_MSG_RESULT(standard)
  OPT_FLAGS="$OPT_FLAGS -O2"
  ;;
mild)
  AC_MSG_RESULT(mild)
  OPT_FLAGS="$OPT_FLAGS -O1"
  ;;
zero)
  AC_MSG_RESULT(zero)
  OPT_FLAGS="$OPT_FLAGS -O0"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-optimization, needs sspeed, speed, size, standard, mild, zero, yes or no])
  ;;
esac

enableval=$arch
AC_MSG_CHECKING([for which architecture to optimize])
AC_ARG_ENABLE(arch,
[  --enable-arch[=arch]      optimize for architecture arch])
case "${enableval}" in
yes)
  m=`uname -m`
  case $m in
  i?86 | k6 | athlon)
    AC_MSG_RESULT($m)
    OPT_FLAGS="$OPT_FLAGS -march=$m"
    ;;
  *)
    AC_MSG_RESULT(default)
    ;;
  esac
  ;;
no)
  AC_MSG_RESULT(default)
  ;;
*)
  AC_MSG_RESULT($enableval)
  OPT_FLAGS="$OPT_FLAGS -march=$enableval"
  ;;
esac

CFLAGS="$COMP_FLAGS $OPT_FLAGS"
CXXFLAGS="$COMP_FLAGS $OPT_FLAGS"

enableval=mpz
AC_MSG_CHECKING([the type of integral values to use as coefficients])
AC_ARG_ENABLE(coefficients,
[  --enable-coefficients   select the type of the coefficients])
case "${enableval}" in
native-int8)
  AC_MSG_RESULT([native 8 bits integers])
  coefficient_kind=native
  coefficient_type=int8_t
  coefficient_mnemonic=nint8
  ;;
native-int16)
  AC_MSG_RESULT([native 16 bits integers])
  coefficient_kind=native
  coefficient_type=int16_t
  coefficient_mnemonic=nint16
  ;;
native-int32)
  AC_MSG_RESULT([native 32 bits integers])
  coefficient_kind=native
  coefficient_type=int32_t
  coefficient_mnemonic=nint32
  ;;
native-int64)
  AC_MSG_RESULT([native 64 bits integers])
  coefficient_kind=native
  coefficient_type="int64_t"
  coefficient_mnemonic=nint64
  ;;
checked-int8)
  AC_MSG_RESULT([checked 8 bits integers])
  coefficient_kind=checked
  coefficient_type=int8_t
  coefficient_mnemonic=int8
  ;;
checked-int16)
  AC_MSG_RESULT([checked 16 bits integers])
  coefficient_kind=checked
  coefficient_type=int16_t
  coefficient_mnemonic=int16
  ;;
checked-int32)
  AC_MSG_RESULT([checked 32 bits integers])
  coefficient_kind=checked
  coefficient_type=int32_t
  coefficient_mnemonic=int32
  ;;
checked-int64)
  AC_MSG_RESULT([checked 64 bits integers])
  coefficient_kind=checked
  coefficient_type="int64_t"
  coefficient_mnemonic=int64
  ;;
mpz)
  AC_MSG_RESULT([GMP mpz])
  coefficient_kind=unbounded
  coefficient_type=mpz_class
  coefficient_mnemonic=mpz
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-coefficients, checked-int32, checked-int64, checked-int16, checked-int8, mpz, native-int32, native-int64, native-int16 or native-int8])
  ;;
esac

AM_CONDITIONAL(USE_NATIVE_INTEGERS, test x"$coefficient_kind" = xnative)
AM_CONDITIONAL(USE_CHECKED_INTEGERS, test x"$coefficient_kind" = xchecked)
AM_CONDITIONAL(USE_INT8, test x"$coefficient_type" = xint8_t)
AM_CONDITIONAL(USE_INT16, test x"$coefficient_type" = xint16_t)
AM_CONDITIONAL(USE_INT32, test x"$coefficient_type" = xint32_t)
AM_CONDITIONAL(USE_INT64, test x"$coefficient_type" = xint64_t)
AM_CONDITIONAL(USE_GMP_INTEGERS, test x"$coefficient_type" = xmpz_class)


if test x"$coefficient_kind" = xnative
then
  AC_DEFINE_UNQUOTED(COEFFICIENT_TYPE, Parma_Polyhedra_Library::Native_Integer<$coefficient_type>, [The integral type used to represent coefficients.])
  AC_DEFINE(NATIVE_INTEGERS, 1, [Defined if the integral type to be used for coefficients is a native one.])
elif test x"$coefficient_kind" = xchecked
then
  AC_DEFINE_UNQUOTED(COEFFICIENT_TYPE, Parma_Polyhedra_Library::Checked_Number<$coefficient_type>)
  AC_DEFINE(CHECKED_INTEGERS, 1, [Defined if the integral type to be used for coefficients is a checked one.])
elif test x"$coefficient_type" = xmpz_class
then
  AC_DEFINE(COEFFICIENT_TYPE, mpz_class)
  AC_DEFINE(GMP_INTEGERS, 1, [Defined if the integral type to be used for coefficients is GMP's one.])
fi

# Allow additions to C and C++ compilation flags

AC_ARG_WITH(cflags,
[  --with-cflags=xxx       add XXX to the options for the C compiler],
CFLAGS="$CFLAGS $with_cflags")

AC_ARG_WITH(cxxflags,
[  --with-cxxflags=xxx     add XXX to the options for the C++ compiler],
CXXFLAGS="$CXXFLAGS $with_cxxflags")

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Checks for C typedefs, structures, compiler and architecture characteristics.
AC_LANG(C)
AC_C_CONST
AC_C_INLINE
AC_C_BIGENDIAN

# Checks for C type sizes.
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(unsigned)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(unsigned long long)
AC_CHECK_SIZEOF(size_t)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)

# Use C++ for the remaining checks.
AC_LANG(C++)

# Check whether the C++ compiler supports flexible arrays.
AC_CXX_SUPPORTS_FLEXIBLE_ARRAYS

# Check whether the C++ compiler supports __attribute__ ((weak)).
AC_CXX_SUPPORTS_ATTRIBUTE_WEAK

# Check whether the C++ compiler provides long double numbers that
# have bigger range or precision than double.
AC_CXX_SUPPORTS_LONG_DOUBLE

# Check whether C++ floats comply with IEC 559.
AC_CXX_FLOAT_IS_IEC_559

# Check whether C++ doubles comply with IEC 559.
AC_CXX_DOUBLE_IS_IEC_559

# Check whether C++ long doubles comply with IEC 559.
AC_CXX_LONG_DOUBLE_IS_IEC_559

# Check whether the C++ compiler supports long long integers.
AC_CXX_SUPPORTS_LONG_LONG

# Checks for the availability of C library functions in C++.
AC_CHECK_DECLS([ffs], , , [#include <strings.h>])
AC_CHECK_DECLS([strtof,strtod,strtold,strtoll,strtoull], , , [#include <cstdlib>])
AC_CHECK_DECLS([fma,fmaf,fmal,rintl], , , [#include <cmath>])

# Must link everything with libm.
extra_libraries="${extra_libraries} -lm"

# GMP library

AC_CHECK_GMP

if test x"$have_gmp" = xno
then
  AC_MSG_ERROR([Cannot find GMP version 4.1.3 or higher.
GMP is the GNU Multi-Precision library:
see http://www.swox.com/gmp/ for more information.
When compiling the GMP library, do not forget to enable the C++ interface:
add --enable-cxx to the configuration options.])
else
  if test x"$have_gmpxx" = xno
  then
    AC_MSG_ERROR([GMP compiled without enabling the C++ interface.
GMP is the GNU Multi-Precision library:
see http://www.swox.com/gmp/ for more information.
When compiling the GMP library, do not forget to enable the C++ interface:
add --enable-cxx to the configuration options.])
  fi
fi

extra_includes="${extra_includes}${extra_includes:+ }${gmp_includes_option}"
extra_libraries="${extra_libraries}${extra_libraries:+ }${gmp_library_option}"

enableval=yes
AC_MSG_CHECKING([whether to build the Parma Watchdog Library])
AC_ARG_ENABLE(watchdog,
[  --enable-watchdog       build also the Parma Watchdog Library])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} for --enable-watchdog, needs yes or no])
  ;;
esac
build_watchdog_library=$enableval
AM_CONDITIONAL(BUILD_WATCHDOG_LIBRARY, test x"$build_watchdog_library" = xyes)

# Enabled or not, the Watchdog subdirectory must be configured.
AC_CONFIG_SUBDIRS(Watchdog)

# This is the list of available interfaces: lowercase and blank-separated.
available_interfaces="cxx c ciao_prolog gnu_prolog sicstus_prolog swi_prolog xsb_prolog yap_prolog"

# This is the list of interfaces that are enabled by default.
#default_interfaces="cxx c swi_prolog"
default_interfaces=${available_interfaces}

enableval=not_specified
AC_MSG_CHECKING([which interfaces are enabled])
AC_ARG_ENABLE(interfaces,
[  --enable-interfaces   enable some or all the library interfaces])
case x"${enableval}" in
xyes | x)
  AC_MSG_ERROR([--enable-interfaces needs at least one argument chosen among ${enableval}, none and all])
  ;;
xnone | xno)
  for interface in ${available_interfaces}
  do
    eval ${interface}_interface_enabled=no
  done
  AC_MSG_RESULT(none)
  ;;
xall)
  for interface in ${available_interfaces}
  do
    eval ${interface}_interface_enabled=yes
  done
  AC_MSG_RESULT([${available_interfaces}])
  ;;
*)
  if test x"${enableval}" = xnot_specified
  then
    enableval=${default_interfaces}
  fi
  # Make the list blank-separated and lowercase; turn "c++" into "cxx".
  required_interfaces=`echo "${enableval}" | sed -e 's/-/_/g' -e 's/[[ 	,]][[ 	,]]*/ /g' -e 's/c++/cxx/g' | tr '[[:upper:]]' '[[:lower:]]'`
  # Check that the specified interfaces exist.
  for interface in ${required_interfaces}
  do
    case " ${available_interfaces} " in
    *\ ${interface}\ *)
      ;;
    *)
      AC_MSG_ERROR([unknown interface ${interface}: must be one of ${available_interfaces}])
      ;;
    esac
  done
  # Initialize the enabled variable for each interface.
  for interface in ${available_interfaces}
  do
    case " ${required_interfaces} " in
    *\ ${interface}\ *)
      eval ${interface}_interface_enabled=yes
      enabled_interfaces="${enabled_interfaces}${enabled_interfaces:+ }${interface}"
      ;;
    *)
      eval ${interface}_interface_enabled=no
      ;;
    esac
  done
  AC_MSG_RESULT([${enabled_interfaces}])
  ;;
esac

# Checks for Prolog systems for which the interface is enabled.

# Ciao Prolog
if test x${ciao_prolog_interface_enabled} = xyes
then
  AC_CHECK_PROG(CIAO_PROLOG, ciao, ciao)
  if test x"$CIAO_PROLOG" = xciao
  then
  AC_CHECK_HEADER(ciao_prolog.h,
                  [],
                  CIAO_PROLOG="")
  fi
fi
AM_CONDITIONAL(BUILD_CIAO_PROLOG_INTERFACE, test x$CIAO_PROLOG = xciao)

# GNU Prolog
if test x${gnu_prolog_interface_enabled} = xyes
then
  AC_CHECK_PROG(GNU_PROLOG, gprolog, gprolog)
  if test x"$GNU_PROLOG" = xgprolog
  then
    AC_CHECK_HEADER(gprolog.h,
                    [],
                    GNU_PROLOG="")
  fi
fi
AM_CONDITIONAL(BUILD_GNU_PROLOG_INTERFACE, test x$GNU_PROLOG = xgprolog)

# SICStus
if test x${sicstus_prolog_interface_enabled} = xyes
then
  AC_CHECK_PROG(SICSTUS_PROLOG, sicstus, sicstus)
  if test x"$SICSTUS_PROLOG" = xsicstus
  then
    # We require SICStus Prolog 3.9.1 or later.
    AC_CHECK_HEADER(sicstus/sicstus.h,
                    AC_MSG_CHECKING([for SICStus version 3.9.1 or later])
                    AC_EGREP_CPP(yes,
                      [
                        #include <sicstus/sicstus.h>
                        #if SICSTUS_MAJOR_VERSION > 3 || (SICSTUS_MAJOR_VERSION == 3 && SICSTUS_MINOR_VERSION > 9) || (SICSTUS_MAJOR_VERSION == 3 && SICSTUS_MINOR_VERSION == 9 && SICSTUS_REVISION_VERSION >= 1)
                        yes
                        #endif
                      ],
                    AC_MSG_RESULT(yes),
                    SICSTUS_PROLOG=""
                    AC_MSG_RESULT(no)
                  ),
                  SICSTUS_PROLOG="")
  fi
fi
AM_CONDITIONAL(BUILD_SICSTUS_PROLOG_INTERFACE, test x$SICSTUS_PROLOG = xsicstus)

# SWI Prolog
if test x${swi_prolog_interface_enabled} = xyes
then
  AC_CHECK_SWI_PROLOG
fi
AM_CONDITIONAL(BUILD_SWI_PROLOG_INTERFACE, test x$have_swi_prolog = xyes)

# XSB
if test x${xsb_prolog_interface_enabled} = xyes
then
  AC_CHECK_PROG(XSB_PROLOG, xsb, xsb)
  # The foreign language interface include file is `cinterf.h',
  # but this resides in the emulator directory and is not installed
  # in any standard place.
  if test x"$XSB_PROLOG" = xxsb
  then
    xsb_emu_dir=`xsb --nobanner --quietload --noprompt \
                  -e "write('emudir='), \
                    xsb_configuration(emudir, X), write(X), nl, halt." \
                      2>/dev/null | sed "s/^emudir=//g"`
    xsb_config_dir=`xsb --nobanner --quietload --noprompt \
                     -e "write('config_dir='), \
                       xsb_configuration(config_dir,X), write(X), nl, halt." \
                         2>/dev/null | sed "s/^config_dir=//g"`
    xsb_includes="-I${xsb_emu_dir} -I${xsb_config_dir}"
  fi
fi
AM_CONDITIONAL(BUILD_XSB_PROLOG_INTERFACE, test x$XSB_PROLOG = xxsb)

# YAP
if test x${yap_prolog_interface_enabled} = xyes
then
  AC_CHECK_PROG(YAP_PROLOG, yap, yap)
  if test x"$YAP_PROLOG" = xyap
  then
    AC_CHECK_HEADER(Yap/c_interface.h,
                    [],
                    YAP_PROLOG="")
  fi
fi
AM_CONDITIONAL(BUILD_YAP_PROLOG_INTERFACE, test x$YAP_PROLOG = xyap)

# Libtool.
# Turn off shared libraries during beta-testing, since they
# make debugging harder and the build process takes too long.
#AC_DISABLE_SHARED
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

# Checks for programs.
AC_CHECK_PROG(MD5SUM, md5sum, md5sum)
AM_CONDITIONAL(HAVE_MD5SUM, test x$MD5SUM = xmd5sum)

# Check for the GLPK library
AC_LANG_PUSH(C)
AC_CHECK_HEADER(glpk.h, HAVE_GLPK_H=yes)
AM_CONDITIONAL(HAVE_GLPK, test x$HAVE_GLPK_H = xyes)
AC_LANG_POP(C)

# Checks for header files.
AC_CHECK_HEADERS([getopt.h signal.h sys/time.h sys/resource.h sys/time.h sys/types.h unistd.h])

# Checks for header declarations.
AC_CHECK_DECLS([RLIMIT_DATA, RLIMIT_RSS, RLIMIT_VMEM, RLIMIT_AS],
               ,
               ,
               [
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
])

# Checks for typedefs, structures, and compiler characteristics.
# Nothing for the time being.

# Checks for library functions.
# Nothing for the time being.

# If we are using GCC we want to compile with warnings enabled.
if test x"$GCC" = xyes
then
  CFLAGS="-W -Wall $CFLAGS"
fi
if test x"$GXX" = xyes
then
  CXXFLAGS="-W -Wall $CXXFLAGS"
fi

# System-dependent adjustments.
text_md5sum="$MD5SUM -t"
no_undefined=no

case "${host_os}" in
darwin*)
  if test x"$GCC" = xyes
  then
    CFLAGS="$CFLAGS -Wno-long-double"
    CXXFLAGS="$CXXFLAGS -Wno-long-double"
  fi
  text_md5sum="$MD5SUM"
  ;;
cygwin)
  no_undefined=yes
  ;;
*)
  ;;
esac
AM_CONDITIONAL(NO_UNDEFINED, test x$no_undefined = xyes)

AC_SUBST(extra_includes)
AC_SUBST(extra_libraries)
AC_SUBST(debug_flag)
AC_SUBST(xsb_includes)
AC_SUBST(coefficient_mnemonic)
AC_SUBST(text_md5sum)

AC_CONFIG_FILES(Makefile
                ppl.lsm ppl.spec
		debian/libppl.copyright
		debian/libppl-pwl.copyright
                src/Makefile
                src/version.hh
                src/ppl-config.cc
                tests/Makefile
                tests/BD_Shape/Makefile
                tests/Polyhedron/Makefile
                utils/Makefile
                m4/Makefile
                demos/Makefile
                demos/ppl_lcdd/Makefile
                demos/ppl_lcdd/examples/Makefile
                demos/ppl_lpsol/Makefile
                demos/ppl_lpsol/examples/Makefile
                doc/Makefile
                doc/user-browse.doxyconf-latex
                doc/devref-browse.doxyconf-latex
                doc/user-print.doxyconf-latex
                doc/devref-print.doxyconf-latex
                doc/user.doxyconf-html
                doc/devref.doxyconf-html
                interfaces/Makefile
                interfaces/C/Makefile
                interfaces/C/ppl_c.h
                interfaces/Prolog/Makefile
                interfaces/Prolog/Ciao/Makefile
                interfaces/Prolog/GNU/Makefile
                interfaces/Prolog/SICStus/Makefile
                interfaces/Prolog/SWI/Makefile
                interfaces/Prolog/XSB/Makefile
                interfaces/Prolog/YAP/Makefile
                interfaces/Prolog/tests/Makefile)
AC_OUTPUT

if test x"$gmp_supports_exceptions" = xno
then
  AC_MSG_WARN([CANNOT PROPAGATE EXCEPTIONS BACK FROM GMP:
*** MEMORY EXHAUSTION MAY RESULT IN ABRUPT TERMINATION.
*** If you are using g++, make sure you use version 4.1.3 or higher
*** and use a version of GMP compiled with the option -fexceptions.
*** To build such a version, you can configure GMP as follows:
*** CPPFLAGS=-fexceptions ./configure --enable-cxx --prefix=/usr/local])
fi

if test x"$coefficient_kind" = xnative
then
  AC_MSG_WARN([USING NATIVE INTEGERS IS EXTREMELY DANGEROUS:
*** CONSIDER ANY RESULT YOU WILL GET FROM THE LIBRARY AS PSEUDO-RANDOM!!!
*** You have requested to use a native integral type to represent
*** coefficients.  This means that arithmetic overflows will be possible
*** and will not be detected.  The consequence of that is that nothing
*** can be said about the behavior of the library:  whatever the
*** documentation says, it does not apply to the version of the library
*** you are about to build.  Just one arithmetic overflow is enough
*** to invalidate any possible significance of the results.
*** Please, do not assume that your computation will not overflow because
*** only small coefficients are involved.  Do not do it because
*** (1) coefficients may grow very rapidly depending on the polyhedra
***     computations being performed;
*** (2) the library may do internal polyhedra computations that you
***     cannot easily anticipate;
*** (3) the library has never had, among its design goals, the one of
***     trying to keep the coefficients small.
***
*** Please, consider using a _checked_ native integral type instead.])
fi
