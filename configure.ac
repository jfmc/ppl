# autoconf source file for the PPL.
# Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)
dnl Minimum autoconf version required.
AC_PREREQ(2.52g)

dnl Every other copy of the package version number gets its value from here
AM_INIT_AUTOMAKE(ppl, 0.4pre1)
AC_SUBST(VERSION)

AM_CONFIG_HEADER(config.h)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

AH_TOP([
#ifndef _ppl_config_h
#define _ppl_config_h 1
])

AH_BOTTOM([

// Neutralize relops for GCC 2.96.
#ifndef __SGI_STL_INTERNAL_RELOPS
#define __SGI_STL_INTERNAL_RELOPS
#endif

#if __GNUC__ >= 3
// Flexible array members at the end of a struct
// are defined as in
//   Type array[];
#define PPL_FLEXIBLE_ARRAY
#else
// Flexible array members at the end of a struct
// are defined as in
//   Type array[0];
#define PPL_FLEXIBLE_ARRAY 0
#endif

#endif
])

dnl C compiler

AC_ARG_WITH(cc,
[  --with-cc=xxx           use XXX as the C compiler],
CC=$with_cc)

AC_LANG_C
AC_PROG_CC

dnl Checks for C compiler characteristics

AC_C_CONST
AC_C_INLINE

dnl C++ compiler

AC_ARG_WITH(cxx,
[  --with-cxx=xxx          use XXX as the C++ compiler],
CXX=$with_cxx)

AC_LANG_CPLUSPLUS
AC_PROG_CXX
AC_PROG_CXXCPP

dnl GMP library

AC_CHECK_GMP

if test x"$have_gmp" = xno
then
  AC_MSG_ERROR([Cannot find GMP version 4 or higher.
GMP is the GNU Multi-Precision library:
see http://www.swox.com/gmp/ for more information.
Do not forget to enable the C++ interface: add --enable-cxx
to the configuration options.])
fi

AC_CHECK_SIZEOF(mp_limb_t, 4, [#include <gmp.h>])

extra_includes="${extra_includes} ${gmp_includes_option}"
extra_libraries="${extra_libraries} ${gmp_library_option}"

dnl Compilation flags

if test x"$GXX" = xyes
then
  COMP_FLAGS="-W -Wall"
else
  COMP_FLAGS=""
fi

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
  COMP_FLAGS="$COMP_FLAGS -pg"
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
  AC_DEFINE(NDEBUG, 1, [Assertions are disabled when this is defined])
  debug_flag="-DNDEBUG=1"
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} --enable-assertions, needs yes or no])
  ;;
esac

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
  AC_MSG_RESULT(speed)
  OPT_FLAGS="$OPT_FLAGS -Os"
  arch=yes
  ;;
standard)
  AC_MSG_RESULT(standard)
  OPT_FLAGS="$OPT_FLAGS -O2"
  ;;
yes)
  AC_MSG_RESULT(standard)
  OPT_FLAGS="$OPT_FLAGS -O2"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} --enable-optimization, needs sspeed, speed, size, standard, yes or no])
  ;;
esac

enableval=$arch
AC_MSG_CHECKING([for which archtitecture to optimize])
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

CXXFLAGS="$COMP_FLAGS $OPT_FLAGS"

dnl Allow the overriding of C and C++ compilation flags

AC_ARG_WITH(cflags,
[  --with-cflags=xxx       add XXX to the options for the C compiler],
CFLAGS="$CFLAGS $with_cflags")

AC_ARG_WITH(cxxflags,
[  --with-cxxflags=xxx     add XXX to the options for the C++ compiler],
CXXFLAGS="$CXXFLAGS $with_cxxflags")

dnl Libtool.
# Turn off shared libraries during beta-testing, since they
# make debugging harder and the build process take too long.
AC_DISABLE_SHARED
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

dnl Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL

dnl Checks for Prolog systems.

dnl SICStus

AC_CHECK_PROG(SICSTUS_PROLOG, sicstus, sicstus)
if test x$SICSTUS_PROLOG = xsicstus
then
dnl We require SICStus Prolog 3.9.0 or later.
AC_CHECK_HEADER(sicstus/sicstus.h,
                AC_MSG_CHECKING([for SICStus version 3.9.0 or later])
                AC_EGREP_CPP(yes,
                  [
                    #include <sicstus/sicstus.h>
                    #if SICSTUS_MAJOR_VERSION > 3 || (SICSTUS_MAJOR_VERSION == 3 && SICSTUS_MINOR_VERSION >= 9)
                      yes
                    #endif
                  ],
                  AC_MSG_RESULT(yes),
                  SICSTUS_PROLOG=""
                  AC_MSG_RESULT(no)))
fi
AM_CONDITIONAL(HAVE_SICSTUS_PROLOG, test x$SICSTUS_PROLOG = xsicstus)

dnl SWI Prolog

AC_CHECK_PROG(SWI_PROLOG, pl, pl)
if test x$SWI_PROLOG = xpl
then
AC_CHECK_HEADER(SWI-Prolog.h,
                [],
                SWI_PROLOG="")
fi
AM_CONDITIONAL(HAVE_SWI_PROLOG, test x$SWI_PROLOG = xpl)

dnl GNU Prolog

AC_CHECK_PROG(GNU_PROLOG, gprolog, gprolog)
if test x$GNU_PROLOG = xgprolog
then
AC_CHECK_HEADER(gprolog.h,
                [],
                GNU_PROLOG="")
fi
AM_CONDITIONAL(HAVE_GNU_PROLOG, test x$GNU_PROLOG = xgprolog)

dnl XSB

AC_CHECK_PROG(XSB_PROLOG, xsb, xsb)
if test x$XSB_PROLOG = xxsb
then
AC_CHECK_HEADER(cinterf.h,
                [],
                XSB_PROLOG="")
fi
AM_CONDITIONAL(HAVE_XSB_PROLOG, test x$XSB_PROLOG = xsb)

dnl YAP

AC_CHECK_PROG(YAP_PROLOG, yap, yap)
if test x$YAP_PROLOG = xyap
then
AC_CHECK_HEADER(Yap/c_interface.h,
                [],
                YAP_PROLOG="")
fi
AM_CONDITIONAL(HAVE_YAP_PROLOG, test x$YAP_PROLOG = xyap)

dnl Checks for header files.
AC_CHECK_HEADER(glpk.h,
                extra_if_C_subdirs="${extra_if_C_subdirs} lpenum")

dnl Checks for typedefs, structures, and compiler characteristics.
# Nothing for the time being.

dnl Checks for library functions.
# Nothing for the time being.

AC_SUBST(extra_includes)
AC_SUBST(extra_libraries)
AC_SUBST(extra_cpp_flags)
AC_SUBST(debug_flag)
AC_SUBST(extra_if_C_subdirs)

AC_OUTPUT(Makefile
          ppl.lsm ppl.spec
          src/Makefile
          tests/Makefile
          m4/Makefile
          doc/Makefile
          doc/user-browse.doxyconf-latex doc/devref-browse.doxyconf-latex
          doc/user-print.doxyconf-latex  doc/devref-print.doxyconf-latex
          doc/user.doxyconf-html         doc/devref.doxyconf-html
          interfaces/Makefile
          interfaces/C/Makefile
          interfaces/C/lpenum/Makefile
          interfaces/Prolog/Makefile
          interfaces/Prolog/GNU/Makefile
          interfaces/Prolog/SICStus/Makefile interfaces/Prolog/SWI/Makefile
          interfaces/Prolog/YAP/Makefile)

if test x"$gmp_supports_exceptions" = xno
then
  AC_MSG_WARN([CANNOT PROPAGATE EXCEPTIONS BACK FROM GMP:
*** MEMORY EXHAUSTION MAY RESULT IN ABRUPT TERMINATION.
*** If you are using g++, make sure you use version 4.0.1 or higher
*** and use a version of GMP compiled with the option -fexceptions.
*** To build such a version, you can configure GMP as follows:
*** CPPFLAGS=-fexceptions ./configure --enable-cxx --prefix=/usr/local])
fi
