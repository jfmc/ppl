# Autoconf source file for the Parma Polyhedra Library.
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

# Process this file with autoconf to produce a configure script.

# Every other copy of the package version number gets its value from here.
AC_INIT([the Parma Polyhedra Library], 0.5pre6, ppl-devel@cs.unipr.it, ppl)

# Minimum autoconf version required.
AC_PREREQ(2.54)

# Make sure the sources are there.
AC_CONFIG_SRCDIR(src/Polyhedron.cc)

# Use Automake.
AM_INIT_AUTOMAKE([foreign dist-bzip2 dist-zip 1.7])
AC_SUBST(VERSION)

# Generate a configuration header file.
AM_CONFIG_HEADER(config.h)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

AH_TOP([
#ifndef PPL_ppl_config_h
#define PPL_ppl_config_h 1
])

AH_BOTTOM([
#endif // !defined(PPL_ppl_config_h)
])

# C compiler

AC_ARG_WITH(cc,
[  --with-cc=xxx           use XXX as the C compiler],
CC=$with_cc)

AC_LANG(C)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE

# C++ compiler

AC_ARG_WITH(cxx,
[  --with-cxx=xxx          use XXX as the C++ compiler],
CXX=$with_cxx)

AC_LANG(C++)

# Check whether the C++ compiler supports flexible arrays.
AC_CXX_SUPPORTS_FLEXIBLE_ARRAYS

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# GMP library

AC_CHECK_GMP

if test x"$have_gmp" = xno
then
  AC_MSG_ERROR([Cannot find GMP version 4 or higher.
GMP is the GNU Multi-Precision library:
see http://www.swox.com/gmp/ for more information.
Do not forget to enable the C++ interface: add --enable-cxx
to the configuration options.])
fi

extra_includes="${extra_includes} ${gmp_includes_option}"
extra_libraries="${extra_libraries} ${gmp_library_option}"

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
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} --enable-assertions, needs yes or no])
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
  AC_DEFINE(EXTRA_ROW_DEBUG, 1, [Enable more assertions when defined.])
  debug_flag="-DEXTRA_ROW_DEBUG=1"
  ;;
no)
  AC_MSG_RESULT(no)
  ;;
*)
  AC_MSG_ERROR([bad value ${enableval} --enable-more-assertions, needs yes or no])
  ;;
esac
enable_more_assertions=${enableval}

if test x"$enable_assertions" = xno
then
  if test x"$enable_more_assertions" = xno
  then
    AC_DEFINE(NDEBUG, 1, [Assertions are disabled when this is defined.])
    debug_flag="$debug_flag -DNDEBUG=1"
  fi
fi

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

# Allow additions to C and C++ compilation flags

AC_ARG_WITH(cflags,
[  --with-cflags=xxx       add XXX to the options for the C compiler],
CFLAGS="$CFLAGS $with_cflags")

AC_ARG_WITH(cxxflags,
[  --with-cxxflags=xxx     add XXX to the options for the C++ compiler],
CXXFLAGS="$CXXFLAGS $with_cxxflags")

enableval=no
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
  AC_MSG_ERROR([bad value ${enableval} --enable-assertions, needs yes or no])
  ;;
esac
build_watchdog_library=$enableval
AM_CONDITIONAL(BUILD_WATCHDOG_LIBRARY, test x"$build_watchdog_library" = xyes)

# Enabled or not, the Watchdog subdirectory must be configured.
AC_CONFIG_SUBDIRS(Watchdog)

# Libtool.
# Turn off shared libraries during beta-testing, since they
# make debugging harder and the build process takes too long.
AC_DISABLE_SHARED
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

# Checks for Prolog systems.

# Ciao Prolog

AC_CHECK_PROG(CIAO_PROLOG, ciao, ciao)
if test x"$CIAO_PROLOG" = xciao
then
AC_CHECK_HEADER(ciao_prolog.h,
                [],
                CIAO_PROLOG="")
fi
AM_CONDITIONAL(HAVE_CIAO_PROLOG, test x$CIAO_PROLOG = xciao)

# GNU Prolog

AC_CHECK_PROG(GNU_PROLOG, gprolog, gprolog)
if test x"$GNU_PROLOG" = xgprolog
then
AC_CHECK_HEADER(gprolog.h,
                [],
                GNU_PROLOG="")
fi
AM_CONDITIONAL(HAVE_GNU_PROLOG, test x$GNU_PROLOG = xgprolog)

# SICStus

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
                  AC_MSG_RESULT(no)))
fi
AM_CONDITIONAL(HAVE_SICSTUS_PROLOG, test x$SICSTUS_PROLOG = xsicstus)

# SWI Prolog

AC_CHECK_PROG(SWI_PROLOG, pl, pl)
if test x"$SWI_PROLOG" = xpl
then
AC_CHECK_HEADER(SWI-Prolog.h,
                [],
                SWI_PROLOG="")
fi
AM_CONDITIONAL(HAVE_SWI_PROLOG, test x$SWI_PROLOG = xpl)

# XSB

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
AM_CONDITIONAL(HAVE_XSB_PROLOG, test x$XSB_PROLOG = xxsb)

# YAP

AC_CHECK_PROG(YAP_PROLOG, yap, yap)
if test x"$YAP_PROLOG" = xyap
then
AC_CHECK_HEADER(Yap/c_interface.h,
                [],
                YAP_PROLOG="")
fi
AM_CONDITIONAL(HAVE_YAP_PROLOG, test x$YAP_PROLOG = xyap)

# Check for the Mercury compiler

AC_CHECK_PROG(MERCURY_COMPILER, mmc, mmc)
AM_CONDITIONAL(HAVE_MERCURY_COMPILER, test x$MERCURY_COMPILER = xmmc)

# Check for the GLPK library

AC_LANG_PUSH(C)
AC_CHECK_HEADER(glpk.h, HAVE_GLPK_H=yes)
AM_CONDITIONAL(HAVE_GLPK, test x$HAVE_GLPK_H = xyes)
AC_LANG_POP(C)

# Checks for header files.

AC_CHECK_HEADERS([sys/resource.h sys/time.h sys/types.h unistd.h])

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
  CFLAGS="$CFLAGS -W -Wall"
fi
if test x"$GXX" = xyes
then
  CXXFLAGS="$CXXFLAGS -W -Wall"
fi

AC_SUBST(extra_includes)
AC_SUBST(extra_libraries)
AC_SUBST(extra_cpp_flags)
AC_SUBST(debug_flag)
AC_SUBST(xsb_includes)

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
          interfaces/Prolog/Ciao/Makefile
          interfaces/Prolog/GNU/Makefile
          interfaces/Prolog/SICStus/Makefile
          interfaces/Prolog/SWI/Makefile
          interfaces/Prolog/XSB/Makefile
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
