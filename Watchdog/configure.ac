# Autoconf source file for the Parma Watchdog Library.
# Copyright (C) 2002-2004 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Watchdog Library (PWL).
#
# The PWL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PWL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see the CS@Parma software
# site: http://www.cs.unipr.it/Software/ . */

# Process this file with autoconf to produce a configure script.

# Every other copy of the package version number gets its value from here.
AC_INIT([the Parma Watchdog Library],[0.4],[bagnara@cs.unipr.it],[watchdog])

# Minimum autoconf version required.
AC_PREREQ(2.57)

# Make sure the sources are there.
AC_CONFIG_SRCDIR([Watchdog.cc])

# Use Automake.
AM_INIT_AUTOMAKE([foreign dist-bzip2 dist-zip 1.7.3])
AC_SUBST(VERSION)

# Generate a configuration header file.
AC_CONFIG_HEADER([config.h])

# C compiler

AC_ARG_WITH(cc,
[  --with-cc=xxx           use XXX as the C compiler],
CC=$with_cc)

AC_LANG(C)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# C++ compiler

AC_ARG_WITH(cxx,
[  --with-cxx=xxx          use XXX as the C++ compiler],
CXX=$with_cxx)

AC_LANG(C++)

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_MAKE_SET
AC_PROG_INSTALL

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
mild)
  AC_MSG_RESULT(mild)
  OPT_FLAGS="$OPT_FLAGS -O1"
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
  AC_MSG_ERROR([bad value ${enableval} --enable-watchdog, needs yes or no])
  ;;
esac
build_watchdog_library=$enableval
AM_CONDITIONAL(BUILD_WATCHDOG_LIBRARY, test x"$build_watchdog_library" = xyes)

if test x"$build_watchdog_library" = xyes
then
  # Checks for header files.
  AC_HEADER_TIME
  AC_CHECK_HEADERS([sys/time.h])

  # Checks for library fuctions.
  # The following invocations of AC_LANG_PUSH(C) and AC_LANG_POP(C)
  # are to work around a bug in AC_CHECK_FUNCS.  For more information,
  # http://www.cs.unipr.it/pipermail/ppl-devel/2002-November/002929.html
  AC_LANG_PUSH(C)
  AC_CHECK_FUNCS([setitimer],
                 [],
                 AC_MSG_ERROR([CANNOT BUILD THE WATCHDOG LIBRARY
*** THE SYSTEM DOES NOT PROVIDE THE SETITIMER FUNCTION.]))
  AC_LANG_POP(C)
fi

# Libtool.
# Turn off shared libraries during beta-testing, since they
# make debugging harder and the build process takes too long.
#AC_DISABLE_SHARED
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

# If we are using GCC we want to compile with warnings enabled.
if test x"$GCC" = xyes
then
  CFLAGS="$CFLAGS -W -Wall"
fi
if test x"$GXX" = xyes
then
  CXXFLAGS="$CXXFLAGS -W -Wall"
fi

# System-dependent adjustments.
no_undefined=no

case "${host_os}" in
cygwin)
  no_undefined=yes
  ;;
*)
  ;;
esac
AM_CONDITIONAL(NO_UNDEFINED, test x$no_undefined = xyes)

AC_SUBST(debug_flag)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
