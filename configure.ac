# Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)
dnl Minimum autoconf version required.
AC_PREREQ(2.12)

dnl Every other copy of the package version number gets its value from here
AM_INIT_AUTOMAKE(ppl, 0.1)
AC_SUBST(VERSION)

AM_CONFIG_HEADER(config.h)

ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)

AC_CANONICAL_HOST

AH_TOP([
#ifndef _config_h
#define _config_h 1
])

AH_BOTTOM([
#if OUTLINE
#define INLINE
#else
#define INLINE inline
#endif

// Neutralize relops for GCC 2.96.
#ifndef __SGI_STL_INTERNAL_RELOPS
#define __SGI_STL_INTERNAL_RELOPS
#endif

#if __GNUC__ >= 3
// Flexible array members at the end of a struct
// are defined as in
//   Type array[];
#define FLEXIBLE_ARRAY
#else
// Flexible array members at the end of a struct
// are defined as in
//   Type array[0];
#define FLEXIBLE_ARRAY 0
#endif

#endif
])

dnl C compiler

AC_ARG_WITH(cc,
[  --with-cc=xxx           use XXX as the C compiler],
CXX=$with_cc)

AC_LANG_C
AC_PROG_CC

dnl C++ compiler

AC_ARG_WITH(cxx,
[  --with-cxx=xxx          use XXX as the C++ compiler],
CXX=$with_cxx)

AC_LANG_CPLUSPLUS
AC_PROG_CXX

dnl GMP library

AC_ARG_WITH(gmp-includes,
            [  --with-gmp-includes=DIR GMP include files are in DIR],
            gmp_includes=${with_gmp_includes}
            gmp_includes_option="-I${gmp_includes}")

AC_ARG_WITH(gmp-dir,
            [  --with-gmp-dir=DIR      GMP library files are in DIR],
            gmp_dir=${with_gmp_dir}
            gmp_library_option="${gmp_dir}/libgmp.a",
            gmp_library_option="-lgmp")


ac_save_LIBS="$LIBS"
LIBS="${gmp_library_option} $LIBS"

ac_save_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="${gmp_includes_option} $CPPFLAGS"

AC_CHECK_HAVE_GMP

if test x"$have_gmp" = xno
then
  AC_MSG_ERROR([Cannot find GMP, the GNU Multi-Precision library])
fi

AC_CHECK_GMP_EXCEPTIONS

CPPFLAGS="$ac_save_CPPFLAGS"
LIBS="$ac_save_LIBS"

extra_includes="${extra_includes} ${gmp_includes_option}"

dnl Compilation flags

if test x"$GXX" = xyes
then
  COMP_FLAGS="-W -Wall"
else
  COMP_FLAGS=""
fi

OPT_FLAGS=""
outline=no

enableval=yes
AC_MSG_CHECKING([whether to compile with debug info])
AC_ARG_ENABLE(debugging,
[  --enable-debugging      compile with debugging information])
case "${enableval}" in
yes)
  AC_MSG_RESULT(yes)
  OPT_FLAGS="-g"
  outline=yes
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
  outline=yes
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
  outline=yes
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

if test x"$outline" = xyes
then
  AC_DEFINE(OUTLINE, 1, [Inlining is disabled when this is not zero])
else
  AC_DEFINE(OUTLINE, 0, [Inlining is disabled when this is not zero])
fi

CXXFLAGS="$COMP_FLAGS $OPT_FLAGS"

dnl Allow the overriding of C and C++ compilation flags

AC_ARG_WITH(cflags,
[  --with-cflags=xxx       use XXX as options for the C compiler],
CXXFLAGS=$with_cflags)

AC_ARG_WITH(cxxflags,
[  --with-cxxflags=xxx     use XXX as options for the C++ compiler],
CXXFLAGS=$with_cxxflags)

dnl Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_RANLIB

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.

# Nothing for the time being.

dnl Checks for library functions.

AC_SUBST(extra_includes)
AC_SUBST(extra_cpp_flags)
AC_SUBST(gmp_library_option)
AC_SUBST(debug_flag)

AC_OUTPUT(Makefile
          src/Makefile
          src/BitSet/Makefile src/Integer/Makefile
          tests/Makefile
          m4/Makefile
          doc/Makefile
          doc/user-browse.doxyconf-latex doc/devref-browse.doxyconf-latex
          doc/user-print.doxyconf-latex  doc/devref-print.doxyconf-latex
          doc/user.doxyconf-html         doc/devref.doxyconf-html
          ppl.lsm ppl.spec)
