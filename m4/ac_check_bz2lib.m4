dnl A function to check for the existence and usability of Bzip2.
dnl Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
dnl USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .
dnl
AC_DEFUN([AC_CHECK_BZ2LIB],
[
dnl Check how to link with libbz2.
AC_LIB_LINKFLAGS([bz2])

ac_save_LIBS="$LIBS"
LIBS="$LIBS $LIBBZ2"
AC_LANG_PUSH(C++)

AC_MSG_CHECKING([for the Bzip2 library])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <bzlib.h>
#include <iostream>

using namespace std;

int main() {
  const char* object_version = BZ2_bzlibVersion();
  cerr << "libbz2 version: " << object_version
       << endl;
  exit(0);
}
]])],
  AC_MSG_RESULT(yes)
  ac_cv_have_bz2lib=yes,
  AC_MSG_RESULT(no)
  ac_cv_have_bz2lib=no,
  AC_MSG_RESULT(no)
  ac_cv_have_bz2lib=no)

have_bz2lib=${ac_cv_have_bz2lib}

AM_CONDITIONAL(HAVE_BZ2LIB, test x"$have_bz2lib" = xyes)

AC_LANG_POP(C++)
LIBS="$ac_save_LIBS"

dnl We use libtool, therefore we take $LTLIBBZ2, not $LIBBZ2.
bz2lib_library_option="$LTLIBBZ2"
])
