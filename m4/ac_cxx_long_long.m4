dnl A function to check whether the C++ compiler supports long long integers.
dnl Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
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
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .
dnl
AC_DEFUN([AC_CXX_SUPPORTS_LONG_LONG],
[
ac_save_CPPFLAGS="$CPPFLAGS"
ac_save_LIBS="$LIBS"
AC_LANG_PUSH(C++)

AC_MSG_CHECKING([whether the C++ compiler supports long long integers])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
long long i = 0;
]])],
  AC_MSG_RESULT(yes)
  ac_cxx_supports_long_long=yes,
  AC_MSG_RESULT(no)
  ac_cxx_supports_long_long=no,
  AC_MSG_RESULT(no)
  ac_cxx_supports_long_long=no)

if test x"$ac_cxx_supports_long_long" = xyes
then
  value=1
else
  value=0
fi
AC_DEFINE_UNQUOTED(CXX_SUPPORTS_LONG_LONG, $value,
  [Not zero if the C++ compiler supports long long integers.])

AC_LANG_POP(C++)
CPPFLAGS="$ac_save_CPPFLAGS"
LIBS="$ac_save_LIBS"
])
