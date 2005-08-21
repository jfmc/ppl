dnl A function to check whether C++ floats comply with IEC 559.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .
dnl
AC_DEFUN([AC_CXX_FLOAT_IS_IEC_559],
[
ac_save_CPPFLAGS="$CPPFLAGS"
ac_save_LIBS="$LIBS"
AC_LANG_PUSH(C++)

AC_MSG_CHECKING([whether C++ floats comply with IEC 559])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <limits>

int main() {
  return std::numeric_limits<float>::is_iec559 ? 0 : 1;
}
]])],
  AC_MSG_RESULT(yes)
  ac_cxx_float_is_iec_559=yes,
  AC_MSG_RESULT(no)
  ac_cxx_float_is_iec_559=no,
  AC_MSG_RESULT(no)
  ac_cxx_float_is_iec_559=no)

if test x"$ac_cxx_float_is_iec_559" = xyes
then
  value=1
else
  value=0
fi
AC_DEFINE_UNQUOTED(CXX_FLOAT_IS_IEC_559, $value,
  [Not zero if C++ floats comply to IEC 559.])

AC_LANG_POP(C++)
CPPFLAGS="$ac_save_CPPFLAGS"
LIBS="$ac_save_LIBS"
])
