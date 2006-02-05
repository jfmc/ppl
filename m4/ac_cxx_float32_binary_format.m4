dnl A function to detect the binary format used by 32-bit floats.
dnl Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
AC_DEFUN([AC_CXX_FLOAT32_BINARY_FORMAT],
[
ac_save_CPPFLAGS="$CPPFLAGS"
ac_save_LIBS="$LIBS"
AC_LANG_PUSH(C++)

AC_MSG_CHECKING([the binary format of 32-bit floats])
ac_cxx_float32_binary_format=unknown
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if SIZEOF_FLOAT == 4
#define FLOAT32_TYPE float
#elif SIZEOF_DOUBLE == 4
#define FLOAT32_TYPE double
#elif SIZEOF_LONG_DOUBLE == 4
#define FLOAT32_TYPE long double
#endif

#ifdef FLOAT32_TYPE
typedef FLOAT32_TYPE float32_t;

inline float32_t
convert(uint32_t x) {
  union {
    float32_t value;
    uint32_t word;
  } u;

  u.word = x;
  return u.value;
}

int
main() {
  return (convert(0xaaacccaaUL)
	  == -3.069535185924732179074680971098132431507110595703125e-13 &&
	  convert(0xcccaaaccUL)
	  == -106255968)
    ? 0 : 1;
}

#else // !defined(FLOAT32_TYPE)

int
main() {
  return 1;
}

#endif // !defined(FLOAT32_TYPE)
]])],
  ac_cxx_float32_binary_format="IEEE754 Single Precision")
AC_MSG_RESULT($ac_cxx_float32_binary_format)

if test x"$ac_cxx_float32_binary_format" = x"IEEE754 Single Precision"
then
  value=1
else
  value=0
fi
AC_DEFINE_UNQUOTED(CXX_FLOAT32_BINARY_FORMAT_IS_IEEE754_SINGLE_PRECISION, $value,
  [Not zero if 32-bit floats use the IEEE754 Single Precision binary format.])

AC_LANG_POP(C++)
CPPFLAGS="$ac_save_CPPFLAGS"
LIBS="$ac_save_LIBS"
])
