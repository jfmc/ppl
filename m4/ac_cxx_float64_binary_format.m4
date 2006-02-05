dnl A function to detect the binary format used by 64-bit floats.
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
AC_DEFUN([AC_CXX_FLOAT64_BINARY_FORMAT],
[
ac_save_CPPFLAGS="$CPPFLAGS"
ac_save_LIBS="$LIBS"
AC_LANG_PUSH(C++)

AC_MSG_CHECKING([the binary format of 64-bit floats])
ac_cxx_float64_binary_format=unknown
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdint.h>

#if SIZEOF_FLOAT == 8
#define FLOAT64_TYPE float
#elif SIZEOF_DOUBLE == 8
#define FLOAT64_TYPE double
#elif SIZEOF_LONG_DOUBLE == 8
#define FLOAT64_TYPE long double
#endif

#ifdef FLOAT64_TYPE
typedef FLOAT64_TYPE float64_t;

float64_t
convert(uint32_t msp, uint32_t lsp) {
  union {
    float64_t value;
    struct {
#ifdef WORDS_BIGENDIAN
      uint32_t msp;
      uint32_t lsp;
#else
      uint32_t lsp;
      uint32_t msp;
#endif
    } parts;
  } u;

  u.parts.msp = msp;
  u.parts.lsp = lsp;
  return u.value;
}

int
main() {
  return (convert(0xaaacccaaUL, 0xacccaaacUL)
	  == -4.018242396032647e-103 &&
	  convert(0xcccaaaccUL, 0xcaaacccaUL)
	  == -85705035845709846787631445265530356117787053916987832397725696.0)
    ? 0 : 1;
}

#else // !defined(FLOAT64_TYPE)

int
main() {
  return 1;
}

#endif // !defined(FLOAT64_TYPE)
]])],
  ac_cxx_float64_binary_format="IEEE754 Double Precision")
AC_MSG_RESULT($ac_cxx_float64_binary_format)

if test x"$ac_cxx_float64_binary_format" = x"IEEE754 Double Precision"
then
  value=1
else
  value=0
fi
AC_DEFINE_UNQUOTED(CXX_FLOAT64_BINARY_FORMAT_IS_IEEE754_DOUBLE_PRECISION, $value,
  [Not zero if 64-bit floats use the IEEE754 Double Precision binary format.])

AC_LANG_POP(C++)
CPPFLAGS="$ac_save_CPPFLAGS"
LIBS="$ac_save_LIBS"
])
