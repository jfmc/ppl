/* Architecture-dependent, floating-point number types.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_float_types_hh
#define PPL_float_types_hh

namespace Parma_Polyhedra_Library {

// FIXME: the following is an experimental hack.
#if SIZEOF_FLOAT == 4
#if CXX_FLOAT32_BINARY_FORMAT_IS_IEEE754_SINGLE_PRECISION
#define CXX_FLOAT_BINARY_FORMAT float_ieee754_single
#endif
#elif SIZEOF_FLOAT == 8
#if CXX_FLOAT64_BINARY_FORMAT_IS_IEEE754_DOUBLE_PRECISION
#define CXX_FLOAT_BINARY_FORMAT float_ieee754_double
#endif
#elif SIZEOF_FLOAT == 12
#if CXX_FLOAT96_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_FLOAT_BINARY_FORMAT float_intel_double_extended
#endif
#elif SIZEOF_FLOAT == 16
#if CXX_FLOAT128_BINARY_FORMAT_IS_IEEE754_QUAD_PRECISION
#define CXX_FLOAT_BINARY_FORMAT float_ieee754_quad
#endif
#elif CXX_FLOAT128_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_FLOAT_BINARY_FORMAT float_intel_double_extended
#endif
#if SIZEOF_DOUBLE == 4
#if CXX_FLOAT32_BINARY_FORMAT_IS_IEEE754_SINGLE_PRECISION
#define CXX_DOUBLE_BINARY_FORMAT float_ieee754_single
#endif
#elif SIZEOF_DOUBLE == 8
#if CXX_FLOAT64_BINARY_FORMAT_IS_IEEE754_DOUBLE_PRECISION
#define CXX_DOUBLE_BINARY_FORMAT float_ieee754_double
#endif
#elif SIZEOF_DOUBLE == 12
#if CXX_FLOAT96_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_DOUBLE_BINARY_FORMAT float_intel_double_extended
#endif
#elif SIZEOF_DOUBLE == 16
#if CXX_FLOAT128_BINARY_FORMAT_IS_IEEE754_QUAD_PRECISION
#define CXX_DOUBLE_BINARY_FORMAT float_ieee754_quad
#endif
#elif CXX_FLOAT128_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_DOUBLE_BINARY_FORMAT float_intel_double_extended
#endif
#if SIZEOF_LONG_DOUBLE == 4
#if CXX_FLOAT32_BINARY_FORMAT_IS_IEEE754_SINGLE_PRECISION
#define CXX_LONG_DOUBLE_BINARY_FORMAT float_ieee754_single
#endif
#elif SIZEOF_LONG_DOUBLE == 8
#if CXX_FLOAT64_BINARY_FORMAT_IS_IEEE754_DOUBLE_PRECISION
#define CXX_LONG_DOUBLE_BINARY_FORMAT float_ieee754_double
#endif
#elif SIZEOF_LONG_DOUBLE == 12
#if CXX_FLOAT96_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_LONG_DOUBLE_BINARY_FORMAT float_intel_double_extended
#endif
#elif SIZEOF_LONG_DOUBLE == 16
#if CXX_FLOAT128_BINARY_FORMAT_IS_IEEE754_QUAD_PRECISION
#define CXX_LONG_DOUBLE_BINARY_FORMAT float_ieee754_quad
#endif
#elif CXX_FLOAT128_BINARY_FORMAT_IS_INTEL_DOUBLE_EXTENDED
#define CXX_LONG_DOUBLE_BINARY_FORMAT float_intel_double_extended
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_float_types_hh)
