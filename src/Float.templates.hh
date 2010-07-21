/* IEC 559 floating point format related functions:
   non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_Float_templates_hh
#define PPL_Float_Templates_hh 1

#include <cmath>

namespace Parma_Polyhedra_Library {

template <typename FP_Interval_Type>
FP_Interval_Type compute_absolute_error(
		 const Floating_Point_Format analyzed_format) {
  typedef typename FP_Interval_Type::boundary_type analyzer_format;

  // Get the necessary information on the analyzed's format.
  unsigned int f_base;
  int f_exponent_bias;
  unsigned int f_mantissa_bits;
  switch (analyzed_format) {
    case IEEE754_HALF:
      f_base = float_ieee754_half::BASE;
      f_exponent_bias = float_ieee754_half::EXPONENT_BIAS;
      f_mantissa_bits = float_ieee754_half::MANTISSA_BITS;
      break;
    case IEEE754_SINGLE:
      f_base = float_ieee754_single::BASE;
      f_exponent_bias = float_ieee754_single::EXPONENT_BIAS;
      f_mantissa_bits = float_ieee754_single::MANTISSA_BITS;
      break;
    case IEEE754_DOUBLE:
      f_base = float_ieee754_double::BASE;
      f_exponent_bias = float_ieee754_double::EXPONENT_BIAS;
      f_mantissa_bits = float_ieee754_double::MANTISSA_BITS;
      break;
    case IBM_SINGLE:
      f_base = float_ibm_single::BASE;
      f_exponent_bias = float_ibm_single::EXPONENT_BIAS;
      f_mantissa_bits = float_ibm_single::MANTISSA_BITS;
      break;
    case IEEE754_QUAD:
      f_base = float_ieee754_quad::BASE;
      f_exponent_bias = float_ieee754_quad::EXPONENT_BIAS;
      f_mantissa_bits = float_ieee754_quad::MANTISSA_BITS;
      break;
    case INTEL_DOUBLE_EXTENDED:
      f_base = float_intel_double_extended::BASE;
      f_exponent_bias = float_intel_double_extended::EXPONENT_BIAS;
      f_mantissa_bits = float_intel_double_extended::MANTISSA_BITS;
      break;
    default:
      throw std::runtime_error("PPL internal error");
  }

  analyzer_format omega = std::max(
  static_cast<analyzer_format>(pow(f_base,
                               static_cast<analyzer_format>(1) -
                               f_exponent_bias - f_mantissa_bits)),
  std::numeric_limits<analyzer_format>::denorm_min());

  FP_Interval_Type result;
  result.build(i_constraint(GREATER_OR_EQUAL, -omega),
               i_constraint(LESS_OR_EQUAL, omega));
  return result;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Float_Templates_hh)
