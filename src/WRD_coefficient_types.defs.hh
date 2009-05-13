/* Coefficient types of weakly-relational domains: declarations.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_WRD_coefficient_types_defs_hh
#define PPL_WRD_coefficient_types_defs_hh 1

#include "meta_programming.hh"

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \ingroup PPL_CXX_interface */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
struct WRD_Extended_Number_Policy {
  const_bool_nodef(check_overflow, true);
  const_bool_nodef(check_inf_add_inf, false);
  const_bool_nodef(check_inf_sub_inf, false);
  const_bool_nodef(check_inf_mul_zero, false);
  const_bool_nodef(check_div_zero, false);
  const_bool_nodef(check_inf_div_inf, false);
  const_bool_nodef(check_inf_mod, false);
  const_bool_nodef(check_sqrt_neg, false);
  const_bool_nodef(has_nan, true);
  const_bool_nodef(has_infinity, true);
  // Do not uncomment the following.
  // The compile time error on conversions is the expected behavior.
  // const_bool_nodef(convertible, false);
  const_bool_nodef(fpu_check_inexact, true);
  const_bool_nodef(fpu_check_nan_result, false);
  // Do not uncomment the following.
  // The compile time error is the expected behavior.
  // static const Rounding_Dir ROUND_DEFAULT_CONSTRUCTOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OPERATOR = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_FUNCTION = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_INPUT = ROUND_UP;
  // static const Rounding_Dir ROUND_DEFAULT_OUTPUT = ROUND_UP;
  static void handle_result(Result r);
};

} // namespace Parma_Polyhedra_Library

#include "WRD_coefficient_types.inlines.hh"

#endif // !defined(PPL_WRD_coefficient_types_defs_hh)
