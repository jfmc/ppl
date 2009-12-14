/* Floating_Point_Expression class implementation:
   non-inline template functions.
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

#ifndef PPL_Floating_Point_Expression_templates_hh
#define PPL_Floating_Point_Expression_templates_hh 1

#include "Linear_Form.defs.hh"
#include <cmath>

namespace Parma_Polyhedra_Library {

template<typename FP_Interval_Type, typename FP_Format>
void
Floating_Point_Expression<FP_Interval_Type, FP_Format>
::relative_error(const FP_Linear_Form& lf, FP_Linear_Form& result) {
  /* FIXME: here we assume that boundary_type can represent
     (2)^(-FP_Format::MANTISSA_BITS) precisely. */
  FP_Interval_Type error_propagator(-pow(FP_Format::BASE,
  -static_cast<typename Floating_Point_Expression<FP_Interval_Type, FP_Format>
  ::boundary_type>(FP_Format::MANTISSA_BITS)));
  error_propagator.join_assign(FP_Interval_Type(pow(FP_Format::BASE,
  -static_cast<typename Floating_Point_Expression<FP_Interval_Type, FP_Format>
  ::boundary_type>(FP_Format::MANTISSA_BITS))));

  // Handle the inhomogeneous term.
  const FP_Interval_Type* current_term = &lf.inhomogeneous_term();
  FP_Interval_Type current_multiplier(std::max(abs(current_term->lower()),
					       abs(current_term->upper())));
  FP_Linear_Form current_result_term(current_multiplier);
  current_result_term *= error_propagator;
  result = FP_Linear_Form(current_result_term);

  // Handle the other terms.
  dimension_type dimension = lf.space_dimension();
  for (dimension_type i = 0; i < dimension; ++i) {
    current_term = &lf.coefficient(Variable(i));
    current_multiplier = FP_Interval_Type(std::max(abs(current_term->lower()),
					       abs(current_term->upper())));
    current_result_term = FP_Linear_Form(Variable(i));
    current_result_term *= current_multiplier;
    current_result_term *= error_propagator;
    result += current_result_term;
  }

  return;
}

template<typename FP_Interval_Type, typename FP_Format>
void
Floating_Point_Expression<FP_Interval_Type, FP_Format>
::intervalize(const FP_Linear_Form& lf,
              const FP_Interval_Abstract_Store& store,
              FP_Interval_Type& result) {
  result = FP_Interval_Type(lf.inhomogeneous_term());
  dimension_type dimension = lf.space_dimension();
  assert(dimension <= store.space_dimension());
  for (dimension_type i = 0; i < dimension; ++i) {
    FP_Interval_Type current_addend = lf.coefficient(Variable(i));
    const FP_Interval_Type& curr_int = store.get_interval(Variable(i));
    current_addend *= curr_int;
    result += current_addend;
  }

  return;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Floating_Point_Expression_templates_hh)
