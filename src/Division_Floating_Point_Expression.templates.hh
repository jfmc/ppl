/* Division_Floating_Point_Expression class implementation:
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

#ifndef PPL_Division_Floating_Point_Expression_templates_hh
#define PPL_Division_Floating_Point_Expression_templates_hh 1

#include "Division_Floating_Point_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename FP_Interval_Type, typename FP_Format>
typename Division_Floating_Point_Expression<FP_Interval_Type, FP_Format>
::FP_Linear_Form Division_Floating_Point_Expression<FP_Interval_Type,
                                                    FP_Format>
::linearize(const FP_Interval_Abstract_Store& store) const {
  FP_Interval_Type intervalized_second_operand = intervalize(
                                second_operand->linearize(store));
  // Check if we may divide by zero.
  // FIXME: check the assumption that boundary_type is comparable with zero.
  if (intervalized_second_operand.lower() <= 0 &&
      intervalized_second_operand.upper() >= 0)
    throw Linearization_Failed();

  FP_Linear_Form linearized_first_operand = first_operand->linearize(store);
  FP_Interval_Type abs_error = FP_Interval_Type(-absolute_error);
  // FIXME: this may be incorrect for some policies.
  abs_error.join_assign(absolute_error);
  FP_Linear_Form result = linearized_first_operand / intervalized_second_operand
    + relative_error(linearized_first_operand) / intervalized_second_operand
    + abs_eror;

  return result;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Division_Floating_Point_Expression_templates_hh)
