/* Difference_Floating_Point_Expression class implementation:
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

#ifndef PPL_Difference_Floating_Point_Expression_templates_hh
#define PPL_Difference_Floating_Point_Expression_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename FP_Interval_Type, typename FP_Format>
void Difference_Floating_Point_Expression<FP_Interval_Type, FP_Format>
::linearize(const FP_Interval_Abstract_Store& int_store,
            const FP_Linear_Form_Abstract_Store& lf_store,
            FP_Linear_Form& result) const {
  first_operand->linearize(int_store, lf_store, result);
  FP_Linear_Form rel_error;
  relative_error(result, rel_error);
  result += rel_error;
  FP_Linear_Form linearized_second_operand;
  second_operand->linearize(int_store, lf_store, linearized_second_operand);
  result -= linearized_second_operand;
  relative_error(linearized_second_operand, rel_error);
  result += rel_error;
  FP_Interval_Type abs_error(-this->absolute_error);
  // FIXME: this may be incorrect for some policies.
  abs_error.join_assign(this->absolute_error);
  result += abs_error;
  return;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Difference_Floating_Point_Expression_templates_hh)
