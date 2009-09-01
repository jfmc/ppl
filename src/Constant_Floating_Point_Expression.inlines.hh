/* Constant_Floating_Point_Expression class implementation: inline functions.
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

#ifndef PPL_Constant_Floating_Point_Expression_inlines_hh
#define PPL_Constant_Floating_Point_Expression_inlines_hh 1

#include "globals.defs.hh"

namespace Parma_Polyhedra_Library {

template <typename FP_Interval_Type, typename FP_Format>
inline
Constant_Floating_Point_Expression<FP_Interval_Type, FP_Format>::
Constant_Floating_Point_Expression(const boundary_type lb,
                                   const boundary_type ub) {
  l_bound = lb;
  u_bound = ub;
}

template <typename FP_Interval_Type, typename FP_Format>
inline
Constant_Floating_Point_Expression<FP_Interval_Type, FP_Format>::
~Constant_Floating_Point_Expression() {}

template <typename FP_Interval_Type, typename FP_Format>
inline typename Constant_Floating_Point_Expression<FP_Interval_Type,
                                                   FP_Format>::FP_Linear_Form
Constant_Floating_Point_Expression<FP_Interval_Type, FP_Format>::linearize(
const FP_Interval_Abstract_Store& store) const {

  FP_Interval_Type result_interval(l_bound);
  FP_Interval_Type result_upper_bound(u_bound);
  result_interval.join_assign(result_upper_bound);
  FP_Linear_Form result = FP_Linear_Form(result_interval);
  return result;

}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Constant_Floating_Point_Expression_inlines_hh)
