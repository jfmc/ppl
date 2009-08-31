/* Declarations for the Constant_Floating_Point_Expression class and its constituents.
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

#ifndef PPL_Constant_Floating_Point_Expression_defs_hh
#define PPL_Constant_Floating_Point_Expression_defs_hh 1

#include "Floating_Point_Expression.defs.hh"
#include "globals.defs.hh"
#include "Constant_Floating_Point_Expression.types.hh"
#include <map>

namespace Parma_Polyhedra_Library {

template <typename FP_Interval_Type, typename FP_Format>
class Constant_Floating_Point_Expression
: public virtual Floating_Point_Expression<FP_Interval_Type, FP_Format> {

public:

  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::
  FP_Linear_Form FP_Linear_Form;

  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::
  FP_Interval_Abstract_Store FP_Interval_Abstract_Store;

  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::boundary_type
  boundary_type;

  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::info_type info_type;

  Constant_Floating_Point_Expression(boundary_type lower_bound,
                                     boundary_type upper_bound);

  ~Constant_Floating_Point_Expression();

  FP_Linear_Form linearize(FP_Interval_Abstract_Store store);

private:

  boundary_type l_bound;

  boundary_type u_bound;

}; // class Constant_Floating_Point_Expression

} // namespace Parma_Polyhedra_Library

#include "Constant_Floating_Point_Expression.inlines.hh"

#endif // !defined(PPL_Constant_Floating_Point_Expression_defs_hh)
