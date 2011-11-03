/* Linear_Expression class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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


#include <ppl-config.h>

#include "Linear_Expression.defs.hh"

#include "Linear_Expression_Impl.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

PPL::dimension_type
PPL::Linear_Expression::max_space_dimension() {
  return Dense_Row::max_size() - 1;
}

const PPL::Linear_Expression* PPL::Linear_Expression::zero_p = 0;

void
PPL::Linear_Expression::initialize() {
  PPL_ASSERT(zero_p == 0);
  zero_p = new Linear_Expression(Coefficient_zero());
}

void
PPL::Linear_Expression::finalize() {
  PPL_ASSERT(zero_p != 0);
  delete zero_p;
  zero_p = 0;
}

PPL::Linear_Expression::Linear_Expression()
  : impl(new Linear_Expression_Impl<Dense_Row>()) {
}

PPL::Linear_Expression::Linear_Expression(dimension_type sz, bool x)
  : impl(new Linear_Expression_Impl<Dense_Row>(sz, x)) {
}

PPL::Linear_Expression::Linear_Expression(const Congruence& c, dimension_type sz)
  : impl(new Linear_Expression_Impl<Dense_Row>(c, sz)) {
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e)
  : impl(new Linear_Expression_Impl<Dense_Row>(*e.impl)) {
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e,
                                     dimension_type sz)
  : impl(new Linear_Expression_Impl<Dense_Row>(*e.impl, sz)) {
}

PPL::Linear_Expression::Linear_Expression(Coefficient_traits::const_reference n)
  : impl(new Linear_Expression_Impl<Dense_Row>(n)) {
}

PPL::Linear_Expression::Linear_Expression(const Constraint& c)
  : impl(new Linear_Expression_Impl<Dense_Row>(c)) {
}

PPL::Linear_Expression::Linear_Expression(const Generator& g)
  : impl(new Linear_Expression_Impl<Dense_Row>(g)) {
}

PPL::Linear_Expression::Linear_Expression(const Grid_Generator& g)
  : impl(new Linear_Expression_Impl<Dense_Row>(g)) {
}

PPL::Linear_Expression::Linear_Expression(const Congruence& cg)
  : impl(new Linear_Expression_Impl<Dense_Row>(cg)) {
}

PPL::Linear_Expression::Linear_Expression(const Variable v)
  : impl(new Linear_Expression_Impl<Dense_Row>(v)) {
}

PPL_OUTPUT_DEFINITIONS(Linear_Expression)
