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
#include "Dense_Row.defs.hh"
#include "Sparse_Row.defs.hh"
#include "Congruence.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Generator.defs.hh"

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

PPL::Linear_Expression::Linear_Expression(Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>();
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>();
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(dimension_type space_dim, bool x,
                                          Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(space_dim, x);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(space_dim, x);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Congruence& c,
                                          dimension_type space_dim) {
  switch (c.expression().representation()) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(c, space_dim);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(c, space_dim);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Congruence& c,
                                          dimension_type space_dim,
                                          Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(c, space_dim);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(c, space_dim);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e) {
  switch (e.representation()) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(*e.impl);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(*e.impl);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e,
                                          Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(*e.impl);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(*e.impl);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e,
                                          dimension_type space_dim) {
  switch (e.representation()) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(*e.impl, space_dim);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(*e.impl, space_dim);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Linear_Expression& e,
                                          dimension_type space_dim,
                                          Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(*e.impl, space_dim);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(*e.impl, space_dim);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(Coefficient_traits::const_reference n,
                                          Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(n);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(n);
    break;

  default:
    PPL_ASSERT(false);
  }
}

PPL::Linear_Expression::Linear_Expression(const Constraint& c)
  : impl(NULL) {
  Linear_Expression tmp(c.expression());
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Constraint& c,
                                          Representation r)
  : impl(NULL) {
  Linear_Expression tmp(c.expression(), r);
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Generator& g)
  : impl(NULL) {
  Linear_Expression tmp(g.expression());
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Generator& g, Representation r)
  : impl(NULL) {
  Linear_Expression tmp(g.expression(), r);
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Grid_Generator& g)
  : impl(NULL) {
  Linear_Expression tmp(g.expression());
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Grid_Generator& g,
                                          Representation r)
  : impl(NULL) {
  Linear_Expression tmp(g.expression(), r);
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Congruence& cg)
  : impl(NULL) {
  Linear_Expression tmp(cg.expression());
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Congruence& cg,
                                          Representation r)
  : impl(NULL) {
  Linear_Expression tmp(cg.expression(), r);
  std::swap(impl, tmp.impl);
}

PPL::Linear_Expression::Linear_Expression(const Variable v, Representation r) {
  switch (r) {
  case DENSE:
    impl = new Linear_Expression_Impl<Dense_Row>(v);
    break;

  case SPARSE:
    impl = new Linear_Expression_Impl<Sparse_Row>(v);
    break;

  default:
    PPL_ASSERT(false);
  }
}

void
PPL::Linear_Expression::set_representation(Representation r) {
  if (representation() == r)
    return;
  Linear_Expression tmp(*this, r);
  swap(tmp);
}

PPL_OUTPUT_DEFINITIONS(Linear_Expression)
