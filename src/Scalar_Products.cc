/* Scalar_Products class implementation (non-inline functions).
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

#include "Scalar_Products.defs.hh"
#include "Scalar_Products.inlines.hh"
#include "Coefficient.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Linear_Expression& x,
                             const Linear_Expression& y) {
  x.scalar_product_assign(z, y);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
                             const Constraint& x, const Generator& y) {
  assign(z, x.expression(), y.expression());
}

void
PPL::Scalar_Products::assign(Coefficient& z,
                             const Generator& x, const Constraint& y) {
  assign(z, x.expression(), y.expression());
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Grid_Generator& x, const Congruence& y) {
  x.expression().scalar_product_assign(z, y.expression(), 0, x.space_dimension() + 1);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Constraint& x,
			     const Grid_Generator& y) {
  assign(z, x.expression(), y.expression());
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Congruence& x, const Grid_Generator& y) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.space_dimension() <= y.space_dimension());
  x.expression().scalar_product_assign(z, y.expression());
}

void
PPL::Scalar_Products::reduced_assign(Coefficient& z,
				     const Linear_Expression& x,
				     const Linear_Expression& y) {
  // The reduced scalar product is only defined
  // if `y' has enough coefficients.
  PPL_ASSERT(x.space_dimension() - 1 <= y.space_dimension());
  x.scalar_product_assign(z, y, 0, x.space_dimension());
}

void
PPL::Scalar_Products::reduced_assign(Coefficient& z,
				     const Grid_Generator& x,
				     const Congruence& y) {
  // The reduced scalar product is only defined if the topology of `x'
  // is NNC and `y' has enough coefficients.
  PPL_ASSERT(x.space_dimension() <= y.space_dimension());
  x.expression().scalar_product_assign(z, y.expression(), 0, x.space_dimension());
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Linear_Expression& x,
					 const Linear_Expression& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.space_dimension() <= y.space_dimension());
  x.scalar_product_assign(z, y, 1, x.space_dimension() + 1);
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Grid_Generator& x,
					 const Congruence& y) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.space_dimension() <= y.space_dimension());
  x.expression().scalar_product_assign(z, y.expression(), 1, x.space_dimension() + 1);
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Grid_Generator& x,
					 const Constraint& y) {
  // Scalar product is only defined if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.space_dimension() <= y.space_dimension());
  x.expression().scalar_product_assign(z, y.expression(), 1, x.space_dimension() + 1);
}
