/* Scalar_Products class implementation (non-inline functions).
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#include "ppl-config.h"
#include "Scalar_Products.defs.hh"
#include "Coefficient.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Linear_Row& x, const Linear_Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.size() <= y.size());
  z = 0;
  for (dimension_type i = x.size(); i-- > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Constraint& c, const Generator& g) {
  // Scalar product is only defined if `c' and `g' are
  // dimension-compatible.
  PPL_ASSERT(c.size() <= g.size());
  z = 0;
  for (dimension_type i = c.size(); i-- > 0; )
    // The following line optimizes the computation of z += c[i] * g[i].
    add_mul_assign(z, c[i], g[i]);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Grid_Generator& gg, const Congruence& cg) {
  // Scalar product is only defined if `gg' and `cg' are
  // dimension-compatible.
  PPL_ASSERT(gg.size() <= cg.size());
  z = 0;
  for (dimension_type i = gg.size() - 1 /* parameter divisor */; i-- > 0; )
    // The following line optimizes the computation of z += gg[i] *
    // cg[i].
    add_mul_assign(z, gg[i], cg[i]);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Constraint& c,
			     const Grid_Generator& gg) {
  // Scalar product is only defined if `c' and `gg' are
  // dimension-compatible.
  PPL_ASSERT(c.size() <= gg.size());
  z = 0;
  for (dimension_type i = c.size(); i-- > 0; )
    // The following line optimizes the computation of z += c[i] * gg[i].
    add_mul_assign(z, c[i], gg[i]);
}

void
PPL::Scalar_Products::assign(Coefficient& z,
			     const Congruence& cg, const Grid_Generator& gg) {
  // Scalar product is only defined if `cg' and `gg' are
  // dimension-compatible.
  PPL_ASSERT(cg.size() <= gg.size());
  z = 0;
  for (dimension_type i = cg.size() - 1; i-- > 0; )
    // The following line optimizes the computation of z += cg[i] *
    // gg[i].
    add_mul_assign(z, cg[i], gg[i]);
}

void
PPL::Scalar_Products::reduced_assign(Coefficient& z,
				     const Linear_Row& x,
				     const Linear_Row& y) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  PPL_ASSERT(!x.is_necessarily_closed());
  PPL_ASSERT(x.size() - 1 <= y.size());
  z = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

void
PPL::Scalar_Products::reduced_assign(Coefficient& z,
				     const Grid_Generator& gg,
				     const Congruence& cg) {
  // The reduced scalar product is only defined if the topology of `gg'
  // is NNC and `cg' has enough coefficients.
  PPL_ASSERT(gg.size() <= cg.size());
  z = 0;
  for (dimension_type i = gg.size() - 1; i-- > 0; )
    // The following line optimizes z += gg[i] * cg[i].
    add_mul_assign(z, gg[i], cg[i]);
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Linear_Row& x,
					 const Linear_Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  PPL_ASSERT(x.size() <= y.size());
  z = 0;
  // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = x.size(); --i > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Grid_Generator& gg,
					 const Congruence& cg) {
  // Scalar product is only defined if `gg' and `cg' are
  // dimension-compatible.
  PPL_ASSERT(gg.size() <= cg.size());
  z = 0;
  // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = gg.size() - 1; --i > 0; )
    // The following line optimizes the computation of z += gg[i] * cg[i].
    add_mul_assign(z, gg[i], cg[i]);
}

void
PPL::Scalar_Products::homogeneous_assign(Coefficient& z,
					 const Grid_Generator& gg,
					 const Constraint& c) {
  // Scalar product is only defined if `gg' and `c' are
  // dimension-compatible.
  PPL_ASSERT(gg.size() - 1 <= c.size());
  z = 0;
  // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = gg.size() - 1; --i > 0; )
    // The following line optimizes the computation of z += gg[i] * c[i].
    add_mul_assign(z, gg[i], c[i]);
}
