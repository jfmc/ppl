/* Definition of various scalar product functions (non-inline functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
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

#include <config.h>

#include "scalar_products.defs.hh"
#include "Coefficient.defs.hh"

namespace PPL = Parma_Polyhedra_Library;

/*! \relates Parma_Polyhedra_Library::Linear_Row */
void
PPL::scalar_product_assign(Coefficient& z,
			   const Linear_Row& x, const Linear_Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size());
  z = 0;
  for (dimension_type i = x.size(); i-- > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Linear_Row */
void
PPL::reduced_scalar_product_assign(Coefficient& z,
				   const Linear_Row& x,
				   const Linear_Row& y) {
  // The reduced scalar product is only defined
  // if the topology of `x' is NNC and `y' has enough coefficients.
  assert(!x.is_necessarily_closed());
  assert(x.size() - 1 <= y.size());
  z = 0;
  for (dimension_type i = x.size() - 1; i-- > 0; )
    // The following line optimizes the computation
    // of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}

/*! \relates Parma_Polyhedra_Library::Linear_Row */
void
PPL::homogeneous_scalar_product_assign(Coefficient& z,
				       const Linear_Row& x,
				       const Linear_Row& y) {
  // Scalar product is only defined  if `x' and `y' are
  // dimension-compatible.
  assert(x.size() <= y.size());
  z = 0;
    // Note the pre-decrement of `i': last iteration should be for `i == 1'.
  for (dimension_type i = x.size(); --i > 0; )
    // The following line optimizes the computation of z += x[i] * y[i].
    add_mul_assign(z, x[i], y[i]);
}
