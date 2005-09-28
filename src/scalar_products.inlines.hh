/* Definition of various scalar product functions (inline functions).
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

#ifndef PPL_scalar_products_inlines_hh
#define PPL_scalar_products_inlines_hh 1

#include "Linear_Row.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.defs.hh"
#include "Generator.defs.hh"

namespace Parma_Polyhedra_Library {

inline int
scalar_product_sign(const Linear_Row& x, const Linear_Row& y) {
  TEMP_INTEGER(z);
  scalar_product_assign(z, x, y);
  return sgn(z);
}

inline int
reduced_scalar_product_sign(const Linear_Row& x, const Linear_Row& y) {
  TEMP_INTEGER(z);
  reduced_scalar_product_assign(z, x, y);
  return sgn(z);
}

inline int
homogeneous_scalar_product_sign(const Linear_Row& x, const Linear_Row& y) {
  TEMP_INTEGER(z);
  homogeneous_scalar_product_assign(z, x, y);
  return sgn(z);
}

inline int
scalar_product_sign(const Constraint& x, const Generator& y) {
  return scalar_product_sign(static_cast<const Linear_Row&>(x),
			     static_cast<const Linear_Row&>(y));
}

inline int
reduced_scalar_product_sign(const Constraint& x, const Generator& y) {
  return reduced_scalar_product_sign(static_cast<const Linear_Row&>(x),
				     static_cast<const Linear_Row&>(y));
}

inline void
homogeneous_scalar_product_assign(Coefficient& z,
				  const Linear_Expression& x,
				  const Generator& y) {
  homogeneous_scalar_product_assign(z,
				    static_cast<const Linear_Row&>(x),
				    static_cast<const Linear_Row&>(y));
}

inline int
homogeneous_scalar_product_sign(const Linear_Expression& x,
				const Generator& y) {
  return homogeneous_scalar_product_sign(static_cast<const Linear_Row&>(x),
					 static_cast<const Linear_Row&>(y));
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_scalar_products_inlines_hh)
