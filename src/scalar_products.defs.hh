/* Declaration of various scalar product functions.
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

#ifndef PPL_scalar_products_defs_hh
#define PPL_scalar_products_defs_hh 1

#include "Linear_Row.types.hh"
#include "Coefficient.types.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Computes the scalar product of \p x and \p y and assigns it to \p z.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void scalar_product_assign(Coefficient& z,
			   const Linear_Row& x, const Linear_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns the sign of the scalar product between \p x and \p y.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int scalar_product_sign(const Linear_Row& x, const Linear_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns the sign of the scalar product between \p x and \p y.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int scalar_product_sign(const Constraint& x, const Generator& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e reduced scalar product of \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored,
//! and assigns the result to \p z.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void reduced_scalar_product_assign(Coefficient& z,
				   const Linear_Row& x, const Linear_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the sign of the \e reduced scalar product of \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int reduced_scalar_product_sign(const Linear_Row& x, const Linear_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the sign of the \e reduced scalar product of \p x and \p y,
//! where the \f$\epsilon\f$ coefficient of \p x is ignored.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int reduced_scalar_product_sign(const Constraint& x, const Generator& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Computes the \e homogeneous scalar product of \p x and \p y,
//! where the inhomogeneous terms are ignored,
//! and assigns the result to \p z.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
void homogeneous_scalar_product_assign(Coefficient& z,
				       const Linear_Row& x,
				       const Linear_Row& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns the sign of the \e homogeneous scalar product of \p x and \p y,
//! where the inhomogeneous terms are ignored,
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
int homogeneous_scalar_product_sign(const Linear_Row& x, const Linear_Row& y);

} // namespace Parma_Polyhedra_Library

#include "scalar_products.inlines.hh"

#endif // !defined(PPL_scalar_products_defs_hh)
