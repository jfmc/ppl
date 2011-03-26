/* Linear_Row class implementation: inline functions.
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

#ifndef PPL_Linear_Row_inlines_hh
#define PPL_Linear_Row_inlines_hh 1

// TODO: Remove this.
// It was added to please KDevelop4.
#include "Linear_Row.defs.hh"

#include "globals.defs.hh"
#include "assert.hh"
#include "math_utilities.defs.hh"
#include <algorithm>

namespace Parma_Polyhedra_Library {

inline
Linear_Row::Linear_Row()
  : Linear_Expression() {
}

inline
Linear_Row::Linear_Row(const dimension_type sz, const dimension_type /* capacity */)
  : Linear_Expression() {
  get_row().resize(sz);
}

inline
Linear_Row::Linear_Row(const dimension_type sz)
  : Linear_Expression() {
  get_row().resize(sz);
}

inline
Linear_Row::Linear_Row(const Linear_Row& y)
  : Linear_Expression(y) {
}

inline
Linear_Row::Linear_Row(const Linear_Row& y,
                       const dimension_type size)
  : Linear_Expression(y, size) {
}

inline
Linear_Row::Linear_Row(const Linear_Row& y,
		       const dimension_type sz, const dimension_type /* capacity */)
  : Linear_Expression(y, sz) {
}

inline
Linear_Row::~Linear_Row() {
}

inline void
Linear_Row::swap(Linear_Row& y) {
  Linear_Expression::swap(y);
}

inline void
Linear_Row::swap(dimension_type i, dimension_type j) {
  get_row().swap(i, j);
}

inline Coefficient_traits::const_reference
Linear_Row::inhomogeneous_term() const {
  return get_row()[0];
}

inline Coefficient_traits::const_reference
Linear_Row::coefficient(const dimension_type k) const {
  return get_row()[k+1];
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Linear_Row */
inline void
swap(Parma_Polyhedra_Library::Linear_Row& x,
     Parma_Polyhedra_Library::Linear_Row& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Linear_Row_inlines_hh)
