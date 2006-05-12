/* Direct_Product class implementation: inline functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Direct_Product_inlines_hh
#define PPL_Direct_Product_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::max_space_dimension() {
  return std::min(D1::max_space_dimension(), D2::max_space_dimension());
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Congruence_System& ccgs)
  : d1(ccgs), d2(ccgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Congruence_System& cgs)
  : d1(const_cast<const Congruence_System&>(cgs)), d2(cgs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(const Grid_Generator_System& gs)
  : d1(gs), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::Direct_Product(Grid_Generator_System& gs)
  : d1(const_cast<const Grid_Generator_System&>(gs)), d2(gs) {
}

template <typename D1, typename D2>
inline
Direct_Product<D1, D2>::~Direct_Product() {
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::external_memory_in_bytes() const {
  return d1.external_memory_in_bytes() + d2.external_memory_in_bytes();
}

template <typename D1, typename D2>
inline memory_size_type
Direct_Product<D1, D2>::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

template <typename D1, typename D2>
inline dimension_type
Direct_Product<D1, D2>::space_dimension() const {
  assert(d1.space_dimension() == d2.space_dimension());
  return d1.space_dimension();
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::upper_bound_assign(const Direct_Product& y) {
  d1.upper_bound_assign(y.d1);
  d2.upper_bound_assign(y.d2);
}

template <typename D1, typename D2>
inline void
Direct_Product<D1, D2>::swap(Direct_Product& y) {
  std::swap(d1, y.d1);
  std::swap(d2, y.d2);
}

} // namespace Parma_Polyhedra_Library

/*! \relates Parma_Polyhedra_Library::Direct_Product */
template <typename D1, typename D2>
inline void
std::swap(Parma_Polyhedra_Library::Direct_Product<D1, D2>& x,
	  Parma_Polyhedra_Library::Direct_Product<D1, D2>& y) {
  x.swap(y);
}

#endif // !defined(PPL_Direct_Product_inlines_hh)
