/* Implementation of global objects: inline functions.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_globals_inlines_hh
#define PPL_globals_inlines_hh 1

#include "Coefficient.defs.hh"
#include <limits>
#include <cassert>

namespace Parma_Polyhedra_Library {

inline dimension_type
not_a_dimension() {
  return std::numeric_limits<dimension_type>::max();
}

inline void
maybe_abandon() {
  if (const Throwable* p = abandon_expensive_computations)
    p->throw_me();
}

inline dimension_type
compute_capacity(const dimension_type requested_size,
		 const dimension_type maximum_size) {
  assert(requested_size <= maximum_size);
  // Speculation factor 2.
  return (requested_size < maximum_size / 2)
    ? 2*(requested_size + 1)
    : maximum_size;
  // Speculation factor 1.5.
  // return (maximum_size - requested_size > requested_size/2)
  //   ? requested_size + requested_size/2 + 1
  //   : maximum_size;
}

inline dimension_type
compute_capacity(const dimension_type requested_size) {
  // Speculation factor 2.
  return 2*(requested_size + 1);
  // Speculation factor 1.5.
  // return requested_size + requested_size/2 + 1;
}

inline void
normalize2(const Coefficient& x, const Coefficient& y,
	   Coefficient& nx, Coefficient& ny) {
  TEMP_INTEGER(gcd);
  gcd_assign(gcd, x, y);
  exact_div_assign(nx, x, gcd);
  exact_div_assign(ny, y, gcd);
}

template <typename T>
inline T
low_bits_mask(unsigned n) {
  assert(n < unsigned(std::numeric_limits<T>::digits));
  return n == 0 ? 0 : ~(~(T(0u)) << n);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_globals_inlines_hh)
