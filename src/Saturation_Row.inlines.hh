/* Saturation_Row class implementation: inline functions.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Saturation_Row_inlines_hh
#define PPL_Saturation_Row_inlines_hh 1

// For the declaration of ffs(3).
#include <cstring>

namespace Parma_Polyhedra_Library {

inline
Saturation_Row::Saturation_Row() {
  mpz_init(vec);
}

inline
Saturation_Row::Saturation_Row(const Saturation_Row& y) {
  mpz_init_set(vec, y.vec);
}

inline
Saturation_Row::~Saturation_Row() {
  mpz_clear(vec);
}

inline Saturation_Row&
Saturation_Row::operator=(const Saturation_Row& y) {
  mpz_set(vec, y.vec);
  return *this;
}

inline void
Saturation_Row::set(const unsigned int k) {
  mpz_setbit(vec, k);
}

inline void
Saturation_Row::clear(const unsigned int k) {
  mpz_clrbit(vec, k);
}

inline void
Saturation_Row::clear_from(const unsigned int k) {
  mpz_tdiv_r_2exp(vec, vec, k);
}

inline unsigned int
Saturation_Row::count_ones() const {
  size_t vec_size = mpz_size(vec);
  assert(vec_size >= 0);
  return mpn_popcount(vec->_mp_d, vec_size);
}

inline bool
Saturation_Row::empty() const {
  return mpz_sgn(vec) == 0;
}

inline void
Saturation_Row::swap(Saturation_Row& y) {
  mpz_swap(vec, y.vec);
}

inline void
Saturation_Row::clear() {
  mpz_set_ui(vec, 0UL);
}

inline memory_size_type
Saturation_Row::external_memory_in_bytes() const {
  return vec[0]._mp_alloc * SIZEOF_MP_LIMB_T;
}

inline memory_size_type
Saturation_Row::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

#if HAVE_DECL_FFS && SIZEOF_MP_LIMB_T == SIZEOF_INT

inline unsigned int
Saturation_Row::first_one(mp_limb_t w) {
  return ffs(w)-1;
}

#endif

/*! \relates Saturation_Row */
inline void
set_union(const Saturation_Row& x, const Saturation_Row& y,
	  Saturation_Row& z) {
  mpz_ior(z.vec, x.vec, y.vec);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
inline void
swap(Parma_Polyhedra_Library::Saturation_Row& x,
     Parma_Polyhedra_Library::Saturation_Row& y) {
  x.swap(y);
}

/*! \relates Parma_Polyhedra_Library::Saturation_Row */
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::Saturation_Row>::iterator x,
	  std::vector<Parma_Polyhedra_Library::Saturation_Row>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_Saturation_Row_inlines_hh)
