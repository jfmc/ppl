/* SatRow class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_SatRow_inlines_hh
#define PPL_SatRow_inlines_hh 1

#include <vector>

namespace Parma_Polyhedra_Library {

inline
SatRow::SatRow() {
  mpz_init(vec);
}

inline
SatRow::SatRow(const SatRow& y) {
  mpz_init_set(vec, y.vec);
}

inline
SatRow::~SatRow() {
  mpz_clear(vec);
}

inline SatRow&
SatRow::operator=(const SatRow& y) {
  mpz_set(vec, y.vec);
  return *this;
}

inline bool
SatRow::operator[](dimension_type k) const {
  return mpz_tstbit(vec, k);
}

inline void
SatRow::set(dimension_type k) {
  mpz_setbit(vec, k);
}

inline void
SatRow::clear(dimension_type k) {
  mpz_clrbit(vec, k);
}

inline void
SatRow::clear_from(dimension_type k) {
  mpz_tdiv_r_2exp(vec, vec, k);
}

/*!
  Returns the number of set bits in the row.
*/
inline dimension_type
SatRow::count_ones() const {
  return mpz_popcount(vec);
}

/*!
  Returns <CODE>true</CODE> if no bit is set in the row.
*/
inline bool
SatRow::empty() const {
  return mpz_sgn(vec) == 0;
}

/*!
  Swaps \p *this with \p y.
*/
inline void
SatRow::swap(SatRow& y) {
  mpz_swap(vec, y.vec);
}

/*!
  Clears all the bit of the row.
*/
inline void
SatRow::clear() {
  mpz_set_ui(vec, 0UL);
}

inline bool
operator==(const SatRow& x, const SatRow& y) {
  return mpz_cmp(x.vec, y.vec) == 0;
}

inline bool
operator!=(const SatRow& x, const SatRow& y) {
  return mpz_cmp(x.vec, y.vec) != 0;
}

inline bool
operator>(const SatRow& x, const SatRow& y) {
  return y < x;
}

inline bool
operator>=(const SatRow& x, const SatRow& y) {
  return y <= x;
}

inline void
set_union(const SatRow& x, const SatRow& y, SatRow& z) {
  mpz_ior(z.vec, x.vec, y.vec);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::SatRow */
inline void
swap(Parma_Polyhedra_Library::SatRow& x, Parma_Polyhedra_Library::SatRow& y) {
  x.swap(y);
}

/*!
  \relates Parma_Polyhedra_Library::SatRow
  Specializes <CODE>std::iter_swap</CODE>.
*/
/* FIXME: this was the original comment causing a doxygen bug.
  Specializes <CODE>std::iter_swap</CODE>
  for <CODE>std::vector<SatRow>::iterator</CODE>.
*/
inline void
iter_swap(std::vector<Parma_Polyhedra_Library::SatRow>::iterator x,
	  std::vector<Parma_Polyhedra_Library::SatRow>::iterator y) {
  swap(*x, *y);
}

} // namespace std

#endif // !defined(PPL_SatRow_inlines_hh)
