/* SatRow class implementation: inline functions.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

inline
Parma_Polyhedra_Library::SatRow::SatRow() {
  mpz_init(vec);
}

inline
Parma_Polyhedra_Library::SatRow::SatRow(const SatRow& y) {
  mpz_init_set(vec, y.vec);
}

inline
Parma_Polyhedra_Library::SatRow::~SatRow() {
  mpz_clear(vec);
}

inline Parma_Polyhedra_Library::SatRow&
Parma_Polyhedra_Library::SatRow::operator =(const SatRow& y) {
  mpz_set(vec, y.vec);
  return *this;
}

inline bool
Parma_Polyhedra_Library::SatRow::operator [](size_t k) const {
  return mpz_tstbit(vec, k);
}

inline void
Parma_Polyhedra_Library::SatRow::set(size_t k) {
  mpz_setbit(vec, k);
}

inline void
Parma_Polyhedra_Library::SatRow::clear(size_t k) {
  mpz_clrbit(vec, k);
}

inline void
Parma_Polyhedra_Library::SatRow::clear_from(size_t k) {
  // FIXME: we ought to provide a better implementation.
  for (int i = k; i >= 0; i = next(i))
    clear(i);
}

/*!
  Returns the number of set bits in the row.
*/
inline size_t
Parma_Polyhedra_Library::SatRow::count_ones() const {
  return mpz_popcount(vec);
}

/*!
  Returns <CODE>true</CODE> if no bit is set in the row.
*/
inline bool
Parma_Polyhedra_Library::SatRow::empty() const {
  return mpz_sgn(vec) == 0;
}

/*!
  Swaps \p *this with \p y.
*/
inline void
Parma_Polyhedra_Library::SatRow::swap(SatRow& y) {
  mpz_swap(vec, y.vec);
}

/*!
  Specialize <CODE>std::swap </CODE> to use the fast swap that is
  provided as a member function instead of using the default
  algorithm (which creates a temporary and uses assignment).
*/
inline void
std::swap(Parma_Polyhedra_Library::SatRow& x,
	  Parma_Polyhedra_Library::SatRow& y) {
  x.swap(y);
}

/*!
  Clears all the bit of the row.
*/
inline void
Parma_Polyhedra_Library::SatRow::clear() {
  mpz_set_ui(vec, 0UL);
}

inline bool
Parma_Polyhedra_Library::operator ==(const SatRow& x, const SatRow& y) {
  return mpz_cmp(x.vec, y.vec) == 0;
}

inline bool
Parma_Polyhedra_Library::operator !=(const SatRow& x, const SatRow& y) {
  return mpz_cmp(x.vec, y.vec) != 0;
}

inline bool
Parma_Polyhedra_Library::operator >(const SatRow& x, const SatRow& y) {
  return y < x;
}

inline bool
Parma_Polyhedra_Library::operator >=(const SatRow& x, const SatRow& y) {
  return y <= x;
}

inline void
Parma_Polyhedra_Library::set_union(const SatRow& x,
				   const SatRow& y,
				   SatRow& z) {
  mpz_ior(z.vec, x.vec, y.vec);
}
