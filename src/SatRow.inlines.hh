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

INLINE
Parma_Polyhedra_Library::SatRow::SatRow() {
}

INLINE
Parma_Polyhedra_Library::SatRow::SatRow(const SatRow& y)
  : vec(y.vec) {
}

INLINE
Parma_Polyhedra_Library::SatRow::~SatRow() {
}

INLINE Parma_Polyhedra_Library::SatRow&
Parma_Polyhedra_Library::SatRow::operator =(const SatRow& y) {
  vec = y.vec;
  return *this;
}

INLINE bool
Parma_Polyhedra_Library::SatRow::operator [](size_t k) const {
  return vec.test(k);
}

/*!
  Sets the bit in position \p i.
*/
INLINE void
Parma_Polyhedra_Library::SatRow::set(size_t i) {
  vec.set(i);
}

/*!
  Clears the bit in position \p i.
*/
INLINE void
Parma_Polyhedra_Library::SatRow::clear(size_t i) {
  vec.clear(i);
}

/*!
  Clears bits from position \p i (included) onwards.
*/
INLINE void
Parma_Polyhedra_Library::SatRow::clear_from(size_t i) {
  // FIXME: we ought to provide a better implementation.
  int last_bit = vec.last();
  if (last_bit >= 0 && unsigned(last_bit) >= i)
    vec.clear(i, last_bit);
}

/*!
  Returns the index of the first set bit and 
  -1 if no bit is set.
*/
INLINE int
Parma_Polyhedra_Library::SatRow::first() const {
  return vec.first();
}

/*!
  Returns the index of the first set bit after \p pos 
  and -1 if no bit after pos is set.
*/INLINE int
Parma_Polyhedra_Library::SatRow::next(int pos) const {
  return vec.next(pos);
}

/*!
  Returns the index of the last set bit and -1 if no bit is set.
*/
INLINE int
Parma_Polyhedra_Library::SatRow::last() const {
  return vec.last();
}

/*!
  Returns the index of the first set bit before \p pos 
  and -1 if no bits before pos is set.
*/
INLINE int
Parma_Polyhedra_Library::SatRow::prev(int pos) const {
  return vec.prev(pos);
}

/*!
  Returns the number of set bits in the row.
*/
INLINE size_t
Parma_Polyhedra_Library::SatRow::count_ones() const {
  return vec.count();
}

/*!
  Returns <CODE>true</CODE> if no bit is set in the row.
*/
INLINE bool
Parma_Polyhedra_Library::SatRow::empty() const {
  return vec.empty();
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  Compares \p x with \p y starting from the least significant bits.
  The ordering is total and has the following property: if \p x and \p y
  are two rows seen as sets of naturals, if \p x is a strict subset
  of \p y, then \p x comes before \p y.

  Returns
  - -1 if \p x comes before \p y in the ordering;
  -  0 if \p x and \p y are equal;
  -  1 if \p x comes after \p y in the ordering.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

INLINE int
Parma_Polyhedra_Library::compare(const SatRow& x, const SatRow& y) {
  return lcompare(x.vec, y.vec);
}

/*!
  Swaps \p *this with \p y.
*/
INLINE void
Parma_Polyhedra_Library::SatRow::swap(SatRow& y) {
  std::swap(vec, y.vec);
}

/*!
  Specialize <CODE>std::swap </CODE> to use the fast swap that is 
  provided as a member function instead of using the default 
  algorithm (which creates a temporary and uses assignment).
*/
INLINE void
std::swap(Parma_Polyhedra_Library::SatRow& x,
	  Parma_Polyhedra_Library::SatRow& y) {
  x.swap(y);
}

/*!
  Clears all the bit of the row.
*/
INLINE void
Parma_Polyhedra_Library::SatRow::clear() {
  vec.clear();
}

INLINE bool
Parma_Polyhedra_Library::operator ==(const SatRow& x, const SatRow& y) {
  return x.vec == y.vec;
}

INLINE bool
Parma_Polyhedra_Library::operator !=(const SatRow& x, const SatRow& y) {
  return x.vec != y.vec;
}

INLINE bool
Parma_Polyhedra_Library::operator <(const SatRow& x, const SatRow& y) {
  return x.vec < y.vec;
}

INLINE bool
Parma_Polyhedra_Library::operator >(const SatRow& x, const SatRow& y) {
  return x.vec > y.vec;
}

INLINE bool
Parma_Polyhedra_Library::operator <=(const SatRow& x, const SatRow& y) {
  return x.vec <= y.vec;
}

INLINE bool
Parma_Polyhedra_Library::operator >=(const SatRow& x, const SatRow& y) {
  return x.vec >= y.vec;
}

INLINE void
Parma_Polyhedra_Library::set_union(const SatRow& x,
				   const SatRow& y,
				   SatRow& z) {
  set_union(x.vec, y.vec, z.vec);
}
