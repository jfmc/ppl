/* Inline functions for the Old_Interval class and its constituents.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Old_Interval_inlines_hh
#define PPL_Old_Interval_inlines_hh 1

#include <cassert>
#include "Checked_Number.defs.hh"
#include "checked_mpz.inlines.hh"

namespace Parma_Polyhedra_Library {

inline
Old_Boundary::Old_Boundary(const ERational& v, Flag f)
  : value(v), flag(f) {
}

inline bool
Old_Boundary::is_closed() const {
  return flag == ZERO;
}

inline const ERational&
Old_Boundary::bound() const {
  return value;
}

inline ERational&
Old_Boundary::bound() {
  return value;
}

inline
LBoundary::LBoundary(const ERational& v, Open_Closed f)
  : Old_Boundary(v, f == CLOSED ? ZERO : POS) {
}

inline
UBoundary::UBoundary(const ERational& v, Open_Closed f)
  : Old_Boundary(v, f == CLOSED ? ZERO : NEG) {
}

/*! \relates Old_Boundary */
inline bool
operator<(const Old_Boundary& x, const Old_Boundary& y) {
  return x.value < y.value ||
    (x.value == y.value && x.flag < y.flag);
}

/*! \relates Old_Boundary */
inline bool
operator>(const Old_Boundary& x, const Old_Boundary& y) {
  return y < x;
}

inline
Old_Interval::Old_Interval()
  : lower(ERational(MINUS_INFINITY), LBoundary::OPEN),
    upper(ERational(PLUS_INFINITY), UBoundary::OPEN) {
}

inline bool
Old_Interval::is_empty() const {
  return lower > upper;
}

inline const LBoundary&
Old_Interval::lower_bound() const {
  return lower;
}

inline LBoundary&
Old_Interval::lower_bound() {
  return lower;
}

inline const UBoundary&
Old_Interval::upper_bound() const {
  return upper;
}

inline UBoundary&
Old_Interval::upper_bound() {
  return upper;
}

inline void
Old_Interval::raise_lower_bound(LBoundary new_lower) {
  if (new_lower > lower)
    lower = new_lower;
}

inline void
Old_Interval::lower_upper_bound(UBoundary new_upper) {
  if (new_upper < upper)
    upper = new_upper;
}

inline void
Old_Interval::set_empty() {
  lower = LBoundary(ERational(PLUS_INFINITY), LBoundary::OPEN);
  upper = UBoundary(ERational(MINUS_INFINITY), UBoundary::OPEN);
  assert(is_empty());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Old_Interval_inlines_hh)
