/* Inline functions for the Interval class and its constituents.
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

#ifndef PPL_Interval_inlines_hh
#define PPL_Interval_inlines_hh 1

#include <cassert>
#include "Checked_Number.defs.hh"
#include "checked_mpz.inlines.hh"

namespace Parma_Polyhedra_Library {

inline
Boundary::Boundary(const ERational& v, Flag f)
  : value(v), flag(f) {
}

inline bool
Boundary::is_closed() const {
  return flag == ZERO;
}

inline const ERational&
Boundary::bound() const {
  return value;
}

inline ERational&
Boundary::bound() {
  return value;
}

inline
LBoundary::LBoundary(const ERational& v, Open_Closed f)
  : Boundary(v, f == CLOSED ? ZERO : POS) {
}

inline
UBoundary::UBoundary(const ERational& v, Open_Closed f)
  : Boundary(v, f == CLOSED ? ZERO : NEG) {
}

/*! \relates Boundary */
inline bool
operator<(const Boundary& x, const Boundary& y) {
  return x.value < y.value ||
    (x.value == y.value && x.flag < y.flag);
}

/*! \relates Boundary */
inline bool
operator>(const Boundary& x, const Boundary& y) {
  return y < x;
}

inline
Interval::Interval()
  : lower(ERational(MINUS_INFINITY), LBoundary::OPEN),
    upper(ERational(PLUS_INFINITY), UBoundary::OPEN) {
}

inline bool
Interval::is_empty() const {
  return lower > upper;
}

inline const LBoundary&
Interval::lower_bound() const {
  return lower;
}

inline LBoundary&
Interval::lower_bound() {
  return lower;
}

inline const UBoundary&
Interval::upper_bound() const {
  return upper;
}

inline UBoundary&
Interval::upper_bound() {
  return upper;
}

inline void
Interval::raise_lower_bound(LBoundary new_lower) {
  if (new_lower > lower)
    lower = new_lower;
}

inline void
Interval::lower_upper_bound(UBoundary new_upper) {
  if (new_upper < upper)
    upper = new_upper;
}

inline void
Interval::set_empty() {
  lower = LBoundary(ERational(PLUS_INFINITY), LBoundary::OPEN);
  upper = UBoundary(ERational(MINUS_INFINITY), UBoundary::OPEN);
  assert(is_empty());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Interval_inlines_hh)
