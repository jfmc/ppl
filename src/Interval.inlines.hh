/* Inline functions for the Interval class and its constituents.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Interval_inlines_hh
#define PPL_Interval_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

inline
ERational::ERational(const Integer& num, const Integer& den)
  : e(0), v(num, den) {
  v.canonicalize();
}

inline
ERational::ERational(char sign)
  : e(sign == '+' ? 1 : -1) {
  assert(sign == '+' || sign == '-');
}

inline
ERational::ERational(const ERational& y)
  : e(y.e) {
  if (e == 0)
    v = y.v;
}

inline ERational&
ERational::operator=(const ERational& y) {
  e = y.e;
  if (e == 0)
    v = y.v;
  return *this;
}

inline int
ERational::direction_of_infinity() const {
  return e;
}

inline const Integer&
ERational::numerator() const {
  return v.get_num();
}

inline const Integer&
ERational::denominator() const {
  return v.get_den();
}

inline bool
operator==(const ERational& x, const ERational& y) {
  return x.e == y.e && (x.e != 0 || x.v == y.v);
}

inline bool
operator!=(const ERational& x, const ERational& y) {
  return !(x == y);
}

inline bool
operator<(const ERational& x, const ERational& y) {
  return x.e < y.e || (x.e == 0 && y.e == 0 && x.v < y.v);
}

inline bool
operator>(const ERational& x, const ERational& y) {
  return y < x;
}

inline bool
operator<=(const ERational& x, const ERational& y) {
  return x < y || x == y;
}

inline bool
operator>=(const ERational& x, const ERational& y) {
  return y <= x;
}

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

inline
LBoundary::LBoundary()
  : Boundary(ERational('-'), POS) {
}

inline
LBoundary::LBoundary(const ERational& v, OpenClosed f)
  : Boundary(v, f == CLOSED ? ZERO : POS) {
}

inline
UBoundary::UBoundary()
  : Boundary(ERational('+'), NEG) {
}

inline
UBoundary::UBoundary(const ERational& v, OpenClosed f)
  : Boundary(v, f == CLOSED ? ZERO : NEG) {
}

inline bool
operator==(const Boundary& x, const Boundary& y) {
  return x.value == y.value && x.flag == y.flag;
}

inline bool
operator!=(const Boundary& x, const Boundary& y) {
  return !(x == y);
}

inline bool
operator<(const Boundary& x, const Boundary& y) {
  return x.value < y.value ||
    (x.value == y.value && x.flag < y.flag);
}

inline bool
operator<(const LBoundary& x, const UBoundary& y) {
  return x.value < y.value;
}

inline bool
operator>(const Boundary& x, const Boundary& y) {
  return x.value > y.value ||
    (x.value == y.value && x.flag > y.flag);
}

inline bool
operator>(const UBoundary& x, const LBoundary& y) {
  return x.value > y.value;
}

inline bool
operator<=(const Boundary& x, const Boundary& y) {
  return !(x > y);
}

inline bool
operator<=(const UBoundary& x, const LBoundary& y) {
  return !(x > y);
}

inline bool
operator>=(const Boundary& x, const Boundary& y) {
  return !(x < y);
}

inline bool
operator>=(const LBoundary& x, const UBoundary& y) {
  return !(x < y);
}

inline bool
operator==(const Boundary& x, const ERational& y) {
  return x.value == y && x.flag == Boundary::ZERO;
}

inline bool
operator!=(const Boundary& x, const ERational& y) {
  return !(x == y);
}

inline bool
operator<(const Boundary& x, const ERational& y) {
  return x.value < y ||
    (x.value == y && x.flag < Boundary::ZERO);
}

inline bool
operator<(const LBoundary& x, const ERational& y) {
  return x.value < y;
}

inline bool
operator>(const Boundary& x, const ERational& y) {
  return x.value > y ||
    (x.value == y && x.flag > Boundary::ZERO);
}

inline bool
operator>(const UBoundary& x, const ERational& y) {
  return x.value > y;
}

inline bool
operator<=(const Boundary& x, const ERational& y) {
  return !(x > y);
}

inline bool
operator<=(const UBoundary& x, const ERational& y) {
  return !(x > y);
}

inline bool
operator>=(const Boundary& x, const ERational& y) {
  return !(x < y);
}

inline bool
operator>=(const LBoundary& x, const ERational& y) {
  return !(x < y);
}

inline
Interval::Interval() {
}

inline bool
Interval::is_empty() const {
  return lower > upper;
}

inline const LBoundary&
Interval::lower_bound() const {
  return lower;
}

inline const UBoundary&
Interval::upper_bound() const {
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
  lower = LBoundary(ERational('+'), LBoundary::OPEN);
  upper = UBoundary(ERational('-'), UBoundary::OPEN);
  assert(is_empty());
}

} // namespace Parma_Polyhedra_Library

#endif
