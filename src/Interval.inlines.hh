/* Inline functions for the Interval class and its constituents.
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

#ifndef _Interval_inlines_hh
#define _Interval_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

inline
ExtendedRational::ExtendedRational(const mpq_class& n)
  : e(0), v(n) {
}

inline
ExtendedRational::ExtendedRational(const Integer& num, const Integer& den)
  : e(0), v(num, den) {
}

inline
ExtendedRational::ExtendedRational(char sign)
  : e(sign == '+' ? 1 : -1) {
  assert(sign == '+' || sign == '-');
}

inline int
ExtendedRational::direction_of_infinity() const {
  return e;
}

#if 0
inline const Integer&
ExtendedRational::numerator() const {
  return v.get_num();
}

inline const Integer&
ExtendedRational::denominator() const {
  return v.get_den();
}
#else
inline void
ExtendedRational::canonicalize() const {
  const_cast<ExtendedRational*>(this)->v.canonicalize();
}

inline Integer
ExtendedRational::numerator() const {
  canonicalize();
  return v.get_num();
}

inline Integer
ExtendedRational::denominator() const {
  canonicalize();
  return v.get_den();
}
#endif

inline bool
operator==(const ExtendedRational& x, const ExtendedRational& y) {
  return x.e == y.e && (x.e != 0 || x.v == y.v);
}

inline bool
operator!=(const ExtendedRational& x, const ExtendedRational& y) {
  return !(x == y);
}

inline bool
operator<(const ExtendedRational& x, const ExtendedRational& y) {
  return x.e < y.e || (x.e == 0 && y.e == 0 && x.v < y.v);
}

inline bool
operator>(const ExtendedRational& x, const ExtendedRational& y) {
  return y < x;
}

inline bool
operator<=(const ExtendedRational& x, const ExtendedRational& y) {
  return x < y || x == y;
}

inline bool
operator>=(const ExtendedRational& x, const ExtendedRational& y) {
  return y <= x;
}

inline
Boundary::Boundary(const ExtendedRational& v, Flag f)
  : value(v), flag(f) {
}

inline bool
Boundary::is_closed() const {
  return flag == ZERO;
}

inline const ExtendedRational&
Boundary::bound() const {
  return value;
}

inline
LBoundary::LBoundary()
  : Boundary(ExtendedRational('-'), POS) {
}

inline
LBoundary::LBoundary(const ExtendedRational& v, OpenClosed f)
  : Boundary(v, f == CLOSED ? ZERO : POS) {
}

inline
UBoundary::UBoundary()
  : Boundary(ExtendedRational('+'), NEG) {
}

inline
UBoundary::UBoundary(const ExtendedRational& v, OpenClosed f)
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
operator==(const Boundary& x, ExtendedRational y) {
  return x.value == y && x.flag == Boundary::ZERO;
}

inline bool
operator!=(const Boundary& x, ExtendedRational y) {
  return !(x == y);
}

inline bool
operator<(const Boundary& x, ExtendedRational y) {
  return x.value < y ||
    (x.value == y && x.flag < Boundary::ZERO);
}

inline bool
operator<(const LBoundary& x, ExtendedRational y) {
  return x.value < y;
}

inline bool
operator>(const Boundary& x, ExtendedRational y) {
  return x.value > y ||
    (x.value == y && x.flag > Boundary::ZERO);
}

inline bool
operator>(const UBoundary& x, ExtendedRational y) {
  return x.value > y;
}

inline bool
operator<=(const Boundary& x, ExtendedRational y) {
  return !(x > y);
}

inline bool
operator<=(const UBoundary& x, ExtendedRational y) {
  return !(x > y);
}

inline bool
operator>=(const Boundary& x, ExtendedRational y) {
  return !(x < y);
}

inline bool
operator>=(const LBoundary& x, ExtendedRational y) {
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
  lower = LBoundary(ExtendedRational('+'), LBoundary::OPEN);
  upper = UBoundary(ExtendedRational('-'), UBoundary::OPEN);
  assert(is_empty());
}

} // namespace Parma_Polyhedra_Library

#endif
