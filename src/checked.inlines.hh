/* Abstract checked arithmetic functions: fallbacks.
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

#include <cassert>

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename Policy, typename Type>
struct FUNCTION_CLASS(assign)<Policy, Type, Type> {
  static inline Result function(Type& to, const Type& from, const Rounding&) {
    to = from;
    return V_EQ;
  }
};

template <typename Policy, typename To, typename From>
inline Result
abs_generic(To& to, const From& from, const Rounding& mode) {
  if (from < 0)
    return neg<Policy>(to, from, mode);
  to = from;
  return V_EQ;
}

inline Result
neg(Result r) {
  assert(!is_special(r));
  Result ret = static_cast<Result>(r & V_EQ);
  if (r & V_LT)
    ret = static_cast<Result>(ret | V_GT);
  if (r & V_GT)
    ret = static_cast<Result>(ret | V_LT);
  return ret;
}

inline Result
add(Result r1, Result r2) {
  assert(!is_special(r1));
  assert(!is_special(r2));
  if (r1 == V_EQ)
    return r2;
  if (r2 == V_EQ)
    return r1;
  if (((r1 & V_LT) && (r2 & V_GT))
      || ((r1 & V_GT) && (r2 & V_LT)))
    return V_LGE;
  return static_cast<Result>((((r1 & r2) & V_EQ) ? V_EQ : 0) |
			       (r1 & (V_LT | V_GT)));
}

inline Result
sub(Result r1, Result r2) {
  return add(r1, neg(r2));
}

template <typename Policy, typename To, typename From>
inline Result
gcd_common(To& to, const From& x, const From& y, const Rounding& mode) {
  To nx = x;
  To ny = y;
  To rm;
  while (ny != 0) {
    Result r = rem<Policy>(rm, nx, ny, mode);
    if (r != V_EQ)
      assert(r == V_EQ);
    nx = ny;
    ny = rm;
  }
  to = nx;
  return V_EQ;
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
gcd_generic(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (x == 0)
    return abs<Policy>(to, y, mode);
  if (y == 0)
    return abs<Policy>(to, x, mode);
  To nx, ny;
  Result r;
  used(r);
  r = abs<Policy>(nx, x, mode);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y, mode);
  assert(r == V_EQ);
  return gcd_common<Policy>(to, nx, ny, mode);
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
lcm_generic(To& to, const From1& x, const From2& y, const Rounding& mode) {
  if (x == 0 || y == 0) {
    to = 0;
    return V_EQ;
  }
  To nx, ny;
  Result r;
  r = abs<Policy>(nx, x, mode);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y, mode);
  assert(r == V_EQ);
  To gcd;
  r = gcd_common<Policy>(gcd, nx, ny, mode);
  assert(r == V_EQ);
  r = div<Policy>(to, nx, gcd, mode);
  assert(r == V_EQ);
  return mul<Policy>(to, to, ny, mode);
}

template <typename Policy, typename Type>
inline Result
sgn_generic(const Type& x) {
  if (x < 0)
    return V_LT;
  if (x > 0)
    return V_GT;
  return V_EQ;
}

template <typename Policy, typename Type>
inline Result
cmp_generic(const Type& x, const Type& y) {
  if (x > y)
    return V_GT;
  if (x < y)
    return V_LT;
  return V_EQ;
}

template <typename Policy, typename To>
inline Result
round(To& to, Result r, const Rounding& mode) {
  switch (mode.direction()) {
  case Rounding::DOWN:
    if (r == V_LT)
      return static_cast<Result>(pred<Policy>(to) | V_GT);
    return r;
  case Rounding::UP:
    if (r == V_GT)
      return static_cast<Result>(succ<Policy>(to) | V_LT);
    return r;
  default:
    return r;
  }
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
