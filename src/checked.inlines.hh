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
  static inline Result function(Type& to, const Type& from, Rounding_Dir) {
    to = from;
    return V_EQ;
  }
};

template <typename Policy, typename To, typename From>
inline Result
abs_generic(To& to, const From& from, Rounding_Dir dir) {
  if (from < 0)
    return neg<Policy>(to, from, dir);
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
gcd_common(To& to, const From& x, const From& y, Rounding_Dir dir) {
  To nx = x;
  To ny = y;
  To rm;
  while (ny != 0) {
    Result r = rem<Policy>(rm, nx, ny, dir);
    assert(r == V_EQ);
    nx = ny;
    ny = rm;
  }
  to = nx;
  return V_EQ;
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
gcd_generic(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (x == 0)
    return abs<Policy>(to, y, dir);
  if (y == 0)
    return abs<Policy>(to, x, dir);
  To nx, ny;
  Result r;
  used(r);
  r = abs<Policy>(nx, x, dir);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y, dir);
  assert(r == V_EQ);
  return gcd_common<Policy>(to, nx, ny, dir);
}

template <typename Policy, typename To, typename From1, typename From2>
inline Result
lcm_generic(To& to, const From1& x, const From2& y, Rounding_Dir dir) {
  if (x == 0 || y == 0) {
    to = 0;
    return V_EQ;
  }
  To nx, ny;
  Result r;
  r = abs<Policy>(nx, x, dir);
  assert(r == V_EQ);
  r = abs<Policy>(ny, y, dir);
  assert(r == V_EQ);
  To gcd;
  r = gcd_common<Policy>(gcd, nx, ny, dir);
  assert(r == V_EQ);
  r = div<Policy>(to, nx, gcd, dir);
  assert(r == V_EQ);
  return mul<Policy>(to, to, ny, dir);
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

template <typename Policy>
inline bool
want_rounding(Rounding_Dir dir) {
  return dir != ROUND_IGNORE &&
    (Policy::use_corrent_rounding || dir != ROUND_CURRENT);
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
